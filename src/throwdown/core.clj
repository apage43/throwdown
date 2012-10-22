(ns throwdown.core
  (:import [javax.xml.transform.stream StreamSource] 
           [org.apache.commons.lang3.text WordUtils StrBuilder])
  (:refer-clojure :exclude [print println])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-yaml.core :as yaml]
            [clojure.data.xml :as xml])
  (:gen-class))

(defn despace [s]
  (string/replace
    (string/trim (string/join " " (map string/trim (string/split-lines s))))
    #" [.,]"
    #(subs % 1)))

(def tab-stop 4)

;; XML translation to internal intermediate representation

(defn el-text-raw [el]
  (string/replace 
    (apply str
           (filter string?
                   (tree-seq :content :content el)))
    "\t" (apply str (repeat tab-stop \space))))

(defn el-text [el]
  (despace (el-text-raw el)))

(defn select [el pred]
  (filter pred (tree-seq :content :content el)))

(defn select-tagname [el tagname]
  (select el (comp (partial = tagname) :tag)))

(declare process)

(defn process-table [el]
  (let [body (first (select-tagname el :tbody))
        head (first (select-tagname el :thead))
        rowlist (map (fn [r] (mapv #(map process (:content %)) (:content r))) (:content body))
        headrow (when head (mapv #(map process (:content %)) (-> head :content first :content)))]
    (merge 
      {:type :table
       :head (or headrow (first rowlist))
       :rows (if headrow rowlist (rest rowlist))}
      (when-let [id (-> el :attrs :id)] {:id id}))))

(defn process [el]
 (cond
   (map? el) (case (:tag el)
               ;section-like elements
               (:chapter :book :section :appendix :preface)
               (merge (:attrs el)
                      {:type (:tag el)
                       :name (el-text (first (select-tagname el :title)))
                       :content (map process (:content el))}) 
               
               :orderedlist {:type :olist
                             :content (map #(map process (:content %)) (:content el))}
               (:simplelist :itemizedlist) {:type :list
                             :content (map #(map process (:content %)) (:content el))}

               :para {:type :para :content (map process (:content el))} 

               :ulink {:type :link :text (el-text el) :href (-> el :attrs :url)}
               :xref [:xref (-> el :attrs :linkend)]

               :link (merge {:type :xlink :content (map process (:content el))} (:attrs el)) 

               (:figure :mediaobject) {:type :image
                        :alt (el-text (select-tagname el :title))
                        :href (-> (select-tagname el :imagedata) first :attrs :fileref) }

               (:table :informaltable) (process-table el)

               :programlisting (merge (:attrs el)
                                      {:type :code
                                       :code (el-text-raw el)}) 

               :remark {:type :comment :attrs (:attrs el)}

               ; TODO find some way to deal with these extra class types
               ; Markdown will not render markdown in block-level elements, but
               ; these would be <div> classes. :/
               ; Non-titled section-like elements
               (:example :warning :tip :note :formalpara :bookinfo :abstract :legalnotice :corpauthor) 
               {:type :div :class (:tag el) :content (map process (:content el))} 

               ; inline code type elements
               ; Could affix <span> classes to these if we wanted
               (:guilabel :command :filename :literal :option :methodname :replaceable :function :userinput)
               {:type :inline-code :code (el-text el) :class (:tag el)}
               
               :emphasis (case (-> el :attrs :role)
                           "bold" [:bold (el-text el)]
                           [:em (el-text el)]) 

               :title [:title (el-text el)] 

               [:unknown-tag (:tag el)])
   (string? el) (despace el) 
   :else [:unknown-thing el]))

(defn extract-toc [procd]
  (walk/postwalk
    (fn [pel]
      (if
        (and (map? pel)
             (#{:section :chapter :book :appendix :preface} (:type pel))) 
        (let [itms (filter #(and (map? %) (:name %)) (:content pel))]
          (merge {:name (str (when (= :appendix (:type pel)) "Appendix: ") (:name pel)) 
                  :section (:id pel)}
                 (when (seq itms) {:items itms}))) 
        pel))
    procd))

;; Markdown Printing

(def wrap-buffer (StrBuilder.))
(def wrap-column 80)

(defn print [& args]
  (binding [*out* (.asWriter wrap-buffer)]
    (apply clojure.core/print args)))

(defn pflush []
  (clojure.core/print (WordUtils/wrap (despace (str wrap-buffer)) wrap-column))
  (.clear wrap-buffer))

(defn println [& args]
  (pflush)
  (apply clojure.core/println args))

(defn indented-println [in s]
  (case s 
    "" (println) ; Don't indent empty lines
    (println (str (apply str (repeat in " ")) s))))

(defn reindented-print [in s]
  (let [lines (drop-while (partial = "") (string/split-lines s)) 
        orig-indent (apply min 1000 (map #(count (take-while (partial = \space) %)) 
                                         (filter (complement string/blank?) lines)))]
    (doseq [cl lines]
      (indented-println in (string/trimr (apply str (drop orig-indent cl)))))))

(defn print-code [s & [lang]]
  (when lang (indented-println 4 (str "`" lang)))
  (reindented-print 4 s))

(defn code-escape [s]
  (string/escape s {\` "\\`"}))

(defn text-escape [s]
  (string/escape s {\* "\\*"
                    \_ "\\_"
                    \# "\\#"
                    \[ "\\[" 
                    \] "\\]"}))

(defn para-escape 
  [s] (str
        (string/escape (subs s 0 1)
                       {\* "\\*"
                        \> "\\>"
                        \: "\\:"}) (subs s 1)) )

(declare mdprint)

(defn table-row-printify [r opts]
  (mapv (fn [ri]
          (despace (with-out-str
                     (doseq [rel ri] (mdprint rel opts))
                     (pflush)))) r))

(defn print-mdtable [el opts]
  (let [printable-head (table-row-printify (:head el) opts)
        printable-rows (map #(table-row-printify % opts) (:rows el))
        printable-all (conj printable-rows printable-head)
        widths (map (fn [hidx]
                      (apply max (count (printable-head hidx))
                             (map #(count (% hidx)) printable-rows))) (range (count printable-head)))
        fmts (map #(str "%-" % "s") widths)
        fmt-row (fn [row]
                  (apply str (interpose " | " (map (fn [fmt td] (format fmt td)) fmts row))))
        separator (apply str (interpose "-|-" (map #(apply str (repeat % \-)) widths)))]
    (println (fmt-row printable-head))
    (println separator)
    (doseq [r printable-rows] (println (fmt-row r)))
    (println)))

(defn print-list [els bullets opts]
  (loop [el-list els
         bulls bullets]
    (when-not (empty? el-list)
      (let [bullet (first bulls)
            el (first el-list)]
        (println
          (str bullet
               (subs
                 (with-out-str 
                   (reindented-print
                     (count bullet) 
                     (with-out-str
                       (doseq [ie el]
                         (mdprint ie opts))
                       (pflush))))
                 (count bullet)))))
      (recur (rest el-list) (rest bulls)))))

(defn xurl [xitem sid opts]
  (str (if (not= (:file xitem) (:current-doc opts))
         (str (:file xitem) ".html") "")
       "#" sid))

(defn mdprint [el opts]
  (cond (map? el) (do
                    ; Emit an HTML ID'd empty anchor for xref links on ID'd elements
                    (when (:id el) (println (str "<a id=\"" (:id el) "\"></a>\n")))
                    (case (:type el)
                      :comment nil

                      (:chapter :section :book :appendix :preface)
                      (do
                        (doseq [e (:content el)]
                          (mdprint e (assoc opts :level (:type el))))) 

                      :div
                      (doseq [e (:content el)] (mdprint e (assoc opts :class (:class el))))

                      :link (print (str " [" (text-escape (:text el)) "](" (:href el) ") ")) 
                      :image (println (str "\n![" (text-escape (:alt el)) "](" (:href el) ")\n")) 
                      :list (print-list (:content el) (repeat " *  ") opts) 
                      :olist (print-list (:content el)
                                         (map #(str " " % ".  ") (iterate inc 1)) opts)
                      :code (do
                              (println)
                              (when (:in-para opts) (println))
                              (print-code (:code el) (:language el))
                              (println))

                      :para (do
                              (println (para-escape
                                         (with-out-str 
                                           (doseq [e (:content el)]
                                             (mdprint e (assoc opts :in-para true)))
                                           (println)))))

                      :xlink (let [xitem ((:xref-index opts) (:linkend el))]
                               (if xitem
                                 (print (str " [" (despace
                                                    (with-out-str
                                                      (doseq [e (:content el)] (mdprint e opts))
                                                      (pflush))) 
                                             "](" (xurl xitem (:linkend el) opts) ") "))
                                 (print (str " **Couldn't resolve link tag: " (text-escape (:linkend el)) "** "))))

                      :table (print-mdtable el opts)

                      :inline-code (when-not (empty? (:code el)) (print (str " `" (:code el) "` "))) 

                      (doseq [e (:content el)]
                        (print (str " **Unhandled containery thing:** `" (:type el) "` "))
                        (mdprint e opts)))) 
        (vector? el) (case (first el)
                       :em (print (str " *" (text-escape (second el)) "* "))
                       :bold (print (str " **" (text-escape (second el)) "** "))

                       :xref (let [xitem ((:xref-index opts) (second el))]
                               (if xitem
                                 (print (str " [" (:text xitem) "](" (xurl xitem (second el) opts) ") "))
                                 (print (str " **Couldn't resolve xref tag: " (text-escape (second el)) "** "))))

                       :title (if (:class opts)
                                (println (str "### `" (second el) "`\n"))
                                (println (str ({:book "# "
                                                :chapter "# "
                                                :section "## "
                                                :preface "## "
                                                :appendix "#Appendix: "} (:level opts))
                                              (second el) "\n"))) 
                       (print (str " **Unhandled:** `" (pr-str el) "` ")))
        (string? el) (print (text-escape el))
        :else (println "**Unhandled thing here**")))

(defn process-file [file]
  (let [xml-tree (xml/parse (StreamSource. file))]
    (process xml-tree)))

(defn -main
  [outdir & args]
  (let [processed (atom {})
        toc (atom [])
        id-index (atom {})
        frontmatter {:toc outdir}]
    (doseq [f args] 
      (println "Processing" f)
      (let [file (io/file f)
            fname (first (string/split (.getName file) #"\.")) 
            procd (process-file file)]
        (swap! processed assoc fname procd)
        (swap! toc conj (assoc (extract-toc procd) :root (str outdir "/" fname)))
        (swap! id-index merge
               (reduce (fn [acc el] (assoc acc (:id el) {:file fname :text (:name el)}))
                       {} (filter #(when (map? %) (:id %)) (tree-seq :content :content procd))))))
    (doseq [[fname procd] @processed]
      (let [mdfile (io/file (str outdir "/" fname ".md"))]
        (println "Writing" fname)
        ;(spit (str outdir "/" fname ".dbgir") (with-out-str (pprint procd)))  ;debugging
        (io/make-parents mdfile)
        (with-open [mdwriter (io/writer mdfile)]
          (binding [*out* mdwriter]
            (println (str"---\n" (yaml/generate-string frontmatter) "\n---\n"))
            (mdprint procd {:xref-index @id-index :current-doc fname})))))
    (spit (str outdir "/" outdir ".yml")
          (yaml/generate-string [{:name outdir :items @toc :root outdir}]))))
