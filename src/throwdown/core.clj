(ns throwdown.core
  (:import [javax.xml.transform.stream StreamSource]
           [org.apache.commons.lang3.text WordUtils StrBuilder])
  (:refer-clojure :exclude [print println])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.data.xml :as xml])
  (:gen-class))

(def opts (atom {}))

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

(def kill-ids
  #{"conventions"
    "contributing"})

(defn kill-table [el]
  ;;Kill tables whose id ends with -summary, begins with couchbase
  (if-let [id (:id (:attrs el))]
    (re-matches #".*-summary" id)))

(defn process [el]
  (cond
    (map? el) (case (:tag el)
                ;section-like elements
                (:chapter :book :section :appendix :preface)
                (if (kill-ids (:id (:attrs el))) ""
                  (merge (:attrs el)
                         {:type (:tag el)
                          :name (el-text (first (select-tagname el :title)))
                          :content (map process (:content el))}))

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

               (:table :informaltable)
               (if (kill-table el) (clojure.core/println "Killing table!") (process-table el))

               (:screen :programlisting) (merge (:attrs el)
                                      {:type :code
                                       :code (el-text-raw el)})

               :remark {:type :comment :attrs (:attrs el)}

               ; KILL these!
               (:bookinfo)
               ""

               ; TODO find some way to deal with these extra class types
               ; Markdown will not render markdown in block-level elements, but
               ; these would be <div> classes. :/
               ; Non-titled section-like elements
               (:example :warning :tip :note :formalpara :abstract :legalnotice
                :corpauthor :quote :important :sidebar :caution :bridgehead
                :footnote :informalfigure)
               {:type :div :class (:tag el) :content (map process (:content el))}

                ; inline code type elements
                ; Could affix <span> classes to these if we wanted
                (:guilabel :command :filename :literal :option :methodname
                           :replaceable :function :userinput :classname :guimenu :guibutton
                           :guimenuitem :superscript :firstterm)
                (let [code (el-text el)
                      tag (:tag el)
                      inlineable? (not (or (.contains code "`")
                                           (.contains code "\n")))]
                  {:type (if inlineable?
                           :inline-code
                           :code)
                   :code (el-text el)
                   :class (:tag el)})

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

(defn code-escape [s]
  (string/escape s {\` "\\`"}))

(defn text-escape [s]
  (string/escape s {\* "\\*"
                    \_ "\\_"
                    \# "\\#"
                    \[ "\\["
                    \] "\\]"}))

(defn para-escape
  [s] (if-not (empty? s)
        (str
          (string/escape (subs s 0 1)
                         {\> "\\>"
                          \: "\\:"}) (subs s 1)) ""))

(def wrap-buffer (StrBuilder.))
(def wrap-column 80)

(defn print [& args]
  (binding [*out* (.asWriter wrap-buffer)]
    (apply clojure.core/print args)))

(defn pflush []
  (clojure.core/print (WordUtils/wrap (despace (para-escape (str wrap-buffer))) wrap-column))
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
        drop-indent (apply min 1000 (map #(count (take-while (partial = \space) %))
                                         (remove string/blank? lines)))
        lines (filter (partial not= "!NO-REINDENT") lines) ]
    (doseq [cl lines]
      (indented-println in (string/trimr (apply str (drop drop-indent cl)))))))

(defn print-code [code & [lang]]
  (println (str "```" (or (@opts :default-lang))))
  (println code)
  (println "```"))

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
                  (string/join " | " (map format fmts row)))
        separator (string/join "-|-" (map #(apply str (repeat % \-)) widths))]
    (println (fmt-row printable-head))
    (println separator)
    (doseq [r printable-rows] (println (fmt-row r)))
    (println)))

(defn print-list [els bullets opts]
  (loop [el-list els
         bulls bullets]
    (when (seq el-list)
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
                         (mdprint ie (assoc opts :in-list true)))
                       (pflush))))
                 (count bullet)))))
      (recur (rest el-list) (rest bulls)))))

(defn xurl [xitem sid opts] (str "#" sid))

(def real-out (atom *out*))
(defn debug [& args]
  (binding [*out* @real-out] (apply println args)))

(defn collect-image [href opts]
  (let [src-file (io/file (:current-doc opts))
        file-parent (.getParentFile src-file)
        imagefile (java.io.File. file-parent (str "../../../common/" href))
        imagefile (if (.exists imagefile) imagefile (java.io.File. file-parent href))]
    (if (.exists imagefile)
      (let [destname (str (:outname opts) "/images/"
                          (.getName imagefile))
            dest (io/file (str "content/" destname))]
        (io/make-parents dest)
        (io/copy imagefile dest)
        (debug "Copied" imagefile "->" dest)
        (str "images/" (.getName imagefile)))
      (do (debug "Couldn't find image" href)
          "/media/bad.png"))))

(defn mdprint [el opts]
  (cond (map? el) (do
                    ; Emit an HTML ID'd empty anchor for xref links on ID'd elements
                    (when (:id el) (println (str "<a id=\"" (:id el) "\"></a>\n")))
                    (case (:type el)
                      :comment nil

                      (:chapter :section :book :appendix :preface)
                      (doseq [e (:content el)]
                        (mdprint e (merge opts
                                          {:level (:type el)
                                           :depth (inc (or (:depth opts) 0))})))

                      :div
                      (doseq [e (:content el)] (mdprint e (assoc opts :class (:class el))))

                      :link (print (str " [" (text-escape (:text el)) "](" (:href el) ") "))
                      :image (println (str "\n![" (text-escape (:alt el)) "]("
                                           (collect-image (:href el) opts) ")\n"))
                      :list (print-list (:content el) (repeat " * ") opts)
                      :olist (print-list (:content el)
                                         (repeat " 1. ") opts)
                      :code (do
                              ;(when (:in-list opts) (println "!NO-REINDENT"))
                              (when-not (:in-list opts) (println))
                              ;(when (:in-para opts) (println))
                              (reindented-print (if (:in-list opts) 1 0)
                                                (with-out-str
                                                  (print-code (:code el) (:language el))))
                              (println))

                      :para (println (with-out-str
                                       (doseq [e (:content el)]
                                         (mdprint e (assoc opts :in-para true)))
                                       (println)))

                      :xlink (let [xitem ((:xref-index opts) (:linkend el))]
                               (if xitem
                                 (print (str " [" (despace
                                                    (with-out-str
                                                      (doseq [e (:content el)] (mdprint e opts))
                                                      (pflush)))
                                             "](" (xurl xitem (:linkend el) opts) ") "))
                                 ;(print (str " **Couldn't resolve link tag: " (text-escape (:linkend el)) "** "))
                                 (doseq [e (:content el)] (mdprint e opts))))

                      :table (print-mdtable el opts)

                      :inline-code (when (seq (:code el)) (print (str " `" (:code el) "` ")))

                      (doseq [e (:content el)]
                        (comment 
                          (print (str " **Unhandled containery thing:** `" (:type el) "` ")))
                        (mdprint e opts))))
        (vector? el) (case (first el)
                       :em (print (str " *" (text-escape (second el)) "* "))
                       :bold (print (str " **" (text-escape (second el)) "** "))

                       :xref (let [xitem ((:xref-index opts) (second el))]
                               (if xitem
                                 (print (str " [" (:text xitem) "](" (xurl xitem (second el) opts) ") "))
                                 (print (str " **Couldn't resolve xref tag: " (text-escape (second el)) "** "))))

                       :title (do (debug "Title: " (second el) ", depth: " (:depth opts))
                               (if (:class opts)
                                (println (str "### " (second el) "\n"))
                                (println (str ({:book "# "
                                                :chapter "--8<-- CUT HERE --8<--\n# "
                                                :section (let [d (or (:depth opts) 0)]
                                                           (if (> d 3)
                                                             "### "
                                                             "## "))
                                                :preface "## "
                                                :appendix "--8<-- CUT HERE --8<--\n# Appendix: "} (:level opts))
                                              (second el) "\n"))))
                       (comment (print (str " **Unhandled:** `" (pr-str el) "` "))))
        (string? el) (print (text-escape el))
        :else (comment (println "**Unhandled thing here**"))))

(defn process-file [file]
  (let [xml-tree (xml/parse (StreamSource. file))]
    (process xml-tree)))

(defn slugify [s]
  (-> s
      (string/replace #"[^A-Za-z0-9\.\-]+" "-")
      (string/replace #"^\." "")
      string/lower-case))

(defn make-slugger []
  (let [slugged (atom {})]
    (fn [s]
      (let [prevmap (swap! slugged update-in [s] (fnil inc -1))
            prev (prevmap s)
            base (slugify s)]
        (str base (when (pos? prev) "-" (inc prev)))))))

(defn identify-split [lines]
  (if-let [title-line (first (filter (partial re-matches #"^# .*") lines))]
    (string/trim (subs title-line 1)) ;else
    (-> "unknown-" gensym str)))

(defn make-splits
  "THIS IS SO HACKY :(
   THIS WHOLE THING IS TOO HACKY :("
  [with-cuts]
  (let [lines (string/split-lines with-cuts)
        chopped (->> lines
                     (partition-by (partial = "--8<-- CUT HERE --8<--"))
                     (filter (partial not= ["--8<-- CUT HERE --8<--"])))
        slugger (make-slugger)]
    (for [cut chopped]
      {:title (slugger (identify-split cut))
       :body (string/join "\n" cut)})))

(defn -main
  ([filename outname]
      (let [processed (atom {})
            id-index (atom {})]
        (reset! real-out *out*)
        (try
          (let [localopts (read-string (slurp ".throwdown-opts.clj"))]
            (println "Found local options: " (pr-str localopts))
            (reset! opts localopts))
          (catch Exception e))
        (let [f filename]
          (println "Processing" f)
          (let [file (io/file f)
                fname (first (string/split (.getName file) #"\."))
                procd (process-file file)]
            (swap! processed assoc fname procd)
            (swap! id-index merge
                   (reduce (fn [acc el] (assoc acc (:id el) {:file fname :text (:name el)}))
                           {} (filter #(when (map? %) (:id %)) (tree-seq :content :content procd))))))
        (doseq [[fname procd] @processed]
          (let [mdstring (with-out-str (mdprint procd {:outname outname :xref-index @id-index :current-doc filename}))
                sections (make-splits mdstring)]
            (doseq [{:keys [title body]} sections]
              (let [mdfile (io/file (str "content/" outname "/" title ".markdown"))]
                (io/make-parents mdfile)
                (spit mdfile body)))
            (with-open [index-writer (io/writer (str "content/" outname "/index.erb"))]
              (binding [*out* index-writer]
                (println (str "---\ntitle: " outname "\n---\n"))
                (doseq [{:keys [title]} sections]
                  (println (str "<%= include_item '" outname "/" title "' %>"))))))))))
