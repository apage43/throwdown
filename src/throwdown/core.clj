(ns throwdown.core
  (:import [javax.xml.transform.stream StreamSource])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-yaml.core :as yaml]
            [clojure.data.xml :as xml])
  (:gen-class))

(defn despace [s]
  (string/trim (string/join " " (map string/trim (string/split-lines s)))))

;; XML translation to internal intermediate representation

(defn el-text [el]
  (let [texts (transient [])]
    (walk/prewalk #(do (when (string? %) (conj! texts %))
                     (if (instance? clojure.data.xml.Element %)
                       (:content %) %)) el)
    (despace (string/join " " (persistent! texts)))))

(defn el-text-raw [el]
  (apply str (flatten (walk/prewalk #(if (instance? clojure.data.xml.Element %)
                                       (:content %) %) el))))

(defn select [el pred]
  (let [results (transient [])]
    (walk/prewalk #(if (instance? clojure.data.xml.Element %)
                     (do (when (pred %) (conj! results %)) (:content %)) %) el)
    (persistent! results)))

(defn select-tagname [el tagname]
  (select el (comp (partial = tagname) :tag)))

(defn process [el]
 (cond
   (map? el) (case (:tag el)
               ;section-like elements
               (:chapter :book :section :appendix)
               (merge (:attrs el)
                      {:type (:tag el)
                       :name (el-text (first (select-tagname el :title)))
                       :content (map process (:content el))}) 
               
               :orderedlist {:type :olist
                             :content (map (comp process first :content) (:content el))}
               :itemizedlist {:type :list
                              :content (map (comp process first :content) (:content el))}

               :para {:type :para :content (map process (:content el))} 

               :ulink {:type :link :text (el-text el) :href (-> el :attrs :url)}
               :xref [:xref (-> el :attrs :linkend)]

               :figure {:type :image
                        :alt (el-text (select-tagname el :title))
                        :href (-> (select-tagname el :imagedata) first :attrs :fileref) }

               :programlisting (merge (:attrs el)
                                      {:type :code
                                       :code (el-text-raw el)}) 
               ; TODO find some way to deal with these extra class types
               ; Markdown will not render markdown in block-level elements, but
               ; these would be <div> classes. :/
               (:example :warning :note) {:type (:tag el) :content (map process (:content el))} 

               ; inline code type elements
               (:guilabel :command :filename :literal :option :methodname :replaceable)
               [:inline-code (el-text el)]
               
               :emphasis (case (-> el :attrs :role)
                           :bold [:bold (el-text el)]
                           [:em (el-text el)]) 

               :title [:title (el-text el)] 

               [:unknown-tag (:tag el)])
   (string? el) (despace el) 
   :else [:unknown-thing el]))

(defn chapter-toc [procd]
  {:pre [(map? procd)
         (= :chapter (:type procd))]}
  (walk/postwalk
    (fn [pel]
      (if
        (and (map? pel)
             (#{:section :chapter} (:type pel))) 
        (let [itms (filter #(and (map? %) (:name %)) (:content pel))]
          (merge {:name (:name pel) 
                  :section (:id pel)}
                 (when (not (empty? itms)) {:items itms}))) 
        pel))
    procd))

;; Markdown Printing

(defn indented-println [in s]
  (case s 
    "" (println) ; Don't indent empty lines
    (println (str (apply str (repeat in " ")) s))))

(defn print-code [s & [lang]]
  (let [code-lines (drop-while (partial = "") (string/split-lines s)) 
        orig-indent (apply min (map #(count (take-while (partial = \space) %)) 
                                    (filter (complement string/blank?) code-lines)))]
    (when lang
      (indented-println 4 (str "`" lang)))
    (doseq [cl code-lines]
      (indented-println 4 (string/trimr (apply str (drop orig-indent cl)))))))

(defn code-escape [s]
  (string/escape s {\` "\\`"}))

(defn text-escape [s]
  (string/escape s {\* "\\*"
                    \_ "\\_"
                    \# "\\#"
                    \[ "\\["
                    \] "\\]" }))

(declare mdprint)

(defn print-list [indent-init els bullets opts]
  (loop [el-list els
         bulls bullets]
    (when-not (empty? el-list)
      (let [bullet (first bulls)
            el (first el-list)]
        (indented-println
          indent-init
          (str bullet (with-out-str
                        (mdprint el (assoc opts :indent (+ indent-init (count bullet)))))))) 
      (recur (rest el-list) (rest bulls))))
  (println))

(defn mdprint [el opts]
  (cond (map? el) (case (:type el)
                    (:chapter :section :book)
                    (do
                      ; HACK! Emit an HTML ID'd empty anchor for xref links
                      (println (str "<a id=\"" (:id el) "\"></a>\n")) 
                      (doseq [e (:content el)]
                        (mdprint e (assoc opts :level (:type el))))) 

                    (:example :warning :note)
                      (doseq [e (:content el)] (mdprint e (assoc opts :class (:type el))))

                    :link (print (str " [" (text-escape (:text el)) "](" (:href el) ") ")) 
                    :image (println (str "\n![" (text-escape (:alt el)) "](" (:href el) ")\n")) 
                    :list (print-list (:indent opts 0) (:content el) (repeat " * ") opts) 
                    :olist (print-list (:indent opts 0) (:content el)
                                       (map #(str " " % ". ") (iterate inc 1)) opts)
                    :code (do
                            (print-code (:code el) (:language el))
                            (println))
                    :para (do
                            (println (despace (with-out-str (doseq [e (:content el)] (mdprint e opts)))))
                            (when-not (:indent opts) (println))) 
                    (doseq [e (:content el)]
                      (print (str " **Unhandled containery thing:** `" (:type el) "` "))
                      (mdprint e opts)))
        (vector? el) (case (first el)
                       :inline-code (print (str " `" (second el) "` "))
                       :em (print (str " *" (text-escape (second el)) "* "))
                       :bold (print (str " **" (text-escape (second el)) "** "))

                       :xref (let [xitem ((:xref-index opts) (second el))]
                               (if xitem
                                 (print (str " [" (:text xitem)"](" (:file xitem) ".html) "))
                                 (print (str " **Couldn't resolve xref tag: " (second el) "** "))))

                       :title (condp = (:class opts)
                                :example (println (str "### `" (second el) "`\n"))
                                (println (str ({:book "#" :chapter "#" :section "##"} (:level opts))
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
        (let [fileidx (transient {})]
          (walk/prewalk #(do
                           (when (:id %) (assoc! fileidx (:id %) {:file fname :text (:name %)}))
                           (when (= :chapter (:type %))
                             (swap! toc conj (assoc (chapter-toc %)
                                                    :root (str outdir "/" fname))))
                           %) procd)
          (swap! id-index merge (persistent! fileidx)))))
    (doseq [[fname procd] @processed]
      (let [mdfile (io/file (str outdir "/" fname ".md"))]
        ;(spit (str outdir "/" fname ".dbgir") (with-out-str (pprint procd)))  ;debugging
        (io/make-parents mdfile)
        (with-open [mdwriter (io/writer mdfile)]
          (binding [*out* mdwriter]
            (println (str"---\n" (yaml/generate-string frontmatter) "\n---\n"))
            (mdprint procd {:xref-index @id-index})))))
    (spit (str outdir "/" outdir ".yml")
          (yaml/generate-string [{:name outdir :items @toc :root outdir}]))
    (pprint @toc)))
