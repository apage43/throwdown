(defproject throwdown "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 ; Woodstox is faster than the Stax impl shipped with JDK, and
                 ; will cache DTDs and other resources
                 [org.codehaus.woodstox/woodstox-core-asl "4.1.4"]
                 [org.apache.commons/commons-lang3 "3.1"]]
  :main throwdown.core)
