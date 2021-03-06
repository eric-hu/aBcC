(defproject abcc "0.1.0-SNAPSHOT"
  :description "A bittorrent client, in clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.aphyr/prism "0.1.3"]
                 [clj-http "1.1.2"]
                 ; SHA-1 library
                 [digest "1.4.4"]]


  :plugins      [[com.aphyr/prism "0.1.1"]]
  :main abcc.core)
