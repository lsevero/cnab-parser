(defproject cnab-parser "0.0.10"
  :description "Parser de CNAB de bancos brasileiros"
  :url "https://github.com/lsevero/cnab-parser"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging "0.6.0"]]
  :profiles {:dev {:repl-options {:init-ns cnab-parser.core}
                   :global-vars {*warn-on-reflection* true}
                   :plugins [[cider/cider-nrepl "0.24.0"]]
                   :source-paths ["src" "test"]
                   }}
  :source-paths ["src"]
  :resource-paths ["resources"]
  :repl-options {:port 17001}
  )
