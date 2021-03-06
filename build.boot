; vim: set ft=clojure:
(set-env!
  :source-paths   #{"src"}
  :resource-paths #{"html"}
  :dependencies '[[adzerk/boot-cljs       "0.0-2727-0" :scope "test"]
                  [adzerk/boot-cljs-repl  "0.1.9"      :scope "test"]
                  [adzerk/boot-reload     "0.2.4"      :scope "test"]
                  [pandeiro/boot-http     "0.6.2"      :scope "test"]
                  [org.clojure/clojure    "1.6.0"      :scope "provided"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha" :scope "provided"]
                  [org.omcljs/om          "0.8.8"] ])

(require
'[adzerk.boot-cljs      :refer [cljs]]
'[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
'[adzerk.boot-reload    :refer [reload]]
'[pandeiro.boot-http    :refer [serve]])

(deftask dev []
  (comp
    (serve :dir "target/")
    (watch)
    (reload)
    (speak)
    (cljs-repl)
    (cljs :unified true
          :source-map true)))
