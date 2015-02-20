; vim: set ft=clojure:
; :dependencies '[[adzerk/boot-cljs      "0.0-2727-0" :scope "test"]
(set-env!
:source-paths   #{"src"}
:resource-paths #{"html"}
:dependencies '[[adzerk/boot-cljs       "0.0-2411-8" :scope "test"]
                [adzerk/boot-cljs-repl  "0.1.7"      :scope "test"]
                [adzerk/boot-reload     "0.2.0"      :scope "test"]
                [pandeiro/boot-http     "0.3.0"      :scope "test"]
                [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                [org.omcljs/om          "0.8.7"]])

(require
'[adzerk.boot-cljs      :refer [cljs]]
'[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
'[adzerk.boot-reload    :refer [reload]]
'[pandeiro.http         :refer [serve]])

(deftask dev []
  (comp
    (serve :dir "target/")
    (watch)
    (reload)
    (speak)
    (cljs-repl)
    (cljs :unified true
          :source-map true)))
