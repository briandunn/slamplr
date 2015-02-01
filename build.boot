; vim: set ft=clojure:
; :dependencies '[[adzerk/boot-cljs      "0.0-2727-0" :scope "test"]
(set-env!
:source-paths   #{"src"}
:resource-paths #{"html"}
:dependencies '[[adzerk/boot-cljs      "0.0-2411-8" :scope "test"]
		[adzerk/boot-cljs-repl "0.1.7"      :scope "test"]
		[adzerk/boot-reload    "0.2.0"      :scope "test"]
		[pandeiro/boot-http    "0.3.0"      :scope "test"]
		[org.omcljs/om "0.8.7"]])

(require
'[adzerk.boot-cljs      :refer [cljs]]
'[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
'[adzerk.boot-reload    :refer [reload]]
'[pandeiro.http         :refer [serve]])
