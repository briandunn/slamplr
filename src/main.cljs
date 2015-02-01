(ns main
    (:require  [om.core :as om :include-macros true]
               [om.dom :as dom :include-macros true]))

(defonce app-state (atom {:files []}))
(defn load-files [files]
  (swap! app-state update-in [:files] into (map (fn [file] (.. file -name) ) files)))

(defn drop-zone [_]
  (reify
    om/IRender
      (render [this]
        (dom/div #js {
                      :id "drop-zone"
                      :onDrop (fn [e]
                                (.preventDefault e)
                                (.stopPropagation e)
                                (load-files (let [files (.. e -dataTransfer -files)]
                                              (map #(.item files %) (range (.. files -length)))
                                              )))
                      :onDragOver (fn [e] (.preventDefault e) (.stopPropagation e))
                      :onDragEnter (fn [e] (.preventDefault e) (.stopPropagation e))
                      } "drop files here"))))

(defn file-item [file]
  (reify
    om/IRender
      (render [_]
        (dom/li nil file))))

(defn file-list [files parent]
  (reify
    om/IRender
      (render [_]
        (apply dom/ul #js {:id "files"} (om/build-all file-item files)))))

(defn root [state parent]
  (reify
    om/IRender
      (render [_]
        (dom/main nil
          (om/build drop-zone nil)
          (om/build file-list (:files state))
        ))))

(om/root root app-state
  {:target (. js/document (getElementById  "app"))})
