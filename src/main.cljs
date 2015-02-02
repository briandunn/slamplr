(ns main
    (:require  [om.core :as om :include-macros true]
               [om.dom :as dom :include-macros true]))

(defonce app-state (atom {:files []}))

(def audio-context (js/AudioContext.))

(defn add-file-buffer! [buffer] (swap! app-state update-in [:files] conj buffer))

(defn load-into-file-buffer [file]
  (let [reader (js/FileReader.) buffer-source (.createBufferSource audio-context)]
    (set! (.-onload reader) (fn [e]
                              (.decodeAudioData audio-context (.. reader -result) (fn [audio-data]
                                                                             (set! (.-source buffer-source) audio-data)
                                                                             (.connect buffer-source (.. audio-context -destination))
                                                                             (add-file-buffer! buffer-source)
                                                                             ))))

    (.readAsArrayBuffer reader file)))

(defn drop-zone [_]
  (reify
    om/IRender
      (render [this]
        (dom/div #js {
                      :id "drop-zone"
                      :onDrop (fn [e]
                                (.preventDefault e)
                                (.stopPropagation e)
                                (let [files (.. e -dataTransfer -files)]
                                  (loop [i 0]
                                    (if (< i (.. files -length))
                                      (load-into-file-buffer (.item files i))
                                      (recur (inc i))))))
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
          (om/build file-list (:files state))))))

(om/root root app-state
  {:target (. js/document (getElementById  "app"))})
