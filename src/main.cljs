(ns main
  (:require-macros  [cljs.core.async.macros :refer  [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <! >!]]))

(defonce file-chan (chan 10))
(defonce sample-chan (chan 10))

(defonce app-state (atom {:files []}))

(defonce audio-context (js/AudioContext.))

(defn add-sample! [sample]
  (swap! app-state update-in [:files] conj sample)
  [:files (dec (count (:files @app-state)))])

(defn play [data]
  (let [buffer-source (.createBufferSource audio-context)]
    (set! (.-buffer buffer-source) data)
    (.connect buffer-source (.. audio-context -destination)
    (.start buffer-source 0))))

(defn load-into-file-buffer [file]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) (fn [e]
                              (.decodeAudioData audio-context (.. reader -result) (fn [audio-data]
                                                                                    (put! sample-chan {:name (.. file -name) :data audio-data})
                                                                                    ))))
    (.readAsArrayBuffer reader file)))

(defn analyze [sample]
  (.getChannelData sample 0))

(go (loop []
      (when-let [sample (<! sample-chan)]
        (let [path (add-sample! sample)]
          (swap! app-state update-in path assoc :analysis (analyze (:data sample)))
          (recur)))))

(go
  (loop []
    (when-let [file (<! file-chan)]
      (load-into-file-buffer file)
      (recur))))

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
                                    (when (< i (.. files -length))
                                      (put! file-chan (.item files i))
                                      (recur (inc i))))))
                      :onDragOver (fn [e] (.preventDefault e) (.stopPropagation e))
                      :onDragEnter (fn [e] (.preventDefault e) (.stopPropagation e))
                      } "drop files here"))))

(defn float32array->seq [array]
  (.call (.. js/Array -prototype -slice) array))

(defn repaint
  [ctx pcm-data]
  (let [
        points (float32array->seq pcm-data)
        x-step (/ (.. ctx -canvas -width) (- (count points) 1))
        y-scale (/ (.. ctx -canvas -height) 2)
        scaled-points (map (fn [i y]
                        [(* x-step i) (+ (* y y-scale) y-scale)]) (range) points)]
    (.beginPath ctx)
    (.moveTo ctx 0 y-scale)
    (doseq [[x y] scaled-points]
      (.lineTo ctx x y))
    (.stroke ctx)))

(defn waveform [raw-array owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (repaint (.getContext (om/get-node owner) "2d") raw-array))
    om/IRender
      (render [_]
        (dom/canvas #js {:width 1000 :height 100} nil))))

(defn file-item [file]
  (reify
    om/IRender
      (render [_]
        (dom/li #js {:onClick (fn [_]
                                (play (:data file)))}
                (dom/h1 nil (:name file))
                (om/build waveform (:analysis file))))))

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
  {:target (. js/document (getElementById "app"))})


