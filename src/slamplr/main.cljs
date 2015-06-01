(ns slamplr.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <! >!]]))


(enable-console-print!)

(defonce file-chan (chan 10))
(defonce sample-chan (chan 10))

(comment
  {:files [{
            :data []
            :analysis []
            :name ""
            :selection [0 0]
            }]})

(defonce app-state (atom {:files []}))

(defonce audio-context (js/AudioContext.))

(defn add-sample! [sample]
  (swap! app-state update-in [:files] conj sample)
  [:files (dec (count (:files @app-state)))])

(defn swap-closest [x selection]
  (let [pairs (sort-by first (map (fn [pt] [(Math/abs (- pt x )) pt]) selection))]
    (vec (sort [x (last (last (rest pairs)))]))))

(defn play
  ( [buffer]
   (let [buffer-source (.createBufferSource audio-context)]
     (set! (.-buffer buffer-source) buffer)
     (.connect buffer-source (.. audio-context -destination)
               (.start buffer-source 0))))

  ( [buffer selection]
   (let [
         slice-range (map #(Math/round (* (.-length buffer) %)) selection)
         slice-length (reduce - (reverse slice-range))
         slice-buffer (.createBuffer audio-context (.-numberOfChannels buffer) slice-length (.-sampleRate buffer))
         start (first slice-range)
         dest (.getChannelData slice-buffer 0)
         src (.getChannelData buffer 0)
         ]
     (doseq [i (range slice-length)]
       (aset dest i (aget src (+ start i))))
     (play slice-buffer))))

(defn load-into-file-buffer [file]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) (fn [e]
                              (.decodeAudioData audio-context (.. reader -result) (fn [audio-data]
                                                                                    (put! sample-chan {:name (.. file -name) :data audio-data :selection [0 1]})))))
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
                              (doseq [file (.. e -dataTransfer -files)]
                                (put! file-chan file)))
                    :onDragOver (fn [e] (.preventDefault e) (.stopPropagation e))
                    :onDragEnter (fn [e] (.preventDefault e) (.stopPropagation e))
                    } "drop files here"))))

(defn scale [points width height]
  (let [
        x-step (/ width (- (count points) 1))
        y-scale (/ height 2)]
    (map (fn [i y] [(* x-step i) (+ (* y y-scale) y-scale)]) (range) points)))

(defn f->% [f] (str (* f 100) "%"))

(defn join [delimiter s]
  (apply str (interpose delimiter s)))

(defn waveform [points owner]
  (reify
    om/IRender
    (render [_]
      (let [resolution 1000
            height 100
            groups (partition-all (/ (count points) resolution) points)
            points->attr (fn [agg] (join " " (flatten (scale (concat [0] (map (partial apply agg) groups) [0]) resolution height))))]
        (dom/svg #js {:width (f->% 1) :height (f->% 1) :viewBox (join " " [0 0 resolution height])}
                 (dom/polygon #js {:points (points->attr max)})
                 (dom/polygon #js {:points (points->attr min)}))))))

(defn css-offsets [[start stop]]
  {:left (f->% start) :width (f->% (- stop start))})

(defn drag-handle [path]
  (fn [cursor owner]
    (reify
      om/IRenderState
      (render-state [_ state]
        (dom/div #js {:className "drag-handle"
                      :onMouseDown (fn [e]
                                     (.preventDefault e)
                                     (let [ dom-rect (.getBoundingClientRect (om/get-node owner))]
                                       (om/set-state! owner :drag-start-position (- (.-pageX e) (.-left dom-rect)))))
                      :onMouseMove (fn [e]
                                     (.preventDefault e)
                                     (when-let [start (:drag-start-position state)]
                                       (let [ dom-rect (.getBoundingClientRect (.. (om/get-node owner) -parentNode -parentNode))
                                              new-end (/ (- (- (.-pageX e) start) (.-left dom-rect)) (.-width dom-rect))]
                                         (print new-end)
                                         (om/update! cursor path new-end))))
                      :onMouseUp   (fn [e]
                                     (.preventDefault e)
                                     (om/set-state! owner :drag-start-position nil))
                      :onMouseOut  (fn [e]
                                     (.preventDefault e)
                                     (om/set-state! owner :drag-start-position false))
                      } nil)))))
(def left-handle (drag-handle [0]))
(def right-handle (drag-handle [1]))

(defn file-item [file owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
              (dom/h1 nil (:name file))
              (dom/button #js {:onClick (fn [e]
                                          (.preventDefault e)
                                          (play (:data file) (:selection file)))} "Play")
              (dom/div nil
                       (dom/div #js {:className "selection"
                                     :style (clj->js (css-offsets (:selection file)))}
                          (om/build left-handle (:selection file))
                          (om/build right-handle (:selection file)))
                       (om/build waveform (:analysis file)))))))

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

(deftype FileList [l i]
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [_] (.item l i))
  INext
  (-next [_] (if (< (inc i) (.. l -length))
               (FileList. l (inc i))
               nil)))

(extend-protocol ISeqable
  js/Float32Array
  (-seq [array] (IndexedSeq. array 0))
  js/FileList
  (-seq [l] (FileList. l 0)))
