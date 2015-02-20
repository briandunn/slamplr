(ns main
  (:require-macros  [cljs.core.async.macros :refer  [go]])
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
  (let [s (or selection [0 0])
        pairs (sort-by first (map (fn [pt] [(Math/abs (- pt x )) pt]) s))]
    (vec (sort [x (last (last (rest pairs)))]))))

(defn play [data]
  (let [buffer-source (.createBufferSource audio-context)]
    (set! (.-buffer buffer-source) data)
    (.connect buffer-source (.. audio-context -destination)
    (.start buffer-source 0))))

(defn load-into-file-buffer [file]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) (fn [e]
                              (.decodeAudioData audio-context (.. reader -result) (fn [audio-data]
                                                                                    (put! sample-chan {:name (.. file -name) :data audio-data})))))
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

(defn summarize [points resolution agg]
  (concat [0] (map agg (partition-all (/ (count points) resolution) points)) [0]))

(defn stroke-path [ctx path]
  (.beginPath ctx)
  (let [[x y ] (first path)] (.moveTo ctx x y))
  (doseq [[x y] (next path)] (.lineTo ctx x y))
  (.stroke ctx)
  (.fill ctx)
  (.closePath ctx))

(defn get-coords [dom e]
  (let [x (- (.-pageX e) (.-offsetLeft dom))
        y (- (.-pageY e) (.-offsetTop dom))]
    [x y]))

(defn repaint [ctx points]
  (time
    (let [
          resolution (.. ctx -canvas -width)
          height (.. ctx -canvas -height)]
      (doseq [edge (map #(scale (summarize points resolution (fn [points] (apply % points))) resolution height) [max min])]
        (stroke-path ctx edge)))))

(defn waveform [file owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (repaint (.getContext (om/get-node owner) "2d") (:analysis file)))
    om/IRender
      (render [_]
        (dom/canvas #js {:width 1000 :height 100} nil))))

(defn css-offsets [[start stop]]
  {:left start :width (- stop start)})

(defn file-item [file owner]
  (reify
    om/IRender
      (render [_]
        (print (keys file))
        (dom/li nil
                (dom/h1 nil (:name file))
                (dom/button #js {:onClick (fn [e]
                                (.preventDefault e)
                                (play (:data file)))} "Play")
                (dom/div #js { :onClick (fn [e]
                                          (.preventDefault e)
                                          (let [dom (om/get-node owner)
                                                [x y] (get-coords dom e)]
                                            (om/transact! file :selection (partial swap-closest x))))}
                  (dom/div #js {:className "selection" :style (clj->js (css-offsets (:selection file)))} nil)
                  (om/build waveform file))))))

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
