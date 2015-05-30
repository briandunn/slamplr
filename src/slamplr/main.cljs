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

(defn summarize [points resolution agg]
  (concat [0] (map agg (partition-all (/ (count points) resolution) points)) [0]))

(defn relative-coords [dom e]
  (let [ dom-rect (.getBoundingClientRect dom)
        x (- (.-pageX e) (.-left dom-rect))
        y (- (.-pageY e) (.-top dom-rect))
        ]
    [(/ x (.-width dom-rect)) (/ y (.-height dom-rect))]))

(defn f->% [f] (str (* f 100) "%"))

(defn waveform [file owner]
  (reify
    om/IRender
    (render [_]
      (let [resolution 1000 height 100 points (:analysis file)]
        (apply dom/svg #js {:width (f->% 1) :height (f->% 1) :viewBox (str "0 0 " resolution " " height)}
               (om/build-all
                 (fn [agg owner]
                   (reify
                     om/IRender
                     (render [_]
                       (dom/polygon #js {:points (apply str (interpose " " (flatten (scale (summarize points resolution (fn [points] (apply agg points))) resolution height))))}))))
                 [min max]))))))

(defn css-offsets [[start stop]]
  {:left (f->% start) :width (f->% (- stop start))})

(defn file-item [file owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
              (dom/h1 nil (:name file))
              (dom/button #js {:onClick (fn [e]
                                          (.preventDefault e)
                                          (play (:data file) (:selection file)))} "Play")
              (dom/div #js { :onClick (fn [e]
                                        (.preventDefault e)
                                        (let [dom (om/get-node owner)
                                              [x y] (relative-coords dom e)]
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
