(ns sudoku.core
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton JLabel JTextField))
(import 'java.awt.event.ActionListener)

(defmacro on-action [component event & body]
  `(. ~component addActionListener
      (proxy [ActionListener] []
	(actionPerformed [~event] ~@body))))

(defn make-nine
  ([f] (make-nine f 0 nil))
  ([f x lst] (if (== x 9)
	       lst
	       (make-nine f (inc x) (cons (eval f) lst)))))

(def inputs (make-nine (fn [] (make-nine (JTextField. 1)))))

(def inputs (make-array Object 9 9))
(def cells (make-array Integer/TYPE 9 9))

(dotimes [i 9]
  (dotimes [k 9]
    (aset inputs i k (JTextField. 1))))

(defn create-panel []
  (let [p (JPanel.)]
    (dotimes [i 9]
      (dotimes [k 9]
	(.add p (aget inputs i k))))
    p))

(defn clear-outputs []
  (dotimes [i 9]
    (dotimes [k 9]
      (.setText (aget inputs i k) ""))))

(defn sync-cells []
  (dotimes [i 9]
    (dotimes [k 9]
      (aset cells i k (cond (= (.getText (aget inputs i k)) "") 0
			    true (Integer/parseInt (.getText (aget inputs i k))))))))

(defn sync-outputs []
  (dotimes [i 9]
    (dotimes [k 9]
      (.setText (aget inputs i k) (str (aget cells i k))))))

(defn find-corner [index]
  (cond (> 3 index) 0
	(> 6 index) 3
	true 6))

(defn get-square-r [r c rindex radj cindex cadj]
  (cond (== cindex (+ cadj 3)) nil
	(== rindex (+ radj 3)) (get-square-r r c radj radj (+ cindex 1) cadj)
	(or (== rindex r) (== cindex c)) (get-square-r r c (+ rindex 1) radj cindex cadj)
	true (cons (aget cells rindex cindex) (get-square-r r c (+ rindex 1) radj cindex cadj))))

(defn get-square [r c]
  (get-square-r r c (find-corner r) (find-corner r) (find-corner c) (find-corner c)))

(defn row-except [r index c num]
  (cond (== index 9) true
	(== index r) (row-except r (inc index) c num)
	(== (aget cells index c) num) false
	true (row-except r (inc index) c num)))

(defn col-except [c index r num]
  (cond (== index 9) true
	(== index c) (col-except c (inc index) r num)
	(== (aget cells r index) num) false
	true (col-except c (inc index) r num)))

(defn valid-cell [r c num]
  (cond (not (row-except r 0 c num)) false
	(not (col-except c 0 r num)) false
	(some #{== num} (get-square r c)) false
	true true))

(defn fill-board [r c num]
  (cond (== c 9) true
	(== r 9) (fill-board 0 (inc c) 1)
	(== num 10) false
	(not (== 0 (aget cells r c))) (fill-board (inc r) c 1)
	(valid-cell r c num) (do (aset cells r c num)
				 (if (fill-board (inc r) c 1)
				   true
				   (do (aset cells r c 0)
				       (fill-board r c (inc num)))))
	true (fill-board r c (inc num))))

(defn create-frame []
  (let [start-button (JButton. "Solve")
	clear-button (JButton. "Clear")
	panel (doto (create-panel)
		(.add start-button)
		(.add clear-button))]
    (doto (JFrame. "Sudoku Solver")
      (.setSize 200 300)
      (.setContentPane panel)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (on-action start-button event (do (sync-cells)
				      (fill-board 0 0 1)
				      (sync-outputs)))
    (on-action clear-button event (clear-outputs))))

(defn -main [& args]
  (create-frame))
