(ns music_theory_overtone.orchestra (:use overtone.core))

(defcgen triangle-osc [freq phase {:default 0.0} harmonics {:default 40}]
  (:ar  (let
            [
             harmonic-numbers (take harmonics (iterate (partial + 2) 1))
             cosines (set (map #(- (* 4 %) 1) (range 1 harmonics))) ;; every 4n -1 is
             ;; there a better way?!
             ]
          (klang [
                  (map #(* freq %) harmonic-numbers) ;; harmonics
                  (map #(/ 1.0 (* % %)) harmonic-numbers) ;; inverse square ampl
                  (map #(+ phase %) (map #(if (cosines %) (. Math PI) 0.0) harmonic-numbers)) ;; conditional phase shift by pi
                  ]))))

(defcgen organ-env [dur {:default 1.0} vol {:default 1.0}]
  ( :kr
    (* vol
       (env-gen (asr 0.1 1.0 0.5) (line:kr 1.0 0.0 dur) :timeScale dur :action FREE)))
  )

(definst organ-cornet-stop [note 60 dur 1.0 vol 1.0]
  (let [freq (midicps note)]
    (*
     (organ-env :dur dur :vol vol)
     (apply + (map #(triangle-osc (* freq %)) (range 1 5)))
     0.25)))
