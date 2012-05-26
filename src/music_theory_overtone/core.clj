(ns music_theory_overtone.core)
(use '[overtone.live :only [at now ctl stop note metronome o] ])

;; First define some player functions. These have been gratuitously
;; stolen from Chris Ford's most exellent Goldberg variations
;; presentation here : https://github.com/ctford/goldberg

;; We are going to use a synthesised organ sound to play the notes.
;; It's in a separate namespace so we better go include it
(use 'music_theory_overtone.orchestra)
(defn play# [metro notes]
  
  (let [
        start-beat (metro)
        beat-length (/ 1 (/ (metro :bpm) 60))
        play-at# (fn [[offset dur midi]]
                   (at (metro (+ start-beat offset)) (organ-cornet-stop :note midi :dur (* beat-length dur))))]
    (->> notes (map play-at#) dorun)))

(defn even-melody [pitches]
  (let [
        times (range)]
     (map vector times (repeat 1) pitches)))

(defn chord [pitches]
  (map vector (repeat 0) (repeat 1) pitches ))

;; I am pretty sure this is a bad idea - just creating a metronome
;; like this
(def play-chord# #(play# (metronome 100) (chord %)))

(defn play-even-melody# [tempo pitches]
  (play# (metronome tempo) (even-melody pitches)))
;; just check that it is working with a couple of notes

(play-even-melody# 160 [60 61 62 63])
(play-chord# [60 64 67])
;; This tutorial aims to introduce a few of the basics of scales,
;; chords and basic harmony. Bias towards Western and mainly classical
;;music as that is what I learnt!

;; If you were to sit down at a keyboard and play every note you would
;; get something like this:

(play-even-melody# 350 (range (note :C1) (note :C5)))

;; You might try and pick out a tune on said piano. Hitting random
;; notes might sound something like this

(play-even-melody# 150 (take 20 (repeatedly #(rand-nth (range (note :C4) (note :C5))))))

;; Eval that a few times. Sounds random (cos it is) and not exactly
;; 'tuneful'. Possibly an infinite number of monkeys with an infinite
;; number of slime sessions might end up with something that sounds
;; good. But without these kind of resources, how do we end up with
;; melody?
(play-even-melody# 200 [55 57 59 62 60 60 64 62 62 67 66 67 62 59 55 57 59 60 62 64 62 60 59 57 59 55 54 55 57 50 54 57 60 59 57 ] )
;; Herts und Mund und Tat und Leben (Jesu Joy of Man's Desiring)
;; JS  Bach BWV 147
;; http://en.wikipedia.org/wiki/Jesu,_Joy_of_Man's_Desiring