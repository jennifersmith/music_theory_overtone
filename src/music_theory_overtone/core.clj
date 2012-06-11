(ns music_theory_overtone.core)
(use '[overtone.live :only [at now ctl stop note metronome  midi->hz scale chord choose-n] ])

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
        expand-notes (fn [[ offset dur note-or-fn :as event]]
                       (if (fn? note-or-fn)
                         (for [note (note-or-fn)] [offset dur note] )
                         [event]))     ;;;eep
        play-at# (fn [[offset dur midi]]
                   
                   (at (metro (+ start-beat offset)) (organ-cornet-stop :note midi :dur (* beat-length dur))))]
    (->> notes
         (mapcat expand-notes)
         (remove #(some nil? %))
         (map play-at#) dorun)))

(defn even-melody [pitches]
  (let [
        times (range)]
     (map vector times (repeat 1) pitches)))

(defn barred-melody [bar-length pitches]
  (let [has-children? (comp coll? second)
        divide-bar (fn [[length notes]] (map vector (repeat (/ length (count notes))) notes ))
        notes-and-durations
        (->> (tree-seq has-children? divide-bar [ (* ( count pitches) bar-length) pitches])
             (remove has-children?))]
    (map cons
         (reductions + 0 (map first notes-and-durations)) notes-and-durations)))



(defn chord-event [start pitches]
  (for [pitch pitches] [start 1 pitch] ))
;; I am pretty sure this is a bad idea - just creating a metronome
;; like this
(defn play-chord# [& chord-pitches] (play# (metronome 75) (mapcat chord-event (range) chord-pitches)))

(defn play-even-melody# [tempo pitches]
  (play# (metronome tempo) (even-melody pitches)))

(defn play-barred-melody# [tempo bar-length  & barred-pitches]
  (let [barred-melodies (map #(barred-melody bar-length %) barred-pitches)
        metro (metronome tempo)]
    (doall
     (map
      #(play# metro %) barred-melodies ))))

;; just check that it is working with a couple of notes

(play-even-melody# 160 [60 61 62 63])

(play-chord# [60 64 67] [70 74 77])

(play-barred-melody# 120 1 [48] [55] [60 60 [72 72 72]]  )

;; This tutorial aims to introduce a few of the basics of scales,
;; chords and basic harmony. Bias towards Western and mainly classical
;;music as that is what I learnt! I am starting with the easy parts
;;and scrimping on some of the detail.

;; If you were to sit down at a keyboard and play every note you would
;; get something like this:

(play-even-melody# 350 (range (note :C1) (note :C5)))

;; The white notes on a keyboard are all given letter names: A to C.
;; The black notes on a keyboard are given modified letter names based
;; on their neigbouring white notes.
;; For example the black key to the left of the white E key is called
;; E Flat  or Eb
;; The the black key to the left of the white F key is called F sharp
;; - or F#
;; Obviously we can give duplicate names to these keys, which is why
;; the following pairs are the same note

(play-even-melody# 100 (map note [:C#3 :Db3 :F#4 :Gb4]))

;; If you do not believe me
(= (note :C#3) (note :Db3))

;; That gives us 12 notes in all

;; Note the number part of a note identifier. This is telling overtone
;; which octave we want to play in.

;; octave 4
(play-even-melody# 120 (map note [:C4 :D4 :E4]))
;; octave 5
(play-even-melody# 120 (map note [:C3 :D3 :E3]))

;; We conventially loop over the octave at C, so :A1 :B1 :C2 :D2 etc.
;; On a keyboard instrument, C is normally the 'home key'

;; Played together, the same note at different octaves sound extremely concordant

(play-even-melody# 120 (map note [:C3 :C4 :C5]))

(play-chord# (map note [:D3 :D4 :D5]))

;; No coincidence, a jump of an octave doubles the frequency
(map (comp midi->hz note) [:A1 :A2 :A3 :A4 :A5 :A6] )

;; A is a special case - it has a nice, regular frequency of 440, 880 etc. This is
;; because it is the standard to which all other notes are tuned
;; (ISO16 !!!)

;; Others are not so regular 
(map (comp midi->hz note) [:D1 :D2 :D3 :D4 :D5 :D6] )
;; but the ratio still holds:
(let [base-freq (midi->hz (note :D1))]
  (->> [:D1 :D2 :D3 :D4 :D5]
       (map (comp midi->hz note))
       (map #(/ % base-freq))))

;; ignore the errors...

;; For more on pitch and frequency, Phil Potter's article is worth a
;; read: http://rhebus.posterous.com/pitch-and-frequency

;; Anyway...
;; You might try and pick out a tune on said piano. Hitting random
;; notes might sound something like this

(play-even-melody# 150 (take 20 (repeatedly #(rand-nth (range (note :C4) (note :C5))))))

;; Eval that a few times. Sounds random (cos it is) and not exactly
;; 'tuneful'. Possibly an infinite number of monkeys with an infinite
;; number of swank servers might end up with something that sounds
;; good. But without these kind of resources, how do we end up with
;; melody?
(play-even-melody# 220 [55 57 59 62 60 60 64 62 62 67 66 67 62 59 55 57 59 60 62 64 62 60 59 57 59 55 54 55 57 50 54 57 60 59 57 ] )
;; Herts und Mund und Tat und Leben (Jesu Joy of Man's Desiring)
;; JS  Bach BWV 147
;; http://en.wikipedia.org/wiki/Jesu,_Joy_of_Man's_Desiring

;; All the notes that appear in that fragment belong to a set which in
;; music we call a key signature.
;; A key signature itself can be computed from a key - one of the
;; 12 notes in the standard set from A->A - and a mode. A mode can be
;; defined as simply a series of intervals.
;; Usually there are 7 of them, meaning that the notes of the key
;; signature can be repeated across each octave.

;; This is the interval pattern for the Major mode:

(def major-mode [2 2 1 2 2 2 1])

;; starting at a key note of C and moving up the interval pattern, we get the following pitches:

(reductions + (note :C3) major-mode )

;; which sounds like:

(play-even-melody# 175 (reductions + (note :C3) major-mode ))

;; and we can keep going
(play-even-melody# 175 (take 32 (reductions + (note :C2) (cycle major-mode) )))

;; Starting to sound like something a bit more melodic. Overtone has a
;; few more you can play with:
;; moody
(play-even-melody# 175 (scale :D3 :minor))

;; a bit more moody
(play-even-melody# 175 (scale :D3 :harmonic-minor))

;; See the documentation for overtone.music.pitch for more fun scales.

;; So going back to the Bach we played earlier
(def bwv-147 [55 57 59 62 60 60 64 62 62 67 66 67 62 59 55 57 59 60 62 64 62 60 59 57 59 55 54 55 57 50 54 57 60 59 57 ] )

;; You will see that all the notes form part of the G major scale
(require '[overtone.music.pitch :as pitch])
(clojure.set/subset? (set bwv-147) (set (pitch/scale-field :g :major))) 

;; If we try our random melody function again but restrict notes to a
;; particular scale it starts to sound more 'tuneful'

(play-even-melody# 150 (take 20 (repeatedly #(rand-nth  (concat (scale :Bb2 :major)  (scale :Bb3 :major ))))))

;; Clearly rand-nth is never going to get us too far, but it's enough
;; to demonstrate the point. There is a technique called "Algorithmic
;; Composition" where notes are generated using a set of rules but
;; it's probably out of the scope of this tutorial.

;; Now it might be good to talk about harmony. We can play certain
;; notes together. Some sound harmonious:

(play-chord# (map note [:C4 :E4 :G4]))
(play-chord# (map note [:G4 :B4 :D5]))

;; Some a little more tense
(play-chord# (map note [:C4 :F4 :G4]))
;; But can resolve themselves
(play-chord# (map note [:C4 :F4 :G4]) (map note [:C4 :E4 :G4]))
;; And some just sound plain nasty (or dissonant as we prefer to call
;;it)
(play-chord# (map note [:C4 :F#4 :B4]))

;; They even call this one "The Devil In Music"
;; (http://en.wikipedia.org/wiki/Tritone)

(play-chord# (take 3 (iterate #(+ 6 %) (note :C4) )))

;; 6 semitones = 3 tones = tritone = evil!

;; I like to think of chords and concordance and dissonance as giving
;; music its colour and drama. Although rules were arguably more
;; strict in earlier musical forms like baroque, more recently we tend
;; to bend the rules and use disonnance as a major musical plot
;; device. Frank Zappa:

;;" The creation and destruction of harmonic and 'statistical' tensions
;; is essential to the maintenance of compositional drama. Any
;; composition (or improvisation) which remains consistent and
;; 'regular' throughout is, for me, equivalent to watching a movie with
;; only 'good guys' in it, or eating cottage cheese."
;; —Frank Zappa, "The Real Frank Zappa Book" page 181, Frank Zappa and Peter Occhiogrosso, 1990


;; We can try and make harmony by picking three notes in a scale and
;; playing them together:

(play-chord#  (choose-n 3 (scale :D4 :major)))

;; If you eval that a few times, somtimes it sounds harmonious,
;; sometimes it sounds pretty dissonant. You can vary the key and scale too.

;; There is of course another system in place - chords. Like a key
;; signature these consist of a root note - e.g C, D Bb and a
;; "quality". Like the modes, the quality is defined as a series of
;; intervals but this time from the root note.

;; A major chord is basically the first, third and fifth notes of the
;; major scale:

(play-chord# (take 4 (map #(nth (scale :G4 :major) %) [0 2 4])) )

;; Same for minor

(play-chord# (take 4 (map #(nth (scale :G4 :minor) %) [0 2 4])) )

;; We don't tend to use the other modes in the same way for chords.
;; After major and minor, other chord qualities are about modifying or
;; adding notes.
;; Again overtone.music.pitch does a fine job of representing these notes

;; A dominant 7th - add on the flattened 7th note of the major scale
(play-chord# (chord :D4 :dom7))

;; minor 6th - adding the sixth note of the minor scale
(play-chord# (chord :E4 :m6))

;; we can also invert chords : change the order that the notes appear.
;; These are effectively the same chord, but given a slightly
;; different sound by the reordering

(def c-major-inversions
  (map
   #(pitch/invert-chord (chord :C3 :major) %) [1 2 3 4 5] ))

(play-even-melody# 150 (flatten c-major-inversions) )
(apply play-chord# c-major-inversions)

;; In a particular key signature a certain set of chords are available
;; to you. Here are a few you can make by just taking the root chord
;; of a scale and going upwards upwards

(def g-major-scale (mapcat #(scale % :major) [:G3 :G4] ))
(def chord-positions (take 9 (iterate #(map inc %) [0 2 4])))
(def chords-in-g-major-scale (for [positions chord-positions] ( map #(nth g-major-scale %) positions) )) 
(apply play-chord# chords-in-g-major-scale )

;; Some chords are more important than others
;; For example, the C major scale contains the notes G, B and
;; D which happily make up the G major chord.
;; As G is the 5th note of the scale of C major, we call this the
;; dominant
;; We can also make F major from the scale of C major. This is called
;; the subdominant as it is one below the domiant (I am sure there is a joke in there somewhere)
;; You can use this combination of tonic, subdominant and dominant
;; chords to play a whole variety of tunes:

;; From folk/traditional

(play-barred-melody# 120 3
                     
                     (partition 3 (map note
                                       [nil nil :G4 :E5 :D5 :C5 :D5 :C5 :A4 :G4 :E4 nil nil nil :G4 :E5 :D5 :C5 :C5 :B4 :C5 :D5 nil nil ] ))
                     (concat [ [nil nil nil]]
                             (map #(apply partial chord %) [[:C3 :major] [:F3 :major] [:C3 :major] [:C3 :major] [:C3 :major] [:F3 :major] [:G3 :major]] ))
                     )

;; to our friend Bach

(play-barred-melody# 120 4
                     (degrees->pitches [ [ :iii :iii :iv :v] [ :v :iv :iii :ii] [ :i :i :ii :iii] [ :iii [:ii :ii] ]]  :major :C4)
                     [#(chord :C3 :major) #(chord :G3 :major) #(chord :C3 :major) #(chord :G3 :major)])

;; to some (comparatively) more modern stuff

(play-barred-melody# 120 4
                     (degrees->pitches [[nil nil nil :v-] [:iii [:iii :iii] :iii :iv] [:iii [nil :v-]] [:iii :iii :iii :iv] [:iii] [:iii [:iii :iii] :iii :iv] [ :v [ :v :iv] :iii] [:ii] [nil nil [ :v- :vi-] :vii-] [:i] [nil nil [:i :ii] :iii ] [:iv] [nil nil nil :iv] [:iii :iii :iii :i] [:ii [:ii :i] :vii- ] [:i]  ] :major :G4  ))

(defn syncopate [bars]
  (let [syncopate-bar (fn [notes] :foo ) ])
  (map syncopate-bar bars)
  )

