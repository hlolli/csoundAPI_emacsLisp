;; Example 5 - Generating Score
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.11


;; First thing is to load the csoundAPI module
(module-load "csnd.so")

;; This line initializes Csound and turns off Csound's atexit handler as well as signal handlers
(csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
			  CSOUNDINIT_NO_SIGNAL_HANDLER))

;; Defining our Csound ORC code within a multline String
(defconst orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1
instr 1 
ipch = cps2pch(p5, 12)
kenv linsegr 0, .05, 1, .05, .7, .4, 0
aout vco2 p4 * kenv, ipch 
aout moogladder aout, 2000, 0.25
outs aout, aout
endin")

;; Example1 static score
(defconst sco1 "i1 0 1 0.5 8.00")


;; Example 2 - Generating Score string using a map with range sequence
(defconst sco2
  (-> (mapcar (lambda (n) (format "i1 %g .25 0.5 8.%02d" (* n 0.25) n))
	      (number-sequence 0 13))
      (string-join "\n")))

;; Example 3 - Generating Score using intermediate data structure (list of lists),
;; then converting to String.
;; initialize a list to hold lists of values representing notes

(setq notes
      (->> (mapcar (lambda (n) (list 1 (* n 0.25) 0.25 0.5 (format "8.%02d" (floor (random 15)))))
		   (number-sequence 0 13))
	   (mapcar (lambda (l) (apply 'format "i %i %f %f %f %s" l)))))

(defconst sco3 (string-join notes "\n"))


(let ((c (csoundCreate)))
  (csoundSetOption c "-odac")
  (csoundCompileOrc c orc)
  (csoundReadScore c sco1)
  ;; (csoundReadScore c sco2)
  ;; (csoundReadScore c sco3)
  (csoundStart c)
  (while (eq 0 (csoundPerformKsmps c)))
  (csoundStop c))
