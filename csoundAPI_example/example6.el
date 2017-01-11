;; Example 6 - Generating Score
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.11


;; First thing is to load the csoundAPI module
(module-load "csnd.so")

;; This line initializes Csound and turns off Csound's atexit handler as well as signal handlers
(csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
			  CSOUNDINIT_NO_SIGNAL_HANDLER))


(defun midi->pch (num)
  "Convert MIDI Note Numbers to Csound PCH format"
  (format " %d.%02d" (+ 3 (/ num 12)) (% num 12)))

(defun note->str (note)
  "Converts a note into a string"
  (let* ((pfields (butlast note))
	 (mpch (midi->pch (car (last note)))))
    (concat "i "
	    (-> (mapcar 'number-to-string pfields)
		(string-join " "))
	    mpch)))

(setq orc "
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



;; generate notes in list using random MIDI pitch values from 60-75
(setq notes
      (mapcar (lambda (n) (list 1 (* 0.25 n) 0.25 0.5 
				(+ 60 (random 15)))) 
	      (number-sequence 0 13)))


;; generate a tansposed set of notes  
(setq transposed
      (mapcar (lambda (n) (append
			   (butlast n)
			   (list (apply '+ 4 (last n)))))
	      notes))


;; convert both sets of notes into a single SCO string
(setq sco
      (-> (mapcar 'note->str (append notes transposed))
	  (string-join "\n")))

(let ((c (csoundCreate)))
  (csoundSetOption c "-odac")
  (csoundCompileOrc c orc)
  (csoundReadScore c sco)
  (csoundStart c)
  (while (eq 0 (csoundPerformKsmps c)))
  (csoundStop c))
