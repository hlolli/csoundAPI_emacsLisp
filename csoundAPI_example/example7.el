;; Example 7 - Communicating continuous values with Csound's Channel System
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.11

;; First thing is to load the csoundAPI module
(module-load "csnd.so")

;; This line initializes Csound and turns off Csound's atexit handler as well as signal handlers
(csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
			  CSOUNDINIT_NO_SIGNAL_HANDLER))

(defun initialize-state ()
  (let ((h (make-hash-table)))
    (puthash :curval 0.0 h)
    (puthash :increment 0 h)
    (puthash :end 0 h)
    (puthash :dur 0 h)
    h))

(defun reset-line (state)
  "Calculates new target value, duration and increment"
  (puthash :dur (1+ (random 256)) state)
  (puthash :end (/ (random 1000)
		   1000.0) state)
  (puthash :increment (/ (- (gethash :end state) (or (gethash :curval state) 0.0))
			 (gethash :dur state)) state))


(defun random-line (base range state)
  "Returns a function that will vary in time with the given base
  value and range"
  (progn
    (puthash :dur (1- (gethash :dur state)) state)
    (when (<= (gethash :dur state) 0) 
      (reset-line state))
    (let ((c (gethash :curval state)))
      (puthash :curval (+ c (gethash :increment state)) state)
      (+ base (* range c)))))


(setq orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1
instr 1 
kamp chnget \"amp\"
kfreq chnget \"freq\"
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
outs aout, aout
endin")

(setq sco "i1 0 60")

(let* ((c (csoundCreate))
       (amp-state (initialize-state))       
       (freq-state (initialize-state))
       (ampfn (lambda ()
		(random-line 0.4 0.2 amp-state)))
       (freqfn (lambda ()
		 (random-line 400.0 80.0 freq-state))))
  (csoundSetOption c "-odac")
  (csoundCompileOrc c orc)
  (csoundReadScore c sco)
  (csoundStart c)
  (while (eq 0 (csoundPerformKsmps c))
    (csoundSetControlChannel c "amp" (funcall ampfn))
    (csoundSetControlChannel c "freq" (funcall freqfn)))
  (csoundStop c))
