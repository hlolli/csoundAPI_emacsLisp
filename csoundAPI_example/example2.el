;; Example 2 - Compilation with Csound without CSD
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.06

;; First thing is to load the csoundAPI module
(module-load "./csnd.so")

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
aout vco2 0.5, 440
outs aout, aout
endin")

;; Defining our Csound SCO code
(defconst sco "i1 0 1")


(let ((c (csoundCreate)))
  ;; Using csoundSetOption() to configure Csound
  (csoundSetOption c "-odac")
  ;; Compile the Csound Orchestra String
  (csoundCompileOrc c orc)
  ;; Compile the Csound SCO String 
  (csoundReadScore c sco)
  ;; When compiling from strings, this call is necessary before doing any performing 
  (csoundStart c)
  ;; Run Csound to completion
  (csoundPerform c)
  (csoundStop c))
