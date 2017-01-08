;; Example 3 - Simple Compilation with Csound
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.08

;; First thing is to load the csoundAPI module
(module-load "./csnd.so")

;; This line initializes Csound turns off Csound's atexit handler
(csoundInitialize CSOUNDINIT_NO_ATEXIT)

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
  (csoundSetOption c "-odac")
  ;; Using csoundSetOption() to configure Csound
  ;; Note: use only one commandline flag at a time
  (csoundCompileOrc c orc)
  ;; Compile the Csound SCO String 
  (csoundReadScore c sco)
  ;; When compiling from strings,
  ;; this call is necessary before doing any performing 
  (csoundStart c)
  ;; The following is our main performance loop. We will perform one block of sound at a time 
  ;; and continue to do so while it returns 0, which signifies to keep processing.  We will
  ;; explore this loop technique in further examples. 
  (while (eq 0 (csoundPerformKsmps c)))
  (csoundStop c))

