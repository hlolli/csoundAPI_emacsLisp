;; Example 4 - Simple Compilation with Csound
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

