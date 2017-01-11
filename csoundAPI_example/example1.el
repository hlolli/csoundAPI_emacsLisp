;; Example 1 - Simple Compilation with Csound
;; Example author: Steven Yi <stevenyi@gmail.com>
;; For Elisp: Hlödver Sigurdsson <hlolli@gmail.com>
;; 2017.01.06


;; First thing is to load the csoundAPI module
(module-load "./csnd.so")

;; This line initializes Csound and turns off Csound's atexit handler as well as signal handlers
(csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
			  CSOUNDINIT_NO_SIGNAL_HANDLER))

;; Create an instance of Csound and assign it to symbol 'csound
(set 'csound (csoundCreate))

;; Compile a pre-defined test1.csd file
(csoundCompile csound "test1.csd")

;; This call runs Csound to completion
(csoundPerform csound)

;; At this point, Csound is already stopped, but this call is here
;; as it is something that you would generally call in real-world contexts
(csoundStop csound)


