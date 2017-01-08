(module-load "./csnd.so")

(setq csound-instance (csoundCreate))

(csoundCompile csound-instance "test1.csd")

(csoundCompileCsd csound-instance "test1.csd")

(csoundPerform csound-instance)

(let* ((a (csoundCreate))
       (b (csoundDestroy a))))

(csoundGetOutputName csound-instance)

CSOUND_CONTROL_CHANNEL

(csoundSetControlChannel csound-instance "test-chan" (float 66999.6))
(csoundGetControlChannel csound-instance "test-chan")

(csoundSetStringChannel csound-instance "test-str-chan" "12345678910")
(csoundGetStringChannel csound-instance "test-str-chan")

(csoundCleanup csound-instance)

(csoundDestroy csound-instance)

(csoundScoreEvent csound-instance "iaaaaaaaaaaaaaaaaaaaaaaaaaa" 2 "a")
