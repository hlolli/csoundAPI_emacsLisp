(require 'cl)

(module-load "./csnd.so")

(setq csound-instance (csoundCreate))

(csoundCompile csound-instance "test1.csd")

(csoundCompileCsd csound-instance "test1.csd")

(csoundPerform csound-instance)

(assert (eq 'user-ptr (type-of csound-instance)))

(csoundCleanup csound-instance)

(csoundDestroy csound-instance)

(let* ((a (csoundCreate))
       (b (csoundDestroy a))))


(csoundGetOutputName csound-instance)
