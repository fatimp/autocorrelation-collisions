(defpackage autocorrelation-collisions
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:fft  #:fftpack5))
  (:export #:autocorrelation
           #:non-trivial-collisions
           #:divisors))
