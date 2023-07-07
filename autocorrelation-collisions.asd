(defsystem :autocorrelation-collisions
    :name :autocorrelation-collisions
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Find non-trivial collisions in periodic autocorrelation of bit vectors"
    :license "2-clause BSD"
    :serial t
    :components ((:file "package")
                 (:file "autocorrelation-collisions"))
    :depends-on (:alexandria :serapeum :fftpack5))
