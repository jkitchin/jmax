(when (require 'undercover nil t)
  (undercover "jmax.el" (:exclude "*-test.el")))

(ert-deftest plain-and-simple ()
  (should t))
