(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax)
  (noffi::clr))

(defparameter *include-dir*
  #+darwin "/Users/awolven/clang/include/"
  #+win32 "C:/Users/awolven/clang/include/"
  #+linux "/home/awolven/clang/include/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq DE.BAUHH.CPP::*CC-ARGS* (list* "-I" *include-dir* DE.BAUHH.CPP::*CC-ARGS*)))

(uiop/filesystem:with-current-directory (*include-dir*)
  #_{#include <clang-c/Index.h>})


