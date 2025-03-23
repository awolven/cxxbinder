(in-package :cl-user)

(defpackage :cxxbinder
  (:use :cl :noffi))

(defpackage :null)

(in-package :cxxbinder)

(defun show (thing)
  (noffi::find-identifier-declaration thing))

(defclass module () ())
(defclass opencascade (module) ())

(defclass abi () ())
(defclass msvc (abi) ())
(defclass itanium (abi) ())

(defclass ffi () ())
(defclass cffi (ffi) ())
(defclass noffi (ffi) ())

(defvar *verbosity* 0)
(defvar *access-specifier* :error)
(defvar *overloads* :unset)
(defvar *lisp-file* :error)
(defvar *module*)
(defvar *abi*)
(defvar *ffi*)
(defvar *pass*)
(defvar *unit* :error)

(defvar *foo*)
(defvar *bar* (make-hash-table))

(defvar *exports2*)

;; a c-name symbol is a c/c++ name string interned in the noffi-c package

(defvar *declaration* :error)

(defvar *visited* :error)
(defvar *dependencies-table* :error)
(defvar *dependencies-list* :error)

(defvar *using-directives* :error)

(defstruct named-declaration
  (name nil)
  (namespace nil)
  (file nil)
  (line nil)
  (column nil)
  (offset nil)
  (punt nil))

(defmethod compose-typename-3 ((thing (eql :int)))
  "int")

(defmethod compose-typename-3 ((thing (eql :unsigned-int)))
  "unsigned int")

(defmethod compose-typename-3 ((thing (eql :long)))
  "long")

(defmethod compose-typename-3 ((thing (eql :unsigned-long)))
  "unsigned long")

(defmethod compose-typename-3 ((thing (eql :long-long)))
  "long long")

(defmethod compose-typename-3 ((thing (eql :unsigned-long-long)))
  "unsigned long long")

(defmethod compose-typename-3 ((thing (eql :char)))
  "char")

(defmethod compose-typename-3 ((thing (eql :unsigned-char)))
  "unsigned char")

(defmethod compose-typename-3 ((thing (eql :short)))
  "short")

(defmethod compose-typename-3 ((thing (eql :unsigned-short)))
  "unsigned short")

(defmethod compose-typename-3 ((thing (eql :float)))
  "float")

(defmethod compose-typename-3 ((thing (eql :double)))
  "double")

(defmethod compose-typename-3 ((thing (eql '#_bool)))
  "bool")

(defmethod compose-typename-3 ((thing symbol)) ;; hack 
  (symbol-name thing))

(defmethod compose-typename-3 (thing)
  (let ((ct (compose-typename thing)))
    (if (consp ct)
	(second ct)
	ct)))

(defmethod compose-typename ((thing symbol))
  thing)

(defmethod compose-typename ((thing list))
  (cond ((type-name-type-p thing)
	 (list :type-name (compose-typename (cadr thing))))
	((template-type-p thing)
	 (let ((name (compose-typename (cadr thing)))
	       (arguments (mapcar #'(lambda (thing)
				      (let ((ct (compose-typename-3 thing)))
					(if (consp ct)
					    (second ct)
					    ct)))
				  (remove-if #'(lambda (item)
						 (or (null item)
						     (and (consp item)
							  (null (cdr item)))))
					     (cddr thing)))))
	   (list :type-name
		 (noffi::cintern
		  (format nil "~A<~{~A, ~}~A>" name (butlast arguments) (first (last arguments)))))))
	((qualified-type-p thing)
	 (noffi::cintern
	  (format nil "~{~A::~}~A" (butlast (cdr thing)) (first (last thing)))))
	((pointerish-type-p thing)
	 (list (car thing) (compose-typename (cadr thing))))
	((type-qualifier-type-p thing)
	 (list :type-qualifier (cadr thing) (compose-typename (caddr thing))))))

(defmethod compose-typename-1 ((named-declaration named-declaration) key)
  (let* ((qualifiers (namespace-qualifiers (named-declaration-namespace named-declaration)))
	 (qualified-string (format nil "~{~A::~}" qualifiers))
	 (template-arguments (mapcar #'(lambda (thing)
				      (let ((ct thing))
					(if (consp ct)
					    (second ct)
					    ct)))
				     (remove-if #'(lambda (item)
					    (or (null item)
						(and (consp item)
						     (null (cdr item)))))
					(mapcar #'(lambda (name)
						    (cond ((null name) nil) ;; hack
							  ((keywordp name)
							   (compose-typename-3 name))
							  ((type-name-type-p name)
							   (if (null (cadr name))
							       nil
							       (compose-typename-3 (cadr name))))
							  (t (let ((decl (dbg-find-decl name))) ;; hack.
							       (when decl (compose-typename-3 decl))))))
						(when (consp key)
						  (cdr key))))))
	 (name (if (consp key)
		   (car key)
		   key)))
    
    (noffi::cintern
     (concatenate 'string qualified-string
		  (if template-arguments
		      (format nil "~A<~{~A, ~}~A>"
			      name (butlast template-arguments) (first (last template-arguments)))
		      (symbol-name name))))))

(defmethod compose-template-name ((template-definition class-template-definition) arguments)
  (let* ((qualifiers (namespace-qualifiers (named-declaration-namespace template-definition)))
	 (qualified-string (format nil "~{~A::~}" qualifiers))
	 (name (class-template-definition-name template-definition))
	 (template-arguments (mapcar #'(lambda (thing)
				      (let ((ct thing))
					(if (consp ct)
					    (second ct)
					    ct)))
				     (mapcar #'compose-typename-3 (remove-if #'(lambda (item)
										 (or (null item)
										     (and (consp item)
											  (null (cdr item)))))
									     arguments)))))

    (noffi::cintern
     (concatenate 'string qualified-string
		  (if template-arguments
		      (format nil "~A<~{~A, ~}~A>"
			      name (butlast template-arguments) (first (last template-arguments)))
		      (format nil "~A<>" name))))))

    
						
	 

(defmethod write-type-decl ((decl named-declaration) name (module module) (ffi noffi) abi stream)
  (let ((decls (make-noffi-type-declarations decl name module abi)))
    (when decls
      (loop for decl in decls
	    when decl
	    do
	       (let ((*print-escape* nil)
		     (*package* (find-package :null)))
		 (print (incf *foo*) stream)
		 (pprint decl stream)
		 (finish-output stream))
	       (terpri stream)
	       (terpri stream)))))
    

(defvar *global-namespace*)
(defvar *current-namespace*)

(defstruct (namespace (:include named-declaration))
  ;; declarations could be keyed on just one symbol
  ;; or a list of symbols, in the case of a template name and it's arguments
  (declarations (make-hash-table :test #'equalp))
  (template-definitions (make-hash-table)))

(defstruct (enum-declaration (:include named-declaration))
  (enum-type nil)
  (constants nil))

(defmethod make-noffi-type-declarations ((decl enum-declaration) name (module module) abi)
  (list (noffi::make-declaration :name name
				 :type (enum-declaration-enum-type decl))))

;; a c/c++ typedef
;; name is a c/c++ name symbol, interned in the noffi-c package
;; type is a noffi type
(defstruct (type-definition (:include named-declaration))
  (type nil))

(defmethod make-noffi-type-declarations ((decl type-definition) name (module module) abi)
  (list (noffi::make-declaration :name name
				 :storage-class :typedef
				 :type (compose-typename (type-definition-type decl)))))

(defmethod deep-copy ((thing type-definition))
  (let ((copy (copy-type-definition thing)))
    (setf (type-definition-type copy)
	  (deep-copy (type-definition-type copy)))
    copy))

(defstruct (type-alias (:include named-declaration))
  (type nil))

(defmethod make-noffi-type-declarations ((decl type-alias) name (module module) abi)
  nil)

(defmethod make-noffi-type-declarations ((decl namespace) name (module module) abi)
  nil)

(defstruct (type-alias-template-decl (:include named-declaration))
  (type-alias-decl nil))

(defvar *type-alias-template-decl* :error)

(defstruct clang-type
  (name nil)
  (cv-qualifiers nil))

(defstruct (primitive-c-type (:include clang-type)))

(defstruct (array-vector-or-complex-type (:include clang-type))
  (element-type nil))

(defstruct (array-type (:include array-vector-or-complex-type)))

(defstruct (reference-type (:include clang-type))
  (pointee-type))




      
(defmethod to-noffi-type :around ((thing clang-type))
  (wrap-noffi-type (clang-type-cv-qualifiers thing) (call-next-method)))

(defstruct (invalid-type (:include clang-type)))

(defmethod deep-copy ((thing invalid-type))
  (copy-invalid-type thing))

(defmethod to-noffi-type ((thing invalid-type))
  (list :type-name (invalid-type-name thing)))

(defstruct (unexposed-type (:include clang-type)))

(defmethod deep-copy ((thing unexposed-type))
  (copy-unexposed-type thing))

(defmethod to-noffi-type ((thing unexposed-type))
  (list :type-name (unexposed-type-name thing)))

(defstruct (void-type (:include primitive-c-type)))

(defmethod deep-copy ((thing void-type))
  (copy-void-type thing))

(defmethod mangle-type ((thing void-type) (abi msvc))
  "X")

(defmethod mangle-type ((thing void-type) (abi itanium))
  "v")

(defmethod mangle-type ((thing (eql :void)) (abi msvc))
  "X")

(defmethod mangle-type ((thing (eql :void)) (abi itanium))
  "v")

(defmethod to-noffi-type ((thing void-type))
 :void)

(defstruct (bool-type (:include primitive-c-type)))

(defmethod deep-copy ((thing bool-type))
  (copy-bool-type thing))

(defmethod mangle-type ((thing bool-type) (abi msvc))
  "_N")

(defmethod mangle-type ((thing bool-type) (abi itanium))
  "b")

(defmethod mangle-type ((thing (eql '#_bool)) (abi msvc))
  "_N")

(defmethod mangle-type ((thing (eql '#_bool)) (abi itanium))
  "b")

(defmethod to-noffi-type ((thing bool-type))
  '(:type-name #_bool))

(defstruct (char-u-type (:include primitive-c-type)))

(defmethod deep-copy ((thing char-u-type))
  (copy-char-u-type thing))

(defmethod to-noffi-type ((thing char-u-type))
  :unsigned-char)

(defstruct (uchar-type (:include primitive-c-type)))

(defmethod deep-copy ((thing uchar-type))
  (copy-uchar-type thing))

(defmethod mangle-type ((thing uchar-type) (abi msvc))
  "E")

(defmethod mangle-type ((thing uchar-type) (abi itanium))
  "h")

(defmethod mangle-type ((thing (eql :unsigned-char)) (abi msvc))
  "E")

(defmethod mangle-type ((thing (eql :unsigned-char)) (abi itanium))
  "h")

(defmethod to-noffi-type ((thing uchar-type))
  :unsigned-char)

(defstruct (char16-type (:include primitive-c-type)))

(defmethod deep-copy ((thing char16-type))
  (copy-char16-type thing))

(defmethod mangle-type ((thing char16-type) (abi msvc))
  "F")

(defmethod mangle-type ((thing char16-type) (abi itanium))
  "s")

(defmethod to-noffi-type ((thing char16-type))
  '(:type-name #_char16))

(defstruct (char32-type (:include primitive-c-type)))

(defmethod deep-copy ((thing char32-type))
  (copy-char32-type thing))

(defmethod to-noffi-type ((thing char32-type))
  '(:type-name #_char32))

(defstruct (ushort-type (:include primitive-c-type)))

(defmethod deep-copy ((thing ushort-type))
  (copy-ushort-type thing))

(defmethod mangle-type ((thing ushort-type) (abi msvc))
  "G")

(defmethod mangle-type ((thing ushort-type) (abi itanium))
  "t")

(defmethod mangle-type ((thing (eql :unsigned-short)) (abi msvc))
  "G")

(defmethod mangle-type ((thing (eql :unsigned-short)) (abi itanium))
  "t")

(defmethod to-noffi-type ((thing ushort-type))
  :unsigned-short)

(defstruct (uint-type (:include primitive-c-type)))

(defmethod deep-copy ((thing uint-type))
  (copy-uint-type thing))

(defmethod mangle-type ((thing uint-type) (abi msvc))
  "I")

(defmethod mangle-type ((thing uint-type) (abi itanium))
  "j")

(defmethod mangle-type ((thing (eql :unsigned-int)) (abi msvc))
  "I")

(defmethod mangle-type ((thing (eql :unsigned-int)) (abi itanium))
  "j")

(defmethod to-noffi-type ((thing uint-type))
  :unsigned-int)

(defstruct (ulong-type (:include primitive-c-type)))

(defmethod deep-copy ((thing ulong-type))
  (copy-ulong-type thing))

(defmethod mangle-type ((thing ulong-type) (abi msvc))
  "K")

(defmethod mangle-type ((thing ulong-type) (abi itanium))
  "m")

(defmethod mangle-type ((thing (eql :unsigned-long)) (abi msvc))
  "K")

(defmethod mangle-type ((thing (eql :unsigned-long)) (abi itanium))
  "m")

(defmethod to-noffi-type ((thing ulong-type))
  :unsigned-long)

(defstruct (ulonglong-type (:include primitive-c-type)))

(defmethod deep-copy ((thing ulonglong-type))
  (copy-ulonglong-type thing))

(defmethod mangle-type ((thing ulonglong-type) (abi msvc))
  "_K")

(defmethod mangle-type ((thing ulonglong-type) (abi itanium))
  "y")

(defmethod mangle-type ((thing (eql :unsigned-long-long)) (abi msvc))
  "_K")

(defmethod mangle-type ((thing (eql :unsigned-long-long)) (abi itanium))
  "y")

(defmethod to-noffi-type ((thing ulonglong-type))
  :unsigned-long-long)

(defstruct (uint128-type (:include primitive-c-type)))

(defun is-128-bit-unsigned-clang-type (ctype)
  (uint128-type-p ctype))

(defmethod deep-copy ((thing uint128-type))
  (copy-uint128-type thing))

(defmethod to-noffi-type ((thing uint128-type))
  '(:type-name #_uint128_t))

(defstruct (char-s-type (:include primitive-c-type)))

(defmethod deep-copy ((thing char-s-type))
  (copy-char-s-type thing))

(defmethod mangle-type ((thing char-s-type) (abi msvc))
  "C")

(defmethod mangle-type ((thing char-s-type) (abi itanium))
  "a")

(defmethod to-noffi-type ((thing char-s-type))
  :signed-char)

(defstruct (schar-type (:include primitive-c-type)))

(defmethod deep-copy ((thing schar-type))
  (copy-schar-type thing))

(defmethod mangle-type ((thing schar-type) (abi msvc))
  "C")

(defmethod mangle-type ((thing schar-type) (abi itanium))
  "a")

(defmethod mangle-type ((thing (eql :signed-char)) (abi msvc))
  "C")

(defmethod mangle-type ((thing (eql :signed-char)) (abi itanium))
  "a")

(defmethod mangle-type ((thing (eql :char)) (abi msvc))
  "D")

(defmethod mangle-type ((thing (eql :char)) (abi itanium))
  "c")

(defmethod to-noffi-type ((thing schar-type))
  :signed-char)

(defstruct (wchar-type (:include primitive-c-type)))

(defmethod deep-copy ((thing wchar-type))
  (copy-wchar-type thing))

(defmethod mangle-type ((thing wchar-type) (abi msvc))
  "_W")

(defmethod mangle-type ((thing wchar-type) (abi itanium))
  "w")

(defmethod mangle-type ((thing (eql '#_wchar_t)) (abi msvc))
  "_W")

(defmethod mangle-type ((thing (eql '#_wchar_t)) (abi itanium))
  "w")

(defmethod to-noffi-type ((thing wchar-type))
  '(:type-name #_wchar_t))

(defstruct (short-type (:include primitive-c-type)))

(defmethod deep-copy ((thing short-type))
  (copy-short-type thing))

(defmethod mangle-type ((thing short-type) (abi msvc))
  "F")

(defmethod mangle-type ((thing short-type) (abi itanium))
  "s")

(defmethod mangle-type ((thing (eql :short)) (abi msvc))
  "F")

(defmethod mangle-type ((thing (eql :short)) (abi msvc))
  "s")  

(defmethod to-noffi-type ((thing short-type))
  :short)

(defstruct (int-type (:include primitive-c-type)))

(defmethod deep-copy ((thing int-type))
  (copy-int-type thing))

(defmethod mangle-type ((thing int-type) (abi msvc))
  "H")

(defmethod mangle-type ((thing int-type) (abi itanium))
  "i")

(defmethod mangle-type ((thing (eql :int)) (abi msvc))
  "H")

(defmethod mangle-type ((thing (eql :int)) (abi itanium))
  "i")

(defmethod to-noffi-type ((thing int-type))
  :int)

(defstruct (long-type (:include primitive-c-type)))

(defmethod deep-copy ((thing long-type))
  (copy-long-type thing))

(defmethod to-noffi-type ((thing long-type))
  :long)

(defstruct (longlong-type (:include primitive-c-type)))

(defmethod deep-copy ((thing longlong-type))
  (copy-longlong-type thing))

(defmethod mangle-type ((thing longlong-type) (abi msvc))
  "_J")

(defmethod mangle-type ((thing longlong-type) (abi itanium))
  "x")

(defmethod mangle-type ((thing (eql :long-long)) (abi msvc))
  "_J")

(defmethod mangle-type ((thing (eql :long-long)) (abi itanium))
  "x")

(defmethod to-noffi-type ((thing longlong-type))
  :long-long)

(defstruct (int128-type (:include primitive-c-type)))

(defmethod deep-copy ((thing int128-type))
  (copy-int128-type thing))

(defmethod to-noffi-type ((thing int128-type))
  '(:type-name #_int128_t))

(defstruct (float-type (:include primitive-c-type)))

(defmethod deep-copy ((thing float-type))
  (copy-float-type thing))

(defmethod mangle-type ((thing float-type) (abi msvc))
  "M")

(defmethod mangle-type ((thing float-type) (abi itanium))
  "f")

(defmethod mangle-type ((thing (eql :float)) (abi msvc))
  "M")

(defmethod mangle-type ((thing (eql :float)) (abi itanium))
  "f")

(defmethod to-noffi-type ((thing float-type))
  :float)

(defstruct (double-type (:include primitive-c-type)))

(defmethod deep-copy ((thing double-type))
  (copy-double-type thing))

(defmethod mangle-type ((thing double-type) (abi msvc))
  "N")

(defmethod mangle-type ((thing double-type) (abi itanium))
  "d")

(defmethod mangle-type ((thing (eql :double)) (abi msvc))
  "N")

(defmethod mangle-type ((thing (eql :double)) (abi itanium))
  "d")

(defmethod to-noffi-type ((thing double-type))
  :double)

(defstruct (long-double-type (:include primitive-c-type)))

(defmethod deep-copy ((thing long-double-type))
  (copy-long-double-type thing))

(defmethod mangle-type ((thing long-double-type) (abi msvc))
  "O")

(defmethod mangle-type ((thing long-double-type) (abi itanium))
  "e")

(defmethod mangle-type ((thing (eql :long-double)) (abi msvc))
  "O")

(defmethod mangle-type ((thing (eql :long-double)) (abi itanium))
  "e")

(defmethod to-noffi-type ((thing long-double-type))
  :long-double)

(defstruct (nullptr-type (:include clang-type)))

(defmethod deep-copy ((thing nullptr-type))
  (copy-nullptr-type thing))

(defstruct (overload-type (:include clang-type)))

(defmethod deep-copy ((thing overload-type))
  (copy-overload-type thing))

(defstruct (dependent-type (:include clang-type)))

(defmethod deep-copy ((thing dependent-type))
  (copy-dependent-type thing))

(defstruct (objc-id-type (:include clang-type)))

(defmethod deep-copy ((thing objc-id-type))
  (copy-objc-id-type thing))

(defstruct (objc-class-type (:include clang-type)))

(defmethod deep-copy ((thing objc-class-type))
  (copy-objc-class-type thing))

(defstruct (objc-sel-type (:include clang-type)))

(defmethod deep-copy ((thing objc-sel-type))
  (copy-objc-sel-type thing))

(defstruct (float128-type (:include clang-type)))

(defmethod deep-copy ((thing float128-type))
  thing)

(defmethod to-noffi-type ((thing float128-type))
  '(:type-name #_float128))

(defstruct (half-type (:include clang-type)))

(defmethod deep-copy ((thing half-type))
  thing)

(defmethod to-noffi-type ((thing half-type))
  '(:type-name #_half))

(defstruct (float16-type (:include clang-type)))

(defmethod mangle-type ((thing float16-type) (abi itanium))
  "DF16_")			

(defmethod deep-copy ((thing float16-type))
  thing)

(defmethod to-noffi-type ((thing float16-type))
  '(:type-name #_float16))

(defstruct (short-accum-type (:include clang-type)))

(defmethod deep-copy ((thing short-accum-type))
  (copy-short-accum-type thing))

(defstruct (accum-type (:include clang-type)))

(defmethod deep-copy ((thing accum-type))
  (copy-accum-type thing))

(defstruct (long-accum-type (:include clang-type)))

(defmethod deep-copy ((thing long-accum-type))
  (copy-long-accum-type thing))

(defstruct (ushort-accum-type (:include clang-type)))

(defmethod deep-copy ((thing ushort-accum-type))
  (copy-ushort-accum-type thing))

(defstruct (uaccum-type (:include clang-type)))

(defmethod deep-copy ((thing uaccum-type))
  (copy-uaccum-type thing))

(defstruct (ulong-accum-type (:include clang-type)))

(defmethod deep-copy ((thing ulong-accum-type))
  (copy-ulong-accum-type thing))

(defstruct (bfloat16-type (:include clang-type)))

(defmethod deep-copy ((thing bfloat16-type))
  thing)

(defstruct (ibm128-type (:include clang-type)))

(defmethod deep-copy ((thing ibm128-type))
  (copy-ibm128-type thing))

(defstruct (complex-type (:include array-vector-or-complex-type)))

(defmethod deep-copy ((thing complex-type))
  (let ((copy (copy-complex-type thing)))
    (setf (complex-type-element-type copy)
	  (deep-copy (complex-type-element-type copy)))
    copy))

(defmethod mangle-type ((thing complex-type) (abi itanium))
  (concatenate 'string "C" (mangle-type (complex-type-element-type thing) abi)))

(defmethod to-noffi-type ((thing complex-type))
  (case (to-noffi-type (complex-type-element-type thing))
    (:float :float-complex)
    (:double :double-complex)
    (:long-double :long-double-complex)
    (t (error "cannot convert ~S to noffi type." thing))))

(defstruct (pointer-type (:include clang-type))
  (pointee-type nil))

(defmethod deep-copy ((thing pointer-type))
  (let ((copy (copy-pointer-type thing)))
    (setf (pointer-type-pointee-type copy)
	  (deep-copy (pointer-type-pointee-type copy)))
    copy))


(defmethod mangle-type ((thing pointer-type) (abi msvc))
  (let ((type-qualifiers (pointer-type-cv-qualifiers thing))
	(pointer-qualifiers '()))
    (flet ((mangle-type-qualifiers ()
	     (cond ((and (member :const type-qualifiers) (member :volatile type-qualifiers)) "D")
		   ((member :volatile type-qualifiers) "C")
		   ((member :const type-qualifiers) "B")
		   (t "A")))
	   (mangle-pointer-qualifiers ()
	     (cond ((and (member :const pointer-qualifiers) (member :volatile pointer-qualifiers)) "S")
		   ((member :volatile pointer-qualifiers) "R")
		   ((member :const pointer-qualifiers) "Q")
		   (t "P")))
	   (mangle-restrict ()
	     (cond ((member :restrict pointer-qualifiers) "I")
		   (t nil))))
      (concatenate 'string (mangle-pointer-qualifiers) "E" (mangle-restrict) (mangle-type-qualifiers)
		   (mangle-type (pointer-type-pointee-type thing) abi)))))


(defmethod mangle-type ((thing pointer-type) (abi itanium))
  (let ((type-qualifiers (pointer-type-cv-qualifiers thing)))
    (flet ((mangle-type-qualifiers ()
	     (cond ((and (member :const type-qualifiers) (member :volatile type-qualifiers)) "VK")
		   ((member :volatile type-qualifiers) "V")
		   ((member :const type-qualifiers) "K")
		   (t nil))))
      (concatenate 'string (mangle-type-qualifiers)
		   (mangle-type (pointer-type-pointee-type thing) abi)))))

(defmethod mangle-type ((thing symbol) abi)
  (copy-seq (symbol-name thing)))

(defmethod mangle-type ((thing (eql '#__m64)) (abi msvc))
  "_m64")

(defmethod mangle-type ((thing (eql '#___m64)) (abi msvc))
  "__m64")

(defmethod mangle-type ((thing (eql '#__m128)) (abi msvc))
  "_m128")

(defmethod mangle-type ((thing (eql '#___m128)) (abi msvc))
  "__m128")

(defmethod mangle-type ((thing (eql '#__m128d)) (abi msvc))
  "_m128d")

(defmethod mangle-type ((thing (eql '#___m128d)) (abi msvc))
  "__m128d")

(defmethod mangle-type ((thing (eql '#__m128i)) (abi msvc))
  "_m128i")

(defmethod mangle-type ((thing (eql '#___m128i)) (abi msvc))
  "__m128i")

(defmethod mangle-type ((thing (eql '#__m256)) (abi msvc))
  "_m256")

(defmethod mangle-type ((thing (eql '#___m256)) (abi msvc))
  "__m256")

(defmethod mangle-type ((thing (eql '#__m256d)) (abi msvc))
  "_m256d")

(defmethod mangle-type ((thing (eql '#___m256d)) (abi msvc))
  "__m256d")

(defmethod mangle-type ((thing (eql '#__m256i)) (abi msvc))
  "_m256i")

(defmethod mangle-type ((thing (eql '#___m256i)) (abi msvc))
  "__m256i")

(defmethod mangle-type ((thing (eql '#__m512)) (abi msvc))
  "_m512")

(defmethod mangle-type ((thing (eql '#___m512)) (abi msvc))
  "__m512")

(defmethod mangle-type ((thing (eql '#__m152d)) (abi msvc))
  "_m512d")

(defmethod mangle-type ((thing (eql '#___m152d)) (abi msvc))
  "__m512d")

(defmethod mangle-type ((thing (eql '#__m512i)) (abi msvc))
  "_m512i")

(defmethod mangle-type ((thing (eql '#___m512i)) (abi msvc))
  "__m512i")

(defmethod to-noffi-type ((thing pointer-type))
  (list :pointer (to-noffi-type (pointer-type-pointee-type thing))))

(defstruct (block-pointer-type (:include clang-type)))

(defmethod deep-copy ((thing block-pointer-type))
  (copy-block-pointer-type thing))

(defstruct (lvalue-reference-type (:include reference-type)))

(defmethod deep-copy ((thing lvalue-reference-type))
  (let ((copy (copy-lvalue-reference-type thing)))
    (setf (lvalue-reference-type-pointee-type copy)
	  (deep-copy (lvalue-reference-type-pointee-type copy)))
    copy))

(defmethod mangle-type ((thing lvalue-reference-type) (abi msvc))
  (let ((type-qualifiers (lvalue-reference-type-cv-qualifiers thing)))
    (flet ((mangle-type-qualifiers ()
	     (cond ((and (member :const type-qualifiers) (member :volatile type-qualifiers)) "D")
		   ((member :volatile type-qualifiers) "C")
		   ((member :const type-qualifiers) "B")
		   (t "A"))))
      (concatenate 'string "AE" (mangle-type-qualifiers)
		   (mangle-type (lvalue-reference-type-pointee-type thing) abi)))))

(defmethod mangle-type ((thing lvalue-reference-type) (abi itanium))
  (let ((type-qualifiers (lvalue-reference-type-cv-qualifiers thing)))
    (flet ((mangle-type-qualifiers ()
	     (cond ((and (member :const type-qualifiers) (member :volatile type-qualifiers)) "VK")
		   ((member :volatile type-qualifiers) "V")
		   ((member :const type-qualifiers) "K")
		   (t nil))))
      (concatenate 'string (mangle-type-qualifiers)
		   (mangle-type (lvalue-reference-type-pointee-type thing) abi)))))

(defmethod to-noffi-type ((thing lvalue-reference-type))
  (list :lvalue-reference (to-noffi-type (lvalue-reference-type-pointee-type thing))))

(defstruct (rvalue-reference-type (:include reference-type)))

(defmethod deep-copy ((thing rvalue-reference-type))
  (let ((copy (copy-rvalue-reference-type thing)))
    (setf (rvalue-reference-type-pointee-type copy)
	  (deep-copy (rvalue-reference-type-pointee-type copy)))
    copy))

(defmethod mangle-type ((thing rvalue-reference-type) (abi msvc))
  (warn "unimplemented")
  (mangle-type (rvalue-reference-type-pointee-type thing) abi))

(defmethod mangle-type ((thing rvalue-reference-type) (abi itanium))
  (concatenate 'string "O" (mangle-type (rvalue-reference-type-pointee-type thing) abi)))

(defmethod to-noffi-type ((thing rvalue-reference-type))
  (list :rvalue-reference (to-noffi-type (rvalue-reference-type-pointee-type thing))))

(defstruct (record-type (:include clang-type)))

(defmethod deep-copy ((thing record-type))
  (copy-record-type thing))

(defmethod to-noffi-type ((thing record-type))
  (list :type-name (record-type-name thing)))

(defstruct (template-class-type (:include record-type))
  (arguments nil))

(defmethod deep-copy ((thing template-class-type))
  (copy-template-class-type thing))

#+NIL
(defmethod mangle-type ((thing template-class-type) (abi msvc))
  (let ((template-name (template-class-type-name thing))
	(template-arguments (template-class-type-arguments thing)))
    (apply #'concatenate
	   'string
	   "?$"
	   (mangle-basic-name template-name abi)
	   "@"
	   (loop for argument in template-arguments
		 collect (let ((type (gethash argument *types*)))
			   (if (null type)
			       (let ((type (literal-is-of-type argument)))
				 (case type
				   (:integer (concatenate 'string "$0" (symbol-name argument)))))
			       (mangle-type type abi)))))))

(defstruct (enum-type (:include clang-type)))

(defmethod deep-copy ((thing enum-type))
  (copy-enum-type thing))



(defmethod to-noffi-type ((thing enum-type))
  (list :type-name (enum-type-name thing)))

(defstruct (typedef-type (:include clang-type))
  (canonical-type nil))

(defmethod deep-copy ((thing typedef-type))
  (copy-typedef-type thing))

(defmethod mangle-type ((thing typedef-type) (abi msvc))
  (if (typedef-type-canonical-type thing)
      (mangle-type (typedef-type-canonical-type thing) abi)
      (error "cannot mangle type ~S" thing)))

(defmethod to-noffi-type ((thing typedef-type))
  (list :type-name (typedef-type-name thing)))

(defstruct (objc-interface-type (:include clang-type)))

(defmethod deep-copy ((thing objc-interface-type))
  (copy-objc-interface-type thing))

(defstruct (objc-object-pointer-type (:include clang-type)))

(defmethod deep-copy ((thing objc-object-pointer-type))
  (copy-objc-object-pointer-type thing))

(defstruct (function-type (:include clang-type))
  (return-type nil)
  (argument-types nil))

(defstruct (function-no-proto-type (:include function-type)))

(defmethod deep-copy ((thing function-no-proto-type))
  (copy-function-no-proto-type thing))

(defmethod to-noffi-type ((thing function-no-proto-type))
  (list :function (to-noffi-type (function-type-return-type thing))
	(mapcar #'(lambda (arg-type)
		    (list (list 'noffi::decl nil arg-type)))
		(mapcar 'to-noffi-type (function-type-argument-types thing)))))

(defstruct (function-proto-type (:include function-type)))

(defmethod deep-copy ((thing function-proto-type))
  (copy-function-proto-type thing))

(defmethod mangle-type ((thing function-proto-type) abi)
  (warn "cannot mangle-type of ~S" thing)
  nil)

(defmethod to-noffi-type ((thing function-proto-type))
  (list :function (to-noffi-type (function-type-return-type thing))
	(mapcar #'(lambda (arg-type)
		    (list (list 'noffi::decl nil arg-type)))
		(mapcar 'to-noffi-type (function-type-argument-types thing)))))

(defstruct (constant-array-type (:include array-type)))

(defmethod deep-copy ((thing constant-array-type))
  (let ((copy (copy-constant-array-type thing)))
    (setf (constant-array-type-element-type copy)
	  (deep-copy (constant-array-type-element-type copy)))
    copy))

(defmethod to-noffi-type ((thing constant-array-type))
  (list :array (to-noffi-type (constant-array-type-element-type thing))))

(defstruct (vector-type (:include array-vector-or-complex-type)))

(defmethod deep-copy ((thing vector-type))
  (let ((copy (copy-vector-type thing)))
    (setf (vector-type-element-type copy)
	  (deep-copy (vector-type-element-type copy)))
    copy))

(defmethod to-noffi-type ((thing vector-type))
  (warn "cannot convert vector-type ~S" thing))

(defstruct (incomplete-array-type (:include array-type)))

(defmethod deep-copy ((thing incomplete-array-type))
  (let ((copy (copy-incomplete-array-type thing)))
    (setf (incomplete-array-type-element-type copy)
	  (deep-copy (incomplete-array-type-element-type copy)))
    copy))

(defmethod to-noffi-type ((thing incomplete-array-type))
  (list :array (to-noffi-type (incomplete-array-type-element-type thing))))

(defstruct (variable-array-type (:include array-type)))

(defmethod deep-copy ((thing variable-array-type))
  (let ((copy (copy-variable-array-type thing)))
    (setf (variable-array-type-element-type copy)
	  (deep-copy (variable-array-type-element-type copy)))
    copy))

(defmethod to-noffi-type ((thing variable-array-type))
  (list :array (to-noffi-type (variable-array-type-element-type thing))))

(defstruct (dependent-sized-array-type (:include array-type)))

(defmethod deep-copy ((thing dependent-sized-array-type))
  (let ((copy (copy-dependent-sized-array-type thing)))
    (setf (dependent-sized-array-type-element-type copy)
	  (deep-copy (dependent-sized-array-type-element-type copy)))
    copy))

(defmethod to-noffi-type ((thing dependent-sized-array-type))
  (list :array (to-noffi-type (dependent-sized-array-type-element-type thing))))

(defstruct (member-pointer-type (:include clang-type)))

(defmethod deep-copy ((thing member-pointer-type))
  (copy-member-pointer-type thing))

(defmethod to-noffi-type ((thing member-pointer-type))
  (list :member-pointer (member-pointer-type-name thing)))

(defstruct (auto-type (:include clang-type)))

(defmethod deep-copy ((thing auto-type))
  (copy-auto-type thing))

(defstruct (elaborated-type (:include clang-type))
  (named-type nil))

(defmethod deep-copy ((thing elaborated-type))
  (let ((copy (copy-elaborated-type thing)))
    (setf (elaborated-type-named-type copy)
	  (deep-copy (elaborated-type-named-type copy)))
    copy))

#+NIL
(defmethod mangle-type ((thing elaborated-type) (abi msvc))
  (mangle-basic-name (clang-type-name thing) abi))

(defmethod to-noffi-type ((thing elaborated-type))
  (or (and (elaborated-type-named-type thing)
	   (to-noffi-type (elaborated-type-named-type thing)))
      (list :type-name (elaborated-type-name thing))))


(defun is-128-bit-signed-clang-type (ctype)
  (int128-type-p ctype))

(defun is-64-bit-unsigned-clang-type (ctype)
  (ulonglong-type-p ctype))

(defun is-64-bit-signed-clang-type (ctype)
  (longlong-type-p ctype))

(defun is-32-bit-unsigned-clang-type (ctype)
  (or (ulong-type-p ctype)
      (uint-type-p ctype)))

(defun is-32-bit-signed-clang-type (ctype)
  (or (long-type-p ctype)
      (int-type-p ctype)
      (char32-type-p ctype)))

(defun is-16-bit-unsigned-clang-type (ctype)
  (ushort-type-p ctype))

(defun is-16-bit-signed-clang-type (ctype)
  (or (short-type-p ctype)
      (char16-type-p ctype)
      (wchar-type-p ctype)))

(defun is-8-bit-unsigned-clang-type (ctype)
  (or (uchar-type-p ctype)
      (char-u-type-p ctype)))  

(defun is-8-bit-signed-clang-type (ctype)
  (or (char-s-type-p ctype)
      (schar-type-p ctype)))

(defmethod integer-clang-type-p ((integer integer) (clang-type primitive-c-type))
  (cond ((void-type-p clang-type) nil)
	((bool-type-p clang-type) nil)
	((minusp integer)
	 (cond ((< integer #.(- (expt 2 127))) nil)
	       ((< integer #.(- (expt 2 63)))
		(is-128-bit-signed-clang-type clang-type))
	       ((< integer #.(- (expt 2 31)))
		(or (is-64-bit-signed-clang-type clang-type)
		    (is-128-bit-signed-clang-type clang-type)))
	       ((< integer #.(- (expt 2 15)))
		(or (is-32-bit-signed-clang-type clang-type)
		    (is-64-bit-signed-clang-type clang-type)
		    (is-128-bit-signed-clang-type clang-type)))
	       ((< integer #.(- (expt 2 7)))
		(or (is-16-bit-signed-clang-type clang-type)
		    (is-32-bit-signed-clang-type clang-type)
		    (is-64-bit-signed-clang-type clang-type)
		    (is-128-bit-signed-clang-type clang-type)))
	       (t
		(or (is-8-bit-signed-clang-type clang-type)
		    (is-16-bit-signed-clang-type clang-type)
		    (is-32-bit-signed-clang-type clang-type)
		    (is-64-bit-signed-clang-type clang-type)
		    (is-128-bit-signed-clang-type clang-type)))))
	((> integer #.(expt 2 128)) nil)
	((> integer #.(expt 2 127)) (is-128-bit-unsigned-clang-type clang-type))
	((> integer #.(expt 2 64)) (or (is-128-bit-signed-clang-type clang-type)
				       (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 63)) (or (is-64-bit-unsigned-clang-type clang-type)
				       (is-128-bit-signed-clang-type clang-type)
				       (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 32)) (or (is-64-bit-signed-clang-type clang-type)
				       (is-64-bit-unsigned-clang-type clang-type)
				       (is-128-bit-signed-clang-type clang-type)
				       (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 31)) (or (is-32-bit-unsigned-clang-type clang-type)
				       (is-64-bit-signed-clang-type clang-type)
				       (is-64-bit-unsigned-clang-type clang-type)
				       (is-128-bit-signed-clang-type clang-type)
				       (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 16)) (or (is-32-bit-signed-clang-type clang-type)
				       (is-32-bit-unsigned-clang-type clang-type)
				       (is-64-bit-signed-clang-type clang-type)
				       (is-64-bit-unsigned-clang-type clang-type)
				       (is-128-bit-signed-clang-type clang-type)
				       (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 15)) (or (is-16-bit-unsigned-clang-type clang-type)
				      (is-32-bit-signed-clang-type clang-type)
				      (is-32-bit-unsigned-clang-type clang-type)
				      (is-64-bit-signed-clang-type clang-type)
				      (is-64-bit-unsigned-clang-type clang-type)
				      (is-128-bit-signed-clang-type clang-type)
				      (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 8)) (or (is-16-bit-signed-clang-type clang-type)
				      (is-16-bit-unsigned-clang-type clang-type)
				      (is-32-bit-signed-clang-type clang-type)
				      (is-32-bit-unsigned-clang-type clang-type)
				      (is-64-bit-signed-clang-type clang-type)
				      (is-64-bit-unsigned-clang-type clang-type)
				      (is-128-bit-signed-clang-type clang-type)
				      (is-128-bit-unsigned-clang-type clang-type)))
	((> integer #.(expt 2 7)) (or (is-8-bit-unsigned-clang-type clang-type)
				      (is-16-bit-signed-clang-type clang-type)
				      (is-16-bit-unsigned-clang-type clang-type)
				      (is-32-bit-signed-clang-type clang-type)
				      (is-32-bit-unsigned-clang-type clang-type)
				      (is-64-bit-signed-clang-type clang-type)
				      (is-64-bit-unsigned-clang-type clang-type)
				      (is-128-bit-signed-clang-type clang-type)
				      (is-128-bit-unsigned-clang-type clang-type)))
	(t t)))

(defmethod float-clang-type-p (thing (ctype primitive-c-type))
  (and (floatp thing)
       (cond ((zerop thing) (or (float128-type-p ctype)
				(double-type-p ctype)
				(float-type-p ctype)
				(float16-type-p ctype)
				(half-type-p ctype)))
	     ((minusp thing)
	      (cond ((< thing -3.4028235d38) (or (float128-type-p ctype) (double-type-p ctype)))
		    ((> thing -1.4012985d-45) (or (float128-type-p ctype) (double-type-p ctype)))
		    (t (or (float-type-p ctype)
			   (float16-type-p ctype)
			   (half-type-p ctype)))))
	     ((> thing 3.4028235d38) (or (float128-type-p ctype) (double-type-p ctype)))
	     ((< thing 1.4012985d-45) (or (float128-type-p ctype) (double-type-p ctype)))
	     (t (or (float-type-p ctype)
		    (float16-type-p ctype)
		    (half-type-p ctype))))))


(defstruct (struct-declaration (:include namespace))
  (base-types nil)
  (fields nil)
  (methods nil)
  (vtable-p nil)
  (size nil))

(defmethod has-vtable-p ((decl struct-declaration))
  (or (struct-declaration-vtable-p decl)
      (some #'(lambda (type)
		(let ((decl (get-decl type)))
		  (when decl
		    (has-vtable-p decl))))
	    (struct-declaration-base-types decl))))

(defmethod has-vtable-p ((decl type-definition))
  (let ((typedef-type (type-definition-type decl)))
    (unless (primitive-cxx-type-p typedef-type)
      (let ((decl (get-decl typedef-type)))
	(when decl
	  (has-vtable-p decl))))))

(defmethod declaration-has-vtable-p ((decl struct-declaration))
  (struct-declaration-vtable-p decl))

(defmethod declaration-has-vtable-p ((decl type-definition))
  (let ((typedef-type (type-definition-type decl)))
    (unless (primitive-cxx-type-p typedef-type)
      (let ((decl (get-decl typedef-type)))
	(when decl
	  (declaration-has-vtable-p decl))))))

(defmethod base-type-has-vtable-p ((decl struct-declaration))
  (some #'(lambda (type)
	    (let ((decl (get-decl type)))
	      (when decl
		(or (declaration-has-vtable-p decl)
		    (base-type-has-vtable-p decl)))))
	(struct-declaration-base-types decl)))

(defmethod base-type-has-vtable-p ((decl type-definition))
  (let ((typedef-type (type-definition-type decl)))
    (unless (primitive-cxx-type-p typedef-type)
      (let ((decl (get-decl typedef-type)))
	(when decl
	  (base-type-has-vtable-p decl))))))

(defmethod make-noffi-type-declarations ((decl struct-declaration) name (module module) abi)
  (list (noffi::make-declaration :name name
			   :storage-class :typedef
			   :type (apply #'noffi::make-struct-type
				  name
				  :members
				  (if (and (null (struct-declaration-vtable-p decl))
					   (null (struct-declaration-base-types decl))
					   (null (struct-declaration-fields decl)))
				      (list (noffi::make-declaration
					     :name '#_byte :type :char))
				      (append
				       (when (and (struct-declaration-vtable-p decl)
						  (not (base-type-has-vtable-p decl)))
					 (list (noffi::make-declaration
						:name '#___vptr
						:type (noffi::make-pointer-type
						       (noffi::make-pointer-type :void)))))
				   
				       (loop for field in (struct-declaration-fields decl)
					     append (make-noffi-type-declarations
						      field
						      (named-declaration-name field)
						      module
						      abi))))
				  (when (struct-declaration-base-types decl)
				    (list :base (mapcar #'compose-typename 
							(struct-declaration-base-types decl))))))))
							  


(defmethod deep-copy ((thing struct-declaration))
  (let ((copy (copy-struct-declaration thing)))
    (setf (struct-declaration-base-types copy)
	  (copy-seq (struct-declaration-base-types copy)))
    (setf (struct-declaration-fields copy)
	  (mapcar #'deep-copy (struct-declaration-fields copy)))
    (setf (struct-declaration-methods copy)
	  (mapcar #'deep-copy (struct-declaration-methods copy)))
    copy))

(defstruct (union-declaration (:include struct-declaration)))

;; a struct or class field
;; name is c/c++ name of the field in the noffi-c package
;; type is a noffi type
(defstruct (field-declaration (:include named-declaration))
  (type nil)
  (bit-offset nil)
  (access nil))

(defmethod make-noffi-type-declarations ((decl field-declaration) name (module module) abi)
  (list (noffi::make-declaration :name name
			   :specifiers (when (field-declaration-bit-offset decl)
					 (list (list :bit-offset (field-declaration-bit-offset decl))))
			   :type (compose-typename (field-declaration-type decl)))))

(defmethod deep-copy ((thing field-declaration))
  (let ((copy (copy-field-declaration thing)))
    (setf (field-declaration-type copy)
	  (deep-copy (field-declaration-type copy)))
    copy))


(defmethod deep-copy ((thing list))
  (if (not (consp thing))
      thing
      (mapcar #'deep-copy thing)))

;; a c++ class declaration
(defstruct (class-declaration (:include struct-declaration)))

(defmethod deep-copy ((thing struct-declaration))
  (let ((copy (copy-class-declaration thing)))
    (setf (class-declaration-base-types copy)
	  (copy-seq (class-declaration-base-types copy)))
    (setf (class-declaration-fields copy)
	  (mapcar #'deep-copy (class-declaration-fields copy)))
    (setf (class-declaration-methods copy)
	  (mapcar #'deep-copy (class-declaration-methods copy)))
    copy))

#+NIL
(defmethod mangle-type ((thing record-type) (abi msvc))
  (let* ((name (record-type-name thing))
	 (decl (get-declaration name)))
    (if (null decl)
	(warn "no declaration for type ~S" name)
	(concatenate 'string 
		     (cond ((class-declaration-p decl) "V")
			   ((enum-declaration-p decl) "W4")
			   ((union-declaration-p decl) "T")
			   (t "U"))
		     (mangle-basic-name name abi)
		     "@@"))))

(defun mangle-basic-typename (thing abi &optional decl-search-path)
  (let ((decl (get-decl thing decl-search-path)))
    (cond ((enum-declaration-p decl)
	   (concatenate 'string "W4" (mangle-parsed-name thing abi) "@@"))
	  ((union-declaration-p decl)
	   (concatenate 'string "T" (mangle-parsed-name thing abi) "@@"))
	  ((class-declaration-p decl)
	   (concatenate 'string "V" (mangle-parsed-name thing abi) "@@"))
	  ((struct-declaration-p decl)
	   (concatenate 'string "U" (mangle-parsed-name thing abi) "@@"))
	  ((type-definition-p decl)
	   (mangle-type (type-definition-type decl) abi))
	  ((type-alias-p decl)
	   (mangle-type (type-alias-type decl) abi))
	  (t (format nil "(error \"foo8\" ~S ~S)" thing decl-search-path)))))

(defmethod mangle-type ((thing list) (abi msvc))
  (labels ((mangle-pointer-basic (thing)
	     (cond ((type-qualifier-type-p thing)
		      (let ((more2 (caddr thing)))
			(cond ((type-qualifier-type-p more2)
			       (cond ((or (and (eql :const (cadr thing))
					       (eql :volatile (cadr more2)))
					  (and (eql :volatile (cadr thing))
					       (eql :const (cadr more2))))
				      (concatenate 'string "D" (mangle-type (caddr more2) abi)))
				     (t (error "foo1"))))
			      (t (cond ((eql :const (cadr thing))
					(concatenate 'string "B" (mangle-type (caddr thing) abi)))
				       ((eql :volatile (cadr thing))
					(concatenate 'string "C" (mangle-type (caddr thing) abi)))
				       (t (error "foo2")))))))
		   (t (concatenate 'string "A" (mangle-type thing abi)))))
	   
	   )
				   
    
	     
    (cond ((lvalue-ref-type-p thing)
	   (concatenate 'string "AE" (mangle-pointer-basic (cadr thing))))

	  ((rvalue-ref-type-p thing)
	   (concatenate 'string "AE" (mangle-pointer-basic (cadr thing))))

	  ((union-type-p thing)
	   (concatenate 'string "T" (mangle-type (cadr thing) abi) "@@"))
	  
	  ((struct-type-p thing)
	   (concatenate 'string "U" (mangle-type (cadr thing) abi) "@@"))

	  ((array-type-expr-p thing)
	   (concatenate 'string "QE" (mangle-type (cadr thing) abi)))

	  ((qualified-type-p thing)
	   (apply #'concatenate 'string
		  (append
		   (mapcar #'(lambda (namespace)
			       (concatenate 'string (symbol-name namespace) "@"))
			   (butlast (cdr thing)))
		   (list (mangle-basic-typename (car (last thing)) abi (butlast (cdr thing)))))))

	  ((unqualified-type-p thing) ;; hack.
	   (apply #'concatenate 'string
		  (append
		   (mapcar #'(lambda (namespace)
			       (concatenate 'string (symbol-name namespace) "@"))
			   (butlast (cdr thing)))
		   (list (mangle-basic-typename (car (last thing)) abi (butlast (cdr thing)))))))
		  
	  ((type-qualifier-type-p thing)
	   (let ((more1 (caddr thing)))
	     (cond ((type-qualifier-type-p more1)
		    (let ((more2 (caddr more1)))
		      (cond ((type-qualifier-type-p more2)
			     (let ((more3 (caddr more2)))
			       (cond ((pointer-type-expr-p more3)
				      (let ((kwds (append (list (cadr thing))
							  (list (cadr more1))
							  (list (cadr more2)))))
					(cond ((and (member :const kwds)
						    (member :volatile kwds)
						    (member :restrict kwds))
					       (concatenate 'string "SEI"
							    (mangle-pointer-basic (cadr more3))))
					      (t (error "foo4")))))
				     (t (error "foo5")))))
			    ((pointer-type-expr-p more2)
			     (let ((kwds (append (list (cadr thing)) (list (cadr more1)))))
			       (cond ((and (member :const kwds)
					   (member :volatile kwds))
				      (concatenate 'string "SE" (mangle-pointer-basic (cadr more2))))
				     ((and (member :const kwds)
					   (member :restrict kwds))
				      (concatenate 'string "QEI" (mangle-pointer-basic (cadr more2))))
				     ((and (member :volatile kwds)
					   (member :restrict kwds))
				      (concatenate 'string "REI" (mangle-pointer-basic (cadr more2))))
				     (t (error "foo3")))))
			    ((lvalue-ref-type-p more2)
			     (error "foo10"))
			    ((rvalue-ref-type-p more2)
			     (error "foo11"))
			    (t (error "foo12")))))
		   ((pointer-type-expr-p more1)
		    (cond ((eql :const (cadr thing))
			   (concatenate 'string "QE" (mangle-pointer-basic (cadr more1))))
			  ((eq :volatile (cadr thing))
			   (concatenate 'string "RE" (mangle-pointer-basic (cadr more1))))
			  ((eq :restrict (cadr thing))
			   (concatenate 'string "PEI" (mangle-pointer-basic (cadr more1))))
			  (t (error "foo6"))))
		   ((lvalue-ref-type-p more1)
		    (error "foo14"))
		   ((rvalue-ref-type-p more1)
		    (error "foo15"))
		   (t (cond ((eql :const (cadr thing))
			     (mangle-type (caddr thing) abi))
			    ((eql :volatile (cadr thing))
			     (error "foo13"))
			    (t (error "foo7")))))))
	  
	  ((pointer-type-expr-p thing)
	   (concatenate 'string "PE" (mangle-pointer-basic (cadr thing))))	  	   
	  
	  ((type-name-type-p thing)
	   (let ((name (cadr thing)))
	     (cond ((or (eql name 'noffi-c::|bool|)
			(eql name 'noffi-c::|size_t|)
			(eql name 'noffi-c::|wchar_t|)
			(eql name 'noffi-c::|char16_t|)
			(eql name 'noffi-c::|char32_t|)
			(eql name 'noffi-c::|uintptr_t|)
			(eql name 'noffi-c::|string|))
		 
		    (mangle-type name abi))

		   ((or (eql name 'noffi-c::|_m128|)
			(eql name 'noffi-c::|_m128i|)
			(eql name 'noffi-c::|_m128d|)
			(eql name 'noffi-c::|_m256|)
			(eql name 'noffi-c::|_m256i|)
			(eql name 'noffi-c::|_m256d|)
			(eql name 'noffi-c::|_m512|)
			(eql name 'noffi-c::|_m512i|)
			(eql name 'noffi-c::|_m512d|))

		    (concatenate 'string "U" (symbol-name name) "@@"))

		   (t (mangle-basic-typename thing abi)))))
	  
	  ((template-type-p thing)
	   (mangle-basic-typename thing abi))
	  (t (format nil "(error \"foo9 ~S\" thing)" thing)))))

(defun split-name (name)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (if (or (null (position #\: name)) (position #\( name)) ;; it's a function pointer type
      (list (noffi::cintern name))
      (mapcar #'noffi::cintern
	      (remove-if #'(lambda (part)
			     (string= "" part))
			 (mapcar #'(lambda (part)
				     (string-trim '(#\:) part))
				 (uiop/utility:split-string name :separator '(#\:)))))))


(defmethod mangle-basic-name (name namespace (abi msvc))
  (when (symbolp name)
    (setq name (symbol-name name)))
  (handler-case 
    (let ((parsed-name (noffi::parse-type name)))
      (setq parsed-name (resolve parsed-name namespace))
      (mangle-parsed-name parsed-name abi))
    (error () nil)))

(defun mangle-parsed-name (parsed-name abi)
  (flet ((mangle-qualifiers (name)
	     (cond ((qualified-type-p name)
		    (mapcar #'(lambda (part)
				(concatenate 'string (symbol-name part) "@"))
			    (butlast (cdr name))))
		   ((unqualified-type-p name)
		    (mapcar #'(lambda (part)
				(concatenate 'string (symbol-name part) "@"))
			    (butlast (cdr name))))
		   ((symbolp name)
		    (list (concatenate 'string (symbol-name name) "@")))
		   (t (list "(error \"foo104\")")))))
    (cond ((template-type-p parsed-name)
	   (mangle-template-name parsed-name abi))
	    ((type-name-type-p parsed-name)
	     (cond ((qualified-type-p (cadr parsed-name))
		    (apply #'concatenate 'string (symbol-name (first (last (cadr parsed-name)))) "@"
			   (mangle-qualifiers (cadr parsed-name))))
		   ((unqualified-type-p (cadr parsed-name))
		    (apply #'concatenate 'string (symbol-name (first (last (cadr parsed-name)))) "@"
			   (mangle-qualifiers (cadr parsed-name))))
		   ((symbolp (cadr parsed-name))
		    (concatenate 'string (symbol-name (cadr parsed-name)) "@"))
		   (t "(error \"foo103\")")))
	    ((qualified-type-p parsed-name)
	     (apply #'concatenate 'string (symbol-name (first (last parsed-name))) "@"
		    (mangle-qualifiers parsed-name)))
	    ((unqualified-type-p parsed-name)
	     (apply #'concatenate 'string (symbol-name (first (last parsed-name))) "@"
		    (mangle-qualifiers parsed-name)))
	    (t "(error \"foo101\")"))))


(defmethod mangle-template-name (template (abi msvc))
  (cond ((template-type-p template)
	 (apply #'concatenate
		'string
		"?$"
		(symbol-name (first (last (cadr template))))
		 "@"
		 (append
		  (loop for argument in (cddr template)
			collect (cond ((or (template-type-p argument)
					   (type-name-type-p argument))
				       (mangle-basic-typename argument abi))
				      ((and (consp argument)
					    (eq :integer-constant (car argument)))
				       (concatenate 'string "$0" (princ-to-string (caddr argument))))))

		  (mapcar #'(lambda (part)
			      (concatenate 'string (symbol-name part) "@"))
			  (butlast (cdr (cadr template)))))))
	(t (error "foo102"))))
	

;; an argument to a function or a class method, types are noffi types
(defstruct function-argument
  (name nil)
  (type nil)
  (default nil))

(defmethod deep-copy ((thing function-argument))
  (let ((copy (copy-function-argument thing)))
    (setf (function-argument-type copy)
	  (deep-copy (function-argument-type copy)))
    copy))

;; a function declaration, return type is noffi type,
;; name is a noffi-c package symbol of c/c++ name string
(defstruct (function-declaration (:include named-declaration))
  (return-type nil)
  (mangled-name nil)
  (constr-destr-manglings nil)
  (arguments nil)
  (num-args nil)
  (variadic? nil)
  (storage-kind nil)
  (linkage-kind nil))

(defmethod deep-copy ((thing function-declaration))
  (let ((copy (copy-function-declaration thing)))
    (setf (function-declaration-return-type copy)
	  (deep-copy (function-declaration-return-type copy)))
    (setf (function-declaration-arguments copy)
	  (mapcar #'deep-copy (function-declaration-arguments copy)))
    copy))

;; a c++ class method declaration
(defstruct (method-declaration (:include function-declaration))
  (class-name nil)
  (virtual? nil)
  (pure-virtual? nil)
  (static? nil)
  (constructor? nil)
  (destructor? nil)
  (access nil)
  (const? nil))

(defmethod make-noffi-type-declarations ((decl method-declaration) name (module module) abi)
  (if (and (or (method-declaration-constructor? decl)
	       (method-declaration-destructor? decl))
	   (method-declaration-constr-destr-manglings decl))
      (loop for mangled-name in (method-declaration-constr-destr-manglings decl)
	    with arguments = (method-declaration-arguments decl)
	    when (and (not (gethash mangled-name *bar*))
		      (find mangled-name *opencascade-exports* :test #'string=))
	      collect (prog1
			  (noffi::make-declaration
			   :name (noffi::cintern mangled-name)
			   :type
			   (noffi::make-pointer-type
			    (noffi::make-function-type
			     (compose-typename (method-declaration-return-type decl))
			     (append
			      (list
			       (noffi::make-declaration
				:name '#_this
				:type (noffi::make-pointer-type
				       (method-declaration-class-name decl))))
			      (loop for argument in arguments
				    collect (noffi::make-declaration
					     :name (function-argument-name argument)
					     :type (compose-typename
						    (function-argument-type argument))))))))
			(setf (gethash mangled-name *bar*) t)
			(setq *exports2* (remove mangled-name *exports2* :test #'string=))
			(setq arguments (butlast arguments))))
      (when (and (not (gethash (method-declaration-mangled-name decl) *bar*))
		 (find (method-declaration-mangled-name decl) *opencascade-exports* :test #'string=))
	(setq *exports2* (remove (method-declaration-mangled-name decl) *exports2* :test #'string=))
	(setf (gethash (method-declaration-mangled-name decl) *bar*) t)
	(list
	 (noffi::make-declaration
	  :name (noffi::cintern (method-declaration-mangled-name decl))
	  :type (noffi::make-pointer-type
		 (noffi::make-function-type
		  (compose-typename (method-declaration-return-type decl))
		  (append
		   (unless (method-declaration-static? decl)
		     (list
		      (noffi::make-declaration
		       :name '#_this
		       :type (noffi::make-pointer-type
			      (method-declaration-class-name decl)))))
		   (loop for argument in (method-declaration-arguments decl)
			 collect (noffi::make-declaration
				  :name (function-argument-name argument)
				  :type (compose-typename
					 (function-argument-type argument))))))))))))

				      

(defmethod deep-copy ((thing method-declaration))
  (let ((copy (copy-method-declaration thing)))
    (setf (method-declaration-return-type copy)
	  (deep-copy (method-declaration-return-type copy)))
    (setf (method-declaration-arguments copy)
	  (mapcar #'deep-copy (method-declaration-arguments copy)))
    copy))

(defmethod mangle-name ((method method-declaration) (abi msvc))
  (noffi::cintern
	 (apply #'concatenate
		'string
		(special-name-mangling method)
		"@"
		(mangle-basic-name (method-declaration-class-name method)
				   (method-declaration-namespace method)
				   abi)
		"@"
		(mangle-modif method abi)
		"E"
		(case (method-declaration-const? method)
		  (:const "B")
		  (:volatile "C")
		  (:const-volatile "D")
		  (otherwise "A"))
		"A"
		(mangle-type (method-declaration-return-type method) abi)
		(append (loop for parameter in (method-declaration-arguments method)
			      collect (mangle-type (function-argument-type parameter) abi))
			;;(if (null (method-declaration-arguments method))
			    (list "Z")
			    ;;(list "@")
			    ))))
	       

(defmethod mangle-modif ((method method-declaration) (abi msvc))
  (concatenate 'string (mangle-modif-1 (method-declaration-static? method)
				       (method-declaration-access method)
				       (method-declaration-pure-virtual? method)
				       abi)))

(defmethod mangle-modif-1 ((static? (eql t)) (access (eql :public)) (virtual? (eql nil)) (abi msvc))
  "S")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :public)) (virtual? (eql nil)) (abi msvc))
  "Q")

(defmethod mangle-modif-1 ((static? (eql t)) (access (eql :private)) (virtual? (eql nil)) (abi msvc))
  "C")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :private)) (virtual? (eql nil)) (abi msvc))
  "A")

(defmethod mangle-modif-1 ((static? (eql t)) (access (eql :protected)) (virtual? (eql nil)) (abi msvc))
  "K")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :protected)) (virtual? (eql nil)) (abi msvc))
  "I")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :public)) (virtual? (eql t)) (abi msvc))
  "U")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :private)) (virtual? (eql t)) (abi msvc))
  "E")

(defmethod mangle-modif-1 ((static? (eql nil)) (access (eql :protected)) (virtual? (eql t)) (abi msvc))
  "M")

(defmethod special-name-mangling ((thing method-declaration))
  (let ((name (method-declaration-name thing)))
    (concatenate
     'string
     "?"
     (cond ((method-declaration-constructor? thing) "0")
	   ((method-declaration-destructor? thing)  "1")
	   ((eq name '#.(noffi::cintern "operator new")) "2")
	   ((eq name '#.(noffi::cintern "operator delete")) "3")
	   ((eq name '#.(noffi::cintern "operator=")) "4")
	   ((eq name '#.(noffi::cintern "operator>>")) "5")
	   ((eq name '#.(noffi::cintern "operator<<")) "6")
	   ((eq name '#.(noffi::cintern "operator!")) "7")
	   ((eq name '#.(noffi::cintern "operator==")) "8")
	   ((eq name '#.(noffi::cintern "operator!=")) "9")
	   ((eq name '#.(noffi::cintern "operator[]")) "A")
	   ((eq name '#.(noffi::cintern "operator returntype")) "B")
	   ((eq name '#.(noffi::cintern "operator->")) "C")
	   ((eq name '#.(noffi::cintern "operator*")) "D")
	   ((eq name '#.(noffi::cintern "operator++")) "E")
	   ((eq name '#.(noffi::cintern "operator--")) "F")
	   ((eq name '#.(noffi::cintern "operator-")) "G")
	   ((eq name '#.(noffi::cintern "operator+")) "H")
	   ((eq name '#.(noffi::cintern "operator&")) "I")
	   ((eq name '#.(noffi::cintern "operator->*")) "J")
	   ((eq name '#.(noffi::cintern "operator/")) "K")
	   ((eq name '#.(noffi::cintern "operator%")) "L")
	   ((eq name '#.(noffi::cintern "operator<")) "M")
	   ((eq name '#.(noffi::cintern "operator<=")) "N")
	   ((eq name '#.(noffi::cintern "operator>")) "O")
	   ((eq name '#.(noffi::cintern "operator>=")) "P")
	   ((eq name '#.(noffi::cintern "operator,")) "Q")
	   ((eq name '#.(noffi::cintern "operator()")) "R")
	   ((eq name '#.(noffi::cintern "operator~")) "S")
	   ((eq name '#.(noffi::cintern "operator^")) "T")
	   ((eq name '#.(noffi::cintern "operator|")) "U")
	   ((eq name '#.(noffi::cintern "operator&&")) "V")
	   ((eq name '#.(noffi::cintern "operator||")) "W")
	   ((eq name '#.(noffi::cintern "operator*=")) "X")
	   ((eq name '#.(noffi::cintern "operator+=")) "Y")
	   ((eq name '#.(noffi::cintern "operator-=")) "Z")
	   ((eq name '#.(noffi::cintern "operator/")) "_0")
	   ((eq name '#.(noffi::cintern "operator%")) "_1")
	   ((eq name '#.(noffi::cintern "operator>>=")) "_2")
	   ((eq name '#.(noffi::cintern "operator<<=")) "_3")
	   ((eq name '#.(noffi::cintern "operator&=")) "_4")
	   ((eq name '#.(noffi::cintern "operator|=")) "_5")
	   ((eq name '#.(noffi::cintern "operator^=")) "_6")
	   ((eq name '#.(noffi::cintern "vftable")) "_7")
	   ((eq name '#.(noffi::cintern "vbtable")) "_8")
	   ((eq name '#.(noffi::cintern "vcall")) "_9")
	   ((eq name '#.(noffi::cintern "typeof")) "_A")
	   ;; these following names probably don't apply to clang asts
	   ((eq name '#.(noffi::cintern "local static guard")) "_B")
	   ((eq name '#.(noffi::cintern "vbase destructor")) "_D")
	   ((eq name '#.(noffi::cintern "vector deleting destructor")) "_E")
	   ((eq name '#.(noffi::cintern "default constructor closure")) "_F")
	   ((eq name '#.(noffi::cintern "scalar deleting destructor")) "_G")
	   ((eq name '#.(noffi::cintern "vector constructor iterator")) "_H")
	   ((eq name '#.(noffi::cintern "vector destructor iterator")) "_I")
	   ((eq name '#.(noffi::cintern "vector vbase constructor iterator")) "_J")
	   ((eq name '#.(noffi::cintern "virtual displacement map")) "_K")
	   ((eq name '#.(noffi::cintern "eh vector constructor iterator")) "_L")
	   ((eq name '#.(noffi::cintern "eh vector destructor iterator")) "_M")
	   ((eq name '#.(noffi::cintern "eh vector vbase constructor iterator")) "_N")
	   ((eq name '#.(noffi::cintern "copy constructor closure")) "_O")
	   ((eq name '#.(noffi::cintern "udt returning")) "_P")
	   ((eq name '#.(noffi::cintern "local vftable")) "_S")
	   ((eq name '#.(noffi::cintern "local vftable constructor closure")) "_T")
	   ((eq name '#.(noffi::cintern "operator new[]")) "_U")
	   ((eq name '#.(noffi::cintern "operator delete[]")) "_V")
	   ((eq name '#.(noffi::cintern "managed vector constructor iterator")) "__A")
	   ((eq name '#.(noffi::cintern "managed vector destructor iterator")) "__B")
	   ((eq name '#.(noffi::cintern "eh vector copy constructor iterator")) "__C")
	   ((eq name '#.(noffi::cintern "eh vector vbase copy constructor iterator")) "__D")
	   ((eq name '#.(noffi::cintern "dynamic initializer")) "__E")
	   ((eq name '#.(noffi::cintern "dynamic atexit destructor")) "__F")
	   ((eq name '#.(noffi::cintern "vector copy constructor iterator")) "__G")
	   ((eq name '#.(noffi::cintern "vector vbase copy constructor iterator")) "__H")
	   ((eq name '#.(noffi::cintern "managed vector copy constructor iterator")) "__I")
	   ((eq name '#.(noffi::cintern "local static thread guard")) "__J")
	   (t (symbol-name name))))))
	   
	   
	   
	   
	   
	   
	   
	    
	 

(defmethod deep-copy (thing)
  thing)

(defstruct (struct-template-definition (:include struct-declaration)))

(defun deep-copy-struct-template-definition-to-struct-declaration (ctd arguments)
  (make-struct-declaration :name (compose-template-name ctd arguments)
			   :namespace (struct-template-definition-namespace ctd)
			   :base-types (copy-seq (struct-template-definition-base-types ctd))
			   :fields (mapcar #'deep-copy
					   (struct-template-definition-fields ctd))
			   :methods (mapcar #'deep-copy
					    (struct-template-definition-methods ctd))
			   :vtable-p (struct-template-definition-vtable-p ctd)
			   :punt (struct-template-definition-punt ctd)))

;; a c++ class template definition
(defstruct (class-template-definition (:include class-declaration))
  (arguments nil))

(defun deep-copy-class-template-definition-to-class-declaration (ctd arguments)
  (make-class-declaration :name (compose-template-name ctd arguments)
			  :namespace (class-template-definition-namespace ctd)
			  :base-types (copy-seq (class-template-definition-base-types ctd))
			  :fields (mapcar #'deep-copy
					  (class-template-definition-fields ctd))
			  :methods (mapcar #'deep-copy
					   (class-template-definition-methods ctd))
			  :vtable-p (class-template-definition-vtable-p ctd)
			  :punt (class-template-definition-punt ctd)))





(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *cursor-kind-plist*
  '(#_CXCursor_UnexposedDecl process-unexposed-decl
    #_CXCursor_StructDecl process-struct-decl
    #_CXCursor_UnionDecl process-union-decl
    #_CXCursor_ClassDecl process-class-decl
    #_CXCursor_EnumDecl process-enum-decl
    #_CXCursor_FieldDecl process-field-decl
    #_CXCursor_EnumConstantDecl process-enum-constant-decl
    #_CXCursor_FunctionDecl process-function-decl
    #_CXCursor_VarDecl process-var-decl
    #_CXCursor_ParmDecl process-parm-decl
    #_CXCursor_ObjCInterfaceDecl process-objc-interface-decl
    #_CXCursor_ObjCCategoryDecl process-objc-category-decl
    #_CXCursor_ObjCProtocolDecl process-objc-protocol-decl
    #_CXCursor_ObjCPropertyDecl process-objc-property-decl
    #_CXCursor_ObjCIvarDecl process-objc-ivar-decl
    #_CXCursor_ObjCInstanceMethodDecl process-objc-instance-method-decl
    #_CXCursor_ObjCCategoryImplDecl process-objc-category-impl-decl
    #_CXCursor_TypedefDecl process-typedef-decl
    #_CXCursor_CXXMethod process-cxx-method
    #_CXCursor_Namespace process-namespace
    #_CXCursor_LinkageSpec process-linkage-spec
    #_CXCursor_Constructor process-constructor
    #_CXCursor_Destructor process-destructor
    #_CXCursor_ConversionFunction process-conversion-function
    #_CXCursor_TemplateTypeParameter process-template-type-parameter
    #_CXCursor_NonTypeTemplateParameter process-non-type-template-parameter
    #_CXCursor_TemplateTemplateParameter process-template-template-parameter
    #_CXCursor_FunctionTemplate process-function-template
    #_CXCursor_ClassTemplate process-class-template
    #_CXCursor_ClassTemplatePartialSpecialization
    process-class-template-partial-specialization
    #_CXCursor_NamespaceAlias process-namespace-alias
    #_CXCursor_UsingDirective process-using-directive
    #_CXCursor_UsingDeclaration process-using-declaration
    #_CXCursor_TypeAliasDecl process-type-alias-decl
    #_CXCursor_ObjCSythesizeDecl process-objc-synthesize-decl
    #_CXCursor_ObjCDynamicDecl process-objc-dynamic-decl
    #_CXCursor_CXXAccessSpecifier process-cxx-access-specifier
    #_CXCursor_ObjCSuperClassRef process-objc-super-class-ref
    #_CXCursor_ObjCProtocolRef process-objc-protocol-ref
    #_CXCursor_ObjCClassRef process-objc-class-ref
    #_CXCursor_TypeRef process-type-ref
    #_CXCursor_CXXBaseSpecifier process-cxx-base-specifier
    #_CXCursor_TemplateRef process-template-ref
    #_CXCursor_NamespaceRef process-namespace-ref
    #_CXCursor_MemberRef process-member-ref
    #_CXCursor_LabelRef process-label-ref
    #_CXCursor_OverloadedDeclRef process-overloaded-decl-ref
    #_CXCursor_VariableRef process-variable-ref
    #_CXCursor_InvalidFile process-invalid-file
    #_CXCursor_NotImplemented process-not-implemented
    #_CXCursor_InvalidCode process-invalid-code
    #_CXCursor_UnexposedExpr process-unexposed-expr
    #_CXCursor_DeclRefExpr process-decl-ref-expr
    #_CXCursor_MemberRefExpr process-member-ref-expr
    #_CXCursor_CallExpr process-call-expr
    #_CXCursor_ObjCMessageExpr process-objc-message-expr
    #_CXCursor_BlockExpr process-block-expr
    #_CXCursor_IntegerLiteral process-integer-literal
    #_CXCursor_FloatingLiteral process-floating-literal
    #_CXCursor_ImaginaryLiteral process-imaginary-literal
    #_CXCursor_StringLiteral process-string-literal
    #_CXCursor_CharacterLiteral process-character-literal
    #_CXCursor_ParenExpr process-paren-expr
    #_CXCursor_UnaryOperator process-unary-operator
    #_CXCursor_ArraySubscriptExpr process-array-subscript-expr
    #_CXCursor_BinaryOperator process-binary-operator
    #_CXCursor_CompoundAssignOperator process-compound-assign-operator
    #_CXCursor_ConditionalOperator process-conditional-operator
    #_CXCursor_CStyleCastExpr process-c-style-cast-expr
    #_CXCursor_CompoundLiteralExpr process-compound-literal-expr
    #_CXCursor_InitListExpr process-init-list-expr
    #_CXCursor_AddrLabelExpr process-addr-label-expr
    #_CXCursor_StmtExpr process-stmt-expr
    #_CXCursor_GenericSelectionExpr process-generic-selection-expr
    #_CXCursor_GNUNullExpr process-gnu-null-expr
    #_CXCursor_CXXStaticCastExpr process-static-cast-expr
    #_CXCursor_CXXDynamicCasrExpr process-dynamic-cast-expr
    #_CXCursor_ReinterpretCastExpr process-reinterpret-cast-expr
    #_CXCursor_CXXConstCastExpr process-cxx-const-cast-expr
    #_CXCursor_CXXFunctionalCastExpr process-cxx-functional-cast-expr
    #_CXCursor_CXXTypeidExpr process-cxx-type-id-expr
    #_CXCursor_CXXBoolLiteralExpr process-cxx-bool-literal-expr
    #_CXCursor_CXXNullPtrLiteralExpr process-cxx-null-ptr-literal-expr
    #_CXCursor_CXXThisExpr process-cxx-this-expr
    #_CXCursor_CXXThrowExpr process-cxx-throw-expr
    #_CXCursor_CXXNewExpr process-cxx-new-expr
    #_CXCursor_DeleteExpr process-cxx-delete-expr
    #_CXCursor_UnaryExpr process-unary-expr
    #_CXCursor_ObjCStringLiteral process-objc-string-literal
    #_CXCursor_ObjCEncodeExpr process-objc-encode-expr
    #_CXCursor_ObjCSelectorExpr process-objc-selector-expr
    #_CXCursor_ObjCProtocolExpr process-objc-protocol-expr
    #_CXCursor_ObjCBridgeCastExpr process-objc-bridged-cast-expr
    #_CXCursor_PackExpansionExpr process-pack-expansion-expr
    #_CXCursor_SizeOfPackExpr process-size-of-pack-expr
    #_CXCursor_LambdaExpr process-lambda-expr
    #_CXCursor_ObjCBoolLiteralExpr process-bool-literal-expr
    #_CXCursor_ObjCSelfExpr process-objc-self-expr
    #_CXCursor_OMPArraySectionExpr process-omp-array-secion-expr
    #_CXCursor_ObjcAvailabilityCheckExpr process-objc-availability-check-expr
    #_CXCursor_FixedPointLiteral process-fixed-point-literal
    #_CXCursor_OMPArrayShapingExpr process-omp-array-shaping-expr
    #_CXCursor_OMPIteratorExpr prcess-omp-iterator-expr
    #_CXCursor_CXXAddrspaceCastExpr process-cxx-addrspace-cast-expr
    #_CXCursor_ConceptSpecializationExpr process-concept-specialization-expr
    #_CXCursor_RequiresExpr process-requires-expr
    #_CXCursor_CXXParenListInitExpr process-paren-list-init-expr
    #_CXCursor_UnexposedStmt process-unexposed-stmt-expr
    #_CXCursor_LabelStmt process-label-stmt
    #_CXCursor_CompoundStmt process-compound-stmt
    #_CXCursor_CaseStmt process-case-stmt
    #_CXCursor_DefaultStmt process-default-stmt
    #_CXCursor_IfStmt process-if-stmt
    #_CXCursor_SwitchStmt process-switch-stmt
    #_CXCursor_WhileStmt process-while-stmt
    #_CXCursor_DoStmt process-do-stmt
    #_CXCursor_ForStmt process-for-stmt
    #_CXCursor_GotoStmt process-goto-stmt
    #_CXCursor_IndirectGotoStmt process-indirect-goto-stmt
    #_CXCursor_BreakStmt process-break-stmt
    #_CXCursor_GCCAsmStmt process-asm-stmt
    #_CXCursor_AsmStmt process-asm-stmt
    #_CXCursor_ObjCAtTryStmt process-objc-at-try-stmt
    #_CXCursor_ObjCAtCatchStmt process-objc-at-catch-stmt
    #_CXCursor_ObjCAtFinallyStmt process-objc-at-finally-stmt
    #_CXCursor_ObjCAtThrowStmt process-objc-at-throw-stmt
    #_CXCursor_ObjCAtSynchronizedStmt process-objc-at-synchronized-stmt
    #_CXCursor_ObjCAutoreleasePoolStmt process-objc-autorelease-pool-stmt
    #_CXCursor_ObjCForCollectionStmt process-objc-for-collection-stmt
    #_CXCursor_CXXCatchStmt process-cxx-catch-stmt
    #_CXCursor_CXXTryStmt process-cxx-try-stmt
    #_CXCursor_CXXForRangeStmt process-for-range-stmt
    #_CXCursor_SEHTryStmt process-seh-try-stmt
    #_CXCursor_SEHExceptStmt process-seh-except-stmt
    #_CXCursor_SEHFinallyStmt process-seh-finally-stmt
    #_CXCursor_MSAsmStmt process-ms-asm-stmt
    #_CXCursor_NullStmt process-null-stmt
    #_CXCursor_DeclStmt process-decl-stmt
    #_CXCursor_OMPParallelDirective process-omp-parallel-directive
    #_CXCursor_OMPSimdDirective process-omp-simd-directive
    #_CXCursor_OMPForDirective process-omp-for-directive
    #_CXCursor_OMPSectionsDirective process-omp-sections-directive
    #_CXCursor_OMPSingleDirective process-omp-single-directive
    #_CXCursor_OMPParallelForDirective process-omp-parallel-for-directive
    #_CXCursor_OMPParallelSectionsDirective process-omp-parallel-sections-directive
    #_CXCursor_OMPTaskDirective process-omp-task-directive
    #_CXCursor_OMPMasterDirective process-omp-master-directive
    #_CXCursor_OMPCriticalDirective process-omp-critical-directive
    #_CXCursor_OMPTaskyieldDirective process-omp-taskyield-directive
    #_CXCursor_OMPBarrierDirective process-omp-barrier-directive
    #_CXCursor_OMPTaskWaitDirective process-omp-taskwait-directive
    #_CXCursor_OMPFlushDirective process-omp-flush-directive
    #_CXCursor_SEHLeaveStmt process-seh-leave-stmt
    #_CXCursor_OMPOrderedDirective process-omp-ordered-directive
    #_CXCursor_OMPAtomicDirective process-omp-atomic-directive
    #_CXCursor_OMPForSimdDirective process-for-simd-directive
    #_CXCursor_OMPParallelForSimdDirective process-omp-parallel-for-simd-directive
    #_CXCursor_OMPTargetDirective process-omp-target-directive
    #_CXCursor_OMPTeamsDirective process-omp-teams-directive
    #_CXCursor_OMPTaskgroupDirective process-omp-taskgroup-directive
    #_CXCursor_OMPCancellationPointDirective process-omp-cancallation-point-directive
    #_CXCursor_OMPCancelDirective process-omp-cancel-directive
    #_CXCursor_OMPTargetDataDirective process-omp-target-data-directive
    #_CXCursor_OMPTaskLoopDirective process-omp-task-loop-directive
    #_CXCursor_OMPTaskLoopSimdDirective process-omp-task-loop-simd-directive
    #_CXCursor_OMPDistributeDirective process-omp-distribute-directive
    #_CXCursor_OMPTargetEnterDataDirective process-omp-target-enter-data-directive
    #_CXCursor_OMPTargetExitDataDirective process-omp-target-exit-data-directive
    #_CXCursor_OMPTargetParallelDirective process-target-parallel-directive
    #_CXCursor_OMPTargetParallelForDirective process-target-parallel-for-directive
    #_CXCursor_OMPTargetUpdateDirective process-omp-target-update-directive
    #_CXCursor_OMPDistributeParallelForDirective
    process-omp-distribute-parallel-for-simd-directive
    #_CXCursor_OMPDistributeParallelForSimdDirective
    process-omp-distribute-parallel-for-simd-directive
    #_CXCursor_OMPDistributeSimdDirective process-omp-distibute-simd-directive
    #_CXCursor_OMPTargetParallelForSimdDirective
    process-omp-target-parallel-for-simd-directive
    #_CXCursor_OMPTargetSimdDirective process-omp-target-simd-directive
    #_CXCursor_OMPTeamsDistributeDirective process-omp-teams-distribute-directive
    #_CXCursor_OMPTeamsDistributeSimdDirective process-omp-teams-distribute-simd-directive
    #_CXCursor_OMPTeamsDistributeParallelForSimdDirective
    process-omp-teams-distribute-parallel-for-simd-directive
    #_CXCursor_OMPTeamsDistributeParallelForDirective
    process-omp-teams-distribute-prallel-for-directive
    #_CXCursor_OMPTargetTeamsDirective process-omp-target-teams-directive
    #_CXCursor_OMPTargetTeamsDistributeDirective
    process-omp-target-teams-distribute-directive
    #_CXCursor_OMPTargetTeamsDistributeParallelForDirective
    process-omp-target-teams-distribute-parallel-for-directive
    #_CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective
    process-omp-target-teams-distribute-parallel-for-simd-directive
    #_CXCursor_OMPTargetTeamsDistributeSimdDirective
    process-omp-target-teams-distribute-simd-directive
    #_CXCursor_BuiltinBitCastExpr process-builtin-bit-cast-expr
    #_CXCursor_OMPMasterTaskLoopDirective process-omp-master-task-loop-directive	 
    #_CXCursor_OMPParallelMasterTaskLoopDirective
    process-omp-parallel-master-task-loop-directive
    #_CXCursor_OMPMasterTaskLoopSimdDirective
    process-omp-master-task-loop-simd-directive
    #_CXCursor_OMPParallelMasterTaskLoopSimdDirective
    process-omp-parallel-master-task-loop-simd-directive
    #_CXCursor_OMPParallelMasterDirective process-omp-parallel-master-directive
    #_CXCursor_OMPDepobjDirective process-omp-depobj-directive
    #_CXCursor_OMPScanDirective process-omp-scan-directive
    #_CXCursor_OMPTileDirective process-omp-tile-directive
    #_CXCursor_OMPCanonicalLoop process-omp-canonical-loop
    #_CXCursor_OMPInteropDirective process-omp-iterop-directive
    #_CXCursor_OMPDispatchDirective process-omp-dispatch-directive
    #_CXCursor_OMPMaskedDirective process-omp-masked-directive
    #_CXCursor_OMPUnrollDirective process-omp-unroll-directive
    #_CXCursor_OMPMetaDirective process-omp-meta-directive
    #_CXCursor_OMPGenericLoopDirective process-omp-generic-loop-directive
    #_CXCursor_OMPTeamsGenericLoopDirective process-teams-generic-loop-directive
    #_CXCursor_OMPTargetTeamsGenericLoopDirective
    process-omp-target-teams-generic-loop-directive
    #_CXCursor_OMPParallelGenericLoopDirective
    process-omp-parallel-generic-loop-directive
    #_CXCursor_OMPTargetParallelGenericLoopDirective
    process-omp-target-parallel-generic-loop-directive
    #_CXCursor_OMPParallelMaskedDirective process-omp-parallel-masked-directive
    #_CXCursor_OMPMaskedTaskLoopDirective process-omp-masked-task-loop-directive
    #_CXCursor_OMPMaskedTaskLoopSimdDirective process-omp-masked-task-loop-simd-directive
    #_CXCursor_OMPParallelMaskedTaskLoopDirective
    process-omp-parallel-masked-task-loop-directive
    #_CXCursor_OMPParallelMaskedTaskLoopSimdDirective
    process-omp-prallel-masked-task-loop-simd-directive
    #_CXCursor_OMPErrorDirective process-omp-error-directive	 
    #_CXCursor_OMPScopeDirective process-omp-scope-directive
    #_CXCursor_TranslationUnit process-translation-unit
    #_CXCursor_UnexposedAttr process-unexposed-attr
    #_CXCursor_IBActionAttr process-ib-action-attr
    #_CXCursor_IBOutletAttr process-ib-outlet-attr
    #_CXCursor_IBOutletCollectionAttr process-ib-outlet-collection-attr
    #_CXCursor_CXXFinalAttr process-cxx-final-attr
    #_CXCursor_CXXOverrideAttr process-cxx-override-atrr
    #_CXCursor_AnnotateAttr process-annotate-attr
    #_CXCursor_AsmLabelAttr process-asm-label-attr
    #_CXCursor_PackedAttr process-packed-attr
    #_CXCursor_PureAttr process-pure-attr
    #_CXCursor_ConstAttr process-const-attr
    #_CXCursor_NoDuplicateAttr process-no-duplicate-attr
    #_CXCursor_CUDAConstantAttr process-cuda-constant-attr
    #_CXCursor_CUDADeviceAttr process-cuda-device-attr
    #_CXCursor_CUDAGlobalAttr process-cuda-global-attr
    #_CXCursor_CUDAHostAttr process-cuda-host-attr
    #_CXCursor_CUDASharedAttr process-cuda-shared-attr
    #_CXCursor_VisibilityAttr process-visibility-attr
    #_CXCursor_DLLExport process-dll-export
    #_CXCursor_DLLImport process-dll-import
    #_CXCursor_NSReturnsRetained process-ns-returns-retained
    #_CXCursor_NSReturnsNotRetained process-ns-returns-not-retained
    #_CXCursor_NSReturnsAutoreleased process-ns-returns-autoreleased
    #_CXCursor_NSConsumesSelf process-ns-consumes-self
    #_CXCursor_NSConsumed process-ns-consumed
    #_CXCursor_ObjCException process-objc-exception
    #_CXCursor_ObjCNSObject process-objc-ns-object
    #_CXCursor_ObjCIndependentClass process-objc-independent-class
    #_CXCursor_ObjCPreciseLifetime process-objc-precise-lifetime
    #_CXCursor_ObjCReturnsInnerPointer process-objc-returns-inner-pointer
    #_CXCursor_ObjCRequiresSuper process-objc-requires-super
    #_CXCursor_ObjCRootClass process-objc-root-class
    #_CXCursor_ObjCSubclassingRestricted process-objc-subclassing-restricted
    #_CXCursor_ObjCExplicitProtocolImpl process-objc-explicit-protocol-impl
    #_CXCursor_ObjCDesignatedInitializer process-objc-designated-initializer
    #_CXCursor_ObjCRuntimeVisible process-objc-runtime-visible
    #_CXCursor_ObjCBoxable process-objc-boxable
    #_CXCursor_FlagEnum process-flag-enum
    #_CXCursor_ConvergentAttr process-convergent-attr
    #_CXCursor_WarnUnusedAttr process-warn-unused-attr
    #_CXCursor_WarnUnusedResultAttr process-warn-unused-result-attr
    #_CXCursor_AlignedAttr process-aligned-attr
    #_CXCursor_PreprocessingDirective process-preprocessing-directive
    #_CXCursor_MacroDefinition process-macro-definition
    #_CXCursor_MacroExpansion process-macro-expansion
    #_CXCursor_InclusionDirective process-inclusion-directive
    #_CXCursor_ModuleImportDecl process-module-import-decl
    #_CXCursor_TypeAliasTemplateDecl process-type-alias-template-decl
    #_CXCursor_StaticAssert process-static-assert
    #_CXCursor_FriendDecl process-friend-decl
    #_CXCursor_ConceptDecl process-concept-decl
    #_CXCursor_OverloadCandidate process-overload-candidate))
)

(defmacro define-default-process-methods ()
  `(progn
     ,@(loop for (kind gf-name) on *cursor-kind-plist* by #'cddr
	     append (list `(defmethod ,gf-name :before (module abi ffi pass cursor parent name data)
			     (when (> *verbosity* 1)
			       (unless (or (eq ',gf-name 'process-macro-expansion)
					   (eq ',gf-name 'process-macro-definition)
					   (eq ',gf-name 'process-inclusion-directive))
				 (format t "~%~A ~A" ',gf-name name)
				 (force-output))))
			  `(defmethod ,gf-name (module abi ffi pass cursor parent name data)
			     :do-nothing
			     t)))))

(define-default-process-methods)

(defun main (&rest args)
  (print (first args))
  (noffi::use-library #+darwin "~/clang/lib/libclang.dylib"
		      #+win32 "~/clang/bin/libclang.dll"
		      #+linux "~/clang/lib/libclang.so"
		      )
  (let* ((index (#_clang_createIndex 0 1))
	 (*unit* (#_clang_parseTranslationUnit index 0 (c-coerce args '#_<char**>) (length args) 0 0
					     (logior #_CXTranslationUnit_DetailedPreprocessingRecord
						     #_CXTranslationUnit_SkipFunctionBodies))))
    (if (null *unit*)
	(error "Unable to parse translation unit.")
	(let ((root (#_clang_getTranslationUnitCursor *unit*))
	      (filename (first args)))
	  (unwind-protect (process-translation-unit
			   *module* *abi* *ffi* *pass* root nil (noffi::cintern filename) nil)
	    ;;(setq *public?* :unset)
	    ;;(setq *base-type* nil)
	    (setq *overloads* :unset)
	    ;;(setq *handle-type?* nil)
	    (#_clang_disposeTranslationUnit *unit*)
	    (#_clang_disposeIndex index)
	    ))))
  #+NIL
  (let ((template-types (reverse *template-types*)))
    (loop for template-type in template-types
	  do (unless (gethash template-type *done-templates*)
	       (setf (gethash template-type *done-templates*) template-type)
	       (let ((*current-template-type* template-type))
		 (with-open-file (cpp-stream "~/cxxbinder/templates.hxx" :direction :output
									 :if-does-not-exist :create
									 :if-exists :supersede)
		   (let ((template-arguments (template-arguments template-type)))
		     (loop for argument in template-arguments
			   do (format cpp-stream "~%#include <~A.hxx>" argument)
			   finally (format cpp-stream "~%#include <~A.hxx>" (template-name template-type))))
		   (format cpp-stream "~%template class ~A;" template-type))
		 (format t "~%Processing ~A~%" template-type)
		 (let ((*template-types* nil))
		   (let ((*pass* 0))
		     (apply #'main "c:/Users/awolven/cxxbinder/templates.hxx" (rest args)))
		   (let ((*overloads* (make-hash-table :test #'equalp)))
		     (let ((*pass* 1))
		       (apply #'main "c:/Users/awolven/cxxbinder/templates.hxx" (rest args)))))
		 )))))
	  
#+NIL
(main "/Users/awolven/OCCT-7_8_0/include/Geom_BezierCurve.hxx" "-I" "/Users/awolven/OCCT-7_8_0/include/" "-I" "/usr/local/include/" "-I" "/Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include/c++/v1" "-I" "/Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include" "-I" "/usr/local/ffigen/include")

#+unix
(defun invoke-with-packed-cxcursor (cont w0 w1 w2 w3)
  (clet& ((cx #_<CXCursor>))
    (setf (c-aref (noffi::c-cast '#_<unsigned long long *> cx) 0) w0)
    (setf (c-aref (noffi::c-cast '#_<unsigned long long *> cx) 1) w1)
    (setf (c-aref (noffi::c-cast '#_<unsigned long long *> cx) 2) w2)
    (setf (c-aref (noffi::c-cast '#_<unsigned long long *> cx) 3) w3)
    (funcall cont cx)
    1))

(defvar *visit-func*)

#+unix
(defcfun (visit_func :int)
    ((data (:pointer :void)) (pad2 :int) (pad3 :int)
     (pad4 :int) (pad5 :int) (pad6 :int)
     (w1 :unsigned-long-long) (w2 :unsigned-long-long)
     (w3 :unsigned-long-long) (w4 :unsigned-long-long)
     (v1 :unsigned-long-long) (v2 :unsigned-long-long)
     (v3 :unsigned-long-long) (v4 :unsigned-long-long))
  (declare (ignore pad2 pad3 pad4 pad5 pad6))
  (declare (ignorable data))
  (invoke-with-packed-cxcursor
   (lambda (c1)
     (invoke-with-packed-cxcursor
      (lambda (c2)
	(funcall *visit-func* (c-aref c2) (c-aref c1) data))
      v1 v2 v3 v4))
   w1 w2 w3 w4))

#+win32
(defcfun (visit_func :int)
	 ((cursor #_<CXCursor*>)
	  (parent #_<CXCursor*>)
	  (data #_<void*>))
  (funcall *visit-func* (when cursor (c-aref cursor)) (when parent (c-aref parent)) data)
  1)

(defvar *header-in-question*)

(defun get-cursor-name (cursor)
  (let ((ident (#_clang_getCursorSpelling cursor)))
    (when ident
      (let ((cstring (#_clang_getCString ident)))
	(when cstring
	  (unwind-protect (let ((name (get-c-string cstring)))
			    (unless (string= name "")
			      (noffi::cintern name)))
	    #+NIL
	    (#_clang_disposeString cstring)))))))
	


(defun ensure-type-definition (key type)
  (let ((existing (gethash key (namespace-declarations *current-namespace*))))
    (if existing
	(progn (when (type-definition-p existing)
		 (unless (type-definition-type existing)
		   (setf (type-definition-type existing) type)))
	       existing)
	(setf (gethash key (namespace-declarations *current-namespace*))
	      (let ((decl
		      (make-type-definition :namespace *current-namespace*
					    :type type)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl)))))

(defun ensure-type-alias (key type)
  (let ((existing (gethash key (namespace-declarations *current-namespace*))))
    (if existing
	(progn (when (type-alias-p existing)
		 (unless (type-alias-type existing)
		   (setf (type-alias-type existing) type)))
	       existing)
	(setf (gethash key (namespace-declarations *current-namespace*))
	      (let ((decl
		      (make-type-alias :namespace *current-namespace*
				       :type type)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl)))))
		

(defun ensure-type-alias-template (key)
  (let ((existing (gethash key (namespace-template-definitions *current-namespace*))))
    (if existing
	(progn (unless (type-alias-template-decl-p existing)
		 (warn "~S is not of type type-alias-template-decl." existing))
		 )
	       existing)
	(setf (gethash key (namespace-template-definitions *current-namespace*))
	      (let ((decl (make-type-alias-template-decl
			   :namespace *current-namespace*)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl))))		

(defun ensure-class-declaration (key)
  (let ((existing (gethash key (namespace-declarations *current-namespace*))))
    (if existing
	(if (not (class-declaration-p existing))
	    (error "expected a class-declaration for ~A, got a ~A" key (type-of existing))
	    existing)
	(setf (gethash key (namespace-declarations *current-namespace*))
	      (let ((decl (make-class-declaration :namespace *current-namespace*)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl)))))
		  

(defun ensure-struct-declaration (key)
  (let ((existing (gethash key (namespace-declarations *current-namespace*))))
    (if existing
	(if (not (struct-declaration-p existing))
	    (error "expected a struct-declaration for ~A, got a ~A" key (type-of existing))
	    existing)
	(setf (gethash key (namespace-declarations *current-namespace*))
		(let ((decl (make-struct-declaration :namespace *current-namespace*)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl)))))		  

(defun ensure-union-declaration (key)
  (let ((existing (gethash key (namespace-declarations *current-namespace*))))
    (if existing
	(if (not (union-declaration-p existing))
	    (error "expected a union-declaration for ~A, got a ~A" key (type-of existing))
	    existing)
	(setf (gethash key (namespace-declarations *current-namespace*))
	      (let ((decl (make-union-declaration :namespace *current-namespace*)))
		(setf (named-declaration-name decl) (compose-typename-1 decl key))
		decl)))))
		  

(defmethod process-inclusion-directive ((module module) abi ffi pass cursor parent name data)
  ;;(format t "~&#include <~A>" name)
  )

(defmethod process-dll-export ((module module) abi ffi pass cursor parent name data)
  (format t "~%export ~A" name))

(defmethod process-cxx-access-specifier ((module module) abi ffi pass cursor parent name data)
  (setq *access-specifier* (ecase (#_clang_getCXXAccessSpecifier cursor)
			     (#.#_CX_CXXInvalidAccessSpecifier :invalid)
			     (#.#_CX_CXXPublic :public)
			     (#.#_CX_CXXProtected :protected)
			     (#.#_CX_CXXPrivate :private))))
			     

(defmethod process-cxx-base-specifier ((module module) abi ffi pass cursor parent name data)
  (let ((type-expression (get-type-expression-from-string name)))
    (setf (struct-declaration-base-types *declaration*)
	  (nconc (struct-declaration-base-types *declaration*)
		 (list type-expression)))
    (unless type-expression
      (setf (named-declaration-punt *declaration*) t))))
	  

(defun get-cursor-location (cursor)
  (let ((location (#_clang_getCursorLocation cursor)))
    (clet ((file #_<CXFile>)
	   (line #_<unsigned int>)
	   (column #_<unsigned int>)
	   (offset #_<unsigned int>))
      (#_clang_getSpellingLocation location (c-addr-of file)
				   (c-addr-of line) (c-addr-of column) (c-addr-of offset))
      (let ((cxstring (#_clang_getFileName file)))
	(let ((cstring (#_clang_getCString cxstring)))
	  (values 
	   (when cstring
	     (get-c-string cstring))
	   line column offset))))))





		      
		      
		  


		      
(defmethod process-linkage-spec :after ((module module) abi ffi pass cursor parent name data)
  (visit-children cursor))

(defun populate-source-location (cursor named-decl)
  (unless (named-declaration-file named-decl)
    (multiple-value-bind (filename line column offset) (get-cursor-location cursor)
      (setf (named-declaration-file named-decl) (pathname filename)
	    (named-declaration-line named-decl) line
	    (named-declaration-column named-decl) column
	    (named-declaration-offset named-decl) offset)
      (values))))

(defmethod process-struct-decl :around ((module opencascade)
					abi ffi pass cursor parent
					(name (eql '#_IRpcChannelBuffer))
					data)
  )

(defmethod process-struct-decl :around ((module module) abi ffi pass cursor parent name data)
  (let* ((decl (ensure-struct-declaration name))
	 (*current-namespace* decl)
	 (*declaration* decl))
    (populate-source-location cursor decl)
    (call-next-method)))

(defmethod process-struct-decl :after ((module module) abi ffi pass cursor parent name data)
  ;; unless full class decl has already been processed:
  (unless (or (struct-declaration-base-types *declaration*)
	      (struct-declaration-methods *declaration*)
	      (struct-declaration-fields *declaration*))
    (visit-children cursor))
  
  (let* ((type (#_clang_getCursorType cursor))
	 (size (#_clang_Type_getSizeOf type)))
    (unless (minusp size)
      (setf (struct-declaration-size *declaration*) size))))

(defmethod process-union-decl :around ((module module) abi ffi pass cursor parent name data)
  (let* ((decl (ensure-union-declaration name))
	 (*current-namespace* decl)
	 (*declaration* decl))
    (populate-source-location cursor decl)
    (call-next-method)))

(defmethod process-class-decl :after ((module module) abi ffi pass cursor parent name data)
  ;; unless full class decl has already been processed:
  (unless (or (class-declaration-base-types *declaration*)
	      (class-declaration-methods *declaration*)
	      (class-declaration-fields *declaration*))
    (visit-children cursor))
  
  (let* ((type (#_clang_getCursorType cursor))
	 (size (#_clang_Type_getSizeOf type)))
    (unless (minusp size)
      (setf (class-declaration-size *declaration*) size))))
  
  
(defmethod process-class-decl :around ((module module) abi ffi pass cursor parent name data)
  ;;(print name)
  (let* ((*verbosity* 0)
	 (decl (ensure-class-declaration name))
	 (*current-namespace* decl)
	 (*declaration* decl))
    (populate-source-location cursor decl)
    (call-next-method)
    ))

  

(defmethod process-field-decl ((module module) abi ffi pass cursor parent name data)
  (let ((decl *declaration*))
    (let* ((type (get-type-expression (#_clang_getCursorType cursor)))
	   (field (make-field-declaration :name name
					  :type type
					  :bit-offset (let ((o (#_clang_Cursor_getOffsetOfField cursor)))
							(unless (minusp o)
							  o))
					  :access *access-specifier*)))
      (populate-source-location cursor field)
      (unless type
	(setf (named-declaration-punt decl) t))
      (setf (struct-declaration-fields decl)
	    (nconc (struct-declaration-fields decl)
		   (list field)))
      (values))))

(defmethod process-cxx-method :around ((module module) abi ffi pass cursor parent name data)
  (unless (or (= (#_.kind parent) #_CXCursor_TranslationUnit)
	      (= (#_.kind parent) #_CXCursor_Namespace))
    ;; don't process toplevel methods in cxxbinder, they are not declarations
    ;; they are implementations, and we don't need them.
    (call-next-method)))

(defmethod process-cxx-method :before ((module module) abi ffi pass cursor parent name data)
  (let ((virtual? (not (= 0 (#_clang_CXXMethod_isVirtual cursor)))))
    (when virtual?
      (let ((decl *declaration*))
	(setf (struct-declaration-vtable-p decl) t))))
  (values))

(defun ensure-class-template-definition (name)
  (let ((existing (gethash name (namespace-template-definitions *current-namespace*))))
    (if existing
	(if (not (typep existing 'class-template-definition))
	    (error "expected a class-template-definition for ~A, got a ~A" name (type-of existing))
	    existing)
	(progn
	  (setf (gethash name (namespace-template-definitions *current-namespace*))
		;;(setf (gethash name (namespace-template-definitions *current-namespace*))
		      (make-class-template-definition
		       :name name :namespace *current-namespace*))))))

(defmethod process-using-directive :after ((module module) abi ffi pass cursor parent name data)
  (let ((*verbosity* 0))
    (visit-children cursor)))

(defmethod process-namespace-ref ((module module) abi ffi pass cursor parent name data)
  (cond ((= (#_.kind parent) #_CXCursor_UsingDirective)
	 (pushnew name *using-directives*))))

(defmethod process-unexposed-decl ((module module) abi ffi pass cursor parent name data)
  ;;(print name)
  )

(defmethod process-type-alias-decl ((module module) abi ffi pass cursor parent name data)
  (let ((type (#_clang_getTypedefDeclUnderlyingType cursor)))
    (let* ((type (get-type-expression type)))
      (cond ((= (#_.kind parent) #_CXCursor_TypeAliasTemplateDecl)
	     (when (type-alias-template-decl-type-alias-decl *type-alias-template-decl*)
	       (warn "there is already a type alias decl for template ~S" (get-cursor-name parent)))
	     (let ((alias (make-type-alias :name name
					   :namespace *current-namespace*
					   :type type)))
	       (setf (type-alias-template-decl-type-alias-decl *type-alias-template-decl*)
		     alias)
	       (unless type
		 (setf (named-declaration-punt alias) t))
	       (populate-source-location cursor alias)))		   
	    (t
	     (let* ((alias (ensure-type-alias name type)))
	       (populate-source-location cursor alias)
	       (unless type
		 (setf (named-declaration-punt alias) t))))))))

(defmethod process-type-alias-template-decl ((module module) abi ffi pass cursor parent name data)
  (let* ((alias (ensure-type-alias-template name))
	 (*type-alias-template-decl* alias))
    (populate-source-location cursor alias)
    (let ((*verbosity* 0))
      (visit-children cursor))))

(defmethod process-type-alias-decl :after ((module module) abi ffi pass cursor parent name data)
  (let ((*verbosity* 0))
    ;;(visit-children cursor)
    ))



(defmethod process-class-template-partial-specialization
    ((module module) abi ffi pass cursor parent name data)
  ;;(print name)
  )

(defmethod process-class-template :around ((module module) abi ffi pass cursor parent name data)
  (let ((*verbosity* 0))
  (let* ((decl (ensure-class-template-definition name))
	 (*declaration* decl)
	 (*current-namespace* decl))
    (populate-source-location cursor decl)
    (call-next-method))))


(defmethod process-class-template :after ((module module) abi ffi pass cursor parent name data)
  (unless (or (class-template-definition-base-types *declaration*)
	      (class-template-definition-methods *declaration*)
	      (class-template-definition-fields *declaration*)
	      #+NIL
	      (class-template-definition-arguments *declaration*))
    (visit-children cursor)))

(defvar *default* :error)

(defmethod process-template-type-parameter :around
    ((module module) abi ffi pass cursor parent name data)
  (let ((*default* ()))
    (call-next-method)))

(defmethod process-template-type-parameter ((module module) abi ffi pass cursor parent name data)
  ;;(format t "~%process-template-type-parameter ~A class ~a" name (get-cursor-name parent))
  (let ((*verbosity* (if (null name)
			 2
			 0)))
    (visit-children cursor)
    (cond ((typep *declaration* 'class-template-definition)
	   (setf (class-template-definition-arguments *declaration*)
		 (nconc (class-template-definition-arguments *declaration*)
			(list (list* :type-name name 
				     (when *default* (list (list* :default (reverse *default*)))))
			      )))))))

(defmethod process-non-type-template-parameter :around
    ((module module) abi ffi pass cursor parent name data)
  (let ((*default* ()))
    (call-next-method)))

(defmethod process-non-type-template-parameter ((module module) abi ffi pass cursor parent name data)
  (visit-children cursor)
  (cond ((typep *declaration* 'class-template-definition)
	 (let* ((type (#_clang_getCursorType cursor))
		(type-expr (get-type-expression type)))
	   (unless type-expr
	     (setf (named-declaration-punt *declaration*) t))
	   (setf (class-template-definition-arguments *declaration*)
		 (nconc (class-template-definition-arguments *declaration*)
			(list (list* type-expr name 
				     (when *default* (list (list* :default (reverse *default*)))))
			      )))))))

(defmethod process-template-template-parameter :around
    ((module module) abi ffi pass cursor parent name data)
  (let ((*default* ()))
    (call-next-method)))

(defmethod process-template-template-parameter ((module module) abi ffi pass cursor parent name data)
  (visit-children cursor)
  (cond ((typep *declaration* 'class-template-definition)
	 (setf (class-template-definition-arguments *declaration*)
	       (nconc (class-template-definition-arguments *declaration*)
		      (list (list* :template name 
				   (when *default* (list (list* :default (reverse *default*)))))
			    ))))))

(defmethod process-decl-ref-expr ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (if *default*
	     (nconc (car *default*) (list (list 'noffi::expr name)))
	     (push (list 'noffi::expr name) *default*)))
	((= (#_.kind parent) #_CXCursor_TypeAliasDecl)
	 :do-nothing)))

(defmethod process-type-ref ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (if *default*
	     (nconc (car *default*) (list (list :type-name name)))
	     (push (list :type-name name) *default*)))
	((= (#_.kind parent) #_CXCursor_TypeAliasDecl)
	 :do-nothing
	 )
	(t 
	 :do-nothing)))
	 

(defmethod process-template-ref ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (if *default*
	     (nconc (car *default*) (list (list :template name)))
	     (push (list :template name) *default*)))
	((= (#_.kind parent) #_CXCursor_TypeAliasDecl)
	 :do-nothing)
	))



(defun get-tokens (unit cursor)
  (let ((extent (#_clang_getCursorExtent cursor)))
    (clet ((tokens #_<CXToken*>)
	   (ntokens #_<unsigned int>))
      (#_clang_tokenize unit extent (c-addr-of tokens) (c-addr-of ntokens))
      (unwind-protect
	   (loop for i from 0 below ntokens
		 collect (let ((cxstring (#_clang_getTokenSpelling unit (c-aref tokens i))))
			   (unwind-protect (get-c-string (#_clang_getCString cxstring))
			     (#_clang_disposeString cxstring))))
	(#_clang_disposeTokens unit tokens ntokens)))))

(defmethod process-floating-literal ((module module) abi ffi pass cursor parent name data)
  (unless (eq *default* :error)
    (push (get-tokens *unit* cursor) *default*)))
      

(defmethod process-integer-literal ((module module) abi ffi pass cursor parent name data)
  (unless (eq *default* :error)
    (push (get-tokens *unit* cursor) *default*)))

(defmethod process-cxx-bool-literal-expr ((module module) abi ffi pass cursor parent name data)
  (unless (eq *default* :error)
    (push (get-tokens *unit* cursor) *default*)))



(defun translate-binary-operator-kind (kind)
  (ecase kind
    (#.#_CXBinaryOperator_Invalid :invalid)
    (#.#_CXBinaryOperator_PtrMemD :ptr-mem-d)
    (#.#_CXBinaryOperator_PtrMemI :ptr-mem-i)
    (#.#_CXBinaryOperator_Mul :mul)
    (#.#_CXBinaryOperator_Div :div)
    (#.#_CXBinaryOperator_Rem :rem)
    (#.#_CXBinaryOperator_Add :add)
    (#.#_CXBinaryOperator_Sub :sub)
    (#.#_CXBinaryOperator_Shl :shl)
    (#.#_CXBinaryOperator_Shr :shr)
    (#.#_CXBinaryOperator_Cmp :cmp)
    (#.#_CXBinaryOperator_LT :lt)
    (#.#_CXBinaryOperator_GT :gt)
    (#.#_CXBinaryOperator_LE :le)
    (#.#_CXBinaryOperator_GE :ge)
    (#.#_CXBinaryOperator_EQ :eq)
    (#.#_CXBinaryOperator_NE :ne)
    (#.#_CXBinaryOperator_And :and)
    (#.#_CXBinaryOperator_Xor :xor)
    (#.#_CXBinaryOperator_Or :or)
    (#.#_CXBinaryOperator_LAnd :logical-and)
    (#.#_CXBinaryOperator_LOr :logical-or)
    (#.#_CXBinaryOperator_Assign :assign)
    (#.#_CXBinaryOperator_MulAssign :mul-assign)
    (#.#_CXBinaryOperator_DivAssign :div-assign)
    (#.#_CXBinaryOperator_RemAssign :rem-assign)
    (#.#_CXBinaryOperator_AddAssign :add-assign)
    (#.#_CXBinaryOperator_SubAssign :sub-assign)
    (#.#_CXBinaryOperator_ShlAssign :shl-assign)
    (#.#_CXBinaryOperator_ShrAssign :shr-assign)
    (#.#_CXBinaryOperator_AndAssign :add-assign)
    (#.#_CXBinaryOperator_XorAssign :xor-assign)
    (#.#_CXBinaryOperator_OrAssign :or-assign)
    (#.#_CXBinaryOperator_Comma :comma)))

(defun translate-unary-operator-kind (kind)
  (ecase kind
    (#.#_CXUnaryOperator_Invalid :invalid)
    (#.#_CXUnaryOperator_PostInc :post-inc)
    (#.#_CXUnaryOperator_PostDec :post-dec)
    (#.#_CXUnaryOperator_PreInc :pre-inc)
    (#.#_CXUnaryOperator_PreDec :pre-dec)
    (#.#_CXUnaryOperator_AddrOf :addr-of)
    (#.#_CXUnaryOperator_Deref :deref)
    (#.#_CXUnaryOperator_Plus :plus)
    (#.#_CXUnaryOperator_Minus :minus)
    (#.#_CXUnaryOperator_Not :not)
    (#.#_CXUnaryOperator_LNot :logical-not)
    (#.#_CXUnaryOperator_Real :real)
    (#.#_CXUnaryOperator_Imag :imag)
    (#.#_CXUnaryOperator_Extension :extension)
    (#.#_CXUnaryOperator_Coawait :coawait)))






(defmethod process-binary-operator :around
    ((module module) abi ffi pass cursor parent name data)
  (call-next-method))

(defmethod process-binary-operator ((module module) abi ffi pass cursor parent name data)
  (push (list :binop (translate-binary-operator-kind (#_clang_getCursorBinaryOperatorKind cursor)))
	*default*))

(defmethod process-unary-operator ((module module) abi ffi pass cursor parent name data)
  (push (list :unop (translate-unary-operator-kind (#_clang_getCursorUnaryOperatorKind cursor)))
	*default*))

(defmethod process-unary-expr ((module module) abi ffi pass cursor parent name data)
  )

(defun get-manglings (cursor)
  (let ((cxstringset (#_clang_Cursor_getCXXManglings cursor)))
    (when cxstringset
      (loop for i from 0 below (#_.Count cxstringset)
	    collect (let ((cxstring (c-aref (#_.Strings cxstringset) i)))
		      (when cxstring
			(let ((cstring (#_clang_getCString cxstring)))
			  (when cstring
			    (get-c-string cstring)))))))))

(defmethod process-cxx-method ((module module) abi ffi pass cursor parent name data)
  (let* ((type (#_clang_getCursorType cursor))
	 (cxx-class-name (get-cursor-name parent))
	 (storage-kind (process-storage-kind cursor))
	 (mangled-name (#_clang_Cursor_getMangling cursor))
	 (virtual? (not (= 0 (#_clang_CXXMethod_isVirtual cursor))))
	 (variadic? (not (= 0 (#_clang_isFunctionTypeVariadic type))))
	 (static? (not (= 0 (#_clang_CXXMethod_isStatic cursor))))
	 (constructor? (= #_CXCursor_Constructor (#_.kind cursor)))
	 (destructor? (= #_CXCursor_Destructor (#_.kind cursor)))
	 (num-args (#_clang_getNumArgTypes type))
	 (linkage (#_clang_getCursorLinkage cursor)))
    (flet ((pure-virtual? ()
	     (and virtual?
		  (not (= 0 (#_clang_CXXMethod_isPureVirtual cursor))))))
      (let ((return-type (process-function-return module ffi pass type))
	    (params (process-function-params module ffi pass cursor type)))
	(let ((method (make-method-declaration
		       :name name
		       :return-type return-type
		       :mangled-name (when mangled-name
				       (let ((cstr (#_clang_getCString mangled-name)))
					 (when cstr
					   (unwind-protect
						(get-c-string cstr)
					     #+NIL
					     (#_clang_disposeString cstr)))))
		       :constr-destr-manglings (unless (= (#_.kind parent) #_CXCursor_ClassTemplate)
						 (when (or constructor? destructor?)
						   (get-manglings cursor)))
		       :num-args num-args
		       :arguments params
		       :variadic? variadic?
		       :storage-kind storage-kind
		       :class-name cxx-class-name
		       :virtual? virtual?
		       :pure-virtual? (pure-virtual?)
		       :static? static?
		       :constructor? constructor?
		       :destructor? destructor?
		       :access *access-specifier*
		       :const? (unless (= 0 (#_clang_CXXMethod_isConst cursor))
				 :const)
		       :linkage-kind (ecase linkage
				       (#.#_CXLinkage_Invalid :invalid)
				       (#.#_CXLinkage_NoLinkage :none)
				       (#.#_CXLinkage_Internal :internal)
				       (#.#_CXLinkage_UniqueExternal :unique-external)
				       (#.#_CXLinkage_External :external))
		       :punt (or (not return-type) (some #'(lambda (argument)
							     (not (function-argument-type argument)))
							 params)))))
	  (populate-source-location cursor method)
	  (setf (struct-declaration-methods *declaration*)
		(nconc (struct-declaration-methods *declaration*)
		       (list method))))))
    (values)))

(defmethod compute-struct-declaration-forward-reference ((ffi noffi) name)
  (noffi::make-declaration
   :storage-class :typedef
   :name name :type (noffi::make-struct-type name)))
  




(defmethod process-typedef-decl ((module module) abi ffi pass cursor parent name data)
  (let ((type (#_clang_getTypedefDeclUnderlyingType cursor)))
    (let* ((type (get-type-expression type))
	   (typedef (ensure-type-definition name type)))
      (populate-source-location cursor typedef)
      (unless type
	(setf (named-declaration-punt typedef) t)))
    #+NIL
    (cond ((or (= (#_.kind parent) #_CXCursor_TranslationUnit)
	       (= (#_.kind parent) #_CXCursor_Namespace))
	   ))))
    






      



(defmethod sanify-method-name (name)
  (setq name (substitute #\_ #\Space name))
  (setq name
	(cond ((search "operator||" name) "operator_logical_OR")
	      ((search "operator|=" name) "operator_bitwise_OR_assign")
	      ((search "operator|" name) "operator_bitwise_OR")
	      ((search "operator()" name) "operator_CALL")
	      (t name)))
  name)


(defun instantiate-template (abi type-expression)
  (when (template-type-p type-expression)
    (let* ((arguments (remove-if #'null (cddr type-expression)))
	   (definition (get-template-definition type-expression)))
      (if (null definition)
	  (warn "could not find template definition for ~S" type-expression)
	  (when (class-template-definition-p definition)
	    (instantiate-template-1 definition abi arguments))))))



(defun literal-is-of-c-type (value ctype)
  (cond ((or (eq value '#.(noffi::cintern "true"))
	     (eq value '#.(noffi::cintern "false")))
	 (bool-type-p ctype))
	(t (let* ((string (string-trim '(#\space #\tab) (symbol-name value))))
	     (multiple-value-bind (value pos) (read-from-string string)
	       (if (/= pos (length string))
		   nil
		   (cond ((integerp value) (integer-clang-type-p value ctype))
			 ((floatp value) (float-clang-type-p value ctype))
			 (t nil))))))))

(defun literal-is-of-type (value)
  (cond ((or (eq value '#.(noffi::cintern "true"))
	     (eq value '#.(noffi::cintern "false"))) :bool)
	(t (let* ((string (string-trim '(#\space #\tab) (symbol-name value))))
	     (multiple-value-bind (value pos) (read-from-string string)
	       (if (/= pos (length string))
		   nil
		   (cond ((integerp value) :integer)
			 ((floatp value) :float)
			 (t nil))))))))

;;nullptr_t exception_ptr nested_exception exception _Num_int_base logic_error runtime_error error_category error_condition _Addr_storage error_code _System_errror _Atomic_counter_t _Ref_count_base type_info codecvt std::atomic_int std::ostream StringRef std::type_info std::stringstream std::streambuf std::streamsize pos_type off_type std:ios_base::seekdir std:ios_base::openmode clocale_t Iterator std::istream Hasher iterator const_iterator 

(defmethod massage-template-argument (type-expression)
  (if (null type-expression)
      nil
      (if (not (listp type-expression))
	  type-expression
	  (if (template-type-p type-expression)
	      (cadr type-expression)
	      (if (type-name-type-p type-expression)
		  (cadr type-expression)
		  (if (eq 'noffi::expr (car type-expression))
		      (cadr type-expression)
		      type-expression))))))

(defmethod instantiate-template-1 ((template class-template-definition) abi arguments)
  (let ((key (cons (class-template-definition-name template) arguments)))
    (format t "~&instantiating ~S" key)

  (let ((new-class-decl (deep-copy-class-template-definition-to-class-declaration template arguments)))
    (fixup-base-types new-class-decl (class-template-definition-arguments template) arguments)
    (mapcar #'(lambda (field)
		(fixup-field-declaration field
					 (class-template-definition-arguments template)
					 arguments))
	    (class-declaration-fields new-class-decl))
    (mapcar #'(lambda (method)
		(fixup-method-declaration method
					  (class-declaration-name new-class-decl)
					  (class-template-definition-arguments template)
					  arguments))
	    (class-declaration-methods new-class-decl))
    
    (prog1 (let ((namespace (namespace-namespace template)))
      (setf (gethash key (namespace-declarations namespace))
	    new-class-decl)
	     )))))

(defmethod compute-template-method-declaration-mangled-name
    ((abi msvc) base-mangled-name return-type arguments)
  base-mangled-name)

(defun fixup-field-declaration (field template-args args)
  (setf (field-declaration-type field)
	(fixup-type (field-declaration-type field) template-args args))
  field)

(defun fixup-method-declaration (method class-name template-args args)
  
  (setf (method-declaration-return-type method)
	(fixup-type (method-declaration-return-type method) template-args args))
  
  (loop for function-argument in (method-declaration-arguments method)
	do (setf (function-argument-type function-argument)
		 (fixup-type (function-argument-type function-argument) template-args args)))
  (setf (method-declaration-class-name method) class-name)
  (when (or (method-declaration-constructor? method)
	    (method-declaration-destructor? method))
    (setf (method-declaration-name method) nil))
  method)

(defun typename-as-const (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (noffi::cintern (concatenate 'string "const " typename)))

(defmacro set-root-type (type new)
  (let ((child (gensym)))
    `(let ((,child (child-type ,type)))
       (if (null ,child)
	   (setf ,type ,new)
	   (set-root-type-1 ,type ,child ,new)))))

(defun set-root-type-1 (type child new)
  (when (null child)
    (error "cannot set-root-type-1."))
  (setf (clang-type-name type) nil)
  (if (null (child-type child))
      (setf (child-type type) new)
      (set-root-type-1 child (child-type child) new)))

(defun fixup-type (type-expression template-args args)
  (loop for targ in template-args
	with fixed-type = type-expression
	for i from 0
	do (let ((arg (nth i args)))
	     (destructuring-bind (template-arg-kind template-arg-name
				  &optional maybe-default) targ
	       ;;	       (declare (ignore template-arg-name))
	       (if arg
		   (when (or (type-name-type-p targ)
			     (template-type-p targ))
		     (setq fixed-type (type-substitute arg targ fixed-type)))
		   (when (and (or (type-name-type-p targ)
			      (template-type-p targ))
			      maybe-default)
		     (setq fixed-type
			   (type-substitute (rest maybe-default) (list template-arg-kind template-arg-name)
					    type-expression))))))
	finally (return fixed-type)))

#+NIL
(defun recompose-typename (name const? reference)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (when const?
    (setq name (concatenate 'string "const " name)))
  (when reference
    (if (eq reference :lvalue)
	(setq name (concatenate 'string name " &"))
	(if (eq reference :rvalue)
	    (setq name (concatenate 'string name " &&"))
	    (if (and (consp reference)
		     (member :pointer reference))
		(progn
		  (setq name (concatenate 'string name " "))
		  (loop repeat (length reference)
			do (setq name (concatenate 'string name "*"))))
		(error "~S" reference)))))
  (noffi::cintern name))

(defmethod invalidate-type-name :around ((type clang-type) (root-type clang-type))
  (unless (eq type root-type)
    (call-next-method)))

(defmethod invalidate-type-name ((type reference-type) root-type)
  (setf (reference-type-name type) nil)
  (invalidate-type-name (reference-type-pointee-type type) root-type)
  (values))

(defmethod invalidate-type-name ((type elaborated-type) root-type)
  (setf (elaborated-type-name type) nil)
  (when (elaborated-type-named-type type)
    (invalidate-type-name (elaborated-type-named-type type) root-type))
  (values))

(defmethod invalidate-type-name ((type primitive-c-type) root-type)
  (error "primitive c type name cannot be invalidated!"))

(defmethod invalidate-type-name ((type array-vector-or-complex-type) root-type)
  (setf (array-vector-or-complex-type-name type) nil)
  (invalidate-type-name (array-vector-or-complex-type-element-type type) root-type)
  (values))




(defun compose-typename-2 (name &rest argument-names)
  (break)
  (if argument-names
      (noffi::cintern (format nil "~A<~{~A, ~}~A>" name (butlast argument-names)
			      (car (last argument-names))))
      (noffi::cintern (format nil "~A<>" name))))
  
(defun type-substitute (new old noffi-parsed-type)
  (if (null noffi-parsed-type)
      nil
      (if (consp noffi-parsed-type)
	  (if (template-type-p noffi-parsed-type)
	      (if (equalp old noffi-parsed-type)
		  new
		  (list* :template
			 (cadr noffi-parsed-type)
			 (type-substitute new old (cddr noffi-parsed-type))))
	      (if (type-name-type-p noffi-parsed-type)
		  (if (equalp old noffi-parsed-type)
		      new
		      noffi-parsed-type)
		  (if (eq 'noffi::expr (car noffi-parsed-type))
		      (if (and (consp old) (eql (cadr old) (cadr noffi-parsed-type)))
			  new
			  noffi-parsed-type)
		      (if (eq :type-qualifier (car noffi-parsed-type))
			  (list* :type-qualifier
				 (cadr noffi-parsed-type)
				 (type-substitute new old (cddr noffi-parsed-type)))
			  (cons (type-substitute new old (car noffi-parsed-type))
				(type-substitute new old (cdr noffi-parsed-type)))))))
	  noffi-parsed-type)))

(defun fixup-base-types (class-decl template-args args)
  (setf (class-declaration-base-types class-decl)
	(loop for type-expression in (class-declaration-base-types class-decl)
	      collect (fixup-type type-expression template-args args))))

(defun wrap-noffi-type (type-qualifiers type)
  (cond ((null type-qualifiers) type)
	((consp type-qualifiers)
	 (list :type-qualifier (pop type-qualifiers) (wrap-noffi-type type-qualifiers type)))))
			    

(defun compute-cursor-kind-processor (kind)
  (cond ((= kind #.#_CXCursor_UnexposedDecl) #'process-unexposed-decl)
	((= kind #.#_CXCursor_StructDecl) #'process-struct-decl)
	((= kind #.#_CXCursor_UnionDecl) #'process-union-decl)
	((= kind #.#_CXCursor_ClassDecl) #'process-class-decl)
	((= kind #.#_CXCursor_EnumDecl) #'process-enum-decl)
	((= kind #.#_CXCursor_FieldDecl) #'process-field-decl)
	((= kind #.#_CXCursor_EnumConstantDecl) #'process-enum-constant-decl)
	((= kind #.#_CXCursor_FunctionDecl) #'process-function-decl)
	((= kind #.#_CXCursor_VarDecl) #'process-var-decl)
	((= kind #.#_CXCursor_ParmDecl) #'process-parm-decl)
	((= kind #.#_CXCursor_ObjCInterfaceDecl) #'process-objc-interface-decl)
	((= kind #.#_CXCursor_ObjCCategoryDecl) #'process-objc-category-decl)
	((= kind #.#_CXCursor_ObjCProtocolDecl) #'process-objc-protocol-decl)
	((= kind #.#_CXCursor_ObjCPropertyDecl) #'process-objc-property-decl)
	((= kind #.#_CXCursor_ObjCIvarDecl) #'process-objc-ivar-decl)
	((= kind #.#_CXCursor_ObjCInstanceMethodDecl) #'process-objc-instance-method-decl)
	((= kind #.#_CXCursor_ObjCCategoryImplDecl) #'process-objc-category-impl-decl)
	((= kind #.#_CXCursor_TypedefDecl) #'process-typedef-decl)
	((= kind #.#_CXCursor_CXXMethod) #'process-cxx-method)
	((= kind #.#_CXCursor_Namespace) #'process-namespace)
	((= kind #.#_CXCursor_LinkageSpec) #'process-linkage-spec)
	((= kind #.#_CXCursor_Constructor) #'process-constructor)
	((= kind #.#_CXCursor_Destructor) #'process-destructor)
	((= kind #.#_CXCursor_ConversionFunction) #'process-conversion-function)
	((= kind #.#_CXCursor_TemplateTypeParameter) #'process-template-type-parameter)
	((= kind #.#_CXCursor_NonTypeTemplateParameter) #'process-non-type-template-parameter)
	((= kind #.#_CXCursor_TemplateTemplateParameter) #'process-template-template-parameter)
	((= kind #.#_CXCursor_FunctionTemplate) #'process-function-template)
	((= kind #.#_CXCursor_ClassTemplate) #'process-class-template)
	((= kind #.#_CXCursor_ClassTemplatePartialSpecialization)
	 #'process-class-template-partial-specialization)
	((= kind #.#_CXCursor_NamespaceAlias) #'process-namespace-alias)
	((= kind #.#_CXCursor_UsingDirective) #'process-using-directive)
	((= kind #.#_CXCursor_UsingDeclaration) #'process-using-declaration)
	((= kind #.#_CXCursor_TypeAliasDecl) #'process-type-alias-decl)
	((= kind #.#_CXCursor_ObjCSynthesizeDecl) #'process-objc-synthesize-decl)
	((= kind #.#_CXCursor_ObjCDynamicDecl) #'process-objc-dynamic-decl)
	((= kind #.#_CXCursor_CXXAccessSpecifier) #'process-cxx-access-specifier)
	((= kind #.#_CXCursor_ObjCSuperClassRef) #'process-objc-super-class-ref)
	((= kind #.#_CXCursor_ObjCProtocolRef) #'process-objc-protocol-ref)
	((= kind #.#_CXCursor_ObjCClassRef) #'process-objc-class-ref)
	((= kind #.#_CXCursor_TypeRef) #'process-type-ref)
	((= kind #.#_CXCursor_CXXBaseSpecifier) #'process-cxx-base-specifier)
	((= kind #.#_CXCursor_TemplateRef) #'process-template-ref)
	((= kind #.#_CXCursor_NamespaceRef) #'process-namespace-ref)
	((= kind #.#_CXCursor_MemberRef) #'process-member-ref)
	((= kind #.#_CXCursor_LabelRef) #'process-label-ref)
	((= kind #.#_CXCursor_OverloadedDeclRef) #'process-overloaded-decl-ref)
	((= kind #.#_CXCursor_VariableRef) #'process-variable-ref)
	((= kind #.#_CXCursor_InvalidFile) #'process-invalid-file)
	((= kind #.#_CXCursor_NotImplemented) #'process-not-implemented)
	((= kind #.#_CXCursor_InvalidCode) #'process-invalid-code)
	((= kind #.#_CXCursor_UnexposedExpr) #'process-unexposed-expr)
	((= kind #.#_CXCursor_DeclRefExpr) #'process-decl-ref-expr)
	((= kind #.#_CXCursor_MemberRefExpr) #'process-member-ref-expr)
	((= kind #.#_CXCursor_CallExpr) #'process-call-expr)
	((= kind #.#_CXCursor_ObjCMessageExpr) #'process-objc-message-expr)
	((= kind #.#_CXCursor_BlockExpr) #'process-block-expr)
	((= kind #.#_CXCursor_IntegerLiteral) #'process-integer-literal)
	((= kind #.#_CXCursor_FloatingLiteral) #'process-floating-literal)
	((= kind #.#_CXCursor_ImaginaryLiteral) #'process-imaginary-literal)
	((= kind #.#_CXCursor_StringLiteral) #'process-string-literal)
	((= kind #.#_CXCursor_CharacterLiteral) #'process-character-literal)
	((= kind #.#_CXCursor_ParenExpr) #'process-paren-expr)
	((= kind #.#_CXCursor_UnaryOperator) #'process-unary-operator)
	((= kind #.#_CXCursor_ArraySubscriptExpr) #'process-array-subscript-expr)
	((= kind #.#_CXCursor_BinaryOperator) #'process-binary-operator)
	((= kind #.#_CXCursor_CompoundAssignOperator) #'process-compound-assign-operator)
	((= kind #.#_CXCursor_ConditionalOperator) #'process-conditional-operator)
	((= kind #.#_CXCursor_CStyleCastExpr) #'process-c-style-cast-expr)
	((= kind #.#_CXCursor_CompoundLiteralExpr) #'process-compound-literal-expr)
	((= kind #.#_CXCursor_InitListExpr) #'process-init-list-expr)
	((= kind #.#_CXCursor_AddrLabelExpr) #'process-addr-label-expr)
	((= kind #.#_CXCursor_StmtExpr) #'process-stmt-expr)
	((= kind #.#_CXCursor_GenericSelectionExpr) #'process-generic-selection-expr)
	((= kind #.#_CXCursor_GNUNullExpr) #'process-gnu-null-expr)
	((= kind #.#_CXCursor_CXXStaticCastExpr) #'process-static-cast-expr)
	((= kind #.#_CXCursor_CXXDynamicCastExpr) #'process-dynamic-cast-expr)
	((= kind #.#_CXCursor_CXXReinterpretCastExpr) #'process-reinterpret-cast-expr)
	((= kind #.#_CXCursor_CXXConstCastExpr) #'process-cxx-const-cast-expr)
	((= kind #.#_CXCursor_CXXFunctionalCastExpr) #'process-cxx-functional-cast-expr)
	((= kind #.#_CXCursor_CXXTypeidExpr) #'process-cxx-type-id-expr)
	((= kind #.#_CXCursor_CXXBoolLiteralExpr) #'process-cxx-bool-literal-expr)
	((= kind #.#_CXCursor_CXXNullPtrLiteralExpr) #'process-cxx-null-ptr-literal-expr)
	((= kind #.#_CXCursor_CXXThisExpr) #'process-cxx-this-expr)
	((= kind #.#_CXCursor_CXXThrowExpr) #'process-cxx-throw-expr)
	((= kind #.#_CXCursor_CXXNewExpr) #'process-cxx-new-expr)
	((= kind #.#_CXCursor_CXXDeleteExpr) #'process-cxx-delete-expr)
	((= kind #.#_CXCursor_UnaryExpr) #'process-unary-expr)
	((= kind #.#_CXCursor_ObjCStringLiteral) #'process-objc-string-literal)
	((= kind #.#_CXCursor_ObjCEncodeExpr) #'process-objc-encode-expr)
	((= kind #.#_CXCursor_ObjCSelectorExpr) #'process-objc-selector-expr)
	((= kind #.#_CXCursor_ObjCProtocolExpr) #'process-objc-protocol-expr)
	((= kind #.#_CXCursor_ObjCBridgedCastExpr) #'process-objc-bridged-cast-expr)
	((= kind #.#_CXCursor_PackExpansionExpr) #'process-pack-expansion-expr)
	((= kind #.#_CXCursor_SizeOfPackExpr) #'process-size-of-pack-expr)
	((= kind #.#_CXCursor_LambdaExpr) #'process-lambda-expr)
	((= kind #.#_CXCursor_ObjCBoolLiteralExpr) #'process-bool-literal-expr)
	((= kind #.#_CXCursor_ObjCSelfExpr) #'process-objc-self-expr)
	((= kind #.#_CXCursor_OMPArraySectionExpr) #'process-omp-array-secion-expr)
	((= kind #.#_CXCursor_ObjCAvailabilityCheckExpr) #'process-objc-availability-check-expr)
	((= kind #.#_CXCursor_FixedPointLiteral) #'process-fixed-point-literal)
	((= kind #.#_CXCursor_OMPArrayShapingExpr) #'process-omp-array-shaping-expr)
	((= kind #.#_CXCursor_OMPIteratorExpr) #'prcess-omp-iterator-expr)
	((= kind #.#_CXCursor_CXXAddrspaceCastExpr) #'process-cxx-addrspace-cast-expr)
	((= kind #.#_CXCursor_ConceptSpecializationExpr) #'process-concept-specialization-expr)
	((= kind #.#_CXCursor_RequiresExpr) #'process-requires-expr)
	((= kind #.#_CXCursor_CXXParenListInitExpr) #'process-paren-list-init-expr)
	((= kind #.#_CXCursor_UnexposedStmt) #'process-unexposed-stmt-expr)
	((= kind #.#_CXCursor_LabelStmt) #'process-label-stmt)
	((= kind #.#_CXCursor_CompoundStmt) #'process-compound-stmt)
	((= kind #.#_CXCursor_CaseStmt) #'process-case-stmt)
	((= kind #.#_CXCursor_DefaultStmt) #'process-default-stmt)
	((= kind #.#_CXCursor_IfStmt) #'process-if-stmt)
	((= kind #.#_CXCursor_SwitchStmt) #'process-switch-stmt)
	((= kind #.#_CXCursor_WhileStmt) #'process-while-stmt)
	((= kind #.#_CXCursor_DoStmt) #'process-do-stmt)
	((= kind #.#_CXCursor_ForStmt) #'process-for-stmt)
	((= kind #.#_CXCursor_GotoStmt) #'process-goto-stmt)
	((= kind #.#_CXCursor_IndirectGotoStmt) #'process-indirect-goto-stmt)
	((= kind #.#_CXCursor_BreakStmt) #'process-break-stmt)
	((= kind #.#_CXCursor_GCCAsmStmt) #'process-asm-stmt)
	((= kind #.#_CXCursor_AsmStmt) #'process-asm-stmt)
	((= kind #.#_CXCursor_ObjCAtTryStmt) #'process-objc-at-try-stmt)
	((= kind #.#_CXCursor_ObjCAtCatchStmt) #'process-objc-at-catch-stmt)
	((= kind #.#_CXCursor_ObjCAtFinallyStmt) #'process-objc-at-finally-stmt)
	((= kind #.#_CXCursor_ObjCAtThrowStmt) #'process-objc-at-throw-stmt)
	((= kind #.#_CXCursor_ObjCAtSynchronizedStmt) #'process-objc-at-synchronized-stmt)
	((= kind #.#_CXCursor_ObjCAutoreleasePoolStmt) #'process-objc-autorelease-pool-stmt)
	((= kind #.#_CXCursor_ObjCForCollectionStmt) #'process-objc-for-collection-stmt)
	((= kind #.#_CXCursor_CXXCatchStmt) #'process-cxx-catch-stmt)
	((= kind #.#_CXCursor_CXXTryStmt) #'process-cxx-try-stmt)
	((= kind #.#_CXCursor_CXXForRangeStmt) #'process-for-range-stmt)
	((= kind #.#_CXCursor_SEHTryStmt) #'process-seh-try-stmt)
	((= kind #.#_CXCursor_SEHExceptStmt) #'process-seh-except-stmt)
	((= kind #.#_CXCursor_SEHFinallyStmt) #'process-seh-finally-stmt)
	((= kind #.#_CXCursor_MSAsmStmt) #'process-ms-asm-stmt)
	((= kind #.#_CXCursor_NullStmt) #'process-null-stmt)
	((= kind #.#_CXCursor_DeclStmt) #'process-decl-stmt)
	((= kind #.#_CXCursor_OMPParallelDirective) #'process-omp-parallel-directive)
	((= kind #.#_CXCursor_OMPSimdDirective) #'process-omp-simd-directive)
	((= kind #.#_CXCursor_OMPForDirective) #'process-omp-for-directive)
	((= kind #.#_CXCursor_OMPSectionsDirective) #'process-omp-sections-directive)
	((= kind #.#_CXCursor_OMPSingleDirective) #'process-omp-single-directive)
	((= kind #.#_CXCursor_OMPParallelForDirective) #'process-omp-parallel-for-directive)
	((= kind #.#_CXCursor_OMPParallelSectionsDirective) #'process-omp-parallel-sections-directive)
	((= kind #.#_CXCursor_OMPTaskDirective) #'process-omp-task-directive)
	((= kind #.#_CXCursor_OMPMasterDirective) #'process-omp-master-directive)
	((= kind #.#_CXCursor_OMPCriticalDirective) #'process-omp-critical-directive)
	((= kind #.#_CXCursor_OMPTaskyieldDirective) #'process-omp-taskyield-directive)
	((= kind #.#_CXCursor_OMPBarrierDirective) #'process-omp-barrier-directive)
	((= kind #.#_CXCursor_OMPTaskwaitDirective) #'process-omp-taskwait-directive)
	((= kind #.#_CXCursor_OMPFlushDirective) #'process-omp-flush-directive)
	((= kind #.#_CXCursor_SEHLeaveStmt) #'process-seh-leave-stmt)
	((= kind #.#_CXCursor_OMPOrderedDirective) #'process-omp-ordered-directive)
	((= kind #.#_CXCursor_OMPAtomicDirective) #'process-omp-atomic-directive)
	((= kind #.#_CXCursor_OMPForSimdDirective) #'process-for-simd-directive)
	((= kind #.#_CXCursor_OMPParallelForSimdDirective) #'process-omp-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPTargetDirective) #'process-omp-target-directive)
	((= kind #.#_CXCursor_OMPTeamsDirective) #'process-omp-teams-directive)
	((= kind #.#_CXCursor_OMPTaskgroupDirective) #'process-omp-taskgroup-directive)
	((= kind #.#_CXCursor_OMPCancellationPointDirective) #'process-omp-cancallation-point-directive)
	((= kind #.#_CXCursor_OMPCancelDirective) #'process-omp-cancel-directive)
	((= kind #.#_CXCursor_OMPTargetDataDirective) #'process-omp-target-data-directive)
	((= kind #.#_CXCursor_OMPTaskLoopDirective) #'process-omp-task-loop-directive)
	((= kind #.#_CXCursor_OMPTaskLoopSimdDirective) #'process-omp-task-loop-simd-directive)
	((= kind #.#_CXCursor_OMPDistributeDirective) #'process-omp-distribute-directive)
	((= kind #.#_CXCursor_OMPTargetEnterDataDirective) #'process-omp-target-enter-data-directive)
	((= kind #.#_CXCursor_OMPTargetExitDataDirective) #'process-omp-target-exit-data-directive)
	((= kind #.#_CXCursor_OMPTargetParallelDirective) #'process-target-parallel-directive)
	((= kind #.#_CXCursor_OMPTargetParallelForDirective) #'process-target-parallel-for-directive)
	((= kind #.#_CXCursor_OMPTargetUpdateDirective) #'process-omp-target-update-directive)
	((= kind #.#_CXCursor_OMPDistributeParallelForDirective)
	 #'process-omp-distribute-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPDistributeParallelForSimdDirective)
	 #'process-omp-distribute-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPDistributeSimdDirective) #'process-omp-distibute-simd-directive)
	((= kind #.#_CXCursor_OMPTargetParallelForSimdDirective)
	 #'process-omp-target-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPTargetSimdDirective) #'process-omp-target-simd-directive)
	((= kind #.#_CXCursor_OMPTeamsDistributeDirective) #'process-omp-teams-distribute-directive)
	((= kind #.#_CXCursor_OMPTeamsDistributeSimdDirective) #'process-omp-teams-distribute-simd-directive)
	((= kind #.#_CXCursor_OMPTeamsDistributeParallelForSimdDirective)
	 #'process-omp-teams-distribute-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPTeamsDistributeParallelForDirective)
	 #'process-omp-teams-distribute-prallel-for-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsDirective) #'process-omp-target-teams-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsDistributeDirective)
	 #'process-omp-target-teams-distribute-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsDistributeParallelForDirective)
	 #'process-omp-target-teams-distribute-parallel-for-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective)
	 #'process-omp-target-teams-distribute-parallel-for-simd-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsDistributeSimdDirective)
	 #'process-omp-target-teams-distribute-simd-directive)
	((= kind #.#_CXCursor_BuiltinBitCastExpr) #'process-builtin-bit-cast-expr)
	((= kind #.#_CXCursor_OMPMasterTaskLoopDirective) #'process-omp-master-task-loop-directive)	 
	((= kind #.#_CXCursor_OMPParallelMasterTaskLoopDirective)
	 #'process-omp-parallel-master-task-loop-directive)
	((= kind #.#_CXCursor_OMPMasterTaskLoopSimdDirective)
	 #'process-omp-master-task-loop-simd-directive)
	((= kind #.#_CXCursor_OMPParallelMasterTaskLoopSimdDirective)
	 #'process-omp-parallel-master-task-loop-simd-directive)
	((= kind #.#_CXCursor_OMPParallelMasterDirective) #'process-omp-parallel-master-directive)
	((= kind #.#_CXCursor_OMPDepobjDirective) #'process-omp-depobj-directive)
	((= kind #.#_CXCursor_OMPScanDirective) #'process-omp-scan-directive)
	((= kind #.#_CXCursor_OMPTileDirective) #'process-omp-tile-directive)
	((= kind #.#_CXCursor_OMPCanonicalLoop) #'process-omp-canonical-loop)
	((= kind #.#_CXCursor_OMPInteropDirective) #'process-omp-iterop-directive)
	((= kind #.#_CXCursor_OMPDispatchDirective) #'process-omp-dispatch-directive)
	((= kind #.#_CXCursor_OMPMaskedDirective) #'process-omp-masked-directive)
	((= kind #.#_CXCursor_OMPUnrollDirective) #'process-omp-unroll-directive)
	((= kind #.#_CXCursor_OMPMetaDirective) #'process-omp-meta-directive)
	((= kind #.#_CXCursor_OMPGenericLoopDirective) #'process-omp-generic-loop-directive)
	((= kind #.#_CXCursor_OMPTeamsGenericLoopDirective) #'process-teams-generic-loop-directive)
	((= kind #.#_CXCursor_OMPTargetTeamsGenericLoopDirective)
	 #'process-omp-target-teams-generic-loop-directive)
	((= kind #.#_CXCursor_OMPParallelGenericLoopDirective)
	 #'process-omp-parallel-generic-loop-directive)
	((= kind #.#_CXCursor_OMPTargetParallelGenericLoopDirective)
	 #'process-omp-target-parallel-generic-loop-directive)
	((= kind #.#_CXCursor_OMPParallelMaskedDirective) #'process-omp-parallel-masked-directive)
	((= kind #.#_CXCursor_OMPMaskedTaskLoopDirective) #'process-omp-masked-task-loop-directive)
	((= kind #.#_CXCursor_OMPMaskedTaskLoopSimdDirective) #'process-omp-masked-task-loop-simd-directive)
	((= kind #.#_CXCursor_OMPParallelMaskedTaskLoopDirective)
	 #'process-omp-parallel-masked-task-loop-directive)
	((= kind #.#_CXCursor_OMPParallelMaskedTaskLoopSimdDirective)
	 #'process-omp-prallel-masked-task-loop-simd-directive)
	((= kind #.#_CXCursor_OMPErrorDirective) #'process-omp-error-directive)	 
	((= kind #.#_CXCursor_OMPScopeDirective) #'process-omp-scope-directive)
	((= kind #.#_CXCursor_TranslationUnit) #'process-translation-unit)
	((= kind #.#_CXCursor_UnexposedAttr) #'process-unexposed-attr)
	((= kind #.#_CXCursor_IBActionAttr) #'process-ib-action-attr)
	((= kind #.#_CXCursor_IBOutletAttr) #'process-ib-outlet-attr)
	((= kind #.#_CXCursor_IBOutletCollectionAttr) #'process-ib-outlet-collection-attr)
	((= kind #.#_CXCursor_CXXFinalAttr) #'process-cxx-final-attr)
	((= kind #.#_CXCursor_CXXOverrideAttr) #'process-cxx-override-atrr)
	((= kind #.#_CXCursor_AnnotateAttr) #'process-annotate-attr)
	((= kind #.#_CXCursor_AsmLabelAttr) #'process-asm-label-attr)
	((= kind #.#_CXCursor_PackedAttr) #'process-packed-attr)
	((= kind #.#_CXCursor_PureAttr) #'process-pure-attr)
	((= kind #.#_CXCursor_ConstAttr) #'process-const-attr)
	((= kind #.#_CXCursor_NoDuplicateAttr) #'process-no-duplicate-attr)
	((= kind #.#_CXCursor_CUDAConstantAttr) #'process-cuda-constant-attr)
	((= kind #.#_CXCursor_CUDADeviceAttr) #'process-cuda-device-attr)
	((= kind #.#_CXCursor_CUDAGlobalAttr) #'process-cuda-global-attr)
	((= kind #.#_CXCursor_CUDAHostAttr) #'process-cuda-host-attr)
	((= kind #.#_CXCursor_CUDASharedAttr) #'process-cuda-shared-attr)
	((= kind #.#_CXCursor_VisibilityAttr) #'process-visibility-attr)
	((= kind #.#_CXCursor_DLLExport) #'process-dll-export)
	((= kind #.#_CXCursor_DLLImport) #'process-dll-import)
	((= kind #.#_CXCursor_NSReturnsRetained) #'process-ns-returns-retained)
	((= kind #.#_CXCursor_NSReturnsNotRetained) #'process-ns-returns-not-retained)
	((= kind #.#_CXCursor_NSReturnsAutoreleased) #'process-ns-returns-autoreleased)
	((= kind #.#_CXCursor_NSConsumesSelf) #'process-ns-consumes-self)
	((= kind #.#_CXCursor_NSConsumed) #'process-ns-consumed)
	((= kind #.#_CXCursor_ObjCException) #'process-objc-exception)
	((= kind #.#_CXCursor_ObjCNSObject) #'process-objc-ns-object)
	((= kind #.#_CXCursor_ObjCIndependentClass) #'process-objc-independent-class)
	((= kind #.#_CXCursor_ObjCPreciseLifetime) #'process-objc-precise-lifetime)
	((= kind #.#_CXCursor_ObjCReturnsInnerPointer) #'process-objc-returns-inner-pointer)
	((= kind #.#_CXCursor_ObjCRequiresSuper) #'process-objc-requires-super)
	((= kind #.#_CXCursor_ObjCRootClass) #'process-objc-root-class)
	((= kind #.#_CXCursor_ObjCSubclassingRestricted) #'process-objc-subclassing-restricted)
	((= kind #.#_CXCursor_ObjCExplicitProtocolImpl) #'process-objc-explicit-protocol-impl)
	((= kind #.#_CXCursor_ObjCDesignatedInitializer) #'process-objc-designated-initializer)
	((= kind #.#_CXCursor_ObjCRuntimeVisible) #'process-objc-runtime-visible)
	((= kind #.#_CXCursor_ObjCBoxable) #'process-objc-boxable)
	((= kind #.#_CXCursor_FlagEnum) #'process-flag-enum)
	((= kind #.#_CXCursor_ConvergentAttr) #'process-convergent-attr)
	((= kind #.#_CXCursor_WarnUnusedAttr) #'process-warn-unused-attr)
	((= kind #.#_CXCursor_WarnUnusedResultAttr) #'process-warn-unused-result-attr)
	((= kind #.#_CXCursor_AlignedAttr) #'process-aligned-attr)
	((= kind #.#_CXCursor_PreprocessingDirective) #'process-preprocessing-directive)
	((= kind #.#_CXCursor_MacroDefinition) #'process-macro-definition)
	((= kind #.#_CXCursor_MacroExpansion) #'process-macro-expansion)
	((= kind #.#_CXCursor_InclusionDirective) #'process-inclusion-directive)
	((= kind #.#_CXCursor_ModuleImportDecl) #'process-module-import-decl)
	((= kind #.#_CXCursor_TypeAliasTemplateDecl) #'process-type-alias-template-decl)
	((= kind #.#_CXCursor_StaticAssert) #'process-static-assert)
	((= kind #.#_CXCursor_FriendDecl) #'process-friend-decl)
	((= kind #.#_CXCursor_ConceptDecl) #'process-concept-decl)
	((= kind #.#_CXCursor_OverloadCandidate) #'process-overload-candidate)
	(t (format t "unknown cursor kind ~A" kind) nil)))


	

(defun universal-visit-func (cursor parent data)
  (let* ((kind (#_.kind cursor))
	 (name (get-cursor-name cursor))
	 (args (list *module* *abi* *ffi* *pass* cursor parent (when name (noffi::cintern name)) data))
	 (fun (compute-cursor-kind-processor kind)))
    (when fun
      (apply fun args))))



(defun visit-children (cursor)
  (let ((*visit-func* #'universal-visit-func))
    (#_clang_visitChildren cursor visit_func nil)))




(defmethod process-constructor ((module module) abi ffi pass cursor parent name data)
  (process-cxx-method module abi ffi pass cursor parent name data))

(defmethod process-destructor ((module module) abi ffi pass cursor parent name data)
  (process-cxx-method module abi ffi pass cursor parent name data))

(defmethod compute-lisp-name-for-member-function ((module module) ffi pass name class-name overload constructor? destructor?)
  (setq name (sanify-method-name (symbol-name name)))
  (let ((name (if constructor? "initialize" (if destructor? "destroy" name))))
    (if (eq 0 overload)
	(format nil "~A::~A" class-name name)
	(format nil "~A::~A__OL_~S" class-name name overload))))
  
(defmethod process-function-return ((module module) ffi pass type)
  (let ((return-type (#_clang_getResultType type)))
    (let ((type (get-type-expression return-type)))
      type)))

(defun process-storage-kind (cursor)
  (let ((linkage (#_clang_getCursorLinkage cursor))
	(storage (#_clang_Cursor_getStorageClass cursor)))
    (if (= linkage #_CXLinkage_External)
	:external
	(if (= storage #_CX_SC_Static)
	    :static
	    (progn
	      (warn "Unimplemented CXLinkageKind ~S, CX_StorageClass ~S" linkage storage)
	      nil)))))

(defun ensure-enum-declaration (name enum-type)
  (let ((existing (gethash name (namespace-declarations *current-namespace*))))
    (if existing
	existing
	(setf (gethash name (namespace-declarations *current-namespace*))
	      (make-enum-declaration :name name
				     :namespace *current-namespace*
				     :enum-type enum-type)))))

(defmethod process-enum-decl :around ((module module) abi ffi pass cursor parent name data)
  (let ((*verbosity* 0))
    (let ((enum-type (get-type-expression (#_clang_getEnumDeclIntegerType cursor))))
      (let ((*declaration* (ensure-enum-declaration name enum-type)))
	(call-next-method)))))

(defmethod process-enum-decl ((module module) abi ffi pass cursor parent name data)
  (unless (enum-declaration-constants *declaration*)
    (visit-children cursor)))

(defmethod process-translation-unit :around ((module module) abi ffi pass cursor parent name data)
  (let ((*using-directives* ()))
    (call-next-method)))

(defmethod process-translation-unit ((module module) abi ffi pass cursor parent name data)
  (let ((*header-in-question* (symbol-name name))
	(*overloads* (make-hash-table :test #'equalp)))
    (visit-children cursor)))

(defun ensure-namespace (name)
  (let ((existing (gethash name (namespace-declarations *current-namespace*))))
    (if existing
	existing
	(setf (gethash name (namespace-declarations *current-namespace*))
	      (make-namespace :name name :namespace *current-namespace*)))))

(defmethod process-namespace :after ((module module) abi ffi pass cursor parent name data)
  (let ((*current-namespace* (ensure-namespace name)))
    (populate-source-location cursor *current-namespace*)
    (visit-children cursor)))


(defvar *builtin-type-array*
  (make-array 41))

(defmethod initialize-builtin-type-array ((ffi cffi))
  (loop for (kind cffi-type) on (list
				 #_CXType_Void :void
				 #_CXType_Bool :bool
				 #_CXType_Char_U :unsigned-char
				 #_CXType_UChar :unsigned-char
				 #_CXType_UShort :unsigned-short
				 #_CXType_UInt :unsigned-int
				 #_CXType_ULong :unsigned-long
				 #_CXType_ULongLong :unsigned-long-long
				 #_CXType_UInt128 :unsigned-long-long-long ;; no such type in cffi
				 #_CXType_Char_S :char
				 #_CXType_SChar :char
				 #_CXType_WChar :short
				 #_CXType_Char16 :short
				 #_CXType_Short :short
				 #_CXType_Int :int
				 #_CXType_Char32 :int
				 #_CXType_Long :long
				 #_CXType_LongLong :long-long
				 #_CXType_Int128 :long-long-long ;; no such type in cffi
				 #_CXType_Float :float
				 #_CXType_Half :short-float
				 #_CXType_BFloat16 :short-float
				 #_CXType_Double :double
				 #_CXType_LongDouble :long-double) ;; no such type in cffi?
     by #'cddr
     do (setf (aref *builtin-type-array* kind) cffi-type))
  (values))

(defmethod initialize-builtin-type-array ((ffi noffi))
  (loop for (kind cffi-type) on (list
				 #_CXType_Invalid 'CXType_Invalid
				 #_CXType_Unexposed 'CXType_Unexposed
				 #_CXType_Void :void
				 #_CXType_Bool :bool
				 #_CXType_Char_U :unsigned-char
				 #_CXType_UChar :unsigned-char
				 #_CXType_Char16 :short
				 #_CXType_Char32 :int
				 #_CXType_UShort :unsigned-short
				 #_CXType_UInt :unsigned-int
				 #_CXType_ULong :unsigned-long
				 #_CXType_ULongLong :unsigned-long-long
				 #_CXType_UInt128 :unsigned-int-128
				 #_CXType_Char_S :signed-char
				 #_CXType_SChar :signed-char
				 #_CXType_WChar :short
				 #_CXType_Short :short
				 #_CXType_Int :int
				 #_CXType_Long :long
				 #_CXType_LongLong :long-long
				 #_CXType_Int128 :int-128
				 #_CXType_Float :float
				 #_CXType_Double :double
				 #_CXType_LongDouble :long-double
				 #_CXType_NullPtr 'CXType_NullPtr
				 #_CXType_Overload 'CXType_Overload
				 #_CXType_Dependent 'CXType_Dependent
				 #_CXType_ObjCId 'CXType_ObjCId
				 #_CXType_ObjCClass 'CXType_ObjCClass
				 #_CXType_ObjCSel 'CXType_ObjCSel
				 #_CXType_Float128 :float-128
				 #_CXType_Half :float-16
				 #_CXType_Float16 :float-16
				 #_CXType_ShortAccum 'CXType_ShortAccum
				 #_CXType_Accum 'CXType_Accum
				 #_CXType_LongAccum 'CXType_LongAccum
				 #_CXType_UShortAccum 'CXType_UShortAccum
				 #_CXType_UAccum 'CXType_UAccum
				 #_CXType_ULongAccum 'CXType_ULongAccum
				 #_CXType_BFloat16 :float-16
				 #_CXType_Ibm128 'CXType_Ibm128)
     by #'cddr
     do (setf (aref *builtin-type-array* kind) cffi-type)))
				 

(defmethod get-builtin-type ((ffi cffi) kind)
  (get-builtin-type-cffi kind))

(defun get-builtin-type-cffi (kind)
  (aref *builtin-type-array* kind))

(defmethod get-builtin-type ((ffi noffi) kind)
  (get-builtin-type-noffi kind))

(defun get-builtin-type-noffi (kind)
  (aref *builtin-type-array* kind))


(defun builtin-type-p (kind)
  (<= #_CXType_FirstBuiltin kind #_CXType_LastBuiltin))

(defun primitive-type-p (kind)
  (or (= kind #_CXType_Vector)
      (= kind #_CXType_Complex)
      (builtin-type-p kind)))


(defun template-typename-p (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (let ((pos1)
	(pos2))
    (and (setq pos1 (position #\< typename))
	 (setq pos2 (position #\> typename :from-end t))
	 (= pos2 (1- (length typename)))
	 (< pos1 pos2))))

(defun namespaced-typename-p (typename)
  (let ((pos (position #\: typename)))
    (when (and pos
	       (> (length typename) (1+ pos))
	       (char= #\: (char typename (1+ pos))))
      pos)))
    

(defun typename-namespace-and-name (typename)
  (let ((pos))
    (when (setq pos (namespaced-typename-p typename))
      (when (> (count #\: typename) 1)
	(values (string-trim '(#\:) (subseq typename 0 pos))
		(string-trim '(#\:) (subseq typename pos)))))))

(defun template-raw-arguments (typename)
  (when (template-typename-p typename)
    (let ((pos))
      (subseq typename (1+ (setq pos (position #\< typename))) (position #\> typename :from-end t)))))

(defun template-arguments (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (let ((raw-arguments (template-raw-arguments typename)))
    (when raw-arguments
      (mapcar #'(lambda (arg)
		  (noffi::cintern (string-trim '(#\space) arg)))
	      (uiop/utility:split-string raw-arguments :separator '(#\,))))))

(defun template-name (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (when (template-typename-p typename)
    (noffi::cintern (subseq typename 0 (position #\< typename)))))

#+NIL
(defun typename-sanity (typename)
  (setq typename (string-trim '(#\Space) typename))
  (when (template-typename-p typename)
    (unless (equalp typename *current-template-type*)
      (pushnew typename *template-types* :test #'string=)))
  typename)

#+NOTYET
(defun lispify-typename (typename)
  (when (symbolp typename)
    (setq typename (copy-seq (symbol-name typename))))
  (when (namespaced-typename-p typename)
    (multiple-value-bind (namespace name)
	(typename-namespace-and-name typename)
      (declare (ignore namespace))
      (setq typename name)))
  (when (template-typename-p typename)
    (let ((name (template-name typename))
	  (arguments (template-arguments typename)))
      (setq typename
	    (if (string= name "handle")
		(apply #'concatenate 'string "Handle_" (mapcar #'typename-sanity arguments))
		(apply #'concatenate 'string name (mapcar #'(lambda (arg)
							      (concatenate 'string "-" arg))
							  (mapcar #'typename-sanity arguments)))))))
  (noffi::cintern typename))


(defun equal-types (a b)
  (or (not (= 0 (#_clang_equalTypes a b)))
      (eq (get-type-name a) (get-type-name b))))


(defun get-type-name (type)
  (let ((name (#_clang_getTypeSpelling type)))
    (when name
      (let ((cstring (#_clang_getCString name)))
	(when cstring
	  (unwind-protect (noffi::cintern (get-c-string cstring))
	    #+NIL
	    (#_clang_disposeString cstring)))))))

(defun get-type-expression (type)
  (let* ((name (get-type-name type))
	 (noffi-type (get-type-expression-from-string name)))
    noffi-type
    #+NIL
    (if noffi-type
	noffi-type
	(progn;; (print (coerce (symbol-name name) '(simple-array character (*))))
	  (to-noffi-type (make-a-clang-type type))))))

(defun get-type-expression-from-string (name)
  (let ((result nil))
    (catch 'foo
      ;;(print (coerce (symbol-name name) '(simple-array character (*))))
      (handler-bind ((error #+NIL noffi::noffi-parsing-error #'(lambda (c)
						     (declare (ignore c))
						     (throw 'foo nil))))
	(let (#+NIL(*resolver-chain* (list #'(lambda (symbol)
					  (when (stringp symbol)
					    (setq symbol (noffi::cintern symbol)))
					  (get-decl symbol (namespace-qualifiers *current-namespace*))))))
	  (setq result (noffi::parse-type (coerce (symbol-name name) '(simple-array character (*)))))))

      (let* ((identifier (get-identifier result)))

	(let ((resolution (resolve identifier *current-namespace*)))
	
	  (unless (equalp identifier resolution)
	    (setq result (type-substitute resolution identifier result))
	    )))
      
      result
      
      )))

(defun template-type-p (type-expression)
  (and (consp type-expression) (eq :template (car type-expression))))

(defun qualified-type-p (type-expression)
  (and (consp type-expression) (eq :qualified (car type-expression))))

(defun unqualified-type-p (type-expression)
  (and (consp type-expression) (eq :unqualified (car type-expression))))

(defun type-name-type-p (type-expression)
  (and (consp type-expression) (eq :type-name (car type-expression))))

(defun lvalue-ref-type-p (type-expression)
  (and (consp type-expression) (eq :lvalue-reference (car type-expression))))

(defun rvalue-ref-type-p (type-expression)
  (and (consp type-expression) (eq :rvalue-reference (car type-expression))))

(defun pointer-type-expr-p (type-expression)
  (and (consp type-expression) (eq :pointer (car type-expression))))

(defun pointerish-type-p (type-expression)
  (or (pointer-type-expr-p type-expression)
      (lvalue-ref-type-p type-expression)
      (rvalue-ref-type-p type-expression)))

(defun type-qualifier-type-p (type-expression)
  (and (consp type-expression) (eq :type-qualifier (car type-expression))))

(defun struct-type-p (type-expression)
  (and (consp type-expression) (eq :struct (car type-expression))))

(defun union-type-p (type-expression)
  (and (consp type-expression) (eq :union (car type-expression))))

(defun array-type-expr-p (type-expression)
  (and (consp type-expression) (eq :array (car type-expression))))

  

(defun valid-identifier-type-p (type-expression)
  (or (keywordp type-expression)
      (type-name-type-p type-expression)
      (template-type-p type-expression)
      (struct-type-p type-expression)
      (union-type-p type-expression)))

(defun resolve (identifier namespace)
  (cond ((keywordp identifier) identifier)
	((type-name-type-p identifier)
	 (let ((result (or (resolve-type-name (cadr identifier) namespace)
			   (cadr identifier))))
	   ;; this nonsense is because resolve-type-name doesn't return uniform results!
	   (cond ((keywordp result)
		  result)
		 ((atom result)
		  (list :type-name result))
		 ((eq (car result) :qualified)
		  (list :type-name result))
		 (t result))))
	((template-type-p identifier)
	 (list* :template (or (resolve-template-name (cadr identifier) namespace)
			      (cadr identifier))
		(mapcar #'(lambda (id)
			    (resolve-template-argument id namespace))
			(cddr identifier))))
	(t identifier)))

(defun qualify-name (unqualified-name namespace &key (template-p nil))
  (do ((ns namespace (namespace-namespace ns)))
      ((null ns) nil)
    (let ((decl
	    (loop for name in (butlast (cdr unqualified-name))
		  with decl = ns
		  do (setq decl (when (namespace-p decl)
				  (gethash name (namespace-declarations decl))))
		     (unless decl
		       (return nil))
		  finally (return (when (namespace-p decl)
				    ;; need to fix: when decl is a typedef,
				    ;; typedef resolves to another type
				    ;; this other type might have a decl
				    ;; or it could be a template needing instantiation
				    ;; so I guess we would have to instantiate template here
				    ;; to avoid an unqualfied name
				    (gethash (first (last unqualified-name))
					     (if template-p
						 (namespace-template-definitions decl)
						 (namespace-declarations decl))))))))
      (when decl
	(return (if (type-alias-p decl)
		    (type-alias-type decl)
		    (let ((qualifiers (namespace-qualifiers (named-declaration-namespace decl))))
		      (if qualifiers
			  (append (list :qualified)
				  qualifiers
				  (last unqualified-name))
			  (list :qualified (cadr unqualified-name))))))))))

(defun resolve-type-name (name-or-qualified-name namespace)
  (cond ((qualified-type-p name-or-qualified-name)
	 (let ((qualified-name (rest name-or-qualified-name)))
	   (let ((decl (get-decl (first (last qualified-name))
				 (butlast qualified-name))))
	     (if (type-alias-p decl)
		 (type-alias-type decl)
		 name-or-qualified-name))))

	((unqualified-type-p name-or-qualified-name)
	 (qualify-name name-or-qualified-name namespace))

	((symbolp name-or-qualified-name)
	 (qualify-name (list :unqualified name-or-qualified-name) namespace))

	(t (warn "unexpected name in resolve-type-name: ~S" name-or-qualified-name)
	   name-or-qualified-name)
		  
	#+NOTNOW ;; todo: move into qualify-name
	(t (let ((name name-or-qualified-name))
	     ;;(print name)
	     (or (loop for ns-name in *using-directives*
		       do (let ((ns (gethash ns-name (namespace-declarations *global-namespace*))))
			    (when ns
			      (let ((decl (gethash name (namespace-declarations ns))))
				(when decl
				  (if (type-alias-p decl)
				      (return (type-alias-type decl))
				      (return (list :qualified ns-name name)))))))
		       finally (return nil))
		 (do ((ns namespace (namespace-namespace ns)))
		     ((null ns) nil)
		   (let ((decl (gethash name (namespace-declarations ns))))
		     (when decl
		       (return
			 
			 (if (type-alias-p decl)
			     (type-alias-type decl)
			     #+NIL
			     (let ((result (type-alias-type decl)))
			       (let* ((identifier (get-identifier result)))
				 (let ((resolution (resolve identifier *current-namespace*)))
				   (unless (equalp identifier resolution)
				     (setq result (type-substitute resolution identifier result))
				     )))					   
			       (print result))
			     (let ((qualifiers (namespace-qualifiers ns)))
			       (if qualifiers
				   (append (list :qualified)
					   qualifiers
					   (list name-or-qualified-name))
				   name-or-qualified-name))))))))))))

(defun resolve-template-name (name-or-qualified-name namespace)
  (cond ((qualified-type-p name-or-qualified-name) name-or-qualified-name)
	((unqualified-type-p name-or-qualified-name)
	 (qualify-name name-or-qualified-name namespace :template-p t))
	((symbolp name-or-qualified-name)
	 (qualify-name (list :unqualified name-or-qualified-name) namespace :template-p t))
	(t (do ((ns namespace (namespace-namespace ns)))
	       ((null ns) nil)
	     (let ((decl (gethash name-or-qualified-name (namespace-declarations ns))))
	       (when decl
		 (return (let ((qualifiers (namespace-qualifiers ns)))
			   (if qualifiers
			       (append (list :qualified)
				       qualifiers
				       (list name-or-qualified-name))
			       name-or-qualified-name)))))))))

(defun resolve-template-argument (identifier namespace)
  (cond ((keywordp identifier) identifier)
	((consp identifier)
	 (cond ((type-name-type-p identifier)
		(list :type-name
		      (or (resolve-type-name (cadr identifier) namespace)
			  (cadr identifier))))
	       ((eq (car identifier) 'noffi::expr)
		(let ((resolved (resolve-type-name (cadr identifier) namespace)))
		  (cond ((consp resolved)
			 resolved)
			(t (if resolved
			       (list :type-name resolved)
			       identifier)
			   ))))))
	(t identifier)))
		    






(defmethod process-arg-type ((module module) (ffi noffi) pass type)
  (let ((type (get-type-expression type)))
    type))

(defmethod process-function-params ((module module) ffi pass cursor type)
  (let ((num-args (#_clang_getNumArgTypes type)))
    
    (loop for i from 0 below num-args
	  collect (let* ((arg-type (#_clang_getArgType type i))
			 (arg (#_clang_Cursor_getArgument cursor i))
			 (arg-name (get-cursor-name arg)))
		    (let (#+NIL(*verbosity* 2)
			  (*default* ()))
		      (visit-children arg)
		      (unless arg-name
			(setq arg-name (noffi::cintern (format nil "ARG~A" i))))
		      (make-function-argument :name arg-name
					      :type (process-arg-type module ffi pass arg-type)
					      :default *default*))))))

  

(defun print-param-names (cursor type)
  (let ((num-args (#_clang_getNumArgTypes type)))
    
    (loop for i from 0 below num-args
       do (let* ((arg (#_clang_Cursor_getArgument cursor i))
		 (arg-name (#_clang_getCursorSpelling arg)))
	    (if arg-name
		(let ((string (get-c-string (#_clang_getCString arg-name))))
		  (if (string= string "")
		      (format *lisp-file* "ARG~A" i)
		      (format *lisp-file* "|~A|" string)))
		(format *lisp-file* "ARG~A" i))
	    (unless (= i (1- num-args))
	      (format *lisp-file* " "))))))

(defvar *opencascade-modules-table*
  (make-hash-table :test #'equalp))

(defvar *opencascade-header-directory*
  "~/OCCT/include/")

(defvar *opencascade-source-directory*
  "c:/Users/awolven/OCCT/src/")

(defun opencascade-module-directories ()
  (mapcar #'(lambda (pathname)
	      (first (last (butlast (UIOP/UTILITY:SPLIT-STRING (directory-namestring pathname) :separator '(#\/))))))
	  (directory (concatenate 'string *opencascade-source-directory* "*.*"))))

(defun opencascade-header-files ()
  (directory (concatenate 'string *opencascade-header-directory* "*.*")))

(defun register-opencascade-modules ()
  (let ((modules (opencascade-module-directories)))
    (loop for m in modules
	  do (setf (gethash m *opencascade-modules-table*) m))))

(defun list-opencascade-modules ()
  (register-opencascade-modules)
  (let ((list ()))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k list))
	     *opencascade-modules-table*)
    list))

(defparameter *preferred-opencascade-module-ordering*
  (list "Mmgt"
	"Standard"
	"gp"

	"NCollection"
	"TCollection"
	"TColStd"
	"TColgp"
	"TobAbs"
	"TopoDS"
	"TopTools"
	"TopLoc"
	"Poly"
	"BRep"
	"BRepTools"
	"BRepBuilder"
	"BRepAlgo"
	"BRepPrimAPI"
	"BRepOffsetAPI"
	"BrepAlgoAPI"
	"Geom2d"
	"Geom"
	"BRepMesh"
	"BisectorCurve"
	"Bisector"
	"BRepLib"
	"BRepFilletAPI"
	"BRepCheck"
	"ShapeBuild"
	"STEPControl"
	"IGESControl"
	"XSControl"
	"StepRepr"
	"IGESData"
	"TransferBRep"
	"Transfer"
	"Interface"
	"ShapeFix"
	"ShapeBuild"
	"ShapeConstruct"
	"ShapeAnalysis"
	"APIHeaderSection"
	"StepData"
	"ShapeUpgrade"
	"GeomAPI"
	"GC"
	"GCE2d"
	"Bnd"
	"BndLib"
	"Adaptor2d"
	"Geom2dAdaptor"
	"Adaptor3d"
	"GeomAdaptor"
	"CPnts"))
  
  

(defun opencascade-module-ordering ()
  (let ((ordering (copy-list *preferred-opencascade-module-ordering*))
	(random-modules (list-opencascade-modules)))
    (loop for module in random-modules
	  do (unless (position module ordering :test #'string-equal)
	       (setf ordering (nconc ordering (list module))))
	  finally (return ordering))))

(defun opencascade-preferred-header-file-ordering ()
  (let ((modules (opencascade-module-ordering)
		  #+NOTYET(copy-list *preferred-opencascade-module-ordering*))
	(list ()))
    (loop for module in modules
	  do (let ((headers (directory (concatenate 'string *opencascade-source-directory* module "/*.hxx"))))
	       (setq headers (remove "FILES" headers :test #'string= :key #'pathname-name))
	       (setf list (nconc list headers)))
	  finally (return list))))
    
    

(defmethod ffi-name ((ffi cffi))
  "cffi")

(defmethod ffi-name ((ffi noffi))
  "noffi")
	
(defun oc.lisp-filename (ffi)
  (concatenate 'string "~/cxxbinder/oc-" #+win32 "windows" #+darwin "darwin" #+linux "linux"
					 "-" (ffi-name ffi) ".lisp"))


#+NIL  
(defmethod reinitialize-generator :around ((module module) ffi)
  (clrhash *structs*)
  (clrhash *typedefs*)
  (clrhash *done-templates*)
  (reinitialize-lisp-output-file module ffi)
  (call-next-method))

#+NIL
(defmethod reinitialize-generator ((module opencascade) ffi)

  (setf (gethash "Handle_Standard_Transient" *structs*) "Handle_Standard_Transient")
  (setf (gethash "Handle_Standard_Transient" *typedefs*) "Handle_Standard_Transient")
  (setf (gethash "Handle_Standard_Type" *structs*) "Handle_Standard_Type")
  (setf (gethash "Handle_Standard_Type" *typedefs*) "Handle_Standard_Type")
  (values))

#+NIL
(defmethod reinitialize-lisp-output-file :around ((module opencascade) ffi)
  (with-open-file (*lisp-file* (oc.lisp-filename ffi)
			       :direction :output :if-exists :supersede
			       :if-does-not-exist :create)
    (format *lisp-file* "(cl:defpackage :oc)~%~%")
    (format *lisp-file* "(cl:in-package :oc)~%~%")
    (call-next-method)))
    
#+NIL
(defmethod reinitialize-lisp-output-file ((module opencascade) (ffi cffi))
  (format *lisp-file* "(cffi:defctype size_t :long-long)~%~%")
  (format *lisp-file* "(cffi:defcstruct Handle_Standard_Transient (bytes (:array :char 8)))~%~%")
  (format *lisp-file* "(cffi:defcstruct Handle_Standard_Type (bytes (:array :char 8)))~%~%")
  (values))

#+NIL
(defmethod reinitialize-lisp-output-file ((module opencascade) (ffi noffi))
  (format *lisp-file* "(noffi::noffi-syntax)~%~%")
  (print '(noffi::decl ((:storage-class :typedef)) (#_size_t :unsigned-long-long)) *lisp-file*)
  (terpri *lisp-file*)
  (print '(noffi::decl ((:storage-class :typedef))
	   (#_Handle_Standard_Transient
	    (:struct #_Handle_Standard_Transient
	     ((noffi::decl nil (#_entity (:pointer (:type-name #_Standard_Transient)))))))) *lisp-file*)
  (terpri *lisp-file*)
  (format *lisp-file* "(cl:defpackage :Handle_Standard_Type)")
  (terpri *lisp-file*)
  (terpri *lisp-file*)
  (print '(noffi::decl ((:storage-class :typedef))
	   (#_Handle_Standard_Type
	    (:struct #_Handle_Standard_Type
	     ((noffi::decl nil (#_entity (:pointer (:type-name #_Standard_Type)))))))) *lisp-file*)
  (terpri *lisp-file*)
  (terpri *lisp-file*)
  (values))


(defun generate-opencascade-cffi ()
  ;;(setq *template-types* nil)
  (setq *module* (make-instance 'opencascade))
  (setq *abi* (make-instance 'msvc))
  (setq *ffi* (make-instance 'cffi))
  (initialize-builtin-type-array *ffi*)
  (setq *pass* 0)
  ;;(reinitialize-generator *module* *ffi*)
  (iterate-headers *module* *ffi* *pass*))
  

(defun generate-opencascade-noffi ()
  (setq *global-namespace* (make-namespace))
  (let ((*current-namespace* *global-namespace*))
    (setq *module* (make-instance 'opencascade))
    (setq *abi* (make-instance 'msvc))
    (setq *ffi* (make-instance 'noffi))
    (setq *pass* 0)
    (setq *foo* 0)
    (setq *exports2* (copy-seq *opencascade-exports*))
    (clrhash *bar*)
    (describe-pass *pass*)
    (time (apply #'main "c:\\Users\\awolven\\cxxbinder\\opencascade.hxx"
		    "-D_HAS_EXCEPTIONS=0"
		    "-I" "C:\\Users\\awolven\\OCCT\\3rdparty-vc14-64\\tcltk-8.6.15-x64\\include"
		    "-I" "C:\\Users\\awolven\\OCCT\\3rdparty-vc14-64\\freetype-2.13.3-x64\\include"
		    (compute-include-arguments *module*)))
    (setq *pass* 1)
    (describe-pass *pass*)
    (with-open-file (*lisp-file* (oc.lisp-filename *ffi*)
				 :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)
      (multiple-value-bind (decl-list decl-table)
	  (compute-all-dependencies)
	(declare (ignore decl-table))

	(loop for decl in decl-list
	      do (write-type-decl decl (named-declaration-name decl)
				  *module* *ffi* *abi* *lisp-file*))

	(loop for decl in decl-list
	      when (and (struct-declaration-p decl)
			(not (named-declaration-punt decl)))
		do (format *lisp-file* "~%~S~%" (struct-declaration-name decl))
		   (loop for method in (struct-declaration-methods decl)
			 when (and (eq (method-declaration-linkage-kind method) :external)
				   (eq (method-declaration-access method) :public)
				   (not (method-declaration-virtual? method))
				   #+NIL
				   (or (method-declaration-pure-virtual? method)
				       (not (method-declaration-virtual? method))))
			 do (write-type-decl method (named-declaration-name method)
					     *module* *ffi* *abi* *lisp-file*)))))))
  

(defmethod compute-headers-list ((module opencascade))
  (opencascade-preferred-header-file-ordering))

(defmethod compute-include-arguments ((module opencascade))
  (apply #'append (mapcar #'(lambda (dirname)
			      (list "-I" (concatenate 'string *opencascade-source-directory* dirname)))
			  (opencascade-module-directories))))

(defmethod describe-pass ((pass (eql 0)))
  (format t "~&Pass ~S: Gathering applicable ASTs of C++ files...." pass))

(defmethod describe-pass ((pass (eql 1)))
  (format t "~&Pass ~S: Instantiating ASTs of template types...." pass))

(defmethod describe-pass ((pass (eql 2)))
  (format t "~&Pass ~S: Emitting type declarations...." pass))

(defmethod describe-pass ((pass (eql 3)))
  (format t "~&Pass ~S: Emitting function bindings...." pass))

(defmethod iterate-headers ((module opencascade) ffi pass)
  (describe-pass pass)
  (finish-output)
  (let ((includes (compute-include-arguments module)))
    (loop for file in (compute-headers-list module)
	  do (apply #'main (namestring file)
		    "-shared"
		    "-D_HAS_EXCEPTIONS=0"
		    "-I" "C:\\Users\\awolven\\OCCT\\3rdparty-vc14-64\\tcltk-8.6.15-x64\\include"
		    "-I" "C:\\Users\\awolven\\OCCT\\3rdparty-vc14-64\\freetype-2.13.3-x64\\include"
		    includes))))

(defun test ()
  (setq *global-namespace* (make-namespace))
  (let ((*current-namespace* *global-namespace*))
    (setq *module* (make-instance 'opencascade))
    (setq *abi* (make-instance 'msvc))
    (setq *ffi* (make-instance 'noffi))
    (initialize-builtin-type-array *ffi*)
    (setq *pass* 0)
    (apply #'main "c:/Users/awolven/cxxbinder/test.cpp"
	   "-D_HAS_EXCEPTIONS=0"
	   (compute-include-arguments *module*))))


(defmethod root-type ((type clang-type))
  type)

(defmethod child-type ((type clang-type))
  nil)


(defmethod child-type ((type pointer-type))
  (pointer-type-pointee-type type))


(defmethod (setf child-type) (new (type pointer-type))
  (setf (pointer-type-pointee-type type) new))

(defmethod child-type ((type reference-type))
  (reference-type-pointee-type type))

(defmethod (setf child-type) (new (type reference-type))
  (setf (reference-type-pointee-type type) new))

#+NIL
(defmethod child-type ((type elaborated-type))
  (elaborated-type-named-type type))

#+NIL
(defmethod (setf child-type) (new (type elaborated-type))
  (setf (elaborated-type-named-type type) new))

(defmethod child-type ((type array-vector-or-complex-type))
  (array-vector-or-complex-type-element-type type))

(defmethod (setf child-type) (new (type array-vector-or-complex-type))
  (setf (array-vector-or-complex-type-element-type type) new))




(defmethod root-type ((type pointer-type))
  (root-type (pointer-type-pointee-type type)))

(defmethod root-type ((type reference-type))
  (root-type (reference-type-pointee-type type)))

#+NIL
(defmethod root-type ((type elaborated-type))
  (or (and (elaborated-type-named-type type) (root-type (elaborated-type-named-type type))) type))

(defmethod root-type ((type array-vector-or-complex-type))
  (root-type (array-vector-or-complex-type-element-type type)))



(defun make-a-clang-type (type)
  (let* ((name (get-type-name type))
	 (const? (not (= 0 (#_clang_isConstQualifiedType type))))
	 (volatile? (not (= 0 (#_clang_isVolatileQualifiedType type))))
	 (restrict? (not (= 0 (#_clang_isRestrictQualifiedType type))))
	 (qualifiers nil)
	 (kind (#_.kind type)))
    (when const?
      (push :const qualifiers))
    (when volatile?
      (push :volatile qualifiers))
    (when restrict?
      (push :restrict qualifiers))
    (case kind
	  (#.#_CXType_Invalid (make-invalid-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Unexposed (make-unexposed-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Void (make-void-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Bool (make-bool-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Char_U (make-char-u-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UChar (make-uchar-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Char16 (make-char16-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Char32 (make-char32-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UShort (make-ushort-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UInt (make-uint-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ULong (make-ulong-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ULongLong (make-ulonglong-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UInt128 (make-uint128-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Char_S (make-char-s-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_SChar (make-schar-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_WChar (make-wchar-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Short (make-short-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Int (make-int-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Long (make-long-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_LongLong (make-longlong-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Int128 (make-int128-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Float (make-float-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Double (make-double-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_LongDouble (make-long-double-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_NullPtr (make-nullptr-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Overload (make-overload-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Dependent (make-dependent-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ObjCId (make-objc-id-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ObjCClass (make-objc-class-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ObjCSel (make-objc-sel-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Float128 (make-float128-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Half (make-half-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Float16 (make-float16-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ShortAccum (make-short-accum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Accum (make-accum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_LongAccum (make-long-accum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UShortAccum (make-ushort-accum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_UAccum (make-uaccum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ULongAccum (make-ulong-accum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_BFloat16 (make-bfloat16-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Ibm128 (make-ibm128-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Complex
	   (make-complex-type :name name
			      :cv-qualifiers qualifiers
			      :element-type
			      (let ((element-type (#_clang_getElementType type)))
				(when element-type
				  (make-a-clang-type element-type)))))
	  (#.#_CXType_Pointer
	   (make-pointer-type :name name
			      :cv-qualifiers qualifiers
			      :pointee-type
			      (let ((pointee-type (#_clang_getPointeeType type)))
				(when pointee-type
				  (unless (equal-types type pointee-type)
				    (make-a-clang-type pointee-type))))))
	  (#.#_CXType_BlockPointer (make-block-pointer-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_LValueReference
	   (make-lvalue-reference-type :name name
				       :cv-qualifiers qualifiers
				       :pointee-type
				       (let ((pointee-type (#_clang_getPointeeType type)))
					 (when pointee-type
					   (make-a-clang-type pointee-type)))))
	  (#.#_CXType_RValueReference
	   (make-rvalue-reference-type :name name
				       :cv-qualifiers qualifiers
				       :pointee-type
				       (let ((pointee-type (#_clang_getPointeeType type)))
					 (when pointee-type
					   (make-a-clang-type pointee-type)))))
	  (#.#_CXType_Record (make-record-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Enum (make-enum-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Typedef (make-typedef-type :name name :cv-qualifiers qualifiers
						 :canonical-type
						 (let ((canonical-type (#_clang_getCanonicalType type)))
						   (when canonical-type
						     (make-a-clang-type canonical-type)))))
	  (#.#_CXType_ObjCInterface (make-objc-interface-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_ObjCObjectPointer
	   (make-objc-object-pointer-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_FunctionNoProto
	   (make-function-no-proto-type :name name :cv-qualifiers qualifiers
					:return-type
					(let ((type (#_clang_getResultType type)))
					  (when type
					    (make-a-clang-type type)))
					:argument-types
					(loop for i from 0 below (#_clang_getNumArgTypes type)
					      collect (let ((arg-type (#_clang_getArgType type i)))
							(when arg-type
							  (make-a-clang-type arg-type))))))
	  (#.#_CXType_FunctionProto
	   (make-function-proto-type :name name :cv-qualifiers qualifiers
				     :return-type
				     (let ((type (#_clang_getResultType type)))
				       (when type
					 (make-a-clang-type type)))
				     
				     :argument-types
				     (loop for i from 0 below (#_clang_getNumArgTypes type)
					   collect (let ((arg-type (#_clang_getArgType type i)))
						     (when arg-type
						       (make-a-clang-type arg-type))))))
	  (#.#_CXType_ConstantArray
	   (make-constant-array-type :name name
				     :cv-qualifiers qualifiers
				     :element-type
				     (let ((element-type (#_clang_getElementType type)))
				       (when element-type
					 (make-a-clang-type element-type)))))
	  (#.#_CXType_Vector
	   (make-vector-type :name name
			     :cv-qualifiers qualifiers
			     :element-type
			     (let ((element-type (#_clang_getElementType type)))
			       (when element-type
				 (make-a-clang-type element-type)))))
				      
	  (#.#_CXType_IncompleteArray
	   (make-incomplete-array-type :name name
				       :cv-qualifiers qualifiers
				       :element-type
				       (let ((element-type (#_clang_getElementType type)))
					 (when element-type
					   (make-a-clang-type element-type)))))
	  (#.#_CXType_VariableArray
	   (make-variable-array-type :name name
				     :cv-qualifiers qualifiers
				     :element-type
				     (let ((element-type (#_clang_getElementType type)))
				       (when element-type
					 (make-a-clang-type element-type)))))
	  (#.#_CXType_DependentSizedArray
	   (make-dependent-sized-array-type
	    :name name
	    :cv-qualifiers qualifiers
	    :element-type
	    (let ((element-type (#_clang_getElementType type)))
	      (when element-type
		(make-a-clang-type element-type)))))
	  (#.#_CXType_MemberPointer (make-member-pointer-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Auto (make-auto-type :name name :cv-qualifiers qualifiers))
	  (#.#_CXType_Elaborated
	   (make-elaborated-type :name name
				 :cv-qualifiers qualifiers
				 :named-type
				 (let ((named-type (#_clang_Type_getNamedType type)))
				   (when named-type
				     (unless (equal-types type named-type)
				       (make-a-clang-type named-type)))))))
    #+NIL
    (if (template-typename-p name)
	(make-template-class-type :name (template-name name) :arguments (template-arguments name)
				  :cv-qualifiers qualifiers)
	)))

;;(setq *dependencies-list* ())
;;(setq *dependencies-table* (make-hash-table :test #'equalp))

;;(setq *visited* (make-hash-table :test #'equalp))


(defmethod compute-all-dependencies ()
  (let* ((*visited* (make-hash-table :test #'equalp))
	 (*dependencies-table* (make-hash-table :test #'equalp))
	 (goals (get-all-goal-decls))
	 (*dependencies-list* goals))
    (mapcar #'compute-dependencies goals)
    #+NIL(compute-dependencies *global-namespace*)
    (values (remove-if #'named-declaration-punt (nreverse *dependencies-list*)) *dependencies-table*)))

(defun namespace-qualifiers (namespace)
  (reverse (namespace-qualifiers-1 namespace)))

(defun namespace-qualifiers-1 (namespace)
  (when (namespace-name namespace)
    (let ((name (namespace-name namespace)))
      (cons name
	  (when (namespace-namespace namespace)
	    (namespace-qualifiers-1 (namespace-namespace namespace)))))))

(defmethod compute-dependencies ((namespace namespace) &optional decl-search-path)
  (declare (ignore decl-search-path))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (unless (or (class-template-definition-p v)
			   (type-alias-template-decl-p v)
			   (type-alias-p v))
		 (compute-dependencies v)))
	   (namespace-declarations namespace)))

(defmethod compute-dependencies ((struct struct-declaration) &optional decl-search-path)
  (declare (ignore decl-search-path))
  (let ((decl-search-path (namespace-qualifiers struct)))
    (mapcar #'(lambda (type)
		(compute-type-dependencies type decl-search-path))
	    (struct-declaration-base-types struct))
    (mapcar #'(lambda (type)
		(compute-dependencies type decl-search-path))
	    (struct-declaration-fields struct))
    (mapcar #'(lambda (type)
		(compute-dependencies type decl-search-path))
	    (struct-declaration-methods struct))))

(defmethod compute-dependencies ((field field-declaration) &optional decl-search-path)
  (compute-type-dependencies (field-declaration-type field) decl-search-path))

(defmethod compute-dependencies ((typedef type-definition) &optional decl-search-path)
  (compute-type-dependencies (type-definition-type typedef) decl-search-path))

(defmethod compute-dependencies ((typedef type-alias) &optional decl-search-path)
  (compute-type-dependencies (type-alias-type typedef) decl-search-path))

(defmethod compute-dependencies ((enum enum-declaration) &optional decl-search-path)
  (declare (ignore decl-search-path)))

(defmethod compute-dependencies ((function function-declaration) &optional decl-search-path)
  (compute-type-dependencies (function-declaration-return-type function) decl-search-path)
  (mapcar #'(lambda (type)
	      (compute-dependencies type decl-search-path))
	  (function-declaration-arguments function)))

(defmethod compute-dependencies ((argument function-argument) &optional decl-search-path)
  (compute-type-dependencies (function-argument-type argument) decl-search-path))

(defun get-identifier (noffi-parsed-type)
  (if (null noffi-parsed-type)
      nil
      (if (not (listp noffi-parsed-type))
	  noffi-parsed-type
	  (if (qualified-type-p noffi-parsed-type)
	      (progn (warn "bad type spec ~S" noffi-parsed-type)
		     (list :type-name noffi-parsed-type))
	      (if (valid-identifier-type-p noffi-parsed-type)
		  noffi-parsed-type
		  (if (type-qualifier-type-p noffi-parsed-type)
		      (get-identifier (caddr noffi-parsed-type))
		      (get-identifier (cadr noffi-parsed-type))))))))

(defun get-template-definition (type-expression)
  (if (null type-expression)
      nil
      (if (not (listp type-expression))
	  (gethash type-expression (namespace-namespace *global-namespace*))
	  (cond ((template-type-p type-expression)
		 (cond ((qualified-type-p (cadr type-expression))
			(let ((qualified-name (cdadr type-expression))
			      (namespace *global-namespace*))
			  (loop for identifier in (butlast qualified-name)
				do (setq namespace (gethash identifier (namespace-declarations namespace)))
				finally (when namespace
					  (return (gethash (first (last qualified-name))
							   (namespace-template-definitions
							    namespace)))))))
		       (t (gethash (cadr type-expression)
				   (namespace-template-definitions *global-namespace*)))))
		(t (error "3: don't know what to do with ~S" (cadr type-expression)))))))



	      
	  

(defun get-decl (noffi-identifier &optional (decl-search-path nil))
  (flet ((search-namespaces (name)
	   (if decl-search-path
	       (let ((qualified-name (append decl-search-path (list name)))
		     (namespace *global-namespace*))
		 ;; walk it up
		 (loop for identifier in (butlast qualified-name)
		       do (setq namespace (gethash identifier (namespace-declarations namespace)))
		       unless namespace
			 do (return nil)
		       finally (return
				 ;; walk it back down
				 (do ((ns namespace (namespace-namespace ns)))
				     ((null ns))
				   (let ((decl (gethash name
							(namespace-declarations ns))))
				     
				     (when decl
				       (unless (class-template-definition-p decl)
					 (return decl))))))))
	       
	       (gethash name (namespace-declarations *global-namespace*)))))
    
    (if (null noffi-identifier)
	nil
	
	(if (not (listp noffi-identifier))
	    (let ((namespace *global-namespace*))
	      (loop for identifier in decl-search-path
		    do (setq namespace (gethash identifier (namespace-declarations namespace)))
		    finally (when namespace
			      (return (gethash noffi-identifier (namespace-declarations namespace))))))
	    
	    (cond ((type-name-type-p noffi-identifier)
		   (cond ((or (qualified-type-p (cadr noffi-identifier))
			      (unqualified-type-p (cadr noffi-identifier))) ;;hack.
			  (let ((qualified-name (cdadr noffi-identifier))
				(namespace *global-namespace*))
			    (loop for identifier in (butlast qualified-name)
				  do (setq namespace (gethash identifier
							      (namespace-declarations namespace)))
				  unless namespace
				    do (return nil)
				  finally (return (let ((decl
							  (gethash (first (last qualified-name))
								   (namespace-declarations namespace))))
						    (when decl
						      (unless (class-template-definition-p decl)
							decl)))))))
			 (t (search-namespaces (cadr noffi-identifier)))))
		  
		  ((template-type-p noffi-identifier)
		   
		   (if (or (qualified-type-p (cadr noffi-identifier))
			   (unqualified-type-p (cadr noffi-identifier))) ;; hack.
		       (let ((qualified-name (cdadr noffi-identifier))
			     (arguments (cddr noffi-identifier))
			     (namespace *global-namespace*))
			 (loop for identifier in (butlast qualified-name)
			       do (setq namespace (gethash identifier (namespace-declarations namespace)))
			       unless namespace
				 do (return nil)
			       finally (return
					 (let ((decl
						 (gethash (list* (first (last qualified-name))
								 arguments)
							  (namespace-declarations namespace))))
					   (when decl
					     (unless (class-template-definition-p decl)
					       decl))))))
		     
		       (let ((name (cadr noffi-identifier))
			     (arguments (cddr noffi-identifier)))
			 (search-namespaces (list* name arguments)))))

		  (t (search-namespaces noffi-identifier)))))))

(defun primitive-cxx-type-p (identifier)
  (or (keywordp identifier)
      (and (type-name-type-p identifier)
	   (or (eql (cadr identifier) 'noffi-c::|bool|)
	       (eql (cadr identifier) 'noffi-c::|size_t|)
	       (eql (cadr identifier) 'noffi-c::|wchar_t|)
	       (eql (cadr identifier) 'noffi-c::|char16_t|)
	       (eql (cadr identifier) 'noffi-c::|char32_t|)
	       (eql (cadr identifier) 'noffi-c::|uintptr_t|)
	       (eql (cadr identifier) 'noffi-c::|string|)
	       (eql (cadr identifier) 'noffi-c::|_m128|)
	       (eql (cadr identifier) 'noffi-c::|_m128i|)
	       (eql (cadr identifier) 'noffi-c::|_m128d|)
	       (eql (cadr identifier) 'noffi-c::|_m512|)
	       (eql (cadr identifier) 'noffi-c::|_m512i|)
	       (eql (cadr identifier) 'noffi-c::|_m512d|)))
      (and (union-type-p identifier)
	   (or (eql (cadr identifier) 'noffi-c::|__m128|)
	       (eql (cadr identifier) 'noffi-c::|__m128i|)
	       (eql (cadr identifier) 'noffi-c::|__m512|)
	       (eql (cadr identifier) 'noffi-c::|__m512i|)))))



(defun compute-type-dependencies (type-expression &optional (decl-search-path nil))
  (let ((identifier (get-identifier type-expression)))

    (unless (primitive-cxx-type-p identifier)

      (when (atom identifier)
	(setq identifier (list :type-name identifier)))

      (unless (valid-identifier-type-p identifier)
	(break "invalid identifier: ~S" identifier))
    
      (unless (gethash identifier *visited*)

	(let ((existing (gethash identifier *dependencies-table*)))
	  (unless existing
	    (setf (gethash identifier *visited*) t)
	    
	    (flet ((process (decl)
		     (compute-dependencies decl)
		     (setf (gethash identifier *dependencies-table*) decl)
		     (push decl *dependencies-list*)))
	      
	      (cond ((template-type-p identifier)
		     (let ((decl (get-decl identifier decl-search-path)))
		       (if decl
			   (process decl)
			   (let ((new-decl (instantiate-template *abi* identifier)))
			     (if new-decl
				 (process new-decl)
				 (warn "3: could not compute dependencies for ~S" identifier))))))
		  
		    ((type-name-type-p identifier)
		     (let ((decl (get-decl identifier decl-search-path)))
		       (if decl
			   (process decl)
			   (warn "2: could not compute dependencies for ~S" identifier))))
		  
		    (t (warn "1: could not compute dependencies for ~S" identifier))))))))))
			     
(defun dbg-find-decl (key)
  (dbg-find-decl-1 key *global-namespace*))

(defun dbg-find-decl-1 (key namespace)
  (let ((decl))
    (block nil
      (maphash (lambda (k v)
		 (if (eql (type-of v) 'namespace)
		     (setq decl (dbg-find-decl-1 key v))
		     (when (equalp key k)
		       (setq decl v)))
		 (when decl (return)))
		 
	       (namespace-declarations namespace)))
    decl))
	     

(defun get-all-goal-decls ()
  (let ((headers (compute-headers-list *module*)))
    (get-all-goal-decls-1 headers *global-namespace*)))

(defun get-all-goal-decls-1 (headers namespace)
  (let ((decls ()))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (if (eql (type-of v) 'namespace)
		   (setq decls (append decls (get-all-goal-decls-1 headers v)))
		   (unless (or (class-template-definition-p v)
			       (type-alias-p v))
		     (when (member (named-declaration-file v) headers :test #'equalp)
		       (push v decls)))))
	     (namespace-declarations namespace))
    decls))

(defun get-function-signature (mangled-name)
  (let ((input-stream
	  (nth-value
	   1
	   (noffi::run-program
	    "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.42.34433\\bin\\Hostx64\\x64\\undname.exe"
	    (list mangled-name)
	    :output :stream))))
    (loop with line
	  do (setq line (read-line input-stream nil :eof))
	     (when (eq line :eof)
	       (return nil))
	     (when (search "is :-" line)
	       (let ((pos1 (position #\" line))
		     (pos2 (position #\" line :from-end t)))
		 (when (eq pos1 pos2)
		   (break))
		 (return (subseq line (1+ pos1) pos2)))))))
