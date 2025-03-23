(in-package :cxxbinder)

(defun nest-pointers (pointers value)
  (if (null pointers)
      value
      (if (eq :const (car pointers))
	  (list :type-qualifier :const (nest-pointers (cdr pointers) value))
	  (list (car pointers) (nest-pointers (cdr pointers) value)))))

(defun foo-lexer (input)
  (macrolet ((aux ()
               `
               (clex2::lexer (input)
		 (:sloc :character-position)
		 (:decimal-integer -> "[0-9]+" => (parse-integer $$))
                 ,@(loop for s in '(
                                    ".*" "?" "." "::" "}" "{" ":" "]" "[" "--" "++" ">>=" "<<=" ">>" "<<" "<=>" ">=" "<=" "!=" "==" "|=" "&=" "^=" "%="
                                    "/=" "*=" "-=" "+=" "|" "&" "^" "%" "/" "*" "-" "+" "!" "~" "->*" "||" "&&" "," "->" ";" "=" ">" "<" "..." ")" "("
				    "`" "'")
                         collect `(,(intern s :keyword) -> (quote ,s) => ,s))
                 (-> (* (or #\space #\tab #\newline #\return #\page)))
                 ("[@$a-zA-Z_][@$a-zA-Z_0-9]*"
                  (let ((q (rassoc $$ (nyala::fancy-lalr-table-terminals (GET 'signature-parser 'NYALA::LALR-TABLE)) :test 'equal)))
                    (return (if q
                                (values (car q) $$)
                                (values :identifier $$)))))
		 
                 (:literal -> "[a-zA-Z_0-9]+" => $$)
                 (:literal -> "[0-9][.a-zA-Z_0-9]*" => $$)
                 (:string-literal -> #\" (= it (* (or (- t #\" #\\) (and #\\ t)))) #\" => it)
                 (:literal -> #\' (* (or (- t #\" #\\) (and #\\ t))) #\' => $$)
                 (-> "^\\s*#.*"))))
    (aux)))




"public: __cdecl BinDrivers_DocumentRetrievalDriver::BinDrivers_DocumentRetrievalDriver(void) __ptr64"



   
(defun foo (input)
  (nyala::lalr-parse (get 'signature-parser 'nyala::lalr-table)
		     (foo-lexer input)
		     (get 'signature-parser 'nyala::actions)))


(nyala::define-parser signature-parser
    (:lexer-function #'foo-lexer)

  (start
   -> undname-signature
   -> "@q" qualified-name
   -> "@t" name-or-template-name
   -> "@r" return-value
   -> "@ps" params
   -> "@p" params
   -> "@ta" template-arg
   )

  (undname-signature
   -> signature => $1
   -> "public" ":" signature => (list :public $3)
   -> "private" ":" signature => (list :private $3)
   -> "protected" ":" signature => (list :protected $3)
   -> "public" ":" "static" signature => (list :public (list :static $4))
   -> "private" ":" "static" signature => (list :private (list :static $4))
   -> "protected" ":" "static" signature => (list :protected (list :static $4))
   -> "public" ":" "virtual" signature => (list :public (list :virtual $4))
   -> "private" ":" "virtual" signature => (list :private (list :virtual $4))
   -> "protected" ":" "virtual" signature => (list :protected (list :virtual $4))

   -> return-value "__ptr64" :identifier => (list :variable $1 $3)
   -> return-value :identifier => (list :variable $1 $2)
   -> :identifier => $1
   )

  (signature
   ;; constructor or destructor
   -> "__cdecl" qualified-name "(" params ")" "__ptr64"
   => (list :pointer (list (if (and (consp (car $2))
				    (eq :destructor (caar $2)))
			       (prog1 :cdecl-destructor
				 (setf (car $2) (cadar $2)))
			       :cdecl-constructor)
			   :void
			   (list (list* :qualified (reverse $2)) (reverse $4))))

   -> "__cdecl" qualified-name "::" "operator" essential-type-id "(" params ")" "__ptr64"
   => (list :pointer (list :cdecl-function (append $2 (list (noffi::cintern "operator")))
			   (list $5 (reverse $7))))

   ;; type-cast operator
   -> "__cdecl" qualified-name "::" "operator" essential-type-id "(" params ")" "const" "__ptr64"
   => (list :pointer (list :type-qualifier :const
			   (list :cdecl-function (append $2 (list (noffi::cintern "operator")))
				 (list $5
				       (reverse $7)))))

   ;; static-like signature
   -> cdecl-return-value qualified-name "(" params ")"
   => (list :cdecl-function $1 (list* :qualified (reverse $2)) (reverse $4))

   ;; normal signature
   -> cdecl-return-value qualified-name "(" params ")" "__ptr64"
   => (list :pointer (list :cdecl-function $1 (list (list* :qualified (reverse $2)) (reverse $4))))

   ;; normal const signature
   -> cdecl-return-value qualified-name "(" params ")" "const" "__ptr64"
   => (list :type-qualifier
	    :const
	    (list :pointer (list :cdecl-function $1 (list (list* :qualified (reverse $2))
							  (reverse $4)))))
   ;; weird anonymous function pointer
   -> return-value "(" "__cdecl" "*" signature2 ")" "(" params ")"
   => (list :cdecl-function $1 (list :pointer $5) '&rest)
   )

  (signature2
   -> "__cdecl" qualified-name "(" params ")"
   => (list :cdecl-function (list (list* :qualified $2) (reverse $4)))
   
   -> "__cdecl" qualified-name "(" params ")" "__ptr64"
   => (list :pointer (list :cdecl-function (list (list* :qualified $2) (reverse $4))))
   
   -> "__cdecl" qualified-name "(" params ")" "const" "__ptr64"
   => (list :type-qualifier
	    :const
	    (list :pointer (list :cdecl-function (list (list* :qualified $2) (reverse $4)))))
   )
  
  (cdecl-return-value
   -> "__cdecl" return-value => $2
   -> return-value "__cdecl" => $1
   )

  (qualified-name
   -> qualified-name "::" name-or-template-name => (cons $3 $1)
   -> name-or-template-name                     => (list $1)
   )

  (name-or-template-name
   -> :identifier "<" template-arg-list ">" => (list* :template (noffi::cintern $1) (reverse $3))
   -> :identifier => (noffi::cintern $1)

   -> "~" :identifier "<" template-arg-list ">"
   => (list :destructor (list* :template (noffi::cintern $2) (reverse $4)))
   
   -> "~" :identifier => (list :destructor $2)
      
   -> "operator" ">>" => (noffi::cintern "operator>>")
   -> "operator" "<<" => (noffi::cintern "operator<<")
   -> "operator" "="  => (noffi::cintern "operator=")
   -> "operator" "==" => (noffi::cintern "operator==")
   -> "operator" "!=" => (noffi::cintern "operator!=")
   -> "operator" "*"  => (noffi::cintern "operator*")
   -> "operator" "-"  => (noffi::cintern "operator-")
   -> "operator" "+"  => (noffi::cintern "operator+")
   -> "operator" "/"  => (noffi::cintern "operator/")
   -> "operator" "<=" => (noffi::cintern "operator<=")
   -> "operator" "<"  => (noffi::cintern "operator<")
   -> "operator" ">=" => (noffi::cintern "operator>=")
   -> "operator" ">"  => (noffi::cintern "operator>")
   -> "operator" "[" "]" => (noffi::cintern "operator[]")
   
   -> "`" "default" "constructor" "closure" "'" => (noffi::cintern "default-constructor-closure")

   -> "`" "copy" "constructor" "closure" "'" => (noffi::cintern "copy-constructor-closure")
   )

  (template-arg-list
   -> template-arg-list "," template-arg => (cons $3 $1)
   -> template-arg => (list $1)
   )

  (params
   -> params "," param => (cons $3 $1)
   -> param            => (list $1)
   )

  (template-arg
   -> essential-type-id => $1
   -> essential-type-id "const" => (list :type-qualifier :const $1)
   -> :decimal-integer => (list :decimal-integer $1)
   )

  (param
   -> essential-type-id (? pointer-seq) (? "const")
   => (nest-pointers (reverse (append $2 (when $3 (list :const))))
		     $1)

   ;;-> essential-type-id "(" "__cdecl" "*" ")" "[" :decimal-integer "]"
   ;;=> (nest-pointers $3 (list :array $1 $6))

   -> return-value "(" "__cdecl" "*" ")" "(" params ")"
   => (list :pointer (list :cdecl-function $1 (list (reverse $7))))

   -> return-value "(" "__cdecl" "*" "const" ")" "(" params ")"
   => (list :type-qualifier :const (list :pointer (list :cdecl-function $1 (list (reverse $8)))))

   -> return-value "(" "__cdecl" "*" "const" "&" "__ptr64" ")" "(" params ")"
   => (list :lvalue-reference
	    (list :type-qualifier :const
		  (list :pointer (list :cdecl-function $1 (list (reverse $10))))))

   -> "..." => '&rest
   )

  (pointer-seq
   -> pointer-seq pointer => (append $1 $2)
   -> pointer => $1
   )

  (pointer
   -> "&" "__ptr64" => (list :lvalue-reference)
   -> "*" "__ptr64" => (list :pointer)
   -> "&&" "__ptr64" => (list :rvalue-reference)

   -> "const" "&" "__ptr64" => (list :const :lvalue-reference)
   -> "const" "*" "__ptr64" => (list :const :pointer)
   -> "const" "&&" "__ptr64" => (list :const :rvalue-reference)
   )

  (return-value
   -> essential-type-id (? pointer-seq)
   => (nest-pointers (reverse $2) $1)

   -> essential-type-id "const"
   => (list :type-qualifier :const $1)

   -> qualified-name
   )

  (basic-type-id
   -> "void" => :void
   -> "double" => :int
   -> "float" => :float
   -> "int" => :int
   -> "unsigned" "int" => :unsigned-int
   -> "long" => :long
   -> "unsigned" "long" => :unsigned-long
   -> "char" => :char
   -> "unsigned" "char" => :unsigned-char
   -> "bool" => :bool
   -> "char16_t" => :char16_t
   -> "char32_t" => :char32_t
   -> "__int64" => :int64
   -> "unsigned" "__int64" => :unsigned-int64
   -> "wchar_t" => :wchar_t
   )

  (extended-type-id
   -> "class" qualified-name  => (list :class (list* :qualified (reverse $2)))
   -> "struct" qualified-name => (list :struct (list* :qualified (reverse $2)))
   -> "enum" qualified-name   => (list :enum (list* :qualified (reverse $2)))
   )

  (essential-type-id
   -> basic-type-id => $1
   -> extended-type-id => $1
   )
  )
