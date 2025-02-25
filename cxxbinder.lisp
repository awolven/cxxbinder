(in-package :cl-user)

(defpackage :clang
  (:use :cl :noffi))

(in-package :clang)

(defun show (thing)
  (noffi::find-identifier-declaration thing))

(defvar *verbosity* 0)
(defvar *public?* :unset)
(defvar *base-type* nil)
(defvar *overloads* :unset)
(defvar *handle-type?* nil)
(defvar *structs* (make-hash-table :test #'equalp))
(defvar *typedefs* (make-hash-table :test #'equalp))
(defvar *lisp-file* :error)
(defvar *module*)
(defvar *abi*)
(defvar *ffi*)
(defvar *pass*)
(defvar *template-types* nil)
(defvar *current-template-type* nil)
(defvar *done-templates* (make-hash-table :test #'equalp))
(defvar *defining-struct?* nil)
(defvar *struct-decl* :error)
(defvar *field-stack* nil)
(defvar *template-arg-list-stack* nil)
(defvar *template-definition* :error)
(defvar *namespace-prefix* (noffi::cintern ""))
(defvar *access-specifier* :error)

;; a c-name symbol is a c/c++ name string interned in the noffi-c package

;; a reverse order list of c-name symbols showing the order type declarations should be dumped
(defvar *type-ordering* ())

;; table of typedefs, struct defs & class decls, keyed by c-name symbol
(defvar *type-declarations* (make-hash-table :test #'equalp))

(defvar *types* (make-hash-table :test #'equalp))

;; table of template definitions keyed by c-name symbol
(defvar *template-definitions* (make-hash-table :test #'equalp)) ;; class-templates, function-templates etc

;; a c/c++ typedef
;; name is a c/c++ name symbol, interned in the noffi-c package
;; type is a noffi type
(defstruct type-definition
  (name nil)
  (namespace nil)
  (type nil))

(defstruct clang-type
  (name nil))

(defstruct (primitive-c-type (:include clang-type)))

(defstruct (array-vector-or-complex-type (:include clang-type))
  (element-type nil))

(defstruct (array-type (:include array-vector-or-complex-type)))

(defstruct (reference-type (:include clang-type))
  (pointee-type))

(defstruct (invalid-type (:include clang-type)))
(defstruct (unexposed-type (:include clang-type)))
(defstruct (void-type (:include primitive-c-type)))
(defstruct (bool-type (:include primitive-c-type)))
(defstruct (char-u-type (:include primitive-c-type)))
(defstruct (uchar-type (:include primitive-c-type)))
(defstruct (char16-type (:include primitive-c-type)))
(defstruct (char32-type (:include primitive-c-type)))
(defstruct (ushort-type (:include primitive-c-type)))
(defstruct (uint-type (:include primitive-c-type)))
(defstruct (ulong-type (:include primitive-c-type)))
(defstruct (ulonglong-type (:include primitive-c-type)))
(defstruct (uint128-type (:include primitive-c-type)))
(defstruct (char-s-type (:include primitive-c-type)))
(defstruct (schar-type (:include primitive-c-type)))
(defstruct (wchar-type (:include primitive-c-type)))
(defstruct (short-type (:include primitive-c-type)))
(defstruct (int-type (:include primitive-c-type)))
(defstruct (long-type (:include primitive-c-type)))
(defstruct (longlong-type (:include primitive-c-type)))
(defstruct (int128-type (:include primitive-c-type)))
(defstruct (float-type (:include primitive-c-type)))
(defstruct (double-type (:include primitive-c-type)))
(defstruct (long-double-type (:include primitive-c-type)))
(defstruct (nullptr-type (:include clang-type)))
(defstruct (overload-type (:include clang-type)))
(defstruct (dependent-type (:include clang-type)))
(defstruct (objc-id-type (:include clang-type)))
(defstruct (objc-class-type (:include clang-type)))
(defstruct (objc-sel-type (:include clang-type)))
(defstruct (float128-type (:include clang-type)))
(defstruct (half-type (:include clang-type)))
(defstruct (float16-type (:include clang-type)))
(defstruct (short-accum-type (:include clang-type)))
(defstruct (accum-type (:include clang-type)))
(defstruct (long-accum-type (:include clang-type)))
(defstruct (ushort-accum-type (:include clang-type)))
(defstruct (uaccum-type (:include clang-type)))
(defstruct (ulong-accum-type (:include clang-type)))
(defstruct (bfloat16-type (:include clang-type)))
(defstruct (ibm128-type (:include clang-type)))
(defstruct (complex-type (:include array-vector-or-complex-type)))
(defstruct (pointer-type (:include clang-type))
  (pointee-type nil))			 
(defstruct (block-pointer-type (:include clang-type)))
(defstruct (lvalue-reference-type (:include reference-type)))
(defstruct (rvalue-reference-type (:include reference-type)))
(defstruct (record-type (:include clang-type)))
(defstruct (enum-type (:include clang-type)))
(defstruct (typedef-type (:include clang-type)))
(defstruct (objc-interface-type (:include clang-type)))
(defstruct (objc-object-pointer-type (:include clang-type)))
(defstruct (function-no-proto-type (:include clang-type)))
(defstruct (function-proto-type (:include clang-type)))
(defstruct (constant-array-type (:include array-type)))
(defstruct (vector-type (:include array-vector-or-complex-type)))
(defstruct (incomplete-array-type (:include array-type)))
(defstruct (variable-array-type (:include array-type)))
(defstruct (dependent-sized-array-type (:include array-type)))
(defstruct (member-pointer-type (:include clang-type)))
(defstruct (auto-type (:include clang-type)))
(defstruct (elaborated-type (:include clang-type))
  (named-type nil))


(defvar *struct-declaration* :error)
;; a c++ struct def
;; fields are field-declarations in reverse-order
;; base-types are symbols in the noffi-c package in reverse order
;; methods are method-declarations in reverse order
;; vtable-p is lisp boolean, telling whether this class (but not necessarily the base-types)
;;  had a virtual method, qualifying it for a vtable pointer
(defstruct struct-declaration
  (name nil)
  (namespace nil)
  (base-types nil)
  (fields nil)
  (methods nil)
  (vtable-p nil)
  (size nil))

;; a struct or class field
;; name is c/c++ name of the field in the noffi-c package
;; type is a noffi type
(defstruct field-declaration
  (name nil)
  (type nil)
  (bit-offset nil)
  (access nil))

;; a c++ class declaration
(defstruct (class-declaration (:include struct-declaration)))

;; an argument to a function or a class method, types are noffi types
(defstruct function-argument
  (name nil)
  (type nil))

;; a function declaration, return type is noffi type,
;; name is a noffi-c package symbol of c/c++ name string
;; arguments are function-arguments in reverse order
(defstruct function-declaration
  (name nil)
  (return-type nil)
  (mangled-name nil)
  (arguments nil)
  (num-args nil)
  (variadic? nil)
  (storage-kind nil))

;; a c++ class method declaration
(defstruct (method-declaration (:include function-declaration))
  (class-name nil)
  (virtual? nil)
  (pure-virtual? nil)
  (static? nil)
  (constructor? nil)
  (destructor? nil)
  (access nil))


;; a c++ class template definition
(defstruct (class-template-definition (:include class-declaration))
  (arguments nil))



(defstruct struct-decl
  (fwd-ref? nil)
  (package? nil)
  (fields? nil)
  (methods? nil)
  (vtable-p nil))



(defstruct template-definition
  (name nil)
  (arguments nil)
  (fields nil)
  (methods nil))

(defclass module () ())
(defclass opencascade (module) ())

(defclass abi () ())
(defclass msvc (abi) ())
(defclass itanium (abi) ())

(defclass ffi () ())
(defclass cffi (ffi) ())
(defclass noffi (ffi) ())

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
	 (unit (#_clang_parseTranslationUnit index 0 (c-coerce args '#_<char**>) (length args) 0 0
					     (logior #_CXTranslationUnit_DetailedPreprocessingRecord
						     #_CXTranslationUnit_SkipFunctionBodies))))
    (if (null unit)
	(error "Unable to parse translation unit.")
	(let ((root (#_clang_getTranslationUnitCursor unit))
	      (filename (first args)))
	  (unwind-protect (with-open-file (*lisp-file* (oc.lisp-filename *ffi*) :direction :output :if-exists :append :if-does-not-exist :create)
			    (process-translation-unit
			     *module* *abi* *ffi* *pass* root nil (noffi::cintern filename) nil))
	    (setq *public?* :unset)
	    (setq *base-type* nil)
	    (setq *overloads* :unset)
	    (setq *handle-type?* nil)
	    (#_clang_disposeTranslationUnit unit)
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
		 ))))
  (setq *template-types* nil))
	  
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
	


(defun ensure-type-definition (name type)
  (let ((existing (gethash name *type-declarations*)))
    (if existing
	(if (not (typep existing 'type-definition))
	    (error "expected a type-definition for ~A, got a ~A" name (type-of existing))
	    ;;(if (not (equalp type (type-definition-type existing)))
	;;	(error "expected type ~S for type-definition ~A, found type ~S"
	;;	       type name (type-definition-type existing))
	    ;;existing)

	    ;; just accept the new type as if it were gospel.
	    (setf (type-definition-type existing) type))
	(setf (gethash name *type-declarations*) (make-type-definition :name name
								       :namespace *namespace-prefix*
								       :type type)))))

(defun ensure-class-declaration (name)
  (let ((existing (gethash name *type-declarations*)))
    (if existing
	(if (not (typep existing 'class-declaration))
	    (error "expected a class-declaration for ~A, got a ~A" name (type-of existing))
	    existing)
	(progn	  
	  (setf (gethash name *type-declarations*) (make-class-declaration
						    :name name :namespace *namespace-prefix*))))))

(defmethod process-cxx-access-specifier ((module module) abi ffi pass cursor parent name data)
  (setq *access-specifier* (ecase (#_clang_getCXXAccessSpecifier cursor)
			     (#.#_CX_CXXInvalidAccessSpecifier :invalid)
			     (#.#_CX_CXXPublic :public)
			     (#.#_CX_CXXProtected :protected)
			     (#.#_CX_CXXPrivate :private))))
			     

(defmethod process-cxx-base-specifier ((module module) abi ffi pass cursor parent name data)
  (push name (struct-declaration-base-types *struct-declaration*)))

(defmethod process-class-decl :around ((module module) abi ffi pass cursor parent name data)
  (let ((*struct-declaration* (ensure-class-declaration name)))
    (prog1 (call-next-method)

      (setf (struct-declaration-base-types *struct-declaration*)
	    (reverse (struct-declaration-base-types *struct-declaration*)))
      (setf (struct-declaration-methods *struct-declaration*)
	    (reverse (struct-declaration-methods *struct-declaration*)))
      (setf (struct-declaration-fields *struct-declaration*)
	    (reverse (struct-declaration-fields *struct-declaration*))))))

(defmethod process-class-decl :after ((module module) abi ffi pass cursor parent name data)
  ;; unless full class decl has already been processed:
  (unless (or (struct-declaration-base-types *struct-declaration*)
	      (struct-declaration-methods *struct-declaration*)
	      (struct-declaration-fields *struct-declaration*))
    (visit-children cursor))
  
  (let* ((type (#_clang_getCursorType cursor))
	 (size (#_clang_Type_getSizeOf type)))
    (unless (minusp size)
      (setf (struct-declaration-size *struct-declaration*) size))))
  

(defmethod process-field-decl ((module module) abi ffi pass cursor parent name data)
  (let ((decl *struct-declaration*))
    (let ((type (make-a-clang-type (#_clang_getCursorType cursor))))
      (push (make-field-declaration :name name
				    :type type
				    :bit-offset (let ((o (#_clang_Cursor_getOffsetOfField cursor)))
						  (unless (minusp o)
						    o))
				    :access *access-specifier*)
	    (struct-declaration-fields decl))
      (maybe-intern-type type)
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
      (let ((decl *struct-declaration*))
	(setf (struct-declaration-vtable-p decl) t))))
  (values))

(defmethod process-class-template :around ((module module) abi ffi pass cursor parent name data)
  (let ((*struct-declaration* (make-class-template-definition
			       :name name
			       :namespace *namespace-prefix*)))
    (prog1 (call-next-method)

      (setf (struct-declaration-base-types *struct-declaration*)
	    (reverse (struct-declaration-base-types *struct-declaration*)))
      (setf (struct-declaration-methods *struct-declaration*)
	    (reverse (struct-declaration-methods *struct-declaration*)))
      (setf (struct-declaration-fields *struct-declaration*)
	    (reverse (struct-declaration-fields *struct-declaration*)))
      (setf (class-template-definition-arguments *struct-declaration*)
	    (reverse (class-template-definition-arguments *struct-declaration*)))

      (let ((existing (gethash name *template-definitions*)))
	(unless existing
	  (setf (gethash name *template-definitions*) nil))
	(pushnew (list (class-template-definition-arguments *struct-declaration*)
		       *struct-declaration*)
		 (gethash name *template-definitions*)
		 :test #'equalp)))))

(defmethod process-class-template :after ((module module) abi ffi pass cursor parent name data)
  (unless (or (class-template-definition-base-types *struct-declaration*)
	      (class-template-definition-methods *struct-declaration*)
	      (class-template-definition-fields *struct-declaration*)
	      (class-template-definition-arguments *struct-declaration*))
    (visit-children cursor)))

(defvar *type-ref* :error)
(defvar *template-ref* :error)
(defvar *decl-ref-expr* :error)
(defvar *binary-operator* :error)
(defvar *expression* :error)
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
    (cond ((typep *struct-declaration* 'class-template-definition)
	   (push (list* name :typename
			(when *default* (list (list* :default (reverse *default*)))))
		 (class-template-definition-arguments *struct-declaration*))))))

(defmethod process-non-type-template-parameter :around
    ((module module) abi ffi pass cursor parent name data)
  (let ((*default* ()))
    (call-next-method)))

(defmethod process-non-type-template-parameter ((module module) abi ffi pass cursor parent name data)
  (visit-children cursor)
  (cond ((typep *struct-declaration* 'class-template-definition)
	 (let ((type (#_clang_getCursorType cursor)))
	   (push (list* name (make-a-clang-type type)
			(when *default* (list (list* :default (reverse *default*)))))
		 (class-template-definition-arguments *struct-declaration*))))))

(defmethod process-template-template-parameter :around
    ((module module) abi ffi pass cursor parent name data)
  (let ((*default* ()))
    (call-next-method)))

(defmethod process-template-template-parameter ((module module) abi ffi pass cursor parent name data)
  (visit-children cursor)
  (cond ((typep *struct-declaration* 'class-template-definition)
	 (push (list* name :template-typename
		      (when *default* (list (list* :default (reverse *default*)))))
	       (class-template-definition-arguments *struct-declaration*)))))

(defmethod process-decl-ref-expr ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (push (list name :decl-ref) *default*)
	 #+NIL
	 (push (make-a-clang-type (#_clang_getCursorType cursor)) *default*)
	 )))

(defmethod process-type-ref ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (push (list name :type-ref) *default*)
	 #+NIL
	 (push (make-a-clang-type (#_clang_getCursorType cursor)) *default*)
	 )))

(defmethod process-template-ref ((module module) abi ffi pass cursor parent name data)
  (cond ((or (= (#_.kind parent) #_CXCursor_TemplateTypeParameter)
	     (= (#_.kind parent) #_CXCursor_NonTypeTemplateParameter)
	     (= (#_.kind parent) #_CXCursor_TemplateTemplateParameter))
	 (push (list name :template-ref) *default*)
	 #+NIL
	 (push (make-a-clang-type (#_clang_getCursorType cursor)) *default*)
	 )))

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
	 (num-args (#_clang_getNumArgTypes type)))
    (flet ((pure-virtual? ()
	     (and virtual?
		  (not (= 0 (#_clang_CXXMethod_isPureVirtual cursor))))))
      (push (make-method-declaration :name name
				     :return-type (process-function-return module ffi pass type)
				     :mangled-name (when mangled-name
						     (let ((cstr (#_clang_getCString mangled-name)))
						       (when cstr
							 (unwind-protect
							      (get-c-string cstr)
							   #+NIL
							   (#_clang_disposeString cstr)))))
				     :num-args num-args
				     :arguments (process-function-params module ffi pass cursor type)
				     :variadic? variadic?
				     :storage-kind storage-kind
				     :class-name cxx-class-name
				     :virtual? virtual?
				     :pure-virtual? (pure-virtual?)
				     :static? static?
				     :constructor? constructor?
				     :destructor? destructor?
				     :access *access-specifier*)
	    (struct-declaration-methods *struct-declaration*)))))
  
  
  


(defmethod compute-struct-declaration ((ffi noffi) name cursor)
  (let ((class-stack ())
	(named-type ())
	(*field-stack* ()))
    (push 'noffi::decl class-stack)
    (push '((:storage-class :typedef)) class-stack)
    (push name named-type)
    (visit-children cursor)
    (when *field-stack*
      (setf (struct-decl-fields? *struct-decl*) t)
      (let ((struct (list (if (struct-decl-vtable-p *struct-decl*)
			      (cons (noffi::make-declaration :name (noffi::cintern "__vfptr")
							     :type (noffi::make-pointer-type
								    (noffi::make-pointer-type :void)))
				    (nreverse *field-stack*))
			      (nreverse *field-stack*)))))
	(push name struct)
	(push :struct struct)
	(push struct (cdr named-type)))
      (push named-type class-stack)
      (nreverse class-stack))))

(defmethod compute-struct-declaration-forward-reference ((ffi noffi) name)
  (noffi::make-declaration
   :storage-class :typedef
   :name name :type (noffi::make-struct-type name)))
  




(defmethod process-typedef-decl ((module module) abi ffi pass cursor parent name data)
  (let ((type (#_clang_getTypedefDeclUnderlyingType cursor)))
    (cond ((or (= (#_.kind parent) #_CXCursor_TranslationUnit)
	       (= (#_.kind parent) #_CXCursor_Namespace))
	   (let ((type (make-a-clang-type type)))
	     (prog1 (ensure-type-definition name type)
	       (maybe-intern-type type)))))))
    


(defmethod write-type-definition-as-lisp ((module module) (ffi cffi) (pass (eql 0)) cursor type name)
  (format *lisp-file* "(cffi:defctype ~A " name)
  (resolve-type-reference module ffi pass type)
  (format *lisp-file* ")~%~%")
  (values))

(defmethod write-type-definition-as-lisp ((module module) (ffi noffi) (pass (eql 0)) cursor type name)
  (format *lisp-file* "~S~%~%"
	  (noffi::make-declaration :name (noffi::cintern name)
				   :type (resolve-type-reference module ffi pass type)
				   :storage-class :typedef)))




      



(defmethod sanify-method-name (name)
  (setq name (substitute #\_ #\Space name))
  (setq name
	(cond ((search "operator||" name) "operator_logical_OR")
	      ((search "operator|=" name) "operator_bitwise_OR_assign")
	      ((search "operator|" name) "operator_bitwise_OR")
	      ((search "operator()" name) "operator_CALL")
	      (t name)))
  name)



(defun instantiate-template (abi typename)
  (let ((string (symbol-name typename)))
    (when (template-typename-p string)
      (let* ((name (noffi::cintern (template-name string)))
	     (arguments (mapcar #'noffi::cintern (template-arguments string)))
	     (template (gethash (list name (length arguments)) *template-definitions*)))
	(if (null template)
	    (warn "could not find template definition for ~a" typename)
	    (instantiate-template-1 template abi typename arguments))))))

(defun tree-substitute (new old tree)
  (if (null tree)
      nil
      (let ((car (car tree)))
	(if (consp car)
	    (cons (tree-substitute new old car)
		  (tree-substitute new old (cdr tree)))
	    (if (eq old car)
		(cons new (tree-substitute new old (cdr tree)))
		(cons car (tree-substitute new old (cdr tree))))))))

(defun exchange (new old thing)
  (if (consp thing)
      (tree-substitute new old thing)
      (if (eq old thing)
	  new
	  thing)))

(defmethod compute-template-method-declaration-mangled-name
    ((abi msvc) base-mangled-name return-type arguments)
  base-mangled-name)

(defmethod make-substitutions ((field field-declaration) replacements originals &key class-name)
  (declare (ignore class-name))
  (loop for new in replacements
	for old in originals
	do (setf (field-declaration-type field)
		 (exchange new old (field-declaration-type field))))
  (values))

(defmethod make-substitutions ((argument function-argument) replacements originals &key class-name)
  (declare (ignore class-name))
  (loop for new in replacements
	for old in originals
	do (setf (function-argument-type argument)
		 (exchange new old (function-argument-type argument))))
  (values))

(defmethod make-substitutions ((method method-declaration) replacements originals &key class-name)
  (loop for argument in (method-declaration-arguments method)
	do (make-substitutions argument replacements originals))
  (loop for new in replacements
	for old in originals
	do (setf (method-declaration-return-type method)
		 (exchange new old (method-declaration-return-type method))))
  (setf (method-declaration-class-name method) class-name)
  (values))

(defun deep-copy-method-declaration (method-declaration)
  (let ((new (copy-method-declaration method-declaration)))
    (when (listp (method-declaration-return-type new))
      (setf (method-declaration-return-type new)
	    (copy-seq (method-declaration-return-type new))))
    (setf (method-declaration-arguments new)
	  (mapcar #'copy-function-argument
		  (method-declaration-arguments new)))
    new))
		  

(defmethod instantiate-template-1 ((template class-template-definition) abi
				   class-name argument-values)
  (let* ((names (cons (class-template-definition-name template) (class-template-definition-arguments template)))
	 (values (cons class-name argument-values))
	 (base-types (copy-seq (class-template-definition-base-types template))))
    (loop for name in names
	  for value in values
	  do (setq base-types (substitute value name base-types)))
    (let ((fields
	    (mapcar #'(lambda (field)
			(make-substitutions field values names)
			field)
		    (mapcar #'copy-field-declaration
			    (class-template-definition-fields template))))
	  (methods
	    (mapcar
	     #'(lambda (method)
		 (make-substitutions method values names :class-name class-name)
		 method)
	     (mapcar #'deep-copy-method-declaration
		     (class-template-definition-methods template)))))

      (setf (gethash class-name *type-declarations*)
	    (make-class-declaration :name class-name
				    :base-types base-types
				    :fields fields
				    :methods methods
				    :vtable-p (class-template-definition-vtable-p template))))))
			 
		     
		     
				    
      
	      


	  


	
    

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



#+NIL
(defmethod process-template-ref ((module module) abi ffi pass cursor parent name data)
  (let ((num-args (#_clang_Cursor_getNumTemplateArguments cursor)))
    (loop for i from 0 below num-args
	  do (let ((kind (#_clang_Cursor_getTemplateArgumentKind cursor i)))
	       (cond ((= kind #_CXTemplateArgumentKind_Null) (print "Null"))
		     ((= kind #_CXTemplateArgumentKind_Type) (print "Type"))
		     ((= kind #_CXTemplateArgumentKind_Declaration) (print "Declaration"))
		     ((= kind #_CXTemplateArgumentKind_NullPtr) (print "NullPtr"))
		     ((= kind #_CXTemplateArgumentKind_Integral) (print "Integral"))
		     ((= kind #_CXTemplateArgumentKind_Template) (print "Template"))
		     ((= kind #_CXTemplateArgumentKind_TemplateExpansion) (print "TemplateExpansion"))
		     ((= kind #_CXTemplateArgumentKind_Expression) (print "Expression"))
		     ((= kind #_CXTemplateArgumentKind_Pack) (print "Pack"))
		     ((= kind #_CXTemplateArgumentKind_Invalid) (print "Invalid")))
	       (let ((type (#_clang_Cursor_getTemplateArgumentType cursor i)))
		 (print (get-type-name type)))))))

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
  

(defmethod write-method-wrapper-as-lisp
    ((module module) (ffi cffi) (pass (eql 1))
     cursor type name mangled-name lisp-name cxx-class-name lisp-class-name
     storage-kind constructor? destructor? static? pure-virtual? override? num-args variadic?)
  (when (eq storage-kind :external)
    (unless variadic?
      (princ ".")
      (force-output)
      (format *lisp-file* "(cffi:defcfun (~S ~A) " mangled-name lisp-name)
      (process-function-return module ffi pass type)
      (let ((space? nil))
	(unless static?
	  (format *lisp-file* " (this (:pointer (:struct ~A)))" lisp-class-name)
	  (unless (= 0 num-args)
	    (setq space? t)))
	(when space?
	  (format *lisp-file* " "))
	(process-function-params module ffi pass cursor type)
	(format *lisp-file* ")~%~%")))))

(defmethod write-method-wrapper-as-lisp
    ((module module) (ffi noffi) (pass (eql 1))
     cursor type name mangled-name lisp-name cxx-class-name lisp-class-name
     storage-kind constructor? destructor? static? pure-virtual? override? num-args variadic?)
  (when (eq storage-kind :external)
    (unless variadic?
      (let ((rtype (#_clang_getResultType type)))
	(unless (or (string= name "GetAllocatorType")
		    (string= name "NewInstance")
		    (string= name "xsgetn")
		    )
	  (princ ".")
	  (force-output)
	  (format *lisp-file* "~S~%~%"
		  (noffi::make-declaration
		   :name (noffi::cintern mangled-name)
		   :storage-class :extern
		   :type (noffi::make-function-type
			  (process-function-return module ffi pass type)
			  (append (unless static?
				    (list (noffi::make-declaration
					   :name '#_this
					   :type (noffi::make-pointer-type
						  (noffi::make-named-type
						   (noffi::cintern cxx-class-name))))))
				  (process-function-params module ffi pass cursor type)))))
	  (format *lisp-file* "(cl:declaim (cl:inline ~A))~%" lisp-name)
	  (format *lisp-file* "(cl:defun ~A (" lisp-name)
	  (let ((space? nil))
	    (unless static?
	      (format *lisp-file* "this")
	      (unless (= 0 num-args)
		(setq space? t)))
	    (when space?
	      (format *lisp-file* " "))
	    (print-param-names cursor type)
	    (if (record-type-p rtype)
		(format *lisp-file* ")~%  (noffi::as-lisp (noffi::c-funcall '~S "
			(noffi::cintern mangled-name))
		(format *lisp-file* ")~%  (noffi::as-lisp (noffi::static-c-funcall ~S "
			(noffi::cintern mangled-name)))
	    (let ((space? nil))
	      (unless static?
		(format *lisp-file* "this")
		(unless (= 0 num-args)
		  (setq space? t)))
	      (when space?
		(format *lisp-file* " "))
	      (print-param-names cursor type)
	      (format *lisp-file* ")))~%~%"))))))))

(defmethod write-method-wrapper-as-lisp :after
    ((module opencascade) (ffi cffi) (pass (eql 1))
     cursor type name mangled-name lisp-name cxx-class-name lisp-class-name
     storage-kind
     constructor? destructor? static? pure-virtual? override? num-args variadic?)
  (when (eq storage-kind :external)
    (unless variadic?
      (when *handle-type?*
	(unless (or constructor? destructor?)
	  (unless static?
	    (format *lisp-file* "(cl:defun Handle_~A (" lisp-name)
	    (format *lisp-file* "Handle_~A" lisp-class-name)
	    (unless (= 0 num-args)
	      (format *lisp-file* " "))
	    (print-param-names cursor type)
	    (format *lisp-file* ")~%")
	    (format *lisp-file* "  (~A (cffi:mem-aref Handle_~A :pointer) " lisp-name lisp-class-name)
	    (print-param-names cursor type)
	    (format *lisp-file* "))~%~%")))))))

(defmethod write-method-wrapper-as-lisp :after
    ((module opencascade) (ffi noffi) (pass (eql 1))
     cursor type name mangled-name lisp-name cxx-class-name lisp-class-name
     storage-kind
     constructor? destructor? static? pure-virtual? override? num-args variadic?)
  (when (eq storage-kind :external)
    (unless variadic?
      (when *handle-type?*
	(unless constructor?
	  (unless static?
	    (let ((rtype (#_clang_getResultType type)))
	      (format *lisp-file* "(cl:defun Handle_~A (" lisp-name)
	      (format *lisp-file* "Handle_~A" lisp-class-name)
	      (unless (= 0 num-args)
		(format *lisp-file* " "))
	      (print-param-names cursor type)
	      (format *lisp-file* ")~%")
	      (if (record-type-p rtype)
		  (format *lisp-file* "  (noffi::as-lisp (noffi::c-funcall '~S (#_.entity Handle_~A) "
			  (noffi::cintern mangled-name) lisp-class-name)
		  (format *lisp-file* "  (noffi::as-lisp (noffi::static-c-funcall ~S (#_.entity Handle_~A) "
			  (noffi::cintern mangled-name) lisp-class-name))
	      (print-param-names cursor type)
	      (format *lisp-file* ")))~%~%"))))))))

#+NIL
(defun record-type-p (type)
  (or (= #_CXType_Record (#_.kind type))
      (let ((nt (#_clang_Type_getNamedType type)))
	(when nt
	  (= #_CXType_Record (#_.kind nt))))))

(defmethod process-function-return ((module module) ffi pass type)
  (let ((return-type (#_clang_getResultType type)))
    (let ((type (make-a-clang-type return-type)))
      (prog1 type
	(maybe-intern-type type)))))

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
    


(defmethod process-translation-unit ((module module) abi ffi pass cursor parent name data)
  (let ((*header-in-question* (symbol-name name))
	(*overloads* (make-hash-table :test #'equalp)))
    (visit-children cursor)))

(defmethod process-namespace :after ((module module) abi ffi pass cursor parent name data)
  (let ((*namespace-prefix* (noffi::cintern
			     (concatenate 'string
					  (symbol-name *namespace-prefix*)
					  (symbol-name name) "::"))))
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

(defmethod write-c-primitive-type-as-lisp ((module module) ffi pass kind)
  (format *lisp-file* "~S" (get-builtin-type ffi kind)))
  

#+NIL
(defmethod process-c-primitive-type ((module module) ffi pass type kind)
  (declare (ignorable type))
  (cond ((= kind #_CXType_Complex) (break))
	((= kind #_CXType_Vector) (break))
	((primitive-c-type-p kind)
	 (write-c-primitive-type-as-lisp module ffi pass kind))))

#+NIL
(defmethod process-c-primitive-type ((module module) (ffi noffi) pass type kind)
  (declare (ignorable type))
  (cond ((= kind #_CXType_Complex) (break))
	((= kind #_CXType_Vector) (break))
	((primitive-c-type-p kind) (get-builtin-type ffi kind))))


(defun builtin-type-p (kind)
  (<= #_CXType_FirstBuiltin kind #_CXType_LastBuiltin))

(defun primitive-type-p (kind)
  (or (= kind #_CXType_Vector)
      (= kind #_CXType_Complex)
      (builtin-type-p kind)))

#+NIL
(defun primitive-c-type-p (kind)
  (< 1 kind 24))

#+NIL
(defmethod write-record-reference-as-lisp ((module module) (ffi cffi) pass type name)
  (format *lisp-file* "(:struct ~A)" (parse-edit-type-name name)))

#+NIL
(defmethod write-record-reference-as-lisp ((module module) (ffi noffi) pass type name)
  (format *lisp-file* "~S" (noffi::make-named-type (noffi::cintern (parse-edit-type-name name)))))

#+NIL
(defmethod process-record-reference ((module module) ffi pass type)
  (let ((name (#_clang_getTypeSpelling type)))
    (if name
	(progn (setq name (get-c-string (#_clang_getCString name)))
	       (write-record-reference-as-lisp module ffi pass type name))
	(format *lisp-file* "(error)"))))
#+NIL
(defmethod process-record-reference ((module module) (ffi noffi) pass type)
  (let ((name (#_clang_getTypeSpelling type)))
    (if name
	(let ((cstring (#_clang_getCString name)))
	  (setq name (get-c-string cstring))
	  (#_clang_disposeString cstring)
	  (let ((type-name (noffi::cintern (parse-edit-type-name name))))
	    (pushnew type-name *type-ordering*)
	    (noffi::make-named-type type-name)))
	'(error))))
#+NIL
(defun process-enum-reference (type)
  (let ((name (#_clang_getTypeSpelling type)))
    (if name
	(let ((cstring (#_clang_getCString name)))
	  (setq name (get-c-string cstring))
	  (#_clang_disposeString cstring)
	  (let ((type-name (noffi::cintern (parse-edit-type-name name))))
	    (pushnew type-name *type-ordering*)
	    (noffi::make-named-type type-name)))
	'(error))))
  
#+NIL
(defmethod process-typedef-reference ((module module) ffi pass type)
  (let ((typedef-def (#_clang_getTypeDeclaration type)))
    (format *lisp-file* (process-ident module ffi pass typedef-def))))

#+NIL
(defmethod process-typedef-reference ((module module) (ffi noffi) pass type)
  (let ((typedef-def (#_clang_getTypeDeclaration type)))
    (let ((type-name (noffi::cintern (process-ident module ffi pass typedef-def))))
      (pushnew type-name *type-ordering*)
      (noffi::make-named-type type-name))))

#+NIL
(defmethod process-ident ((module module) ffi pass cursor)
  (let ((name (#_clang_getCursorSpelling cursor)))
    (if name
	(progn
	  (let ((string (get-c-string (#_clang_getCString name))))
	    (#_clang_disposeString name)
	    string))
	(let ((location (#_clang_getCursorLocation cursor)))
	  (clet ((file #_<CXFile>)
		 (line #_<unsigned int>))
	    (#_clang_getSpellingLocation location (c-addr-of file)
					 (c-addr-of line) nil nil)
	    (let* ((filename (#_clang_File_tryGetRealPathName file))
		   (path (get-c-string (#_clang_getCString filename))))
	      (#_clang_disposeString filename)
	      (when (string= path "")
		(setq filename (#_clang_getFileName file))
		(setq path (get-c-string (#_clang_getCString filename)))
		(#_clang_disposeString filename))
	      (pathname-name path)))))))

	      
	      
		    
					 
	      
    
  
#+NIL
(defun process-array (type)
  (declare (ignorable type)))
#+NIL
(defun process-incomplete-array (type)
  (declare (ignorable type)))
#+NIL
(defun process-function-proto (type)
  (declare (ignorable type)))
#+NIL
(defun process-objc-object-pointer (type)
  (declare (ignorable type)))
#+NIL
(defun process-objc-id (type)
  (declare (ignorable type)))
#+NIL
(defun process-objc-sel (type)
  (declare (ignorable type)))
#+NIL
(defun process-objc-type-param (type)
  (declare (ignorable type)))
#+NIL
(defun process-objc-class (type)
  (declare (ignorable type)))
#+NIL
(defun process-block-pointer (type)
  (declare (ignorable type)))
#+NIL
(defun process-ext-vector (type)
  (declare (ignorable type)))
#+NIL
(defun process-function-no-proto (type)
  (declare (ignorable type)))
#+NIL
(defun process-variable-array (type)
  (declare (ignorable type)))
#+NIL
(defmethod get-pointee-type :around ((module opencascade) ffi pass type)
  (let ((pointee-type (#_clang_getPointeeType type)))
    (when pointee-type
      (let* ((name (#_clang_getTypeSpelling pointee-type)))
	(when name
	  (setq name (get-c-string (#_clang_getCString name)))
	  ;;(unless (or (string= name "Standard_OStream")
	;;	      (string= name "Standard_SStream"))
	    (call-next-method))))));;)
#+NIL
(defmethod get-pointee-type ((module module) ffi pass type)
  (let ((pointee-type (#_clang_getPointeeType type)))
    (when pointee-type
      (when (= (#_.kind pointee-type) #_CXType_Unexposed)
	(let ((canonical-type (#_clang_getCanonicalType type)))
	  (when canonical-type
	    (if (/= (#_.kind canonical-type) #_CXType_Invalid)
		(setq pointee-type canonical-type)
		(let ((name (#_clang_getTypeSpelling type)))
		  (warn "libclang unexposed type: ~S" (get-c-string (#_clang_getCString name)))
		  (#_clang_disposeString name)
		  (setf (#_.kind pointee-type) #_CXType_Void)))))
	pointee-type))))

(defun parse-edit-type-name (typename)
  (let ((present (search "const " typename)))
    (when (eq 0 present)
      (setq typename (subseq typename 6)))
    (setq typename (string-trim '(#\&) typename))
    (setq typename (typename-sanity typename))
    (string-trim '(#\Space) typename)))

(defun template-typename-p (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (let ((pos1)
	(pos2))
    (and (setq pos1 (position #\< typename))
	 (setq pos2 (position #\> typename :from-end t))
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
		  (string-trim '(#\space) arg))
	      (uiop/utility:split-string raw-arguments :separator '(#\,))))))

(defun template-name (typename)
  (when (symbolp typename)
    (setq typename (symbol-name typename)))
  (when (template-typename-p typename)
    (subseq typename 0 (position #\< typename))))

(defun typename-sanity (typename)
  (setq typename (string-trim '(#\Space) typename))
  (when (template-typename-p typename)
    (unless (equalp typename *current-template-type*)
      (pushnew typename *template-types* :test #'string=)))
  typename)

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

#+NIL
(defmethod write-pointer-type-as-lisp ((module module) (ffi cffi) pass pointee-type)
  (format *lisp-file* "(:pointer ")
  (resolve-type-reference module ffi pass pointee-type)
  (format *lisp-file* ")"))
#+NIL
(defmethod write-pointer-type-as-lisp ((module module) (ffi cffi) pass (pointee-type (eql nil)))
  (format *lisp-file* "~S" (noffi::make-pointer-type :void)))

(defun equal-types (a b)
  (or (not (= 0 (#_clang_equalTypes a b)))
      (eq (get-type-name a) (get-type-name b))))

#+NIL
(defmethod resolve-pointer-type-reference ((module module) (ffi cffi) pass type)
  (let* ((pointee-type (get-pointee-type module ffi pass type)))
    (if (equal-types pointee-type type)
	(format *lisp-file* "(:pointer ~S)" (get-type-name pointee-type))
	(write-pointer-type-as-lisp module ffi pass pointee-type))))
#+NIL
(defmethod resolve-pointer-type-reference ((module module) (ffi noffi) pass type)
  (let* ((pointee-type (get-pointee-type module ffi pass type)))
    (if (null pointee-type)
	(noffi::make-pointer-type :void)
	(if (equal-types pointee-type type)
	    (let ((type-name (get-type-name pointee-type)))
	      (pushnew type-name *type-ordering*)
	      (noffi::make-pointer-type type-name))
	    (noffi::make-pointer-type (resolve-type-reference module ffi pass pointee-type))))))

#+NIL
(defmethod write-reference-type-as-lisp :around ((module module) (ffi cffi) pass (name (eql nil)))
  (format *lisp-file* ":pointer"))
#+NIL
(defmethod write-reference-type-as-lisp :around ((module module) (ffi cffi) pass (name (eql nil)))
  (format *lisp-file* "~S" (noffi::make-pointer-type :void)))
#+NIL
(defmethod write-reference-type-as-lisp :around ((module opencascade) ffi pass name)
  ;;(if (or (string= name "Standard_OStream")
;;	  (string= name "Standard_SStream"))
  ;;    (write-reference-type-as-lisp module ffi pass nil)
      (call-next-method));;)  
#+NIL
(defmethod write-reference-type-as-lisp ((module module) (ffi cffi) pass name)
  (setq name (parse-edit-type-name name))
  (if (gethash name *structs*)
      (format *lisp-file* "(:struct ~A)" name)
      (format *lisp-file* "~A" name)))

#+NIL
(defmethod resolve-reference-type-reference ((module module) (ffi cffi) pass type)
  (let* ((name (#_clang_getTypeSpelling type))
	 (cstring (#_clang_getCString name)))
    (setq cstring (when cstring (get-c-string cstring)))
    (setq cstring (parse-edit-type-name cstring))
    (write-reference-type-as-lisp module ffi pass cstring)))

#+NIL
(defmethod resolve-reference-type-reference ((module module) (ffi noffi) pass type)
  (let* ((name (#_clang_getTypeSpelling type))
	 (cstring (#_clang_getCString name)))
    (setq cstring (when cstring (get-c-string cstring)))
    (setq cstring (parse-edit-type-name cstring))
    (let ((type-name (noffi::cintern cstring)))
      (pushnew type-name *type-ordering*)
      (noffi::make-pointer-type (noffi::make-named-type type-name)))))

(defvar *depth* 0)
(defvar *stacky* nil)

(defun get-type-name (type)
  (let ((name (#_clang_getTypeSpelling type)))
    (when name
      (let ((cstring (#_clang_getCString name)))
	(when cstring
	  (unwind-protect (noffi::cintern (get-c-string cstring))
	    #+NIL
	    (#_clang_disposeString cstring)))))))
  
#+NIL
(defmethod resolve-type-reference ((module module) ffi pass type)
  (if (> *depth* 5)
      (break "~S" *stacky*)
      (let ((*depth* (1+ *depth*)))
	(let* ((kind (#_.kind type))
	       (type-kind-name (#_clang_getTypeKindSpelling kind)))
	  (unwind-protect
	       (if (primitive-c-type-p kind)
		   (process-c-primitive-type module ffi pass type kind)
		   (progn
		     ;;(print kind)
		     (cond ((= kind #_CXType_Pointer)
			    (let ((*stacky* (cons :pointer *stacky*)))
			      (resolve-pointer-type-reference module ffi pass type)))
			   ((= kind #_CXType_LValueReference)
			    (let ((*stacky* (cons :lvalueref *stacky*)))
			      (resolve-reference-type-reference module ffi pass type)))
			   ((= kind #_CXType_RValueReference)
			    (let ((*stacky* (cons :rvalueref *stacky*)))
			      (resolve-reference-type-reference module ffi pass type)))
			   ((= kind #_CXType_Elaborated)
			    (let ((*stacky* (cons :elaborated *stacky*)))
			      (let ((named-type (#_clang_Type_getNamedType type)))
				(if (equal-types named-type type)
				    (let ((type-name (get-type-name named-type)))
				      (pushnew type-name *type-ordering*)
				      type-name)
				    (resolve-type-reference
				     module ffi pass named-type)))))
			   ((= kind #_CXType_Record)
			    (let ((*stacky* (cons :record *stacky*)))
			      (process-record-reference module ffi pass type)))
			   ((= kind #_CXType_Enum)
			    (process-enum-reference type))
			   ((= kind #_CXType_Typedef)
			    (let ((*stacky* (cons :typedef *stacky*)))
			      (process-typedef-reference module ffi pass type)))
			   ((= kind #_CXType_ConstantArray)
			    (process-array type))
			   ((= kind #_CXType_IncompleteArray)
			    (process-incomplete-array type))
			   ((= kind #_CXType_FunctionProto)
			    (process-function-proto type))
			   ((= kind #_CXType_ObjCObjectPointer)
			    (process-objc-object-pointer type))
			   ((= kind #_CXType_ObjCId)
			    (process-objc-id type))
			   ((= kind #_CXType_ObjCSel)
			    (process-objc-sel type))
			   ((= kind #_CXType_ObjCTypeParam)
			    (process-objc-type-param type))
			   ((= kind #_CXType_ObjCClass)
			    (process-objc-class type))
			   ((= kind #_CXType_BlockPointer)
			    (process-block-pointer type))
			   ((= kind #_CXType_ExtVector)
			    (process-ext-vector type))
			   ((= kind #_CXType_FunctionNoProto)
			    (process-function-no-proto type))
			   ((= kind #_CXType_VariableArray)
			    (process-variable-array type))
			   ((= kind #_CXType_Unexposed)
			    (let ((thetype type))
			      (let ((canonical-type (#_clang_getCanonicalType type)))
				(when canonical-type
				  (if (/= (#_.kind canonical-type) #_CXType_Invalid)
				      (setq thetype canonical-type)
				      (let ((name (#_clang_getTypeSpelling type)))
					(warn "libclang unexposed type: ~S" (get-c-string (#_clang_getCString name)))
					(#_clang_disposeString name)
					(setf (#_.kind type) #_CXType_Void)))))
			      (let ((*stacky* (cons :unexposed *stacky*)))
				(if (equal-types type thetype)
				    (noffi::cintern "unexposed")
				    (resolve-type-reference module ffi pass thetype)))))
			   (t (warn "Error: reference type ~S is not implemented." (get-c-string (#_clang_getCString type-kind-name)))))))
	    (#_clang_disposeString type-kind-name))))))
	
    

	
    

#+NIL
(defmethod process-arg-type ((module module) ffi pass type)
  (let ((kind (#_.kind type)))
    (cond ((or (= kind #_CXType_ConstantArray)
	       (= kind #_CXType_VariableArray)
	       (= kind #_CXType_IncompleteArray))
	   (let ((element-type (#_clang_getArrayElementType type)))
	     (format *lisp-file* "(:pointer ")
	     (resolve-type-reference module ffi pass element-type)
	     (format *lisp-file* ")")))
	  (t (resolve-type-reference module ffi pass type)))))

(defun uninstantiated-template-p (typename)
  (let ((entry (gethash typename *types*)))
    (and (unexposed-type-p entry)
	 (template-typename-p typename)
	 typename)))

(defun maybe-intern-type (type)
  (unless (class-template-definition-p *struct-declaration*)
    (let ((root-type (root-type type)))
      (unless (primitive-c-type-p root-type)
	(let ((type-name (clang-type-name root-type)))
	  (let ((existing (gethash type-name *types*)))
	    (when (and existing (elaborated-type-p existing) (or (typedef-type-p root-type)
								 (record-type-p root-type)))
	      ;; prefer record types and typedef-types over elaborated-types
	      (setf (gethash type-name *types*) root-type))
	    (when (and existing (unexposed-type-p existing) (or (typedef-type-p root-type)
								(record-type-p root-type)
								(elaborated-type-p root-type)))
	      (setf (gethash type-name *types*) root-type))
	    (unless existing
	      (setf (gethash type-name *types*) root-type)
	      (push type-name *type-ordering*))))))))

(defmethod process-arg-type ((module module) (ffi noffi) pass type)
  (let ((type (make-a-clang-type type)))
    (prog1 type
      (maybe-intern-type type)))
  #+NIL
  (let ((kind (#_.kind type)))
    (cond ((or (= kind #_CXType_ConstantArray)
	       (= kind #_CXType_VariableArray)
	       (= kind #_CXType_IncompleteArray))
	   (let ((element-type (#_clang_getArrayElementType type)))
	     (list :array
		   (resolve-type-reference module ffi pass element-type))))
	  (t (resolve-type-reference module ffi pass type)))))

(defmethod process-function-params ((module module) ffi pass cursor type)
  (let ((num-args (#_clang_getNumArgTypes type)))
    
    (loop for i from 0 below num-args
	  collect (let* ((arg-type (#_clang_getArgType type i))
			 (arg (#_clang_Cursor_getArgument cursor i))
			 (arg-name (get-cursor-name arg)))
		    (unless arg-name
		      (setq arg-name (noffi::cintern (format nil "ARG~A" i))))
		    (make-function-argument :name arg-name
					    :type (process-arg-type module ffi pass arg-type))))))

  

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
  "~/OCCT/src/")

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
	#+NIL
	(
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
	"CPnts")))

(defun opencascade-module-ordering ()
  (let ((ordering (copy-list *preferred-opencascade-module-ordering*))
	(random-modules (list-opencascade-modules)))
    (loop for module in random-modules
	  do (unless (position module ordering :test #'string-equal)
	       (setf ordering (nconc ordering (list module))))
	  finally (return ordering))))

(defun opencascade-preferred-header-file-ordering ()
  (let ((modules #+NOTYET(opencascade-module-ordering)
		 (copy-list *preferred-opencascade-module-ordering*))
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

(defmethod reinitialize-generator :around ((module module) ffi)
  (clrhash *structs*)
  (clrhash *typedefs*)
  (clrhash *done-templates*)
  (reinitialize-lisp-output-file module ffi)
  (call-next-method))

(defmethod reinitialize-generator ((module opencascade) ffi)

  (setf (gethash "Handle_Standard_Transient" *structs*) "Handle_Standard_Transient")
  (setf (gethash "Handle_Standard_Transient" *typedefs*) "Handle_Standard_Transient")
  (setf (gethash "Handle_Standard_Type" *structs*) "Handle_Standard_Type")
  (setf (gethash "Handle_Standard_Type" *typedefs*) "Handle_Standard_Type")
  (values))

(defmethod reinitialize-lisp-output-file :around ((module opencascade) ffi)
  (with-open-file (*lisp-file* (oc.lisp-filename ffi)
			       :direction :output :if-exists :supersede
			       :if-does-not-exist :create)
    (format *lisp-file* "(cl:defpackage :oc)~%~%")
    (format *lisp-file* "(cl:in-package :oc)~%~%")
    (call-next-method)))
    

(defmethod reinitialize-lisp-output-file ((module opencascade) (ffi cffi))
  (format *lisp-file* "(cffi:defctype size_t :long-long)~%~%")
  (format *lisp-file* "(cffi:defcstruct Handle_Standard_Transient (bytes (:array :char 8)))~%~%")
  (format *lisp-file* "(cffi:defcstruct Handle_Standard_Type (bytes (:array :char 8)))~%~%")
  (values))

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
  (setq *template-types* nil)
  (setq *module* (make-instance 'opencascade))
  (setq *abi* (make-instance 'msvc))
  (setq *ffi* (make-instance 'cffi))
  (initialize-builtin-type-array *ffi*)
  (setq *pass* 0)
  (reinitialize-generator *module* *ffi*)
  (iterate-headers *module* *ffi* *pass*))

(defun generate-opencascade-noffi ()
  (setq *template-types* nil)
  (setq *module* (make-instance 'opencascade))
  (setq *abi* (make-instance 'msvc))
  (setq *ffi* (make-instance 'noffi))
  (initialize-builtin-type-array *ffi*)
  (setq *pass* 0)
  (reinitialize-generator *module* *ffi*)
  (clrhash *type-declarations*)
  (clrhash *template-definitions*)
  (setq *type-ordering* nil)
  (clrhash *types*)
  (time (iterate-headers *module* *ffi* *pass*))
  #+NIL
  (loop for type in *type-ordering*
	do (when (template-typename-p (symbol-name type))
	     (instantiate-template *abi* type)))
  t)
  

(defmethod compute-headers-list ((module opencascade))
  (opencascade-preferred-header-file-ordering))

(defmethod compute-include-arguments ((module opencascade))
  (list "-I" "c:/Users/awolven/OCCT/include/"))

(defmethod describe-pass ((pass (eql 0)))
  (format t "Pass ~S: Emitting type declarations...." pass))

(defmethod describe-pass ((pass (eql 1)))
  (format t "Pass ~S: Defining foreign function calls...." pass))

(defmethod iterate-headers ((module opencascade) ffi pass)
  (describe-pass pass)
  (finish-output)
  (let ((includes (compute-include-arguments module)))
    (loop for file in (compute-headers-list module)
	  do (apply #'main (namestring file) includes))))

(defun test ()
  (setq *template-types* nil)
  (setq *module* (make-instance 'opencascade))
  (setq *abi* (make-instance 'msvc))
  (setq *ffi* (make-instance 'noffi))
  (initialize-builtin-type-array *ffi*)
  (setq *pass* 0)
  (clrhash *type-declarations*)
  (clrhash *template-definitions*)
  (setq *type-ordering* nil)
  (clrhash *types*)
  (reinitialize-generator *module* *ffi*)
  (apply #'main "c:/Users/awolven/cxxbinder/test.hxx" (compute-include-arguments *module*)))

(defmethod root-type ((type clang-type))
  type)

(defmethod root-type ((type pointer-type))
  (root-type (pointer-type-pointee-type type)))

(defmethod root-type ((type lvalue-reference-type))
  (root-type (lvalue-reference-type-pointee-type type)))

(defmethod root-type ((type rvalue-reference-type))
  (root-type (rvalue-reference-type-pointee-type type)))

(defmethod root-type ((type elaborated-type))
  (or (and (elaborated-type-named-type type) (root-type (elaborated-type-named-type type))) type))

(defmethod root-type ((type array-vector-or-complex-type))
  (root-type (array-vector-or-complex-type-element-type type)))

(defun make-a-clang-type (type)
  (let* ((name (get-type-name type))
	 (kind (#_.kind type))
	 (struct (case kind
		   (#.#_CXType_Invalid (make-invalid-type :name name))
		   (#.#_CXType_Unexposed (make-unexposed-type :name name))
		   (#.#_CXType_Void (make-void-type :name name))
		   (#.#_CXType_Bool (make-bool-type :name name))
		   (#.#_CXType_Char_U (make-char-u-type :name name))
		   (#.#_CXType_UChar (make-uchar-type :name name))
		   (#.#_CXType_Char16 (make-char16-type :name name))
		   (#.#_CXType_Char32 (make-char32-type :name name))
		   (#.#_CXType_UShort (make-ushort-type :name name))
		   (#.#_CXType_UInt (make-uint-type :name name))
		   (#.#_CXType_ULong (make-ulong-type :name name))
		   (#.#_CXType_ULongLong (make-ulonglong-type :name name))
		   (#.#_CXType_UInt128 (make-uint128-type :name name))
		   (#.#_CXType_Char_S (make-char-s-type :name name))
		   (#.#_CXType_SChar (make-schar-type :name name))
		   (#.#_CXType_WChar (make-wchar-type :name name))
		   (#.#_CXType_Short (make-short-type :name name))
		   (#.#_CXType_Int (make-int-type :name name))
		   (#.#_CXType_Long (make-long-type :name name))
		   (#.#_CXType_LongLong (make-longlong-type :name name))
		   (#.#_CXType_Int128 (make-int128-type :name name))
		   (#.#_CXType_Float (make-float-type :name name))
		   (#.#_CXType_Double (make-double-type :name name))
		   (#.#_CXType_LongDouble (make-long-double-type :name name))
		   (#.#_CXType_NullPtr (make-nullptr-type :name name))
		   (#.#_CXType_Overload (make-overload-type :name name))
		   (#.#_CXType_Dependent (make-dependent-type :name name))
		   (#.#_CXType_ObjCId (make-objc-id-type :name name))
		   (#.#_CXType_ObjCClass (make-objc-class-type :name name))
		   (#.#_CXType_ObjCSel (make-objc-sel-type :name name))
		   (#.#_CXType_Float128 (make-float128-type :name name))
		   (#.#_CXType_Half (make-half-type :name name))
		   (#.#_CXType_Float16 (make-float16-type :name name))
		   (#.#_CXType_ShortAccum (make-short-accum-type :name name))
		   (#.#_CXType_Accum (make-accum-type :name name))
		   (#.#_CXType_LongAccum (make-long-accum-type :name name))
		   (#.#_CXType_UShortAccum (make-ushort-accum-type :name name))
		   (#.#_CXType_UAccum (make-uaccum-type :name name))
		   (#.#_CXType_ULongAccum (make-ulong-accum-type :name name))
		   (#.#_CXType_BFloat16 (make-bfloat16-type :name name))
		   (#.#_CXType_Ibm128 (make-ibm128-type :name name))
		   (#.#_CXType_Complex
		    (make-complex-type :name name
				       :element-type
				       (let ((element-type (#_clang_getElementType type)))
					 (when element-type
					   (make-a-clang-type element-type)))))
		   (#.#_CXType_Pointer
		    (make-pointer-type :name name
				       :pointee-type
				       (let ((pointee-type (#_clang_getPointeeType type)))
					 (when pointee-type
					   (unless (equal-types type pointee-type)
					     (make-a-clang-type pointee-type))))))
		   (#.#_CXType_BlockPointer (make-block-pointer-type :name name))
		   (#.#_CXType_LValueReference
		    (make-lvalue-reference-type :name name
						:pointee-type
						(let ((pointee-type (#_clang_getPointeeType type)))
						  (when pointee-type
						    (make-a-clang-type pointee-type)))))
		   (#.#_CXType_RValueReference
		    (make-rvalue-reference-type :name name
						:pointee-type
						(let ((pointee-type (#_clang_getPointeeType type)))
						  (when pointee-type
						    (make-a-clang-type pointee-type)))))
		   (#.#_CXType_Record (make-record-type :name name))
		   (#.#_CXType_Enum (make-enum-type :name name))
		   (#.#_CXType_Typedef (make-typedef-type :name name))
		   (#.#_CXType_ObjCInterface (make-objc-interface-type :name name))
		   (#.#_CXType_ObjCObjectPointer
		    (make-objc-object-pointer-type :name name))
		   (#.#_CXType_FunctionNoProto
		    (make-function-no-proto-type :name name))
		   (#.#_CXType_FunctionProto (make-function-proto-type :name name))
		   (#.#_CXType_ConstantArray
		    (make-constant-array-type :name name
					      :element-type
					      (let ((element-type (#_clang_getElementType type)))
						(when element-type
						  (make-a-clang-type element-type)))))
		   (#.#_CXType_Vector
		    (make-vector-type :name name
				      :element-type
				      (let ((element-type (#_clang_getElementType type)))
					(when element-type
					  (make-a-clang-type element-type)))))
				      
		   (#.#_CXType_IncompleteArray
		    (make-incomplete-array-type :name name
						:element-type
						(let ((element-type (#_clang_getElementType type)))
						  (when element-type
						    (make-a-clang-type element-type)))))
		   (#.#_CXType_VariableArray
		    (make-variable-array-type :name name
					      :element-type
					      (let ((element-type (#_clang_getElementType type)))
						(when element-type
						  (make-a-clang-type element-type)))))
		   (#.#_CXType_DependentSizedArray
		    (make-dependent-sized-array-type
		     :name name
		     :element-type
		     (let ((element-type (#_clang_getElementType type)))
		       (when element-type
			 (make-a-clang-type element-type)))))
		   (#.#_CXType_MemberPointer (make-member-pointer-type :name name))
		   (#.#_CXType_Auto (make-auto-type :name name))
		   (#.#_CXType_Elaborated
		    (make-elaborated-type :name name
					  :named-type
					  (let ((named-type (#_clang_Type_getNamedType type)))
					    (when named-type
					      (unless (equal-types type named-type)
						(make-a-clang-type named-type))))
					  )))))
    struct))


