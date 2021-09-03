[
  "local"
  "return"
  "do"
  "end"
  "record"
  "enum"
  "union"
  "function"
  "if"
] @keyword

(SelfId) @variable.builtin
(BuiltinType) @type.builtin
(BuiltinGenericType) @type.builtin
(PointerType ("pointer" @type.builtin))
(ArrayType ("array" @type.builtin))
(VariantType ("variant" @type.builtin))

[(Nil) (True) (False)] @variable.builtin

(BinaryOperation ([
  "or"
  "and"
  "<"
  "<="
  ">"
  ">="
  "=="
  "~="
  "|"
  "~"
  "&"
  "<<"
  ">>"
  ">>>"
  "+"
  "-"
  "*"
  "/"
  "//"
  "///"
  "%"
  "%%%"
  ".."
  "^"
] @operator))

(Local ((Function) @function))
(Global ((Function) @function))
(FuncDef) @function

(FunctionArguments (IdDecls (IdDecl (RawIdDecl ((Name) @variable.parameter)))))
(FunctionArguments (IdDecls (IdDecl ((PreprocessExpr) @variable.parameter))))

(Annotation) @attribute

(Type ("@" @type))

["," ";"] @punctuation.delimiter

["{" "}" "(" ")" "[" "]"] @punctuation.bracket
(AnnotationList (["<" ">"] @punctuation.bracket))

(Preprocess) @preprocess

(UnaryOperation (["not" "#" "-" "~" "&" "$"] @operator)) 

(RecordField ((FieldName) @property))
(UnionField ((FieldName) @property))
(EnumField ((FieldName) @property))

(DotIndex ((Name) @property))
(ColonIndex ((Name) @property))

(Pair key: ((Name) @property))

(Label) @label

(CallFunction) @function

((PrimType) @type)
((Number) @number)
((Comment) @comment)
((String) @string)