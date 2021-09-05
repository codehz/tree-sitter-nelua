[
  "local"
  "global"
  "return"
  "do"
  "end"
  "record"
  "enum"
  "union"
  "function"
  "if"
  "then"
  "else"
  "elseif"
  "for"
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

(Function (NameDecl) @function)
(FuncDef (FunctionName) @function)
(CallFunction (PrimExpression) @function)
(ExpressionSuffixed (PrimExpression) @function [(Call) (CallMethod)])

(FunctionArguments (IdDecls (IdDecl [
  (RawIdDecl (Name) @variable.parameter)
  (PreprocessExpr) @variable.parameter
])))

(Annotation) @attribute

(Type "@" @type)

["," ";"] @punctuation.delimiter

["{" "}" "(" ")" "[" "]"] @punctuation.bracket
(AnnotationList ["<" ">"] @punctuation.bracket)

(Preprocess) @preprocess
(PreprocessName) @preprocess
(PreprocessExpr) @preprocess

(UnaryOperation ["not" "#" "-" "~" "&" "$"] @operator)

(RecordField (FieldName) @property)
(UnionField (FieldName) @property)
(EnumField (FieldName) @property)

(DotIndex (Name) @property)
(ColonIndex (Name) @property)

(Pair key: ((Name) @property))

(Label (Name) @label)
(Goto (Name) @label)

(PrimType) @type
(Number) @number
(Comment) @comment
(String) @string
