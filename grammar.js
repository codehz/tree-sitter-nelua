/// <reference types="tree-sitter-cli/dsl" />

const PREC = {
  TYPE: -5,
  VARIANT: -4,
  ASSIGN: -3,
  COMMA: -2,
  CALL: -1,
  PRIORITY: 1,

  OR: 1, //=> or
  AND: 2, //=> and
  COMPARE: 3, //=> < <= == ~= >= >
  BIT_OR: 4, //=> |
  BIT_NOT: 5, //=> ~
  BIT_AND: 6, //=> &
  SHIFT: 7, //=> << >>
  CONCAT: 8, //=> ..
  PLUS: 9, //=> + -
  MULTI: 10, //=> * / // %
  UNARY: 11, //=> not # - ~
  POWER: 12, //=> ^
};

module.exports = grammar({
  name: "nelua",

  extras: $ => [
    $.Comment,
    $.Preprocess,
    /[\s\n]/,
  ],

  inline: $ => [
  ],

  conflicts: $ => [
    [$.Variable, $.CallFunction],
    [$.Variable],
    [$.CallFunction],
    [$.TypeExpression],
    [$.PrimExpression, $._PrimTypeExpression],
    [$.PrimExpression, $.ExpressionSuffixed],
    [$.Expression, $.ExpressionSuffixed],
    [$.ExpressionSuffixed],
    [$.IdSuffixed, $.PrimExpression],
  ],

  externals: $ => [
    $.Comment,
    $.String,
    $.Preprocess,
  ],

  word: $ => $.RawName,

  rules: {
    SourceFile: $ =>
      seq(
        optional($.SHEBANG),
        optional($.Block),
      ),

    Block: $ =>
      repeat1(choice(
        ";",
        $.Local,
        $.Global,
        $.FuncDef,
        $.Return,
        $.Do,
        $.Defer,
        $.If,
        $.Switch,
        $.For,
        $.While,
        $.Repeat,
        $.Break,
        $.Continue,
        $.Goto,
        $.Label,
        $.Assign,
        $.CallFunction,
      )),

    PreprocessName: $ => /#\|.*?\|#/,
    PreprocessExpr: $ => /#\[.*?\]#/,

    RawName: $ => /[_a-zA-Z\u00A0-\uFFFF][_a-zA-Z0-9\u00A0-\uFFFF]*/,
    Name: $ => choice($.RawName, $.PreprocessName),

    Annotation: $ => $.Name,
    AnnotationList: $ => seq('<', repeatSeq($.Annotation), '>'),

    RawId: $ => $.Name,
    Id: $ => prec(1, choice($.Id, $.PreprocessExpr)),
    RawIdDecl: $ => seq($.Name, optional($._WithTypeExpression), optional($.AnnotationList)),
    TypedDecl: $ => seq($.Name, $._WithTypeExpression, optional($.AnnotationList)),

    NameDecl: $ => $.Name,

    IdDecl: $ => choice($.RawIdDecl, $.PreprocessExpr),
    IdDecls: $ => prec.right(repeatSeq($.IdDecl)),
    IdSuffixed: $ => seq($.Id, repeat1($.DotIndex)),
    FunctionName: $ => seq($.Id, repeat($.DotIndex), optional($.ColonIndex)),
    DotIndex: $ => seq(".", $.Name),
    ColonIndex: $ => seq(":", $.Name),
    KeyIndex: $ => seq("[", $._Expression, "]"),

    Call: $ => $._CallArguments,
    CallMethod: $ => seq(":", $.Name, $._CallArguments),
    _IndexSuffix: $ => choice($.DotIndex, $.KeyIndex),
    _CallSuffix: $ => choice($.Call, $.CallMethod),

    InitList: $ => seq("{", optional(repeatSeq($.Field, $._FieldSep, true)), "}"),
    Field: $ => choice($._Expression, $.Pair),
    Pair: $ => choice(
      seq("[", field("key", $._Expression), "]", "=", field("value", $._Expression)),
      seq(field("key", $.Name), "=", field("value", $._Expression)),
      seq("=", field("key", $.Id))
    ),

    _CallArguments: $ => choice(seq("(", optional($.ExpressionList), ")"), $.InitList, $.String),

    GlobalDecl: $ => seq(choice($.IdSuffixed, $.Name), optional($._WithTypeExpression), optional($.AnnotationList)),
    _GlobalDecl: $ => choice($.GlobalDecl, $.PreprocessExpr),
    GlobalDecls: $ => repeatSeq($.GlobalDecl),

    Variable: $ => choice(
      $.Deref,
      seq(
        $.PrimExpression,
        repeat(choice(
          $._IndexSuffix,
          seq(repeat1($._CallSuffix), $._IndexSuffix)
        ))
      )
    ),
    Variables: $ => repeatSeq($.Variable),
    CallFunction: $ => prec(PREC.CALL, seq(
      $.PrimExpression,
      repeat(choice(
        $._CallSuffix,
        seq(repeat1($._IndexSuffix), $._CallSuffix)
      ))
    )),

    Nil: $ => "nil",
    True: $ => "true",
    False: $ => "false",

    Number: $ => prec.right(seq($.NumberValue, optional($.NumberSuffix))),
    NumberSuffix: $ => token.immediate(/_[a-zA-Z0-9]+/),
    NumberValue: $ => choice($._number_hex, $._number_dec, $._number_bin),
    _number_hex: $ => seq(/0[xX]/, /[0-9a-fA-F]+(?:\.[0-9a-fA-F]*)?|\.[0-9a-fA-F]+/, optSeq(/[pP]/, $._number_exp)),
    _number_bin: $ => seq(/x[bB]/, /[01]+(?:\.[01]*)?|\.[01]+/, optSeq(/[pP]/, $._number_exp)),
    _number_dec: $ => seq(/[0-9]+(?:\.[0-9]*)?|\.[0-9]+/, optSeq(/[eE]/, $._number_exp)),
    _number_exp: $ => /[+-]?[0-9]+/,

    Deref: $ => prec.left(seq("$", $._Expression)),
    Paren: $ => prec(100, seq("(", $._Expression, ")")),
    PrimExpression: $ => choice($.SelfId, $.Id, $.Paren),
    SelfId: $ => "self",
    Expression: $ => choice(
      $.Nil,
      $.InitList,
      $.BinaryOperation,
      $.UnaryOperation,
      $.Number,
      $.True,
      $.False,
      $.String,
      $.Type,
      $.PrimExpression,
      $.ExpressionSuffixed,
      $.DoExpr,
    ),
    _Expression: $ => choice($.Expression, $.PreprocessExpr),
    ExpressionList: $ => repeatSeq($._Expression),
    ExpressionSuffixed: $ => seq($.PrimExpression, repeat1(choice($._IndexSuffix, $._CallSuffix))),

    DoExpr: $ => seq("(", "do", $.Block, "end", ")"),

    BinaryOperation: $ => choice(
      ...[
        ['or', PREC.OR],
        ['and', PREC.AND],
        ['<', PREC.COMPARE],
        ['<=', PREC.COMPARE],
        ['==', PREC.COMPARE],
        ['~=', PREC.COMPARE],
        ['>=', PREC.COMPARE],
        ['>', PREC.COMPARE],
        ['|', PREC.BIT_OR],
        ['~', PREC.BIT_NOT],
        ['&', PREC.BIT_AND],
        ['<<', PREC.SHIFT],
        ['>>', PREC.SHIFT],
        ['>>>', PREC.SHIFT],
        ['+', PREC.PLUS],
        ['-', PREC.PLUS],
        ['*', PREC.MULTI],
        ['/', PREC.MULTI],
        ['//', PREC.MULTI],
        ['///', PREC.MULTI],
        ['%', PREC.MULTI],
        ['%%%', PREC.MULTI],
      ].map(([operator, precedence]) => prec.left(precedence, seq(
        $._Expression,
        operator,
        $._Expression
      ))),
      ...[
        ['..', PREC.CONCAT],
        ['^', PREC.POWER],
      ].map(([operator, precedence]) => prec.right(precedence, seq(
        $._Expression,
        operator,
        $._Expression
      )))
    ),

    UnaryOperation: $ => prec.left(PREC.UNARY, seq(
      choice('not', '#', '-', '~', '&', '$'),
      $._Expression
    )),

    TypeArgument: $ => choice(field("inner", $.TypeExpression), seq("(", $._Expression, ")"), $._Expression),

    Type: $ => prec.left(PREC.TYPE, seq("@", $.TypeExpression)),
    TypeExpression: $ => choice(
      alias($.TypeArrayOperation, $.ArrayType),
      $.TypeUnaryOperation,
      $.VariantTypeOperation,
      $.ArrayType,
      $.PointerType,
      $.SimpleType,
      $.VariantType,
      $.PrimType,
      $.BuiltinType,
    ),
    VariantTypeOperation: $ => prec.right(PREC.VARIANT, repeat1Seq($.TypeExpression, "|")),
    TypeArrayOperation: $ => prec.left(PREC.UNARY, seq("[", field("length", optional($._Expression)), "]", field("inner", $.TypeExpression))),
    TypeUnaryOperation: $ => prec.left(PREC.UNARY, seq(
      alias(choice("*", "?"), $.Operator),
      $.TypeExpression
    )),
    TypeGenericArguments: $ => prec.right(seq("(", repeatSeq($.TypeArgument), ")")),
    VariantType: $ => prec.right(seq("variant", optSeq("(", repeatSeq($.TypeExpression), ")"))),
    ArrayType: $ => prec.right(seq("array", optSeq("(", field("inner", $.TypeExpression), optSeq(",", field("length", $._Expression)), ")"))),
    PointerType: $ => prec.right(seq("pointer", optSeq("(", field("inner", $.TypeExpression), ")"))),
    SimpleType: $ => choice($.RecordType, $.UnionType, $.EnumType, $.FunctionType),
    PrimType: $ => prec.right(seq(choice($._PrimTypeExpression, $.BuiltinGenericType), optional($.TypeGenericArguments))),
    _PrimTypeExpression: $ => choice($.IdSuffixed, $.Id),
    _WithTypeExpression: $ => seq(":", $.TypeExpression),

    BuiltinGenericType: $ => choice(
      "facultative", "overload", "decltype"
    ),

    BuiltinType: $ => choice(
      "niltype", "nilptr", "type", "void", "auto", "boolean", "table",
      "int8", "int16", "int32", "int64", "int128", "isize", "uint8", "uint16", "uint32", "uint64", "usize", "float32", "float64", "float128", "byte",
      "cchar", "cschar", "cshort", "cint", "clong", "clonglong", "cptrdiff", "cuchar", "cushort", "cuint", "culong", "csize", "clongdouble", "cstring", "cdouble", "cfloat" , "cvararg", "cvalist", "cclock_", "ctime_t",
      "integer", "uinteger", "number",
      "string", "any", "varanys", "varargs"
    ),

    RecordType: $ => seq(
      "record", "{",
      optional(repeatSeq($.RecordField, $._FieldSep, true)),
      "}"
    ),
    UnionType: $ => seq(
      "union", "{",
      optional(repeatSeq($.UnionField, $._FieldSep, true)),
      "}"
    ),
    EnumType: $ => seq(
      "enum", optSeq("(", field("type", $.TypeExpression), ")"), "{",
      optional(repeatSeq($.EnumField, $._FieldSep, true)),
      "}"
    ),
    RecordField: $ => seq(alias($.Name, $.FieldName), ":", $.TypeExpression),
    UnionField: $ => choice(seq(alias($.Name, $.FieldName), ":", $.TypeExpression), $.TypeExpression),
    EnumField: $ => seq(alias($.Name, $.FieldName), optSeq("=", $._Expression)),
    FunctionType: $ => seq("function", "(", $.FunctionTypeArguments, ")", optSeq(":", $.FunctionReturnsType)),

    VarargsType: $ => seq("...", optional($._WithTypeExpression)),

    // // Statements
    Label: $ => seq("::", $.Name, "::"),
    Return: $ => seq("return", $._Expression),
    Break: $ => "break",
    Continue: $ => "continue",
    Goto: $ => seq("goto", $.Name),
    Do: $ => seq("do", optional($.Block), "end"),
    Defer: $ => seq("defer", optional($.Block), "end"),
    While: $ => seq("while", $._Expression, "do", optional($.Block), "end"),
    Repeat: $ => seq("repeat", optional($.Block), "until", $._Expression),
    If: $ => seq(
      "if", alias($._Expression, $.ConditionExpression), "then",
      $.Block,
      repeat($.ElseIf),
      optional($.Else),
      "end"
    ),
    ElseIf: $ => seq(
      "elseif", alias($._Expression, $.ConditionExpression), "then",
      optional($.Block)
    ),
    Else: $ => prec.right(seq("else", optional($.Block))),
    Switch: $ => seq(
      "switch", $._Expression, optional("do"),
      repeat1($.SwitchCase),
      optional($.Else),
      "end"
    ),
    SwitchCase: $ => seq("case", $._Expression, "then", optional($.Block)),
    For: $ => seq("for", choice($.ForNum, $.ForIn)),
    ForNum: $ => seq(
      $.IdDecl, "=", $._Expression, ",", optional($._for_cmp), $._Expression, optSeq(",", $._Expression), "do",
      optional($.Block),
      "end"
    ),
    ForIn: $ => seq(
      $.IdDecls, "in", $.ExpressionList, "do",
      optional($.Block),
      "end"
    ),
    _for_cmp: $ => /(~=)|(<=)|(<)|(>=)|(>)/,
    Local: $ => seq("local", choice(alias($._Function, $.Function), alias($.LocalVariable, $.Variable))),
    Global: $ => seq("global", choice(alias($._Function, $.Function), alias($.GlobalVariable, $.Variable))),
    FuncDef: $ => seq("function", $.FunctionName, $.FunctionBody),
    _Function: $ => seq("function", $.NameDecl, $.FunctionBody),
    FunctionBody: $ => seq(
      "(", optional($.FunctionArguments), ")",
      optSeq(":", $.FunctionReturnsType),
      optional($.AnnotationList),
      optional($.Block),
      "end"
    ),
    FunctionArguments: $ => choice(seq($.IdDecls, optSeq(",", $.VarargsType)), $.VarargsType),
    FunctionTypeArguments: $ => choice(seq(repeatSeq($.FunctionTypeArgument), optSeq(",", $.VarargsType)), $.VarargsType),
    FunctionTypeArgument: $ => choice($.TypedDecl, $.TypeExpression),
    FunctionReturnsType: $ => choice(seq("(", repeatSeq($.TypeExpression), ")"), $.TypeExpression),
    LocalVariable: $ => seq($.IdDecls, optSeq("=", $.ExpressionList)),
    GlobalVariable: $ => seq($.GlobalDecls, optSeq("=", $.ExpressionList)),
    Assign: $ => prec.left(PREC.ASSIGN, seq($.Variables, "=", $.ExpressionList)),
    SHEBANG: $ => /#!.*\n/,
    _FieldSep: $ => choice(",", ";")
  },
});

function repeatSeq(rule, sep = ",", last = false) {
  return last
    ? seq(rule, repeat(seq(sep, rule)), optional(sep))
    : seq(rule, repeat(seq(sep, rule)));
}

function repeat1Seq(rule, sep = ",", last = false) {
  return last
    ? seq(rule, repeat1(seq(sep, rule)), optional(sep))
    : seq(rule, repeat1(seq(sep, rule)));
}

function optSeq(...rules) {
  return optional(seq(...rules))
}