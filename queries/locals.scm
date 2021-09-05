(Block) @scope
(For) @scope

(Label (Name) @definition)
(Goto (Name) @reference)
(Id (RawId (Name) @reference))

(Function (NameDecl (Name) @definition.function)) @scope
(FuncDef (FunctionName (Id (RawId (Name) @definition.function)))) @scope

(Variable (IdDecls (IdDecl (RawIdDecl (Name) @definition.variable))))
(Variable (GlobalDecls (GlobalDecl (Name) @definition.variable)))
(Variable (GlobalDecls (GlobalDecl (IdSuffixed (Id (RawId (Name) @definition.variable)) (DotIndex (Name) @reference.field)))))

(TypeExpression) @reference.type

(RecordType (RecordField (Name) @definition.field))
(UnionType (UnionField (Name) @definition.field))
(EnumType (EnumField (Name) @definition.field))
