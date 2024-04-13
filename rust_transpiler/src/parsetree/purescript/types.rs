#![allow(dead_code)]
#![allow(non_snake_case)]
use std::fmt::Debug;
use serde::{Serialize, Deserialize};
use super::names as N;

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct SourcePos {
    pub srcLine: i32,
    pub srcColumn: i32,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct SourceRange {
    pub srcStart: SourcePos,
    pub srcEnd: SourcePos,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Comment<L: Debug + PartialEq> {
    Comment(String),
    Space(i32),
    Line(L),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]

pub enum LineFeed {
    LF,
    CRLF,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct TokenAnn {
    pub tokRange: SourceRange,
    pub tokLeadingComments: Vec<Comment<LineFeed>>,
    pub tokTrailingComments: Vec<Comment<()>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
pub enum SourceStyle {
    ASCII,
    Unicode,
}


#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Token {
    TokLeftParen,
    TokRightParen,
    TokLeftBrace,
    TokRightBrace,
    TokLeftSquare,
    TokRightSquare,
    TokLeftArrow(SourceStyle),
    TokRightArrow(SourceStyle),
    TokRightFatArrow(SourceStyle),
    TokDoubleColon(SourceStyle),
    TokForall(SourceStyle),
    TokEquals,
    TokPipe,
    TokTick,
    TokDot,
    TokComma,
    TokUnderscore,
    TokBackslash,
    TokLowerName(Vec<String>, String),
    TokUpperName(Vec<String>, String),
    TokOperator(Vec<String>, String),
    TokSymbolName(Vec<String>, String),
    TokSymbolArr(SourceStyle),
    TokHole(String),
    TokChar(String, char),
    TokString(String, String),
    TokRawString(String),
    TokInt(String, i64),
    TokNumber(String, f64),
    TokLayoutStart,
    TokLayoutSep,
    TokLayoutEnd,
    TokEof,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct SourceToken {
    pub tokAnn: TokenAnn,
    pub tokValue: Token,
}
#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Ident {
    pub getIdent: String,
}
#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Name<A> {
    pub nameTok: SourceToken,
    pub nameValue: A,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct QualifiedName<A> {
    pub qualTok: SourceToken,
    pub qualModule: Option<N::ModuleName>,
    pub qualName: A,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Label {
    pub lblTok: SourceToken,
    pub lblName: String,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Wrapped<A> {
    pub wrpOpen: SourceToken,
    pub wrpValue: A,
    pub wrpClose: SourceToken,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Separated<A> {
    pub sepHead: A,
    pub sepTail: Vec<(SourceToken, A)>,
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Labeled<A, B> {
    pub lblLabel: A,
    pub lblSep: SourceToken,
    pub lblValue: B,
}

pub type Delimited<A> = Wrapped<Option<Separated<A>>>;
pub type DelimitedNonEmpty<A> = Wrapped<Separated<A>>;

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum OneOrDelimited<A> {
    One(A),
    Many(DelimitedNonEmpty<A>),
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Type<A> {
    TypeVar(A, Name<Ident>),
    TypeConstructor(A, QualifiedName<N::ProperName<N::ProperNameType>>),
    TypeWildcard(A, SourceToken),
    TypeHole(A, Name<Ident>),
    TypeString(A, SourceToken, String),
    TypeInt(A, Option<SourceToken>, SourceToken, i64),
    TypeRow(A, Wrapped<Row<A>>),
    TypeRecord(A, Wrapped<Row<A>>),
    TypeForall(A, SourceToken, Vec<TypeVarBinding<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeKinded(A, Box<Type<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeApp(A, Box<Type<A>>, Box<Type<A>>), // Box ???
    TypeOp(A, Box<Type<A>>, QualifiedName<N::OpName<N::OpNameType>>, Box<Type<A>>),
    TypeOpName(A, QualifiedName<N::OpName<N::OpNameType>>),
    TypeArr(A, Box<Type<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeArrName(A, SourceToken),
    TypeConstrained(A, Constraint<A>, SourceToken, Box<Type<A>>),
    TypeParens(A, Wrapped<Box<Type<A>>>), // Box ???
    TypeUnaryRow(A, SourceToken, Box<Type<A>>), // Box ???
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum TypeVarBinding<A> {
    TypeVarKinded(Wrapped<Labeled<(Option<SourceToken>, Name<Ident>), Type<A>>>),
    TypeVarName((Option<SourceToken>, Name<Ident>)),
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Constraint<A> {
    Constraint(A, QualifiedName<N::ProperName<N::ProperNameType>>, Vec<Type<A>>),
    ConstraintParens(A, Wrapped<Box<Constraint<A>>>), // Box ???
}

#[derive(Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Row<A> {
    pub rowLabels: Option<Separated<Labeled<Label, Box<Type<A>>>>>,
    pub rowTail: Option<(SourceToken, Box<Type<A>>)>,
}


#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct Module<A> {
    pub modAnn: A,
    pub modKeyword: SourceToken,
    pub modNamespace: Name<N::ModuleName>,
    pub modExports: Option<DelimitedNonEmpty<Export<A>>>,
    pub modWhere: SourceToken,
    pub modImports: Vec<ImportDecl<A>>,
    pub modDecls: Vec<Declaration<A>>,
    pub modTrailingComments: Vec<Comment<LineFeed>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Export<A> {
    ExportValue(A, Name<Ident>),
    ExportOp(A, Name<N::OpName<N::OpNameType>>),
    ExportType(A, Name<N::ProperName<N::ProperNameType>>, Option<DataMembers<A>>),
    ExportTypeOp(A, SourceToken, Name<N::OpName<N::OpNameType>>),
    ExportClass(A, SourceToken, Name<N::ProperName<N::ProperNameType>>),
    ExportModule(A, SourceToken, Name<N::ModuleName>),
}
// Making New Types
#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum DataMembers<T> {
    DataAll(T, SourceToken),
    DataEnumerated(T, Delimited<Name<N::ProperName<N::ProperNameType>>>),
}

// Assuming types like DataHead, SourceToken, Separated, DataCtor, Type, Name, N.ProperName, ClassHead, NonEmpty, Labeled, Instance, InstanceHead, ValueBindingFields, FixityFields, Foreign, and Role are defined
#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Declaration<T> {
    DeclData(T, DataHead<T>, Option<(SourceToken, Separated<DataCtor<T>>)>),
    DeclType(T, DataHead<T>, SourceToken, Type<T>),
    DeclNewtype(T, DataHead<T>, SourceToken, Name<N::ProperName<N::ProperNameType>>, Type<T>),
    DeclClass(T, ClassHead<T>, Option<(SourceToken, Vec<Labeled<Name<Ident>, Type<T>>>)>),
    DeclInstanceChain(T, Separated<Instance<T>>),
    DeclDerive(T, SourceToken, Option<SourceToken>, InstanceHead<T>),
    DeclKindSignature(T, SourceToken, Labeled<Name<N::ProperName<N::ProperNameType>>, Type<T>>),
    DeclSignature(T, Labeled<Name<Ident>, Type<T>>),
    DeclValue(T, ValueBindingFields<T>),
    DeclFixity(T, FixityFields),
    DeclForeign(T, SourceToken, SourceToken, Foreign<T>),
    DeclRole(T, SourceToken, SourceToken, Name<N::ProperName<N::ProperNameType>>, Vec<Role>)
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Instance<T> {
    instHead: InstanceHead<T>,
    instBody: Option<(SourceToken, Vec<InstanceBinding<T>>)>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum InstanceBinding<T> {
    InstanceBindingSignature(T, Labeled<Name<Ident>, Type<T>>),
    InstanceBindingName(T, ValueBindingFields<T>),
}
#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ImportDecl<T> {
    impAnn: T,
    impKeyword: SourceToken,
    impModule: Name<N::ModuleName>,
    impNames: Option<(Option<SourceToken>, DelimitedNonEmpty<Import<T>>)>,
    impQual: Option<(SourceToken, Name<N::ModuleName>)>,
}
#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Import<T> {
    ImportValue(T, Name<Ident>),
    ImportOp(T, Name<N::OpName<N::OpNameType>>),
    ImportType(T, Name<N::ProperName<N::ProperNameType>>, Option<DataMembers<T>>),
    ImportTypeOp(T, SourceToken, Name<N::OpName<N::ProperNameType>>),
    ImportClass(T, SourceToken, Name<N::ProperName<N::ProperNameType>>)
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct DataHead<T> {
    dataHdKeyword: SourceToken,
    dataHdName: Name<N::ProperName<N::ProperNameType>>,
    dataHdVars: Vec<TypeVarBinding<T>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct DataCtor<T> {
    dataCtorAnn: T,
    dataCtorName: Name<N::ProperName<N::ProperNameType>>,
    dataCtorFields: Vec<Type<T>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassHead<T> {
    clsKeyword: SourceToken,
    clsSuper: Option<(OneOrDelimited<Constraint<T>>, SourceToken)>,
    clsName: Name<N::ProperName<N::ProperNameType>>,
    clsVars: Vec<TypeVarBinding<T>>,
    clsFundeps: Option<(SourceToken, Separated<ClassFundep>)>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassFundep {
    FundepDetermined(SourceToken, Vec<Name<Ident>>),
    FundepDetermines(Vec<Name<Ident>>, SourceToken, Vec<Name<Ident>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct InstanceHead<T> {
    instKeyword: SourceToken,
    instNameSep: Option<(Name<Ident>, SourceToken)>,
    instConstraints: Option<(OneOrDelimited<Constraint<T>>, SourceToken)>,
    instClass: QualifiedName<N::ProperName<N::ProperNameType>>,
    instTypes: Vec<Type<T>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Fixity {
    Infix,
    Infixl,
    Infixr,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum FixityOp {
    FixityValue(QualifiedName<Result<Ident, N::ProperName<N::ProperNameType>>>, SourceToken, Name<N::OpName<N::OpNameType>>),
    FixityType(SourceToken, QualifiedName<N::ProperName<N::ProperNameType>>, SourceToken, Name<N::OpName<N::ProperNameType>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct FixityFields {
    fxtKeyword: (SourceToken, Fixity),
    fxtPrec: (SourceToken, i64),
    fxtOp: FixityOp,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ValueBindingFields<T> {
    valName: Name<Ident>,
    valBinders: Vec<Binder<T>>,
    valGuarded: Guarded<T>,
}


#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Guarded<T> {
    Unconditional(SourceToken, Where<T>),
    Guarded(Vec<GuardedExpr<T>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct GuardedExpr<T> {
    grdBar: SourceToken,
    grdPatterns: Separated<PatternGuard<T>>,
    grdSep: SourceToken,
    grdWhere: Where<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct PatternGuard<T> {
    patBinder: Option<(Binder<T>, SourceToken)>,
    patExpr: Expr<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Foreign<T> {
    ForeignValue(Labeled<Name<Ident>, Type<T>>),
    ForeignData(SourceToken, Labeled<Name<N::ProperName<N::ProperNameType>>, Type<T>>),
    ForeignKind(SourceToken, Name<N::ProperName<N::ProperNameType>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum RRole {
    Nominal,
    Representational,
    Phantom,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Role {
    roleTok: SourceToken,
    roleValue: RRole,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum Expr<T> {
    ExprHole(T, Name<Ident>),
    ExprSection(T, SourceToken),
    ExprIdent(T, QualifiedName<Ident>),
    ExprConstructor(T, QualifiedName<N::ProperName<N::ProperNameType>>),
    ExprBoolean(T, SourceToken, bool),
    ExprChar(T, SourceToken, char),
    ExprString(T, SourceToken, N::PSString),
    ExprNumber(T, SourceToken, Result<i64, f64>),
    ExprArray(T, Delimited<Box<Expr<T>>>),
    ExprRecord(T, Delimited<RecordLabeled<Box<Expr<T>>>>),
    ExprParens(T, Wrapped<Box<Expr<T>>>),
    ExprTyped(T, Box<Expr<T>>, SourceToken, Type<T>),
    ExprInfix(T, Box<Expr<T>>, Wrapped<Box<Expr<T>>>, Box<Expr<T>>),
    ExprOp(T, Box<Expr<T>>, QualifiedName<N::OpName<N::OpNameType>>, Box<Expr<T>>),
    ExprOpName(T, QualifiedName<N::OpName<N::OpNameType>>),
    ExprNegate(T, SourceToken, Box<Expr<T>>),
    ExprRecordAccessor(T, Box<RecordAccessor<T>>),
    ExprRecordUpdate(T, Box<Expr<T>>, DelimitedNonEmpty<Box<RecordUpdate<T>>>),
    ExprApp(T, Box<Expr<T>>, Box<Expr<T>>),
    ExprVisibleTypeApp(T, Box<Expr<T>>, SourceToken, Type<T>),
    ExprLambda(T, Box<Lambda<T>>),
    ExprIf(T, Box<IfThenElse<T>>),
    ExprCase(T, Box<CaseOf<T>>),
    ExprLet(T, Box<LetIn<T>>),
    ExprDo(T, DoBlock<T>),
    ExprAdo(T, Box<AdoBlock<T>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum RecordLabeled<T> {
    RecordPun(Name<Ident>),
    RecordField(Label, SourceToken, T),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum RecordUpdate<T> {
    RecordUpdateLeaf(Label, SourceToken, Box<Expr<T>>),
    RecordUpdateBranch(Label, DelimitedNonEmpty<Box<RecordUpdate<T>>>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct RecordAccessor<T> {
    recExpr: Expr<T>,
    recDot: SourceToken,
    recPath: Separated<Label>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Lambda<T> {
    lmbSymbol: SourceToken,
    lmbBinders: Vec<Binder<T>>,
    lmbArr: SourceToken,
    lmbBody: Expr<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct IfThenElse<T> {
    iteIf: SourceToken,
    iteCond: Expr<T>,
    iteThen: SourceToken,
    iteTrue: Expr<T>,
    iteElse: SourceToken,
    iteFalse: Expr<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct CaseOf<T> {
    caseKeyword: SourceToken,
    caseHead: Separated<Expr<T>>,
    caseOf: SourceToken,
    caseBranches: Vec<(Separated<Binder<T>>, Guarded<T>)>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct LetIn<T> {
    letKeyword: SourceToken,
    letBindings: Vec<LetBinding<T>>,
    letIn: SourceToken,
    letBody: Expr<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Where<T> {
    whereExpr: Expr<T>,
    whereBindings: Option<(SourceToken, Vec<LetBinding<T>>)>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum LetBinding<T> {
    LetBindingSignature(T, Labeled<Name<Ident>, Type<T>>),
    LetBindingName(T, ValueBindingFields<T>),
    LetBindingPattern(T, Binder<T>, SourceToken, Where<T>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct DoBlock<T> {
    doKeyword: SourceToken,
    doStatements: Vec<DoStatement<T>>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum DoStatement<T> {
    DoLet(SourceToken, Vec<LetBinding<T>>),
    DoDiscard(Expr<T>),
    DoBind(Binder<T>, SourceToken, Expr<T>),
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct AdoBlock<T> {
    adoKeyword: SourceToken,
    adoStatements: Vec<DoStatement<T>>,
    adoIn: SourceToken,
    adoResult: Expr<T>,
}

#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Binder<T> {
    BinderWildcard(T, SourceToken),
    BinderVar(T, Name<Ident>),
    BinderNamed(T, Name<Ident>, SourceToken, Box<Binder<T>>),
    BinderConstructor(T, QualifiedName<N::ProperName<N::ProperNameType>>, Vec<Binder<T>>),
    BinderBoolean(T, SourceToken, bool),
    BinderChar(T, SourceToken, char),
    BinderString(T, SourceToken, N::PSString),
    BinderNumber(T, Option<SourceToken>, SourceToken, Result<i64, f64>),
    BinderArray(T, Delimited<Box<Binder<T>>>),
    BinderRecord(T, Delimited<RecordLabeled<Box<Binder<T>>>>),
    BinderParens(T, Wrapped<Box<Binder<T>>>),
    BinderTyped(T, Box<Binder<T>>, SourceToken, Type<T>),
    BinderOp(T, Box<Binder<T>>, QualifiedName<N::OpName<N::OpNameType>>, Box<Binder<T>>),
}