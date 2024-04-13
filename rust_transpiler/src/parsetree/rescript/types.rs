#![allow(non_snake_case)]
use serde::{Serialize, Deserialize};


pub type Structure = Vec<StructureItem>;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
pub struct StructureItem {
    pstrDesc: StructureItemDesc, // Assuming StructureItemDesc is another type you'll define
    pstrLoc: Location,            // Assuming Location is another type you'll define
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum StructureItemDesc {
    PstrEval(Expression, Attributes),
    PstrValue(RecFlag, Vec<ValueBinding>),
    PstrPrimitive(ValueDescription),
    PstrType(RecFlag, Vec<TypeDeclaration>),
    PstrTypext(TypeExtension),
    PstrException(ExtensionConstructor),
    PstrModule(ModuleBinding),
    PstrRecmodule(Vec<ModuleBinding>),
    PstrModtype(ModuleTypeDeclaration),
    PstrOpen(OpenDescription),
    PstrClass(()), // Assuming this is a placeholder for something more specific
    PstrClassType(Vec<ClassTypeDeclaration>),
    PstrInclude(IncludeDescription),
    PstrAttribute(Attribute),
    PstrExtension(Extension, Attributes),
}


// Location Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Location {
    locStart: Position, // Assuming Position is another type you'll define
    locEnd: Position,
    locGhost: bool,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Position {
    posFname: String,
    posLnum: i32,
    posBol: i32,
    posCnum: i32,
}

// Longident Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Longident {
    Lident(String),
    Ldot(Box<Longident>, String),
    Lapply(Box<Longident>, Box<Longident>),
}

// Module Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ModuleBinding {
    pmbName: Loc<String>,
    pmbExpr: ModuleExpr,
    pmbAttributes: Attributes,
    pmbLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ModuleExpr {
    pmodDesc: ModuleExprDesc, // Direct use, assuming you'll define or replace
    pmodLoc: Location, // Previously defined
    pmodAttributes: Attributes, // Direct use, keeping track of previous context
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ModuleExprDesc {
    PmodIdent(Loc<Longident>),
    PmodStructure(Structure),
    PmodFunctor(Loc<String>, Option<ModuleType>, Box<ModuleExpr>),
    PmodApply(Box<ModuleExpr>, Box<ModuleExpr>),
    PmodConstraint(Box<ModuleExpr>, ModuleType),
    PmodUnpack(Expression),
    PmodExtension(Extension),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ModuleType {
    pmtyDesc: Box<ModuleTypeDesc>, // Direct naming, assuming definition elsewhere
    pmtyLoc: Location, // Using the previously defined Location type
    pmtyAttributes: Attributes, // Direct naming, assuming definition elsewhere
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ModuleTypeDesc {
    PmtyIdent(Loc<Longident>),
    PmtySignature(Signature),
    PmtyFunctor(Loc<String>, Option<ModuleType>, ModuleType),
    PmtyWith(ModuleType, WithConstraint),
    PmtyTypeof(Expression),
    PmtyExtension(Extension),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ModuleDeclaration {
    pmdName: Loc<String>,
    pmdType: Option<ModuleType>,
    pmdAttributes: Attributes,
    pmdLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ModuleTypeDeclaration {
    pmtdName: Loc<String>,
    pmtdType: Option<ModuleType>,
    pmtdAttributes: Attributes,
    pmtdLoc: Location,
}

// Open Desctiption

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct OpenDescription {
    popenLid: Loc<Longident>,
    popenOverride: bool,
    popenLoc: Location,
    popenAttributes: Attributes,
}

// Include Declaration
type IncludeDescription = IncludeInfos<ModuleType>;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct IncludeInfos<T> {
    pinclMod: T,
    pinclLoc: Location,
    pinclAttributes: Attributes,
}

type IncludeDeclaration =  IncludeInfos<ModuleExpr>;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum WithConstraint {
    PwithType(Loc<Longident>, TypeDeclaration),
    PwithModule(Loc<Longident>, Loc<Longident>),
    PwithTypeSubst(Loc<Longident>, TypeDeclaration),
    PwithModSubst(Loc<Longident>, Loc<Longident>),
}

// Class Types

type ClassTypeDeclaration = ClassInfos<ClassType>;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassInfos<T> {
    pciVirt: VirtualFlag,
    pciParams: Vec<(CoreType, Variance)>,
    pciName: Loc<String>,
    pciExpr: T,
    pciLoc: Location,
    pciAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassStructure {
    pcstrSelf: Pattern,
    pcstrFields: Vec<ClassField>,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassField {
    pcfDesc: ClassFieldDesc,
    pcfLoc: Location,
    pcfAttributes: Attributes,
}


#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassFieldDesc {
    PcfInherit(()),
    PcfVal(Loc<Label>, MutableFlag, ClassFieldKind),
    PcfMethod(Loc<Label>, PrivateFlag, ClassFieldKind),
    PcfConstraint(CoreType, CoreType),
    PcfInitializer(Expression),
    PcfAttribute(Attribute),
    PcfExtension(Extension),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassFieldKind {
    CfkVirtual(CoreType),
    CfkConcrete(OverrideFlag, Expression),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassType {
    pctyDesc: Box<ClassTypeDesc>,
    pctyLoc: Location,
    pctyAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassTypeDesc {
    PctyConstr(Loc<Longident>, Vec<CoreType>),
    PctySignature(ClassSignature),
    PctyArrow(ArgLabel, CoreType, ClassType),
    PctyExtension(Extension),
    PctyOpen(OverrideFlag, Loc<Longident>, ClassType),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassSignature {
    pcsigSelf: CoreType,
    pcsigFields: Vec<ClassTypeField>,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassTypeField {
    pctfDesc: ClassTypeFieldDesc,
    pctfLoc: Location,
    pctfAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassTypeFieldDesc {
    PctfInherit(ClassType),
    PctfVal(Loc<Label>, MutableFlag, CoreType),
    PctfMethod(Loc<Label>, PrivateFlag, CoreType),
    PctfConstraint(CoreType, CoreType),
    PctfAttribute(Attribute),
    PctfExtension(Extension),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ClassExpr {
    pclDesc: Box<ClassExprDesc>,
    pclLoc: Location,
    pclAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClassExprDesc {
    PclStructure(ClassStructure),
    PclFunctor(Loc<String>, Option<ModuleType>, ClassExpr),
    PclApply(ClassExpr, ClassExpr),
    PclLet(RecFlag, Vec<ValueBinding>, ClassExpr),
    PclConstraint(ClassExpr, ClassType),
    PclExtension(Extension),
}

// Expression Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Expression {
    pexpDesc: Box<ExpressionDesc>,
    pexpLoc: Location,
    pexpAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ExpressionDesc {
    PexpIdent(Loc<Longident>),
    PexpConstant(Constant),
    PexpLet(RecFlag, Vec<ValueBinding>, Box<Expression>),
    PexpFunction(Vec<Case>),
    PexpFun(ArgLabel, Option<Expression>, Pattern, Box<Expression>),
    PexpApply(Expression, Vec<(ArgLabel, Expression)>),
    PexpMatch(Expression, Vec<Case>, Option<Expression>),
    PexpTry(Expression, Vec<Case>),
    PexpTuple(Vec<Expression>),
    PexpConstruct(Loc<Longident>, Option<Expression>),
    PexpVariant(Label, Option<Expression>),
    PexpRecord(Vec<(Loc<Longident>, Expression)>, Option<Expression>),
    PexpField(Expression, Loc<Longident>),
    PexpSetfield(Expression, Loc<Longident>, Expression),
    PexpArray(Vec<Expression>),
    PexpIfthenelse(Expression, Expression, Option<Expression>),
    PexpSequence(Expression, Expression),
    PexpWhile(Expression, Expression),
    PexpFor(Pattern, Expression, Expression, DirectionFlag, Expression),
    PexpConstraint(Expression, CoreType),
    PexpCoerce(Expression, Option<CoreType>, CoreType),
    PexpSend(Expression, Loc<Label>),
    PexpNew(Loc<Longident>),
    PexpSetinstvar(Loc<Label>, Expression),
    PexpOverride(Vec<(Loc<Label>, Expression)>),
    PexpLetmodule(Loc<String>, ModuleExpr, Expression),
    PexpLetexception(ExtensionConstructor, Expression),
    PexpAssert(Expression),
    PexpLazy(Expression),
    PexpPoly(Expression, Option<CoreType>),
    PexpObject(ClassStructure),
    PexpNewtype(Loc<String>, Expression),
    PexpPack(ModuleExpr),
    PexpOpen(OverrideFlag, Loc<Longident>, Expression),
    PexpExtension(Extension),
    PexpUnreachable,
    Nolabel
}

// Attribute Types

type Attributes = Vec<Attribute>;

type Attribute = (Loc<String>, Payload);

// Payload Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Payload {
    PStr(Structure),
    PSig(Signature),
    PTyp(CoreType),
    PPat(Pattern, Option<Expression>),
}

// Signature Types
type Signature = Vec<SignatureItem>;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct SignatureItem {
    psigDesc: SignatureItemDesc,
    psigLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum SignatureItemDesc {
    PsigValue(ValueDescription),
    PsigType(TypeDeclaration),
    PsigTypext(TypeExtension),
    PsigException(ExtensionConstructor),
    PsigModule(ModuleDeclaration),
    PsigRecmodule(Vec<ModuleDeclaration>),
    PsigModtype(ModuleTypeDeclaration),
    PsigOpen(OpenDescription),
    PsigInclude(IncludeDeclaration),
    PsigClass(()),
    PsigClassType(ClassTypeDeclaration),
    PsigAttribute(Attribute),
    PsigExtension(Extension),
}

// Core Types

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct CoreType {
    ptypDesc: Box<CoreTypeDesc>,
    ptypLoc: Location,
    ptypAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum CoreTypeDesc {
    PtypAny,
    PtypVar(String),
    PtypArrow(ArgLabel, CoreType, CoreType),
    PtypTuple(Vec<CoreType>),
    PtypConstr(Loc<Longident>, Vec<CoreType>),
    PtypObject(Vec<ObjectField>, ClosedFlag),
    PtypClass(Loc<Longident>, Vec<CoreType>),
    PtypAlias(CoreType, String),
    PtypVariant(Vec<RowField>, ClosedFlag, Option<Vec<Label>>),
    PtypPoly(Vec<Loc<String>>, CoreType),
    PtypPackage(PackageType),
    PtypExtension(Extension),
}

// Pakage type
type PackageType = (Loc<Longident>, Vec<(Loc<Longident>, CoreType)>);


// Object Fields Types
#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ObjectField {
    Otag(Loc<Label>, Attributes, CoreType),
    Oinherit(CoreType),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum RowField {
    Rtag(Loc<Label>, Attributes, bool, Vec<CoreType>),
    Rinherit(CoreType),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ValueBinding {
    pvbPat: Pattern,
    pvbExpr: Expression,
    pvbAttributes: Attributes,
    pvbLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Case {
    pcLhs: Pattern,
    pcGuard: Option<Expression>,
    pcRhs: Expression,
}
#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Pattern {
    ppatDesc: Box<PatternDesc>,
    ppatLoc: Location,
    ppatAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum PatternDesc {
    PpatAny,
    PpatVar(Loc<String>),
    PpatAlias(Pattern, Loc<String>),
    PpatConstant(Constant),
    PpatInterval(Constant, Constant),
    PpatTuple(Vec<Pattern>),
    PpatConstruct(Loc<Longident>, Option<Pattern>),
    PpatVariant(Label, Option<Pattern>),
    PpatRecord(Vec<(Loc<Longident>, Pattern)>, ClosedFlag),
    PpatArray(Vec<Pattern>),
    PpatOr(Pattern, Pattern),
    PpatConstraint(Pattern, CoreType),
    PpatType(Loc<Longident>),
    PpatLazy(Pattern),
    PpatUnpack(Loc<String>),
    PpatException(Pattern),
    PpatExtension(Extension),
    PpatOpen(Loc<Longident>, Pattern),
}

type Extension = (Loc<String>, Payload);

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ExtensionConstructor {
    pextName: Loc<String>,
    pextKind: ExtensionConstructorKind,
    pextLoc: Location,
    pextAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ExtensionConstructorKind {
    PextDecl(ConstructorArguments, Option<CoreType>),
    PextRebind(Loc<Longident>),
}


#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ConstructorDeclaration {
    pcdName: Loc<String>,
    pcdArgs: ConstructorArguments,
    pcdRes: Option<CoreType>,
    pcdLoc: Location,
    pcdAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ConstructorArguments {
    PcstrTuple(Vec<CoreType>),
    PcstrRecord(Vec<(Loc<Longident>, CoreType, MutableFlag)>),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct ValueDescription {
    pvalName: Loc<String>,
    pvalType: CoreType,
    pvalPrim: Vec<String>,
    pvalAttributes: Attributes,
    pvalLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct TypeDeclaration {
    ptypeName: Loc<String>,
    ptypeParams: Vec<Loc<String>>,
    ptypeCtxt: Vec<CoreType>,
    ptypeKind: TypeKind,
    ptypePrivate: PrivateFlag,
    ptypeManifest: Option<CoreType>,
    ptypeAttributes: Attributes,
    ptypeLoc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct TypeExtension {
    ptyextPath: Loc<Longident>,
    ptyextParams: Vec<(CoreType, Variance)>,
    ptyextConstructors: Vec<ExtensionConstructor>,
    ptyextPrivate: PrivateFlag,
    ptyextAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum TypeKind {
    PtypeAbstract,
    PtypeVariant(Vec<ConstructorDeclaration>),
    PtypeRecord(Vec<LabelDeclaration>),
    PtypeOpen,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct LabelDeclaration {
    pldName: Loc<String>,
    pldMutable: MutableFlag,
    pldType: CoreType,
    pldLoc: Location,
    pldAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Constant {
    PconstInteger(String, Option<char>),
    PconstChar(i32),
    PconstString(String, Option<String>),
    PconstFloat(String, Option<char>),
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum RecFlag {
    Nonrecursive,
    Recursive,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum DirectionFlag {
    Upto,
    Downto,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum PrivateFlag {
    Private,
    Public,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum MutableFlag {
    Immutable,
    Mutable,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum VirtualFlag {
    Virtual,
    Concrete,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum OverrideFlag {
    Override,
    Fresh,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ClosedFlag {
    Closed,
    Open,
}

type Label = String;

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum ArgLabel {
    Nolabel,
    Labelled(String),
    Optional(String),
}


#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
struct Loc<T> {
    txt: T,
    loc: Location,
}

#[derive( Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}