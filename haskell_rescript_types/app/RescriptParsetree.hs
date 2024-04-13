{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module RescriptParsetree where

import Prelude
import Data.Char
import Data.Aeson
import Data.Maybe
import Data.Data (typeOf)
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Text.Read as TR

-- Structure Types

type Structure = [StructureItem]

data StructureItem = StructureItem
    { pstrDesc :: StructureItemDesc
    , pstrLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON StructureItem where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON StructureItem where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data StructureItemDesc =
    PstrEval Expression Attributes
  | PstrValue RecFlag [ValueBinding]
  | PstrPrimitive ValueDescription
  | PstrType RecFlag [TypeDeclaration]
  | PstrTypext TypeExtension
  | PstrException ExtensionConstructor
  | PstrModule ModuleBinding
  | PstrRecmodule [ModuleBinding]
  | PstrModtype ModuleTypeDeclaration
  | PstrOpen OpenDescription
  | PstrClass ()
  | PstrClassType [ClassTypeDeclaration]
  | PstrInclude IncludeDeclaration
  | PstrAttribute Attribute
  | PstrExtension Extension Attributes
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Location Types

data Location = Location
    { locStart :: Position
    , locEnd :: Position
    , locGhost :: Bool
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Location where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Location where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data Position = Position
    { posFname :: String
    , posLnum :: Int
    , posBol :: Int
    , posCnum :: Int
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Position where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Position where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Longident Types

data Longident =
    Lident String
  | Ldot Longident String
  | Lapply Longident Longident
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Module Types

data ModuleBinding = ModuleBinding
    { pmbName :: Loc String
    , pmbExpr :: ModuleExpr
    , pmbAttributes :: Attributes
    , pmbLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ModuleBinding where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ModuleBinding where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ModuleExpr = ModuleExpr
    { pmodDesc :: ModuleExprDesc
    , pmodLoc :: Location
    , pmodAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ModuleExpr where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ModuleExpr where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ModuleExprDesc
    = PmodIdent (Loc Longident)
    | PmodStructure Structure
    | PmodFunctor (Loc String) (Maybe ModuleType) ModuleExpr
    | PmodApply ModuleExpr ModuleExpr
    | PmodConstraint ModuleExpr ModuleType
    | PmodUnpack Expression
    | PmodExtension Extension
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ModuleType = ModuleType
    { pmtyDesc :: ModuleTypeDesc
    , pmtyLoc :: Location
    , pmtyAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ModuleType where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ModuleType where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ModuleTypeDesc
    = PmtyIdent (Loc Longident)
    | PmtySignature Signature
    | PmtyFunctor (Loc String) (Maybe ModuleType) ModuleType
    | PmtyWith ModuleType [WithConstraint]
    | PmtyTypeof ModuleExpr
    | PmtyExtension Extension
    | PmtyAlias (Loc Longident)
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ModuleDeclaration = ModuleDeclaration
    { pmdName :: Loc String
    , pmdType :: ModuleType
    , pmdAttributes :: Attributes
    , pmdLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ModuleDeclaration where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ModuleDeclaration where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ModuleTypeDeclaration = ModuleTypeDeclaration
    { pmtdName :: Loc String
    , pmtdType :: Maybe ModuleType
    , pmtdAttributes :: Attributes
    , pmtdLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ModuleTypeDeclaration where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ModuleTypeDeclaration where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Open Description

data OpenDescription = OpenDescription
    { popenLid :: Loc Longident
    , popenOverride :: OverrideFlag
    , popenLoc :: Location
    , popenAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON OpenDescription where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON OpenDescription where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Include Description

type IncludeDescription = IncludeInfos ModuleType

data IncludeInfos a = IncludeInfos
    { pinclMod :: a
    , pinclLoc :: Location
    , pinclAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON a => ToJSON (IncludeInfos a) where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON a => FromJSON (IncludeInfos a) where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

type IncludeDeclaration =  IncludeInfos ModuleExpr

-- With Constraint

data WithConstraint
    = PwithType (Loc Longident) TypeDeclaration
    | PwithModule (Loc Longident) (Loc Longident)
    | PwithTypeSubst (Loc Longident) TypeDeclaration
    | PwithModSubst (Loc Longident) (Loc Longident)
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)


-- Class Types

type ClassTypeDeclaration = ClassInfos ClassType

data ClassInfos a = ClassInfos
    { pciVirt :: VirtualFlag
    , pciParams :: [(CoreType, Variance)]
    , pciName :: Loc String
    , pciExpr :: a
    , pciLoc :: Location
    , pciAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON a => ToJSON (ClassInfos a) where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON a => FromJSON (ClassInfos a) where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassStructure = ClassStructure
    { pcstrSelf :: Pattern
    , pcstrFields :: [ClassField]
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ClassStructure where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ClassStructure where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassField = ClassField
    { pcfDesc :: ClassFieldDesc
    , pcfLoc :: Location
    , pcfAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ClassField where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ClassField where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassFieldDesc
    = PcfInherit ()
    | PcfVal (Loc Label, MutableFlag, ClassFieldKind)
    | PcfMethod (Loc Label, PrivateFlag, ClassFieldKind)
    | PcfConstraint (CoreType, CoreType)
    | PcfInitializer Expression
    | PcfAttribute Attribute
    | PcfExtension Extension
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ClassFieldKind
    = CfkVirtual CoreType
    | CfkConcrete OverrideFlag Expression
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ClassType = ClassType
    { pctyDesc :: ClassTypeDesc
    , pctyLoc :: Location
    , pctyAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ClassType where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ClassType where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassTypeDesc
    = PctyConstr (Loc Longident) [CoreType]
    | PctySignature ClassSignature
    | PctyArrow ArgLabel CoreType ClassType
    | PctyExtension Extension
    | PctyOpen OverrideFlag (Loc Longident) ClassType
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ClassSignature = ClassSignature
    { pcsigSelf :: CoreType
    , pcsigFields :: [ClassTypeField]
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ClassSignature where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ClassSignature where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassTypeField = ClassTypeField
    { pctfDesc :: ClassTypeFieldDesc
    , pctfLoc :: Location
    , pctfAttributes :: Attributes
    }
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ClassTypeFieldDesc
    = PctfInherit ClassType
    | PctfVal (Loc Label, MutableFlag, VirtualFlag, CoreType)
    | PctfMethod (Loc Label, PrivateFlag, VirtualFlag, CoreType)
    | PctfConstraint (CoreType, CoreType)
    | PctfAttribute Attribute
    | PctfExtension Extension
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ClassExpr = ClassExpr
    { pclDesc :: ClassExprDesc
    , pclLoc :: Location
    , pclAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ClassExpr where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ClassExpr where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ClassExprDesc
    = PclConstr (Loc Longident) [CoreType]
    | PclStructure ClassStructure
    | PclFun ArgLabel (Maybe Expression) Pattern ClassExpr
    | PclApply ClassExpr [(ArgLabel, Expression)]
    | PclLet RecFlag [ValueBinding] ClassExpr
    | PclConstraint ClassExpr ClassType
    | PclExtension Extension
    | PclOpen OverrideFlag (Loc Longident) ClassExpr
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)


-- Expression Types

data Expression = Expression
    { pexpDesc :: ExpressionDesc
    , pexpLoc :: Location
    , pexpAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Expression where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Expression where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ExpressionDesc
  = PexpIdent (Loc Longident)
  | PexpConstant Constant
  | PexpLet RecFlag [ValueBinding] Expression
  | PexpFunction [Case]
  | PexpFun ArgLabel (Maybe Expression) Pattern Expression
  | PexpApply Expression [(ArgLabel, Expression)]
  | PexpMatch Expression [Case]
  | PexpTry Expression [Case]
  | PexpTuple [Expression]
  | PexpConstruct (Loc Longident) (Maybe Expression)
  | PexpVariant Label (Maybe Expression)
  | PexpRecord [(Loc Longident, Expression)] (Maybe Expression)
  | PexpField Expression (Loc Longident)
  | PexpSetfield Expression (Loc Longident) Expression
  | PexpArray [Expression]
  | PexpIfthenelse Expression Expression (Maybe Expression)
  | PexpSequence Expression Expression
  | PexpWhile Expression Expression
  | PexpFor Pattern Expression Expression DirectionFlag Expression
  | PexpConstraint Expression CoreType
  | PexpCoerce Expression (Maybe CoreType) CoreType
  | PexpSend Expression (Loc Label)
  | PexpNew (Loc Longident)
  | PexpSetinstvar (Loc Label) Expression
  | PexpOverride [(Loc Label, Expression)]
  | PexpLetmodule (Loc String) ModuleExpr Expression
  | PexpLetexception ExtensionConstructor Expression
  | PexpAssert Expression
  | PexpLazy Expression
  | PexpPoly Expression (Maybe CoreType)
  | PexpObject ClassStructure
  | PexpNewtype (Loc String) Expression
  | PexpPack ModuleExpr
  | PexpOpen OverrideFlag (Loc Longident) Expression
  | PexpExtension Extension
  | PexpUnreachable
  deriving (Generic, Show, Read, Eq)

instance ToJSON ExpressionDesc where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON ExpressionDesc where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Attributes Types

type Attributes = [Attribute]

type Attribute = (Loc String, Payload)

-- Payload Types

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp CoreType
  | PPat Pattern (Maybe Expression)
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Signature Types

type Signature = [SignatureItem]

data SignatureItem = SignatureItem
    { psigDesc :: SignatureItemDesc
    , psigLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON SignatureItem where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON SignatureItem where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data SignatureItemDesc =
    PsigValue ValueDescription
  | PsigType TypeDeclaration
  | PsigTypext TypeExtension
  | PsigException ExtensionConstructor
  | PsigModule ModuleDeclaration
  | PsigRecmodule [ModuleDeclaration]
  | PsigModtype ModuleTypeDeclaration
  | PsigOpen OpenDescription
  | PsigInclude IncludeDeclaration
  | PsigClass ()
  | PsigClassType ClassTypeDeclaration
  | PsigAttribute Attribute
  | PsigExtension Extension
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- CoreType Types

data CoreType = CoreType
    { ptypDesc :: CoreTypeDesc
    , ptypLoc :: Location
    , ptypAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON CoreType where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON CoreType where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data CoreTypeDesc
  = PtypAny
  | PtypVar String
  | PtypArrow ArgLabel CoreType CoreType
  | PtypTuple [CoreType]
  | PtypConstr (Loc Longident) [CoreType]
  | PtypObject [ObjectField] ClosedFlag
  | PtypClass (Loc Longident) [CoreType]
  | PtypAlias CoreType String
  | PtypVariant [RowField] ClosedFlag (Maybe [Label])
  | PtypPoly [Loc String] CoreType
  | PtypPackage PackageType
  | PtypExtension Extension
  deriving (Generic, Show, Read, Eq)

instance ToJSON CoreTypeDesc where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON CoreTypeDesc where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Package Type

type PackageType = (Loc Longident, [(Loc Longident, CoreType)])

-- Object Field Types

data ObjectField
  = Otag (Loc Label) Attributes CoreType
  | Oinherit CoreType
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Row Field Types

data RowField
  = Rtag (Loc Label) Attributes Bool [CoreType]
  | Rinherit CoreType
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Value Binding Types

data ValueBinding = ValueBinding
    { pvbPat :: Pattern
    , pvbExpr :: Expression
    , pvbAttributes :: Attributes
    , pvbLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ValueBinding where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ValueBinding where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Case Types

data Case = Case
    { pcLhs :: Pattern
    , pcGuard :: Maybe Expression
    , pcRhs :: Expression
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Case where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Case where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Pattern Types

data Pattern = Pattern
    { ppatDesc :: PatternDesc
    , ppatLoc :: Location
    , ppatAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Pattern where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Pattern where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data PatternDesc =
    PpatAny
  | PpatVar (Loc String)
  | PpatAlias Pattern (Loc String)
  | PpatConstant Constant
  | PpatInterval Constant Constant
  | PpatTuple [Pattern]
  | PpatConstruct (Loc Longident) (Maybe Pattern)
  | PpatVariant Label (Maybe Pattern)
  | PpatRecord [(Loc Longident, Pattern)] ClosedFlag
  | PpatArray [Pattern]
  | PpatOr Pattern Pattern
  | PpatConstraint Pattern CoreType
  | PpatType (Loc Longident)
  | PpatLazy Pattern
  | PpatUnpack (Loc String)
  | PpatException Pattern
  | PpatExtension Extension
  | PpatOpen (Loc Longident) Pattern
  deriving (Generic, Show, Read, Eq)

instance ToJSON PatternDesc where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON PatternDesc where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}


-- Extension Types

data Extension = Extension (Loc String) Payload
  deriving (Generic, Show, Read, Eq)

instance ToJSON Extension where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON Extension where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ExtensionConstructor = ExtensionConstructor
  {
    pextName :: Loc String
  , pextKind :: ExtensionConstructorKind
  , pextLoc :: Location
  , pextAttributes :: Attributes
  }
  deriving (Generic, Show, Read, Eq)

instance ToJSON ExtensionConstructor where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ExtensionConstructor whereConstructorArguments
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ExtensionConstructorKind =
    PextDecl ConstructorArguments (Maybe CoreType)
  | PextRebind (Loc Longident)
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)


-- Constructor Declaration and Arguments Types

data ConstructorDeclaration = ConstructorDeclaration
    { pcdName :: Loc String
    , pcdArgs :: ConstructorArguments
    , pcdRes :: Maybe CoreType
    , pcdLoc :: Location
    , pcdAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ConstructorDeclaration where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ConstructorDeclaration where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

data ConstructorArguments
    = PcstrTuple [CoreType]
    | PcstrRecord [LabelDeclaration]
    deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- Value Description Types

data ValueDescription = ValueDescription
    { pvalName :: Loc String
    , pvalType :: CoreType
    , pvalPrim :: [String]
    , pvalAttributes :: Attributes
    , pvalLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON ValueDescription where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON ValueDescription where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Type Declaration Types

data TypeDeclaration = TypeDeclaration
    { ptypeName :: Loc String
    , ptypeParams :: [(CoreType, Variance)]
    , ptypeCstrs :: [(CoreType, CoreType, Location)]
    , ptypeKind :: TypeKind
    , ptypePrivate :: PrivateFlag
    , ptypeManifest :: Maybe CoreType
    , ptypeAttributes :: Attributes
    , ptypeLoc :: Location
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON TypeDeclaration where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON TypeDeclaration where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Type Extension Types

data TypeExtension = TypeExtension
    { ptyextPath :: Loc Longident
    , ptyextParams :: [(CoreType, Variance)]
    , ptyextConstructors :: [ExtensionConstructor]
    , ptyextPrivate :: PrivateFlag
    , ptyextAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON TypeExtension where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON TypeExtension where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- TypeKind Types

data TypeKind
  = PtypeAbstract
  | PtypeVariant [ConstructorDeclaration]
  | PtypeRecord [LabelDeclaration]
  | PtypeOpen
  deriving (Generic, Show, Read, Eq)

instance ToJSON TypeKind where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON TypeKind where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Label Declaration Types

data LabelDeclaration = LabelDeclaration
    { pldName :: Loc String
    , pldMutable :: MutableFlag
    , pldType :: CoreType
    , pldLoc :: Location
    , pldAttributes :: Attributes
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON LabelDeclaration where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON LabelDeclaration where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- AST TYPES --

-- Constant Types

data Constant =
    PconstInteger String (Maybe Char)
  | PconstChar Int
  | PconstString String (Maybe String)
  | PconstFloat String (Maybe Char)
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

-- RecFlag Types

data RecFlag = Nonrecursive | Recursive
  deriving (Generic, Show, Read, Eq)

instance ToJSON RecFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON RecFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Direction Flag Types

data DirectionFlag = Upto | Downto
  deriving (Generic, Show, Read, Eq)

instance ToJSON DirectionFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON DirectionFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Private Flag Types

data PrivateFlag
  = Private
  | Public
  deriving (Generic, Show, Read, Eq)

instance ToJSON PrivateFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON PrivateFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Mutable Flag Types

data MutableFlag = Immutable | Mutable
  deriving (Generic, Show, Read, Eq)

instance ToJSON MutableFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON MutableFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Virtual Flag Types

data VirtualFlag = Virtual | Concrete
  deriving (Generic, Show, Read, Eq)

instance ToJSON VirtualFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON VirtualFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Override Flag Types

data OverrideFlag = Override | Fresh
  deriving (Generic, Show, Read, Eq)

instance ToJSON OverrideFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON OverrideFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Closed Flag Types

data ClosedFlag
  = Closed
  | Open
  deriving (Generic, Show, Read, Eq)

instance ToJSON ClosedFlag where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON ClosedFlag where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Label

type Label = String

-- ArgLabel Types

data ArgLabel
  = Nolabel
  | Labelled String
  | Optional String
  deriving (Generic, Show, Read, Eq)

instance ToJSON ArgLabel where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON ArgLabel where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}

-- Loc Type

data Loc a = Loc
    { txt :: a
    , loc :: Location
    }
  deriving (Generic, Show, Read, Eq)

instance ToJSON a => ToJSON (Loc a) where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON a => FromJSON (Loc a) where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

-- Variance Types

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Generic, Show, Read, Eq)

instance ToJSON Variance where
  toJSON = genericToJSON $ defaultOptions {allNullaryToStringTag = False}

instance FromJSON Variance where
  parseJSON = genericParseJSON $ defaultOptions {allNullaryToStringTag = False}


-- TESTING --

importParseTree :: IO ()
importParseTree = do
  jsonData <- readFile "rescript_parsetree.json"
  -- print jsonData
  let parseTree = eitherDecode $ LBS.fromStrict $ TE.encodeUtf8 $ T.pack jsonData :: Either String Structure
  case parseTree of
    Left err -> print err
    Right val -> do
      print "Parsed Success"
      print (typeOf val)

-- Testing ToJSON and FromJSON instances

type DummyTuple = (String, String)

data TestJSON = SomeCons
  deriving (Generic, Show, Read, Eq)

instance ToJSON TestJSON where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True,  allNullaryToStringTag = False}

instance FromJSON TestJSON where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True, allNullaryToStringTag = False}

-- encodeVal :: LBS.ByteString
-- encodeVal = encode (SomeCons (TestJSON2 {key1 = "hello", key2 = Nothing} :: TestJSON2 String))

encodeVal :: LBS.ByteString
encodeVal = encode (SomeCons)

decodeVal :: Maybe TestJSON
decodeVal = decode $ LBS.fromStrict $ TE.encodeUtf8 $ T.pack "{\"contents\":[\"Hello\",\"Bye\"],\"tag\":\"SomeCons\"}"

data TestJSON2 a = TestJSON2 {
  key1 :: String,
  key2 :: Maybe a
}
  deriving (Generic, Show, Read, Eq)

instance ToJSON a => ToJSON (TestJSON2 a) where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

instance FromJSON a => FromJSON (TestJSON2 a) where
  parseJSON = genericParseJSON $ defaultOptions {tagSingleConstructors = True}

encodeVal2 :: LBS.ByteString
encodeVal2 = encode (TestJSON2 { key1 = "foo", key2 = Nothing } :: TestJSON2 String)

decodeVal2 :: Maybe (TestJSON2 String)
decodeVal2 = decode $ LBS.fromStrict $ TE.encodeUtf8 $ T.pack "{\"key1\":\"foo\",\"key2\":null,\"tag\":\"TestJSON2\"}"
