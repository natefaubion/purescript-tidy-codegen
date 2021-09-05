module PureScript.CST.Codegen.Monad where

import Prelude

import Control.Monad.Free (Free, runFree)
import Control.Monad.State (class MonadTrans, StateT, modify_, runStateT, state)
import Data.Array as Array
import Data.Foldable (fold, for_)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import PureScript.CST.Codegen (module_)
import PureScript.CST.Codegen as Codegen
import PureScript.CST.Codegen.Class (class ToModuleName, class ToName, class ToToken, toModuleName, toToken)
import PureScript.CST.Codegen.Common (toSourceToken)
import PureScript.CST.Codegen.Types (Qualified(..), SymbolName(..))
import PureScript.CST.Types (Declaration(..), Export, Foreign(..), Ident, Import, Labeled(..), Module, ModuleName, Name(..), Operator(..), Proper, QualifiedName(..))
import Safe.Coerce (coerce)

data CodegenExport
  = CodegenExportType Boolean Proper
  | CodegenExportTypeOp SymbolName
  | CodegenExportClass Proper
  | CodegenExportValue Ident
  | CodegenExportOp SymbolName
  | CodegenExportModule ModuleName

derive instance Eq CodegenExport
derive instance Ord CodegenExport

data CodegenImport
  = CodegenImportType Boolean Proper
  | CodegenImportTypeOp SymbolName
  | CodegenImportClass Proper
  | CodegenImportValue Ident
  | CodegenImportOp SymbolName

derive instance Eq CodegenImport
derive instance Ord CodegenImport

type CodegenState e =
  { exports :: Set CodegenExport
  , importsOpen :: Set ModuleName
  , importsFrom :: Map ModuleName (Set (CodegenImport))
  , importsQualified :: Map ModuleName (Set ModuleName)
  , declarations :: List (Declaration e)
  }

newtype CodegenT e m a = CodegenT (StateT (CodegenState e) m a)

derive newtype instance Functor m => Functor (CodegenT e m)
derive newtype instance Monad m => Apply (CodegenT e m)
derive newtype instance Monad m => Applicative (CodegenT e m)
derive newtype instance Monad m => Bind (CodegenT e m)
derive newtype instance Monad m => Monad (CodegenT e m)
derive newtype instance MonadTrans (CodegenT e)

type Codegen e = CodegenT e (Free Identity)

export :: forall e m. Monad m => CodegenExport -> CodegenT e m Unit
export exp = CodegenT $ modify_ \st -> st
  { exports = case exp of
      CodegenExportType false n | Set.member (CodegenExportType true n) st.exports ->
        st.exports
      CodegenExportType true n ->
        Set.insert exp $ Set.delete (CodegenExportType false n) st.exports
      _ ->
        Set.insert exp st.exports
  }

exportValue :: forall e m name. Monad m => ToToken name Ident => name -> CodegenT e m Unit
exportValue = export <<< CodegenExportValue <<< snd <<< toToken

exportOp :: forall e m name. Monad m => ToToken name SymbolName => name -> CodegenT e m Unit
exportOp = export <<< CodegenExportOp <<< snd <<< toToken

exportType :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportType = export <<< CodegenExportType false <<< snd <<< toToken

exportTypeAll :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportTypeAll = export <<< CodegenExportType true <<< snd <<< toToken

exportTypeOp :: forall e m name. Monad m => ToToken name SymbolName => name -> CodegenT e m Unit
exportTypeOp = export <<< CodegenExportTypeOp <<< snd <<< toToken

exportClass :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportClass = export <<< CodegenExportClass <<< snd <<< toToken

exportModule :: forall e m name. Monad m => ToToken name ModuleName => name -> CodegenT e m Unit
exportModule = export <<< CodegenExportModule <<< snd <<< toToken

write :: forall m e. Monad m => Declaration e -> CodegenT e m Unit
write decl = CodegenT $ modify_ \st -> st { declarations = List.Cons decl st.declarations }

writeAndExport :: forall m e. Monad m => Declaration e -> CodegenT e m Unit
writeAndExport decl = do
  write decl
  case decl of
    DeclData { name: Name { name } } Nothing ->
      exportType name
    DeclData { name: Name { name } } (Just _) ->
      exportTypeAll name
    DeclType { name: Name { name } } _ _ ->
      exportType name
    DeclNewtype { name: Name { name } } _ _ _ ->
      exportTypeAll name
    DeclClass { name: Name { name } } members -> do
      exportClass name
      for_ members \(Tuple _ ms) ->
        for_ ms \(Labeled { label: Name { name: label } }) ->
          exportValue label
    DeclKindSignature _ (Labeled { label: Name { name } }) ->
      exportType name
    DeclSignature (Labeled { label: Name { name } }) ->
      exportValue name
    DeclValue { name: Name { name } } ->
      exportValue name
    DeclForeign _ _ (ForeignData _ (Labeled { label: Name { name } })) ->
      exportType name
    DeclForeign _ _ (ForeignValue (Labeled { label: Name { name } })) ->
      exportValue name
    _ ->
      pure unit

exporting :: forall e m a. Monad m => CodegenT e m a -> CodegenT e m a
exporting m = do
  old <- CodegenT $ state \st ->
    Tuple st.declarations st { declarations = List.Nil }
  res <- m
  new <- CodegenT $ state \st ->
    Tuple st.declarations st { declarations = old }
  for_ (List.reverse new) writeAndExport
  pure res

data ImportName name = ImportName CodegenImport (QualifiedName name)

importFrom
  :: forall e m mod name
   . Monad m
  => ToModuleName mod
  => mod
  -> ImportName name
  -> CodegenT e m (QualifiedName name)
importFrom mod (ImportName imp qn@(QualifiedName { module: mbMod })) =
  CodegenT $ state \st -> do
    Tuple qn $ case mbMod of
      Nothing -> st
        { importsFrom = Map.alter
            case imp, _ of
              CodegenImportType true n, Just is ->
                Just $ Set.insert imp $ Set.delete (CodegenImportType false n) is
              CodegenImportType false n, Just is | Set.member (CodegenImportType true n) is ->
                Just is
              _, Just is ->
                Just $ Set.insert imp is
              _, Nothing ->
                Just $ Set.singleton imp
            (toModuleName mod)
            st.importsFrom
        }
      Just qualMod -> st
        { importsQualified = Map.alter
            case _ of
              Nothing -> Just $ Set.singleton qualMod
              Just ms -> Just $ Set.insert qualMod ms
            (toModuleName mod)
            st.importsQualified
        }

importOpen :: forall e m mod. Monad m => ToModuleName mod => mod -> CodegenT e m Unit
importOpen mod = CodegenT $ modify_ \st ->
  st { importsOpen = Set.insert (toModuleName mod) st.importsOpen }

withQualifiedName :: forall from to r. ToToken from (Qualified to) => (to -> QualifiedName to -> r) -> from -> r
withQualifiedName k from = do
  let (Tuple token (Qualified mn name)) = toToken from
  k name (QualifiedName { module: mn, name, token: toSourceToken token })

importValue :: forall name. ToToken name (Qualified Ident) => name -> ImportName Ident
importValue = withQualifiedName (ImportName <<< CodegenImportValue)

importOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportName Operator
importOp = withQualifiedName \a b -> ImportName (CodegenImportOp a) (coerce b)

importTypeOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportName Operator
importTypeOp = withQualifiedName \a b -> ImportName (CodegenImportTypeOp a) (coerce b)

importClass :: forall name. ToToken name (Qualified Proper) => name -> ImportName Proper
importClass = withQualifiedName (ImportName <<< CodegenImportClass)

importType :: forall name. ToToken name (Qualified Proper) => name -> ImportName Proper
importType = withQualifiedName (ImportName <<< CodegenImportType false)

importCtor :: forall ty ctor. ToToken ty Proper => ToToken ctor (Qualified Proper) => ty -> ctor -> ImportName Proper
importCtor = withQualifiedName <<< const <<< ImportName <<< CodegenImportType true <<< snd <<< toToken

runCodegenT :: forall m e a. CodegenT e m a -> m (Tuple a (CodegenState e))
runCodegenT (CodegenT m) = runStateT m
  { exports: Set.empty
  , importsOpen: Set.empty
  , importsFrom: Map.empty
  , importsQualified: Map.empty
  , declarations: List.Nil
  }

runCodegenTModule :: forall e m a name. Functor m => ToName name ModuleName => name -> CodegenT e m a -> m (Tuple a (Module e))
runCodegenTModule name = map (map (moduleFromCodegenState name)) <<< runCodegenT

codegenModule :: forall e name. ToName name ModuleName => name -> Codegen e Unit -> Module e
codegenModule name = snd <<< runFree coerce <<< runCodegenTModule name

moduleFromCodegenState :: forall e name. ToName name ModuleName => name -> CodegenState e -> Module e
moduleFromCodegenState name st = module_ name exports (importsOpen <> importsNamed) decls
  where
  decls =
    st.declarations
      # List.reverse
      # Array.fromFoldable

  exports =
    st.exports
      # Array.fromFoldable
      # map codegenExportToCST

  importsOpen =
    st.importsOpen
      # Array.fromFoldable
      # map (flip Codegen.declImport [])
      # withLeadingBreaks

  importsFrom =
    st.importsFrom
      # Map.toUnfoldable
      # map (map (map codegenImportToCST <<< Array.fromFoldable))

  importsNamed = withLeadingBreaks do
    Tuple mn imps <- importsFrom
    case Map.lookup mn st.importsQualified of
      Nothing ->
        pure $ Codegen.declImport mn imps
      Just quals ->
        Array.cons (Codegen.declImport mn imps)
          $ Codegen.declImportAs mn [] <$> Array.fromFoldable quals

  withLeadingBreaks =
    fold <<< Array.modifyAt 0 (Codegen.leading (Codegen.lineBreaks 2))

codegenExportToCST :: forall e. CodegenExport -> Export e
codegenExportToCST = case _ of
  CodegenExportType all name ->
    if all then Codegen.exportTypeAll name
    else Codegen.exportType name
  CodegenExportTypeOp name -> Codegen.exportTypeOp name
  CodegenExportClass name -> Codegen.exportClass name
  CodegenExportValue name -> Codegen.exportValue name
  CodegenExportOp name -> Codegen.exportOp name
  CodegenExportModule name -> Codegen.exportModule name

codegenImportToCST :: forall e. CodegenImport -> Import e
codegenImportToCST = case _ of
  CodegenImportType all name ->
    if all then Codegen.importTypeAll name
    else Codegen.importType name
  CodegenImportTypeOp name -> Codegen.importTypeOp name
  CodegenImportClass name -> Codegen.importClass name
  CodegenImportValue name -> Codegen.importValue name
  CodegenImportOp name -> Codegen.importOp name
