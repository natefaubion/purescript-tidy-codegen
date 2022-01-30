module Tidy.Codegen.Monad
  ( CodegenState
  , CodegenT(..)
  , Codegen
  , CodegenExport(..)
  , CodegenImport(..)
  , ImportName(..)
  , write
  , writeAndExport
  , exportValue
  , exportOp
  , exportType
  , exportTypeAll
  , exportTypeOp
  , exportClass
  , exportModule
  , exporting
  , importFrom
  , importOpen
  , importValue
  , importOp
  , importType
  , importTypeAll
  , importTypeOp
  , importClass
  , importCtor
  , codegenModule
  , runCodegenT
  , runCodegenTModule
  , moduleFromCodegenState
  , class ToImportFrom
  , toImportFrom
  , class ToImportFromRecord
  , toImportFromRecord
  ) where

import Prelude

import Control.Monad.Free (Free, runFree)
import Control.Monad.State (class MonadTrans, StateT, modify_, runStateT, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadTell)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (fold, for_, traverse_)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import PureScript.CST.Types (Declaration(..), Export, Foreign(..), Ident, Import, Labeled(..), Module, ModuleName, Name(..), Operator, Proper, QualifiedName(..))
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Tidy.Codegen (module_)
import Tidy.Codegen as Codegen
import Tidy.Codegen.Class (class ToModuleName, class ToName, class ToToken, toModuleName, toQualifiedName, toToken)
import Tidy.Codegen.Common (toSourceToken)
import Tidy.Codegen.Types (Qualified(..), SymbolName)
import Type.Proxy (Proxy(..))

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

-- | A Monad transformer which tracks module imports/exports. With this, you
-- | can define codegen procedures in a modular way without having to manually
-- | calculate imports or do post-traversals.
newtype CodegenT e m a = CodegenT (StateT (CodegenState e) m a)

derive newtype instance Functor m => Functor (CodegenT e m)
derive newtype instance Monad m => Apply (CodegenT e m)
derive newtype instance Monad m => Applicative (CodegenT e m)
derive newtype instance Monad m => Bind (CodegenT e m)
derive newtype instance Monad m => Monad (CodegenT e m)
derive newtype instance MonadTrans (CodegenT e)

instance Monad m => MonadTell (Array (Declaration e)) (CodegenT e m) where
  tell = traverse_ write

instance monadEffectCodegen :: MonadEffect m => MonadEffect (CodegenT e m) where
  liftEffect = lift <<< liftEffect

type Codegen e = CodegenT e (Free Identity)

-- | Exports a specific reference.
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

-- | Exports a value.
exportValue :: forall e m name. Monad m => ToToken name Ident => name -> CodegenT e m Unit
exportValue = export <<< CodegenExportValue <<< snd <<< toToken

-- | Exports an operator.
exportOp :: forall e m name. Monad m => ToToken name SymbolName => name -> CodegenT e m Unit
exportOp = export <<< CodegenExportOp <<< snd <<< toToken

-- | Exports a type.
exportType :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportType = export <<< CodegenExportType false <<< snd <<< toToken

-- | Exports a type with all data members.
exportTypeAll :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportTypeAll = export <<< CodegenExportType true <<< snd <<< toToken

-- | Exports a type operator.
exportTypeOp :: forall e m name. Monad m => ToToken name SymbolName => name -> CodegenT e m Unit
exportTypeOp = export <<< CodegenExportTypeOp <<< snd <<< toToken

-- | Exports a class.
exportClass :: forall e m name. Monad m => ToToken name Proper => name -> CodegenT e m Unit
exportClass = export <<< CodegenExportClass <<< snd <<< toToken

-- | Exports a module re-export.
exportModule :: forall e m name. Monad m => ToToken name ModuleName => name -> CodegenT e m Unit
exportModule = export <<< CodegenExportModule <<< snd <<< toToken

-- | Writes a declaration to the module.
write :: forall m e. Monad m => Declaration e -> CodegenT e m Unit
write decl = CodegenT $ modify_ \st -> st { declarations = List.Cons decl st.declarations }

-- | Writes a declaration and exports it.
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

-- | Exports all declarations written within the provided block.
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

-- | Imports from a particular module, yielding a `QualifiedName` which can be
-- | used with the `Tidy.Codegen` constructors. If the requested import is
-- | qualified, an appropriate qualified import will be generated.
-- |
-- | ```purescript
-- | example = do
-- |   -- Generates a `import Effect.Class.Console as Console` import
-- |   consoleLog <- importFrom "Effect.Class.Console" (importValue "Console.log")
-- |   -- Generates a `import Data.Map (Map)` import
-- |   mapType <- importFrom "Data.Map" (importType "Map")
-- |   -- Group multiple imports with a record
-- |   dataMap <- import From "Data.Map"
-- |     { type: importType "Map"
-- |     , lookup: importValue "Map.lookup"
-- |     }
-- |   ...
-- | ```
importFrom
  :: forall e m mod name imp
   . Monad m
  => ToModuleName mod
  => ToImportFrom name imp
  => mod
  -> name
  -> CodegenT e m imp
importFrom mod = toImportFrom \(ImportName imp qn@(QualifiedName { module: mbMod })) ->
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

-- | Imports a module with an open import.
-- |
-- | ```purescript
-- | example = do
-- |   importOpen "Prelude"
-- |   ...
-- | ```
importOpen :: forall e m mod. Monad m => ToModuleName mod => mod -> CodegenT e m Unit
importOpen mod = CodegenT $ modify_ \st ->
  st { importsOpen = Set.insert (toModuleName mod) st.importsOpen }

withQualifiedName :: forall from to r. ToToken from (Qualified to) => (to -> QualifiedName to -> r) -> from -> r
withQualifiedName k from = do
  let (Tuple token (Qualified mn name)) = toToken from
  k name (QualifiedName { module: mn, name, token: toSourceToken token })

-- | Imports a value. Use with `importFrom`.
importValue :: forall name. ToToken name (Qualified Ident) => name -> ImportName Ident
importValue = withQualifiedName (ImportName <<< CodegenImportValue)

-- | Imports a value operator, yield. Use with `importFrom`.
importOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportName Operator
importOp = withQualifiedName \a b -> ImportName (CodegenImportOp a) (toQualifiedName b)

-- | Imports a type operator. Use with `importFrom`.
importTypeOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportName Operator
importTypeOp = withQualifiedName \a b -> ImportName (CodegenImportTypeOp a) (toQualifiedName b)

-- | Imports a class. Use with `importFrom`.
importClass :: forall name. ToToken name (Qualified Proper) => name -> ImportName Proper
importClass = withQualifiedName (ImportName <<< CodegenImportClass)

-- | Imports a type. Use with `importFrom`.
importType :: forall name. ToToken name (Qualified Proper) => name -> ImportName Proper
importType = withQualifiedName (ImportName <<< CodegenImportType false)

-- | Imports a type. Use with `importFrom`.
importTypeAll :: forall name. ToToken name (Qualified Proper) => name -> ImportName Proper
importTypeAll = withQualifiedName (ImportName <<< CodegenImportType true)

-- | Imports a data constructor for a type. Use with `importFrom`.
-- |
-- | ```purescript
-- | example = do
-- |   just <- importFrom "Data.Maybe" (importCtor "Maybe" "Just")
-- | ```
importCtor :: forall ty ctor. ToToken ty Proper => ToToken ctor (Qualified Proper) => ty -> ctor -> ImportName Proper
importCtor = withQualifiedName <<< const <<< ImportName <<< CodegenImportType true <<< snd <<< toToken

-- | Extracts codegen state and the produced value.
runCodegenT :: forall m e a. CodegenT e m a -> m (Tuple a (CodegenState e))
runCodegenT (CodegenT m) = runStateT m
  { exports: Set.empty
  , importsOpen: Set.empty
  , importsFrom: Map.empty
  , importsQualified: Map.empty
  , declarations: List.Nil
  }

-- | Extracts a CST Module and the produced value.
runCodegenTModule :: forall e m a name. Functor m => ToName name ModuleName => name -> CodegenT e m a -> m (Tuple a (Module e))
runCodegenTModule name = map (map (moduleFromCodegenState name)) <<< runCodegenT

-- | Extracts a CST Module.
codegenModule :: forall e name. ToName name ModuleName => name -> Codegen e Unit -> Module e
codegenModule name = snd <<< runFree coerce <<< runCodegenTModule name

-- | Constructs a CST Module from codegen state.
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

  importsFrom = do
    Tuple mn imps <- Map.toUnfoldable st.importsFrom
    pure $ Tuple mn $ Codegen.declImport mn $ codegenImportToCST <$> Set.toUnfoldable imps

  importsQualified = do
    Tuple mn quals <- Map.toUnfoldable st.importsQualified
    qual <- Set.toUnfoldable quals
    pure $ Tuple mn $ Codegen.declImportAs mn [] qual

  importsNamed = withLeadingBreaks do
    (map (map Left) importsFrom <> map (map Right) importsQualified)
      # Array.sortBy (comparing fst)
      # map (either identity identity <<< snd)

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

type ImportResolver f = forall n. ImportName n -> f (QualifiedName n)

class ToImportFrom name imp | name -> imp where
  toImportFrom :: forall f. Applicative f => ImportResolver f -> name -> f imp

instance ToImportFrom (ImportName name) (QualifiedName name) where
  toImportFrom = ($)

instance
  ( RowToList rin rl
  , ToImportFromRecord rl (Record rin) (Record rout)
  ) =>
  ToImportFrom (Record rin) (Record rout) where
  toImportFrom k = map Builder.buildFromScratch <<< toImportFromRecord k (Proxy :: _ rl)

class ToImportFromRecord (rl :: RowList Type) rin rout | rl rin -> rout where
  toImportFromRecord :: forall f. Applicative f => ImportResolver f -> Proxy rl -> rin -> f (Builder {} rout)

instance
  ( ToImportFrom val imp
  , ToImportFromRecord rest (Record rin) (Record rout')
  , IsSymbol sym
  , Row.Cons sym val rx rin
  , Row.Cons sym imp rout' rout
  , Row.Lacks sym rout'
  ) =>
  ToImportFromRecord (RowList.Cons sym val rest) (Record rin) (Record rout) where
  toImportFromRecord k _ r =
    (\imp builder -> Builder.insert (Proxy :: _ sym) imp <<< builder)
      <$> toImportFrom k (Record.get (Proxy :: _ sym) r)
      <*> toImportFromRecord k (Proxy :: _ rest) r

instance ToImportFromRecord (RowList.Nil) (Record rin) {} where
  toImportFromRecord _ _ _ = pure identity
