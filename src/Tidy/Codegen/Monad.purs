module Tidy.Codegen.Monad
  ( UnqualifiedImportModule(..)
  , CodegenState
  , CodegenT(..)
  , Codegen
  , CodegenExport(..)
  , CodegenImport(..)
  , ImportName(..)
  , ImportFromValue
  , ImportFromType
  , ImportFromTypeOp
  , ImportFromOp
  , ImportFromClass
  , ImportFromCtor
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
  , importFromAlias
  , importOpen
  , importOpenHiding
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
import Prim hiding (Type)

import Control.Monad.Free (Free, runFree)
import Control.Monad.ST.Class (class MonadST, liftST)
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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect, liftEffect)
import Prim as Prim
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import PureScript.CST.Types (Declaration(..), Export, Expr, Foreign(..), Ident, Import, Labeled(..), Module, ModuleName, Name(..), Operator, Proper, QualifiedName(..), Type)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Tidy.Codegen (binaryOp, exprCtor, exprIdent, exprOpName, module_, typeCtor, typeOpName)
import Tidy.Codegen as Codegen
import Tidy.Codegen.Class (class ToModuleName, class ToName, class ToToken, toModuleName, toQualifiedName, toToken)
import Tidy.Codegen.Common (toSourceToken)
import Tidy.Codegen.Types (BinaryOp, Qualified(..), SymbolName)
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

-- | `import Foo` = `OpenHiding Set.empty`
-- | `import Foo hiding (bar)` = `OpenHiding (Set.singleton bar)`
-- | `import Foo (bar)` = `ClosedImporting (NES.singleton bar)`
-- |
-- | If a module is imported multiple times unqualified,
-- | import forms on the left will be overridden by import forms on the right
-- | ```
-- | ClosedImport _ < OpenImport Set.empty < OpenImport nonEmptySet
-- | ```
data UnqualifiedImportModule
  = ClosedImporting (NonEmptySet CodegenImport)
  | OpenHiding (Set CodegenImport)

derive instance eqUnqualifiedImportModule :: Eq UnqualifiedImportModule
derive instance ordUnqualifiedImportModule :: Ord UnqualifiedImportModule

type CodegenState e =
  { exports :: Set CodegenExport
  , importsUnqualified :: Map ModuleName UnqualifiedImportModule
  -- | `import Foo as Bar` = `Map.singleton "Foo" $ Map.singleton "Bar" Nothing`
  -- | `import Foo (baz) as Bar` = `Map.singleton "Foo" $ Map.singleton "Bar" $ Just $ NES.singleton "baz"`
  -- |
  -- | If a module is imported multiple times qualified,
  -- | import forms on the left will be overridden by import forms on the right
  -- | ```
  -- | let map1 = Map.singleton
  -- | (map1 "foo" (map1 "bar" $ Just _)) < (map1 "foo" (map1 "bar" Nothing))
  -- | ```
  , importsQualified :: Map ModuleName (Map ModuleName (Maybe (NonEmptySet CodegenImport)))
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

instance MonadEffect m => MonadEffect (CodegenT e m) where
  liftEffect = lift <<< liftEffect

instance (Monad m, MonadST h m) => MonadST h (CodegenT e m) where
  liftST = lift <<< liftST

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
        { importsUnqualified = Map.alter
            case _ of
              Nothing ->
                Just $ ClosedImporting $ NES.singleton imp
              is@(Just (OpenHiding _)) ->
                is
              is@(Just (ClosedImporting closedImports)) ->
                case imp of
                  CodegenImportType true n ->
                    Just $ ClosedImporting
                      $ maybe (NES.singleton imp) (NES.insert imp)
                      $ NES.delete (CodegenImportType false n) closedImports
                  CodegenImportType false n | NES.member (CodegenImportType true n) closedImports ->
                    is
                  _ ->
                    Just $ ClosedImporting $ NES.insert imp closedImports
            (toModuleName mod)
            st.importsUnqualified
        }
      Just qualMod -> st
        { importsQualified = Map.alter
            case _ of
              Nothing -> Just $ Map.singleton qualMod Nothing
              Just aliases -> Just $ Map.alter
                (const $ Just Nothing)
                qualMod
                aliases
            (toModuleName mod)
            st.importsQualified
        }

-- | Imports from a particular module, guaranteeing that the
-- | imported members are always referenced via the given qualifier.
-- |
-- | ```purescript
-- | example = do
-- |   -- Generates a `import Effect.Class.Console (log) as Console` import
-- |   consoleLog <- importFromAlias "Effect.Class.Console" "Console" $ importValue "log"
-- |   -- Generates a `import Data.Map (Map) as MyMap` import
-- |   mapType <- importFromAlias "Data.Map" "MyMap" $ importType "Something.Map"
-- |   -- Group multiple imports with a record
-- |   dataMap <- importFromAlias "Data.Map" "Map2"
-- |     { type: importType "Map"
-- |     , lookup: importValue "Map.lookup"
-- |     }
-- |   ...
-- | ```
importFromAlias
  :: forall e m mod alias name imp
   . Monad m
  => ToModuleName mod
  => ToModuleName alias
  => ToImportFrom name imp
  => mod
  -> alias
  -> name
  -> CodegenT e m imp
importFromAlias mod alias = toImportFrom \(ImportName imp (QualifiedName qnRec)) -> do
  let
    qualMod = toModuleName alias
    qn = QualifiedName $ qnRec { module = Just qualMod }
  CodegenT $ state \st -> do
    Tuple qn $ st
      { importsQualified = Map.alter
            case _ of
              Nothing -> Just $ Map.singleton qualMod $ Just $ NES.singleton imp
              Just aliases -> Just $ Map.alter
                case _ of
                  Nothing ->
                    Just $ Just $ NES.singleton imp
                  is@(Just Nothing) ->
                    is
                  is@(Just (Just explicitImports)) ->
                    case imp of
                      CodegenImportType true n ->
                        Just $ Just $ maybe (NES.singleton imp) (NES.insert imp)
                          $ NES.delete (CodegenImportType false n) explicitImports
                      CodegenImportType false n | NES.member (CodegenImportType true n) explicitImports ->
                        is
                      _ ->
                        Just $ Just $ NES.insert imp explicitImports
                qualMod
                aliases
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
  st
    { importsUnqualified = Map.alter
        case _ of
          Nothing ->
            Just $ OpenHiding Set.empty
          Just (ClosedImporting _) ->
            Just $ OpenHiding Set.empty
          alreadyOpenAndHidingImports ->
            alreadyOpenAndHidingImports
        (toModuleName mod)
        st.importsUnqualified
    }

-- | Imports a module as an open import with imported members hidden.
-- |
-- | ```purescript
-- | example = do
-- |   importOpenHiding "Prim"
-- |     { someType: importType "Type" }
-- |   ...
-- | ```
importOpenHiding
  :: forall e m mod name imp
   . Monad m
  => ToModuleName mod
  => ToImportFrom name imp
  => mod
  -> name
  -> CodegenT e m Unit
importOpenHiding mod = void <<< toImportFrom \(ImportName imp qn) ->
  CodegenT $ state \st -> do
    Tuple qn $ st
        { importsUnqualified = Map.alter
            case _ of
              is@(Just (OpenHiding hiddenImports)) ->
                case imp of
                  CodegenImportType true n ->
                    Just $ OpenHiding
                      $ Set.insert imp
                      $ Set.delete (CodegenImportType false n) hiddenImports
                  CodegenImportType false n | Set.member (CodegenImportType true n) hiddenImports ->
                    is
                  _ ->
                    Just $ OpenHiding $ Set.insert imp hiddenImports
              _ ->
                Just $ OpenHiding $ Set.singleton imp
            (toModuleName mod)
            st.importsUnqualified
        }

withQualifiedName :: forall from to r. ToToken from (Qualified to) => (to -> QualifiedName to -> r) -> from -> r
withQualifiedName k from = do
  let (Tuple token (Qualified mn name)) = toToken from
  k name (QualifiedName { module: mn, name, token: toSourceToken token })

-- | Imports a value. Use with `importFrom`.
importValue :: forall name. ToToken name (Qualified Ident) => name -> ImportFromValue
importValue = ImportFromValue <<< withQualifiedName (ImportName <<< CodegenImportValue)

-- | Imports a value operator, yield. Use with `importFrom`.
importOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportFromOp
importOp = ImportFromOp <<< withQualifiedName \a b -> ImportName (CodegenImportOp a) (toQualifiedName b)

-- | Imports a type operator. Use with `importFrom`.
importTypeOp :: forall name. ToToken name (Qualified SymbolName) => name -> ImportFromTypeOp
importTypeOp = ImportFromTypeOp <<< withQualifiedName \a b -> ImportName (CodegenImportTypeOp a) (toQualifiedName b)

-- | Imports a class. Use with `importFrom`.
importClass :: forall name. ToToken name (Qualified Proper) => name -> ImportFromClass
importClass = ImportFromClass <<< withQualifiedName (ImportName <<< CodegenImportClass)

-- | Imports a type. Use with `importFrom`.
importType :: forall name. ToToken name (Qualified Proper) => name -> ImportFromType
importType = ImportFromType <<< withQualifiedName (ImportName <<< CodegenImportType false)

-- | Imports a type. Use with `importFrom`.
importTypeAll :: forall name. ToToken name (Qualified Proper) => name -> ImportFromCtor
importTypeAll = ImportFromCtor <<< withQualifiedName (ImportName <<< CodegenImportType true)

-- | Imports a data constructor for a type. Use with `importFrom`.
-- |
-- | ```purescript
-- | example = do
-- |   just <- importFrom "Data.Maybe" (importCtor "Maybe" "Just")
-- | ```
importCtor :: forall ty ctor. ToToken ty Proper => ToToken ctor (Qualified Proper) => ty -> ctor -> ImportFromCtor
importCtor ty ctor = ImportFromCtor $ withQualifiedName (const $ ImportName $ CodegenImportType true $ snd $ toToken ty) ctor

-- | Extracts codegen state and the produced value.
runCodegenT :: forall m e a. CodegenT e m a -> m (Tuple a (CodegenState e))
runCodegenT (CodegenT m) = runStateT m
  { exports: Set.empty
  , importsUnqualified: Map.empty
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

  unqualImports =
    st.importsUnqualified
      # Map.toUnfoldable
      # flip Array.foldl { open: [], closed: [] } (\acc -> case _ of
        Tuple mn (OpenHiding set) ->
          acc
            { open = Array.snoc acc.open $ if Set.isEmpty set then Codegen.declImport mn []
                else Codegen.declImportHiding mn $ codegenImportToCST <$> Set.toUnfoldable set
            }
        Tuple mn (ClosedImporting set) ->
          acc
            { closed = Array.snoc acc.closed $ Tuple mn $ Codegen.declImport mn $ codegenImportToCST <$> NES.toUnfoldable set
            }
      )

  importsOpen = withLeadingBreaks unqualImports.open

  importsQualified = do
    Tuple mn quals <- Map.toUnfoldable st.importsQualified
    Tuple alias explicitImps <- Map.toUnfoldable quals
    pure $ Tuple mn $ Codegen.declImportAs mn (fromMaybe [] $ map (map codegenImportToCST <<< NES.toUnfoldable) explicitImps) alias

  importsNamed = withLeadingBreaks do
    (map (map Left) unqualImports.closed <> map (map Right) importsQualified)
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

newtype ImportFromType = ImportFromType (ImportName Proper)
newtype ImportFromCtor = ImportFromCtor (ImportName Proper)
newtype ImportFromTypeOp = ImportFromTypeOp (ImportName Operator)
newtype ImportFromValue = ImportFromValue (ImportName Ident)
newtype ImportFromOp = ImportFromOp (ImportName Operator)
newtype ImportFromClass = ImportFromClass (ImportName Proper)

type ImportResolver f = forall n. ImportName n -> f (QualifiedName n)

class ToImportFrom name imp | name -> imp where
  toImportFrom :: forall f. Applicative f => ImportResolver f -> name -> f imp

instance ToImportFrom ImportFromValue (Expr e) where
  toImportFrom f (ImportFromValue a) = map exprIdent (f a)

instance ToImportFrom ImportFromType (Type e) where
  toImportFrom f (ImportFromType a) = map typeCtor (f a)

instance ToImportFrom ImportFromCtor (Expr e) where
  toImportFrom f (ImportFromCtor a) = map exprCtor (f a)

instance ToImportFrom ImportFromClass (Type e) where
  toImportFrom f (ImportFromClass a) = map typeCtor (f a)

instance ToImportFrom ImportFromTypeOp { binaryOp :: b -> BinaryOp b, typeOpName :: Type e } where
  toImportFrom f (ImportFromTypeOp a) =
    map (\op -> { binaryOp: binaryOp op, typeOpName: typeOpName op}) (f a)

instance ToImportFrom ImportFromOp { binaryOp :: b -> BinaryOp b, exprOpName :: Expr e } where
  toImportFrom f (ImportFromOp a) =
    map (\op -> { binaryOp: binaryOp op, exprOpName: exprOpName op}) (f a)

instance
  ( RowToList rin rl
  , ToImportFromRecord rl (Record rin) (Record rout)
  ) =>
  ToImportFrom (Record rin) (Record rout) where
  toImportFrom k = map Builder.buildFromScratch <<< toImportFromRecord k (Proxy :: _ rl)

class ToImportFromRecord (rl :: RowList Prim.Type) rin rout | rl rin -> rout where
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
