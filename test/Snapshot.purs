module Test.Snapshot where

import Prelude

import Control.Alternative (guard)
import Control.Parallel (parTraverse)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (stripSuffix)
import Effect (Effect)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, Error, catchError, effectCanceler, makeAff, throwError, try)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, freeze)
import Node.Buffer as Buffer
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, defaultExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readdir, writeFile)
import Node.Path (basename)
import Node.Path as Path
import Node.Stream as Stream

data SnapshotResult
  = Passed
  | Saved
  | Accepted
  | Failed String
  | ErrorRunningTest Error

type SnapshotTest =
  { name :: String
  , output :: String
  , result :: SnapshotResult
  }

isBad :: SnapshotResult -> Boolean
isBad = case _ of
  Failed _ -> true
  ErrorRunningTest _ -> true
  _ -> false

snapshotMainOutput :: String -> Boolean -> Maybe Pattern -> Aff (Array SnapshotTest)
snapshotMainOutput directory accept mbPattern = do
  paths <- readdir directory
  block <- AVar.empty
  for_ (Array.range 1 4) \_ -> do
    liftEffect $ EffectAVar.put unit block mempty
  flip parTraverse (pursPaths paths) \name -> do
    AVar.take block
    result <- runSnapshot name
    _ <- liftEffect $ EffectAVar.put unit block mempty
    pure result
  where
  pursPaths =
    mapMaybe (filterPath <=< stripSuffix (Pattern ".purs") <<< basename)

  filterPath = case mbPattern of
    Just pat -> \path -> guard (String.contains pat path) $> path
    Nothing -> pure

  makeErrorResult :: String -> Error -> Aff SnapshotTest
  makeErrorResult name err = pure { name, output: "", result: ErrorRunningTest err }

  runSnapshot :: String -> Aff SnapshotTest
  runSnapshot name = flip catchError (makeErrorResult name) do
    result <- exec $ "node -e 'require(\"./output/" <> name <> "/index.js\").main()'"
    case result of
      { error: Just err } ->
        throwError err
      { stdout } -> do
        output <- liftEffect $ bufferToUTF8 stdout
        let
          outputFile = Path.concat [ directory, name <> ".output" ]
          acceptOutput = do
            writeFile outputFile =<< liftEffect (Buffer.fromString output UTF8)
        savedOutput <- try $ readFile outputFile
        case savedOutput of
          Left _ -> do
            acceptOutput
            pure { name, output, result: Saved }
          Right buffer -> do
            savedOutput' <- liftEffect $ bufferToUTF8 buffer
            if output == savedOutput' then
              pure { name, output, result: Passed }
            else if accept then do
              acceptOutput
              pure { name, output, result: Accepted }
            else do
              { stdout: diffOutput } <- execWithStdin ("diff " <> outputFile <> " -") output
              diffOutput' <- liftEffect $ bufferToUTF8 diffOutput
              pure { name, output, result: Failed diffOutput' }

exec :: String -> Aff ExecResult
exec command = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input mempty
  Stream.end (ChildProcess.stdin childProc) mempty
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Effect String
bufferToUTF8 = map (ImmutableBuffer.toString UTF8) <<< freeze
