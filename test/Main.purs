module Test.Main where

import Prelude

import Ansi.Output (foreground, withGraphics)
import Data.Either (Either(..))
import Data.Foldable (any, findMap, for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Dodo.Ansi (Color(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (ModuleName(..))
import Test.GenerateExamples (GenerateError(..), generateExamplesModule)
import Test.Snapshot (SnapshotResult(..), isBad, snapshotMainOutput)

main :: Effect Unit
main = do
  args <- Process.argv
  let accept = any (eq "--accept" || eq "-a") args
  let printOutput = any (eq "--print-output" || eq "-p") args
  let filter = Pattern <$> findMap (String.stripPrefix (Pattern "--filter=")) args
  let gen = findMap (String.stripPrefix (Pattern "--generate=")) args
  launchAff_ case gen of
    Just arg ->
      case String.split (Pattern ":") arg of
        [ path, name ] -> do
          currentDir <- liftEffect Process.cwd
          fullPath <- liftEffect $ Path.resolve [ currentDir ] path
          contents <- FS.readTextFile UTF8 fullPath
          case generateExamplesModule (ModuleName name) contents of
            Right exampleContents -> do
              FS.writeTextFile UTF8 (Path.concat [ ".", "test", "snapshots", name <> ".purs" ]) exampleContents
            Left errs -> do
              for_ errs \(ExampleParseError field err) ->
                Console.error $ field <> ":" <> printParseError err.error
              liftEffect $ Process.exit 1
        _ -> do
          Console.error $ "Invalid generate argument: " <> arg
          liftEffect $ Process.exit 1
    Nothing -> do
      results <- snapshotMainOutput "./test/snapshots" accept filter
      for_ results \{ name, output, result } -> case result of
        Passed -> do
          Console.log $ withGraphics (foreground Green) "✓" <> " " <> name <> " passed."
          when printOutput $ Console.log output
        Saved -> do
          Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
          when printOutput $ Console.log output
        Accepted -> do
          Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
          when printOutput $ Console.log output
        Failed diff -> do
          Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
          Console.log diff
          when printOutput $ Console.log output
        ErrorRunningTest err -> do
          Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed due to an error."
          Console.log $ Error.message err
      when (any (isBad <<< _.result) results) do
        liftEffect $ Process.exit 1
