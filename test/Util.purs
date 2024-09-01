module Test.Util where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream (writeString')

log :: String -> Effect Unit
log = void <<< flip (writeString' Process.stdout UTF8) mempty
