{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map (Map)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import System.Process (Pid, ProcessHandle, proc, createProcess, getPid)

main :: IO ()
main = do
    ps <- traverse exec sampleSpecs
    traverse_ printProcess ps

printProcess :: Process -> IO ()
printProcess p = do
    putStrLn $ "process pid = " <> show p.pid
    putStrLn $ "process status = " <> show p.status

-- | Define ProcessSpec
-- TODO add helht check
data ProcessSpec = ProcessSpec
    { command     :: Text
    , args        :: [Text]
    , description :: Text
    , enviroment  :: Map Text Text
    , workingDir  :: FilePath
    , dependecies :: [ProcessSpec]
    } deriving (Eq, Show)

data ProcessStatus
    = StartingProcess
    | StoppedProcess
    | RunnigProcess
    | WaitingProcess
    | CrashedProcess
    deriving (Eq, Show)

data Process = Process
    { pid :: Maybe Pid
    , handle :: ProcessHandle
    , status :: ProcessStatus
    , spec   :: ProcessSpec
    }

-- | TODO define docs
exec :: ProcessSpec -> IO Process
exec spec = do
    (hstdin, hstdout, hstderr, handle) <- createProcess cproc
    pid <- getPid handle
    pure Process
        { pid
        , handle
        , status = StartingProcess
        , spec
        }
  where
    cproc = proc (Text.unpack spec.command) (Text.unpack <$> spec.args)


sampleSpecs :: [ProcessSpec]
sampleSpecs = [
    ProcessSpec
        { command     = "warp"
        , args        = ["-p", "8080"]
        , description = "Sample web server using WARP"
        , enviroment  = mempty
        , workingDir  = "/tmp"
        , dependecies = []
       }
    , ProcessSpec
        { command     = "hoogle"
        , args        = ["server", "--port", "8123", "--local", "--haskell"]
        , description = "Sample start local Hoggle server"
        , enviroment  = mempty
        , workingDir  = "/tmp"
        , dependecies = []
       }
    ]
