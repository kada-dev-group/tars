{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map (Map)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Control.Concurrent.STM (TVar, newTVarIO, writeTVar, readTVarIO, atomically)
import System.Process (Pid, ProcessHandle, proc, createProcess, getPid, waitForProcess, terminateProcess)
import System.Exit (ExitCode (..))
import Control.Concurrent.Async (mapConcurrently_, async, wait)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    ps <- traverse exec sampleSpecs
    traverse_ printProcess ps
    task <- async $ watcher ps
    putStrLn "esto no deberia verse"
    stopProcess $ head ps
    putStrLn "saliendo del stop.."
    -- threadDelay 1_000_000
    putStrLn "imprimiendo..."
    -- traverse_ printProcess ps
    putStrLn "terminando..."
    wait task

printProcess :: Process -> IO ()
printProcess p = do
    status <- readTVarIO p.status
    putStrLn $ "process pid = " <> show p.pid
    putStrLn $ "process label = " <> show p.label
    putStrLn $ "process status = " <> show status
    putStrLn $ "--------------"

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
    | CrashedProcess Int
    deriving (Eq, Show)

data Process = Process
    { pid :: Maybe Pid
    , label :: Text
    , handle :: ProcessHandle
    , status :: TVar ProcessStatus
    , spec   :: ProcessSpec
    }

-- | TODO define docs
exec :: ProcessSpec -> IO Process
exec spec = do
    (hstdin, hstdout, hstderr, handle) <- createProcess cproc
    pid <- getPid handle
    status <- newTVarIO StartingProcess
    pure Process
        { pid
        , label = spec.command
        , handle
        , status
        , spec
        }
  where
    cproc = proc (Text.unpack spec.command) (Text.unpack <$> spec.args)

-- | TODO define docs
watchProcess :: Process -> IO ()
watchProcess p = do
    ec <- waitForProcess p.handle
    putStrLn $ "Termina wait for process" <> show ec
    case ec of
        ExitSuccess ->
           atomically $ writeTVar p.status StoppedProcess
        ExitFailure code ->
           atomically $ writeTVar p.status (CrashedProcess code)
    putStrLn  "Saliendo del wait for process"
    putStrLn  "========================"

stopProcess :: Process -> IO ()
stopProcess p = do
    putStrLn "Inicia stop process..."
    terminateProcess p.handle -- NOTE esta linea bloquea el proceso
    putStrLn "Termina stop process..."

watcher :: [Process] -> IO ()
watcher = mapConcurrently_ watchProcess


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
        , args        = ["server", "--port", "9090", "--local", "--haskell"]
        , description = "Sample start local Hoggle server"
        , enviroment  = mempty
        , workingDir  = "/tmp"
        , dependecies = []
       }
    ]
