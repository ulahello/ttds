module Proc (Proc, launch, call) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import System.IO (BufferMode (..), Handle, hGetLine, hPutStrLn, hSetBuffering)
import System.Process (CmdSpec (ShellCommand), CreateProcess (..), ProcessHandle, StdStream (CreatePipe, Inherit), createProcess)

type Proc = MVar Process

data Process = Process
  { getStdin :: Handle,
    getStdout :: Handle
  }

data ProcFailureException = CantGatherProc | NeedCmd deriving (Show)

instance Exception ProcFailureException

call :: Proc -> String -> IO String
call lock line =
  withMVar lock $ \proc ->
    let stdin = getStdin proc
        stdout = getStdout proc
     in hPutStrLn stdin line >> hGetLine stdout

launch :: [String] -> IO Proc
launch [] = throwIO NeedCmd
launch cmd =
  let proc =
        CreateProcess
          { cmdspec = ShellCommand $ unwords cmd,
            std_in = CreatePipe,
            std_err = Inherit,
            std_out = CreatePipe,
            cwd = Nothing,
            env = Nothing,
            close_fds = True,
            create_group = False,
            delegate_ctlc = False,
            detach_console = True,
            create_new_console = False,
            new_session = True,
            child_group = Nothing,
            child_user = Nothing,
            use_process_jobs = False
          }
   in createProcess proc >>= gatherProc >>= newMVar

gatherProc :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Process
gatherProc (Just stdin, Just stdout, _, _) =
  hSetBuffering stdout NoBuffering
    >> hSetBuffering stdin NoBuffering
    >> return
      Process
        { getStdin = stdin,
          getStdout = stdout
        }
gatherProc _ = throwIO CantGatherProc
