module PostfixSTLC.Shell

import Effects
import Effect.File
import Effect.State
import Effect.StdIO
import Effect.Quit
import Effect.System

import Control.Monad.State
import Data.List

import PostfixSTLC
import STLC
import Debug.Trace


getCmd : String -> Maybe Cmd
getCmd "Num" = Just PushNum
getCmd "->" = Just MkArr
getCmd "vz" = Just VarZ
getCmd "vs" = Just VarS
getCmd "."  = Just EmptyCtxt
getCmd ","  = Just ExtendCtxt
getCmd "v"  = Just MkVar
getCmd "zero" = Just MkZero
getCmd "succ" = Just MkSucc
getCmd "lam" = Just MkLam
getCmd "app" = Just MkApp
getCmd "dup" = Just Dup
getCmd "drop" = Just Drop
getCmd "swap" = Just Swap
getCmd "z" = Just NatZ
getCmd "s" = Just NatS
getCmd "get" = Just Get
getCmd "$" = Just Apply
getCmd _ = Nothing

covering
readCommands : List String -> Either String (List Cmd, List String)
readCommands [] = Right ([],[])
readCommands ("`" :: xs) = do (quoted, rest) <- readCommands xs
                              (cmds, remaining) <- readCommands rest
                              return (Quote quoted :: cmds, remaining)
readCommands ("'" :: xs) = Right ([], xs)
readCommands (x :: xs) with (getCmd x)
  readCommands (x :: xs) | Nothing = Left $ "Unknown command " ++ x
  readCommands (x :: xs) | Just y  = do (cmds, rest) <- readCommands xs
                                        return (y::cmds, rest)


record ShellState where
  constructor MkShellState
  fuel : Nat
  programState : ProgramState
  previous : Maybe ShellState

instance Default ProgramState where
  default = MkState [] [] []

instance Default ShellState where
  default = MkShellState 10000 default Nothing

ShellEffects : List EFFECT
ShellEffects = [STATE ShellState, STDIO, QUIT]

Shell : Type -> Type
Shell a = Eff a ShellEffects

App : Type -> Type
App a = Eff a (FILE_IO () :: SYSTEM :: ShellEffects)

save : Shell ()
save = do curr <- get
          update (record {previous = Just curr})

undo : Shell ()
undo = case previous !get of
         Nothing   => return ()
         Just prev => put prev

data ShellCmd = Undo | Quit | Done

readShellCmd : String -> Maybe ShellCmd
readShellCmd ":undo" = Just Undo
readShellCmd ":quit" = Just Quit
readShellCmd ":done" = Just Done
readShellCmd _       = Nothing

runShellCmd : ShellCmd -> Shell ()
runShellCmd Undo = undo
runShellCmd Quit = exit 0
runShellCmd Done = case programState !get of
                     MkState [] [Deriv [] t prog] [] => printLn $ extract prog
                     _ => putStrLn "Can't extract in this state"

getInput : Shell String
getInput = do putStr "> "
              getStr

covering
printState : Shell ()
printState = putStrLn $ show $ reverse $ stack $ programState !get

addCommands : List Cmd -> Shell ()
addCommands cmds =
  do st <- get
     let inner  = programState st
     let inner' = record {program = program inner ++ cmds} inner
     put $ record {programState = inner'} st

covering
runProgram : Shell ()
runProgram = do save
                st <- get
                runProgram' (fuel st) (programState st)
  where
    covering
    runProgram' : Nat -> ProgramState -> Shell ()
    runProgram' Z s = do putStrLn "No more fuel"
                         update (record {programState = s})
                         printState
    runProgram' (S k) s = case runFor k (run s) of
                            Left (MkState prog stk k) =>
                              do putStrLn $ "Failed with stack: " ++ show (reverse stk) ++
                                            " and program: " ++ show prog ++
                                            " and cont: " ++ show k
                                 undo
                            Right (s', k') =>
                              update (record {programState=s', fuel=k'})

covering
withCmds : String -> Shell a -> (List Cmd -> Shell a) -> Shell a
withCmds input err k =
  do let toks = filter (/= "") $ split isSpace input
     case readCommands toks of
       Left msg  => do putStrLn msg; err
       Right (_   , tok :: _) => do putStrLn $ "Unknown: " ++ tok
                                    err
       Right (cmds, []      ) => k cmds

covering
shell : Shell ()
shell = do printState
           input <- getInput
           case readShellCmd (trim input) of
             Just sCmd => do runShellCmd sCmd; shell
             Nothing   =>
               withCmds input shell $ \ cmds =>
                 with Effects
                 do addCommands cmds
                    runProgram
                    shell

covering
batch : String -> Shell ()
batch input = withCmds input (runShellCmd Quit) $ \ cmds =>
                with Effects
                do addCommands cmds
                   runProgram
                   runShellCmd Done
                   runShellCmd Quit


usage : Eff () [STDIO]
usage = putStrLn "Usage: postlambda [INPUTFILE]"

covering
fileContents : Eff String [FILE_IO (OpenFile Read)] [FILE_IO ()]
fileContents = contents' ""
  where
    covering
    contents' : String -> Eff String [FILE_IO (OpenFile Read)] [FILE_IO ()]
    contents' acc =
      if !eof
        then do close
                return acc
        else contents' (acc ++ !readLine)

covering
go : App ()
go = case !getArgs of
       [] => shell
       [_] => shell
       [_, inFile] => do True <- open inFile Read
                           | False => do putStrLn $ "Couldn't open " ++ inFile ++ " for reading"
                                         exit 1
                         contents <- fileContents
                         batch contents
       _ => usage
   

namespace Main
  covering
  main : IO ()
  main = run go
