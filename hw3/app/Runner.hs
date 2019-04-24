{-# LANGUAGE LambdaCase #-}
module Runner (execBash) where

import Prelude hiding (lookup)
import Parser
import System.Exit
import System.Process
import Control.Exception
import Data.IORef
import Data.List (map, filter, intercalate)
import Data.Map (Map, insert, empty, lookup)
import Data.Maybe
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Trans.Reader
import System.Directory
import System.FilePath ((</>), normalise)

data Context = Context { runPrint :: String -> IO (),
                         getDirRef :: IORef FilePath,
                         getVarsRef :: IORef (Map String String) }

type Bash = ReaderT Context IO

readRef :: (Context -> IORef a) -> Bash a
readRef selector = do
    ref <- asks selector
    val <- lift $ readIORef ref
    lift $ return val

mutateRef :: (Context -> IORef a) -> (a -> a) -> Bash ()
mutateRef selector f = do
    ref <- asks selector
    lift $ modifyIORef ref f

printP :: String -> Bash ()
printP x = asks runPrint >>= (\f -> lift $ f x)

execBash :: [Action] -> [String] -> IO ()
execBash actions args = do
    vars <- newIORef (createArgs args 1)
    curDir <- getCurrentDirectory
    dirRef <- newIORef $ curDir
    runReaderT (runActions actions) (Context { getVarsRef = vars, runPrint = putStr, getDirRef = dirRef } )
    where
        createArgs :: [String] -> Int -> Map String String
        createArgs [] _ = empty
        createArgs (a:b) i = insert (show i) a (createArgs b (i + 1))

runActions :: [Action] -> Bash ()
runActions [] = return ()
runActions (action:rest) = do
    case action of
        ActionDefine define -> execDefinition define
        ActionExec exec -> execCommands exec >>= \case
                                                    Just x -> printP x
                                                    Nothing -> return ()
    runActions rest

execCommands :: [Exec] -> Bash (Maybe String)
execCommands execs = mconcat <$> sequenceA (map execCommand execs)

resolve :: BashString -> Bash String
resolve (BashChar x) = return [x]
resolve (InlineCommand x) = (execCommands x) >>= (return . (fromMaybe ""))
resolve (BashSequence s) = mconcat <$> mapM resolve s
resolve (BashDollar name) = do
    ref <- asks getVarsRef
    vars <- lift $ readIORef ref
    return $ fromMaybe "" $ lookup name vars

resolveN :: [BashString] -> Bash String
resolveN string = (intercalate " ") <$> (Prelude.mapM resolve string)

putVar :: String -> String -> Bash ()
putVar key value = do
    ref <- asks getVarsRef
    lift $ modifyIORef ref (insert key value)

execDefinition :: Define -> Bash ()
execDefinition (Define name val) = do
    value <- resolve val
    mutateRef getVarsRef (insert name value)
    return ()

echo :: [String] -> Bash (Maybe String)
echo args = let newLine = if "-n" `elem` args then "" else "\n"
                otherArgs = filter (/= "-n") args
            in lift $ return . Just $ (intercalate " " otherArgs) ++ newLine

scan :: [String] -> Bash (Maybe String)
scan vars = do
    line <- lift $ getLine
    vals <- lift $ splitTokens line
    putVars vars vals
    return Nothing

putVars :: [String] -> [String] -> Bash ()
putVars (var:[]) vals = putVar var (intercalate " " vals)
putVars [] _ = return ()
putVars (_:_) [] = return ()
putVars (var:restVars) (val:restVals) = do
    putVar var val
    putVars restVars restVals

runBashCommand :: [String] -> Bash (Maybe String)
runBashCommand [] = error "Invalid command."
runBashCommand (name:args) =
    case name of
        "echo" -> echo args
        "read" -> scan args
        "cd" -> cd args
        "pwd" -> pwd args
        "exit" -> exit args
        _ -> forkProcess name args

exit :: [String] -> Bash a
exit (code:_) = case ((read code) :: Int) of
                    0 -> lift $ exitWith (ExitSuccess)
                    _ -> lift $ exitWith (ExitFailure $ read code)
exit [] = error "Invaliad arguments to exit."

cd :: [String] -> Bash (Maybe String)
cd [] = error "Invalid arguments for cd. No arguments supplied."
cd (path:[]) = do
    curDir <- readRef getDirRef
    let newCd = normalise (curDir </> path)
    isExist <- lift $ doesDirectoryExist newCd
    when (not isExist) $ error ("Path " ++ show newCd ++ " does not exist")
    mutateRef getDirRef (const newCd)
    return Nothing

cd (_:_) = error "Too many arguments for cd."

pwd :: [String] -> Bash (Maybe String)
pwd _ = (Just . (++ "\n")) <$> readRef getDirRef

forkProcess :: String -> [String] -> Bash (Maybe String)
forkProcess name args = do
    cwd' <- readRef getDirRef
    (code, out, _) <- lift $ readCreateProcessWithExitCode ((proc name args) { cwd = Just cwd' }) ""
    case code of
        ExitSuccess -> lift $ return (Just $ out ++ "\n")
        ExitFailure _ -> do
            printP $ (show code) ++ "\n"
            lift $ return Nothing

execCommand :: Exec -> Bash (Maybe String)
execCommand (Exec symbols) = do
    command <- resolveN symbols
    ctx <- ask
    logger <- asks runPrint
    lift $ catch (runReaderT (lift (splitTokens command) >>= runBashCommand) ctx) (defaultCatch (logger . (++ "\n")))

defaultCatch :: (String -> IO ()) -> ErrorCall -> IO (Maybe String)
defaultCatch logger e = do
    logger $ case e of
                ErrorCallWithLocation msg _ -> msg
    return Nothing