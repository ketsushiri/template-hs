module Init
  where

import System.Environment (getArgs)
import System.Exit        (die)
import System.IO -- write to file
import System.Directory   (createDirectoryIfMissing)

 -- fields access.
infixl 1 &
(&) :: a -> (a -> b) -> b
(&) x f = f x

 -- logging
 --
data LogLevel
  = Error
  | Warning
  | Info
  deriving Show

data Log = Log 
  { logLevel :: LogLevel
  , logMessage :: String
  } deriving Show

logOut :: Log -> IO ()
logOut (Log { logLevel = level, logMessage = msg }) = do
    putStrLn (show level ++ ": " ++ msg)

 -- config
 --
data Status
  = Failed
  | Config
    { needExtraDir :: Bool
    , needExtraExt :: Bool
    , dir :: String
    , extDir :: String -- ./static/init.cpp by default 
    , names :: [String]
    , ext :: String -- .cpp by default
    } deriving Show

 -- messages 
 --
helpMessage :: String
helpMessage = "invalid args.\nUsage:\n\ttemplate DIR_NAME [NAMES]"

failedParseMessage :: String -> String
failedParseMessage flag = "failed to parse " ++ flag ++ " flag option."

 -- parse utils
 --
parseFlagArg :: String -> [String] -> Maybe String
parseFlagArg _ x | length x < 2 = Nothing
parseFlagArg f (x:xs) = case x == f of
                         True -> Just $ head xs
                         False -> parseFlagArg f xs

parseNames :: [String] -> [String] -> [String]
parseNames [] res = res
parseNames (x:xs) res | head x == '-' = res
                      | otherwise = parseNames xs (x : res) 

-- Will also perform logs output if some error will occur.
-- 
-- Need to implement more correct errors parsing and so on.
-- Also need to use 'Warnings' for border cases and 'Info' for info if '-v' is set.
-- 
initConfig :: IO Status
initConfig = do
    args <- getArgs
    -- implement more correct errors parsing.
    let _stop = \msg -> do
            logOut (Log { logLevel = Error, logMessage = msg })
            return Failed
    case length args < 2 of
        True -> _stop helpMessage
        False -> do
            let (needExtD, extD) = case "-d" `elem` args of
                                    True -> (True, parseFlagArg "-d" args)
                                    False -> (False, Just "./static/init.cpp")
            let (needExtE, extE) = case "-e" `elem` args of
                                    True -> (True, parseFlagArg "-e" args)
                                    False -> (False, Just ".cpp")
            -- _* used for values of * keys.
            -- ex: '_names' is value of 'names' key.
            let _names = parseNames (tail args) []

            case extE of
                Nothing -> _stop $ failedParseMessage "-e"
                Just _ext -> case extD of
                    Nothing -> _stop $ failedParseMessage "-d"
                    Just _extDir -> case length _names == 0 of
                        True -> _stop "failed to get file's names."
                        _ -> return Config { needExtraDir = needExtD,
                                             needExtraExt = needExtE,
                                             dir = head args,
                                             extDir = _extDir,
                                             ext = _ext, 
                                             names = _names }

 -- create directory and files
 -- 
performCreation :: Status -> IO ()
performCreation config@(Config { dir = dir', extDir = from_dir, ext = ext' }) = do
    let new_ext = if head ext' == '.' then ext' else '.':ext'
    case head ext' == '.' of
      False -> logOut (Log { logLevel = Warning, logMessage = "missing '.' in the extension name." })
      _ -> return ()

    template <- readFile from_dir
    createDirectoryIfMissing False dir'
    let path = if last dir' == '/' then dir' else dir' ++ "/"

    let writeTo = \f -> do
            h <- openFile (path ++ f ++ new_ext) ReadWriteMode
            hPutStr h template
            hClose h

    mapM_ writeTo (config & names)
    return ()

mainInit :: IO ()
mainInit = do
    config <- initConfig
    case config of
        Failed -> putStrLn "Aborting."
        config' -> performCreation config'

