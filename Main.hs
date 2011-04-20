{-# LANGUAGE CPP #-}
module Main where

-- import Data.Char (toLower)
import qualified Version as V (version, builddeps)
-- import Data.Version hiding (Version)
import Paths_kata
import System.Environment
import Control.Applicative
import Control.Monad.Error
-- import System.FilePath
-- import System.Directory
import System.IO
import Text.I18n.Po hiding (Locale)
import Text.I18n.Printf 
import qualified Text.I18n.Po as Po
import System.Console.GetOpt
import System.Console.CmdArgs
import Kata.Config hiding (term)
import System.Console.Terminfo.Base 
import System.Console.Terminfo.Color
-- import Data.Maybe (maybe)
-- import UI.HSCurses.Curses


-- options needed, ironically, during option parsing
data Flag = Locale String | PODir String | NoColor | Color | Term String | Exotic | Boring
    deriving (Eq)

options :: [OptDescr Flag]
options = [ Option [] ["locale"] (ReqArg Locale "LOCALE") "locale to use" 
          , Option [] ["po-dir"] (ReqArg PODir "DIR") "path to the PO files used for kata localization" 
          , Option [] ["no-color"] (NoArg NoColor) "disable color"
          , Option [] ["color"] (NoArg Color) "force color" 
          , Option [] ["term"] (ReqArg Term "TERM") "terminfo entry to use" 
-- TODO: fix
--           , Option [] ["help"] (OptArg (\a -> maybe Boring (\c -> if c == "HTML" then Exotic else Boring) a)) "help format"

          , Option [] ["html"] (NoArg Exotic) "html help format" ]

-- TODO: wrap these up in a product type
interpretOpts :: String -> FilePath -> Bool -> String -> [Flag] -> (String, FilePath,Bool, String)
interpretOpts l p c t []              = (l, p, c, t)
interpretOpts _ p c t (Locale l : xs) = interpretOpts l p c t xs
interpretOpts l _ c t (PODir p : xs)  = interpretOpts l p c t xs
interpretOpts l p _ t (NoColor : xs)  = interpretOpts l p False t xs
interpretOpts l p _ t (Color : xs)    = interpretOpts l p True t xs
interpretOpts l p c _ (Term t : xs)   = interpretOpts l p c t xs
interpretOpts l p c t (_ : xs)        = interpretOpts l p c t xs

main :: IO () 
main = do
    is_interactive <- hIsTerminalDevice stdin
    should_colorize <- hIsTerminalDevice stdout 

    -- we need to bootstrap localization before everything else, so we can print localized help
    (opts, _, _, _) <- getOpt' Permute options <$> getArgs -- '
    envLocale <- (getEnv "LC_MESSAGES" `catchError` \ _ -> getEnv "LC_ALL") 
                                       `catchError` \ _ -> return "en_US"

    envTerm <- getEnv "TERM" `catchError` \ _ -> return "dumb";

    -- TODO get cabal to let us use the appropriate /share so we can put these where the real l10n messages are
    defaultpodir <- getDataFileName "po"
    let (loc, podir, want_color, term) = interpretOpts envLocale defaultpodir should_colorize envTerm opts

    terminal <- setupTerm term
    let html = any (==Exotic) opts
        withColor :: TermStr a => Color -> a -> a
        withColor = case guard (want_color && not html) >> getCapability terminal withForegroundColor of
            Just c -> c
            Nothing -> \_ a -> a

        appName = withColor Red ("Kata " ++ V.version)
        progName = withColor Red "kata"

    (l10n, _errors) <- getL10n podir
    -- TODO: display l10n errors as diagnostics
    -- when (not (null errors)) $ Prelude.putStrLn $ "po parse error: " ++ show errors
        
    let local = localize l10n (Po.Locale loc)
        translate :: PrintfType a => String -> a
        translate = local . gettext
        env = EnvConfig 
            { env_colorize = should_colorize && not html -- this is the 'default' so it should be what we thought before we parsed. we'll reparse below -- '
            , env_interactive = if is_interactive then Interactive else Unattended
            , env_po_dir = defaultpodir -- likewise
            , env_progName = progName 
            , env_term = envTerm }

    -- TODO: In a perfect world, here we'd capture the fact that there was an error and send the help text to the user's viewer, if any
    -- but cmdArgs locks us out of that functionality right now

    cmd <- cmdArgs appName (config translate env)
    case cmd of
        License{} -> Prelude.putStrLn =<< readFile =<< getDataFileName "LICENSE"
        Help{}  -> Prelude.putStrLn =<< cmdArgsHelp appName (config translate env) (reformat_help (help_format cmd))
        Version { exact = False } -> Prelude.putStrLn appName 
        Version { exact = True  } -> do
                -- TODO: Add l10n support for dates and times to Text.I18n.
                Prelude.putStrLn $ local (gettext ("%s compiled on %s at %s with packages:\n\n%s")) appName __DATE__ __TIME__ V.builddeps
        _       -> print cmd

    
{-
    case mode of 
        Configure{} -> do
            packages <- find_packages mode
            withDiagnostics mode $
                forM_ packages $ \package -> 
                    withPackage mode package $ 
                        configure 
            
            withConfigurePackage 
            -- configure packages
            return 0
        Install{} -> do
            packages <- find_packages mode
            withPackage 
            -- install packages
            return 0
        Upgrade{} -> do
            packages <- find_packages mode
            -- upgrade packages
            return 0

-- iterate outward from the specified filePath
find_package :: (String -> Bool) -> FilePath -> IO [FilePath]
find_package f dir = do
    canonical_dir <- canonicalizePath dir
    files <- filter (\x -> takeExtension x == packageExtension && f (takeBaseName x)) <$> getDirectoryContents canonical_dir
    case files of
        [] -> do
            (dir', _) <- splitFileName canonical_dir -- '
            canonical_dir' <- canonicalizePath dir'
            if canonical_dir == canonical_dir' then -- '
                return []
            else
                find_package f dir' -- '
        (_:_) -> return files
              
find_packages :: Mode -> IO [FilePath]
find_packages mode = find_package sieve (packagedir args)
    where sieve | null (package args) = const True
                | otherwise = (`elem` package args)


withPackage :: Command -> FilePath -> (Package -> Q a) ->  Q a
withPackage cmd packagePath action = do 
     current <- getCurrentDirectory
     workingDir <- fst <$> splitPath packagePath
     buildDir <- combine workingDir (builddir cmd)
     createDirectoryIfMissing True buildDir
     package <- openPackage packagePath
     withCommand cmd $ handleDiagnostic $ action package
     return result
        
     

withDiagnostics :: Command -> Q a -> IO a
withDiagnostics cmd q = do
-}
