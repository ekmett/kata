{-# LANGUAGE DeriveDataTypeable, CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Kata.Config where

import System.Console.CmdArgs hiding (HTML, Text)
import qualified System.Console.CmdArgs as CmdArgs
import System.FilePath
import Data.Data
import Data.Typeable

kitExtension :: String
kitExtension = "kit"

data Target = User | Global
    deriving (Data,Typeable,Show,Eq)

#define TARGET target :: Target

#define FLAGS flags :: [String]

#define BUILD builddir :: FilePath

#define COMMON \
        colorize :: Bool \
      , interactive :: Interactivity \
      , locale :: String \
      , po_dir :: String \
      , term :: String

#define KIT \
        kitdir :: FilePath \
      , kit :: [String]

#define DIAGNOSTICS \
        warnings_as_errors :: Bool \
      , warning_as_error :: [String] \
      , warning_no_error :: [String] \
      , fatal_errors :: Bool \
      , fatal_error :: [String] \
      , non_fatal_error :: [String] 

#define CONFIGURE \
        TARGET \
      , extra_include_dirs :: [FilePath] \
      , extra_lib_dirs :: [FilePath] \
      , DIAGNOSTICS \
      , KIT \
      , BUILD

#define REINSTALL reinstall :: Bool

data HelpDataFormat = Text | HTML
    deriving (Data,Typeable,Show,Eq)

reformat_help :: HelpDataFormat -> HelpFormat
reformat_help Text = CmdArgs.Text
reformat_help HTML = CmdArgs.HTML

data EnvConfig = EnvConfig 
        { env_colorize :: Bool
        , env_interactive :: Interactivity
        , env_po_dir :: FilePath 
        , env_progName :: String 
        , env_term :: String } 

data Interactivity = Interactive | Unattended
    deriving (Data,Typeable,Show,Eq)

data Config
    = Configure
        { COMMON
        , CONFIGURE } 
    | Upgrade
        { COMMON
        , CONFIGURE 
        , REINSTALL } 
    | Install
        { COMMON
        , CONFIGURE
        , REINSTALL } 
    | Build
        { COMMON
        , TARGET
        , BUILD
        , KIT }
    | License 
        { COMMON }
    | Version 
        { COMMON
        , exact :: Bool } 
    | Help 
        { COMMON
        , help_format :: HelpDataFormat }
    deriving (Data,Typeable,Show,Eq)

-- TODO: read a config file in the data directory as a starting point for each of these values
config :: (String -> String) -> EnvConfig -> [Mode Config]
config translate env =
    [ mode $ configure &= gettext ("Setup kits") & prog (env_progName env)
    , mode $ build     &= gettext ("Compile kits")
    , mode $ install   &= gettext ("Install kits")
    , mode $ upgrade   &= gettext ("Upgrade kits")
    , mode $ license   &= gettext ("Show license")
    , mode $ version   &= gettext ("Show version")
    , mode $ help      &= gettext ("Show help") & defMode 
    ] 
  where
    gettext = text . translate
    configure = configureOpts$ Configure {} 
    upgrade   = reinstallOpt$ configureOpts$ Upgrade {}
    install   = reinstallOpt$ configureOpts$ Install {}
    build     = commonOpts$ buildOpts$ kitOpts$ targetOpt$ Build {}
    license   = commonOpts$ License {}
    version   = commonOpts$ Version { exact = def &= empty False & gettext ("Show exact version information about the kata executable") } 
    help      = commonOpts$ Help { help_format = enum Text [ Text &= gettext ("Display help in text format")
                                                           , HTML &= gettext ("Display help as an HTML table") ] }

    commonOpts m = m 
        { colorize = enum (env_colorize env) [ True &= explicit & flag "color" & gettext ("Colorize diagnostic messages")
                                             , False &= explicit & flag "no-color" & gettext ("Do not colorize diagnostic messages") ]
        , interactive = enum (env_interactive env) [ Interactive &= gettext ("Allow interaction when appropriate")
                                                   , Unattended &= gettext ("Do not use interactive prompts") ]
        , locale = def &= empty "en_US" & gettext ("Locale used for errors")
        , po_dir = def &= empty (env_po_dir env) & gettext ("Location of the kata l10n files") 
        , term = def &= empty (env_term env) & gettext ("Terminfo entry to use for colorization") } 
    targetOpt m = m { target = enum User [ User &= gettext ("Enable doing a per-user installation")

                                         , Global &= gettext ("Disable doing a per-user installation") ] }
    kitOpts m = m
        { kitdir = def &= empty "" & flag "C" & typDir & gettext ("Starting directory for .kit search")
        , kit    = def &= args & typ "KIT" & gettext ("Kit to configure") } 
    buildOpts m = m 
        { builddir = def &= empty "dist" & typDir & gettext ("The directory where kata installs generated build files") }
    configureOpts m = diagnostics$  kitOpts$ targetOpt$ buildOpts m
        { extra_include_dirs = def &= flag "I" & typDir & gettext ("Add directory to search for include files")
        , extra_lib_dirs = def &= flag "L" & typDir & gettext ("Add directory to search for library files") }
    reinstallOpt m = m 
        { reinstall = def &= empty False & gettext ("Reinstall even if it means installing the same version again") }
    diagnostics m = commonOpts $ m 
        { warnings_as_errors = def &= empty False & gettext ("Report all warnings as errors, unless it is marked with --warning-no-error") 
        , warning_as_error = def &= gettext ("Report the specified class of warnings as errors instead")
        , warning_no_error = def &= gettext ("Report the specified class of warning as a warning, even if --warnings-as-errors is set")
        , fatal_errors = def &= empty False & gettext ("Consider any error as fatal, unless it is marked with --non-fatal-error")
        , fatal_error = def &= gettext ("Report the specified class of errors as fatal")
        , non_fatal_error = def &= gettext ("Report the specified class of error as an error even if --fatal-errors is set") }
