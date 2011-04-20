module Kata.Diagnostic where

import Kata.Position.Annotation
import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Data.Rope (Rope)
import Data.Rope.Annotated (Ann)
import Data.ByteString (ByteString)

-- TODO: allow local pragma based suppression of diagnostic messages, this requires pragma annotations, which aren't implemented yet
-- #pragma warning ( disable : monad_fail; enable : monad_mzero; default : cpp; once : lex )
-- #pragma warning ( push 
-- #pragma warning ( pop )

data DiagnosticState = DiagnosticState 
    { diagnosticMapping :: Map ByteString Severity
    , diagnosticKinds   :: Map ByteString DiagnosticKind
    , diagnosticLocale  :: DiagnosticLocale,
    , warningsAsErrors  :: Bool,                -- -Werror
    , errorsAsFatal     :: Bool,                -- -Wfatal-errors
    , ignoreAllWarnings :: Bool                 -- -Wignore-warnings
    , errorHasOccurred  :: Bool
    , fatalErrorHasOccurred  :: Bool
    , suppressAllDiagnostics :: Bool            -- a fatal error has occurred, or 
    , showSourceColumn   :: Bool                -- --hide-column
    , showSourceLocation :: Bool                -- --hide-source-location
    , showSourceIncludePath :: Bool             -- --hide-include-path
    , showSourceRanges :: Bool                  -- --hide-source-ranges
    , showFixits       :: Bool                  -- --hide-fixits
    }

defaultDiagnosticState = DiagnosticState
    { stateDiagnosticMapping = Map.empty
    , diagnosticLocale = defaultLocale
    , warningsAsErrors = False
    , errorsAsFatal = False
    , ignoreAllWarnings = False
    , errorHasOccurred = False
    , fatalErrorHasOccurred = False
    , stateWarnings

class Monad m => MonadDiagnostic m where
    -- retrieve the information about how diagnostics are being handled
    getDiagnosticState :: m DiagnosticState
    putDiagnosticState :: DiagnosticState -> m ()
    -- throw a diagnostic until it can be handled
    throwDiagnostic :: Diagnostic -> m ()
    catchDiagnostic :: m a -> (Diagnostic -> m a) -> m a
    -- log a diagnostic without breaking control flow
    tellDiagnostic :: Diagnostic -> m ()
    
data Source = forall s. Source FilePath (Ann PositionAnn s)

data Range = Range Source Int Int

data FixIt = Fixit
    { fixitRange :: Range
    , fixitText :: ByteString
    } 

data Diagnostic = Diagnostic 
    { diagnosticKind     :: Kind
    , diagnosticLocation :: Location
    , diagnosticMessage  :: Locale -> String
    , diagnosticNotes    :: [Diagnostic]
    , diagnosticRanges   :: [Range]
    , diagnosticFixits   :: [FixIt]
    }

data Severity
    = Ignored
    | Warning
    | WarningNoError
    | Error
    | ErrorNoFatal
    | Fatal
    deriving (Show,Read,Eq,Ord)

type DiagnosticTags = [ByteString]

newtype Kind = Kind Tags Severity

kindSeverity :: MonadDiagnosticState m => DiagnosticKind -> m Severity
kindSeverity (DiagnosticKind tags dflt) state = do
        state <- getDiagnosticState
        return $ ext state (go tags dflt (stateDiagnosticMapping state))
    where
        go :: DiagnosticTags -> Severity -> Map ByteString Severity -> Severity
        go [] dflt _ = dflt
        go (t:ts) dflt map = case lookup t map of
            Just severity -> severity
            Nothing -> go ts dflt map

        ext :: DiagnosticState -> Severity -> Severity
        ext _ Ignored = Ignored
        ext s Warning 
            | ignoreAllWarnings s = Ignored
            | warningsAsErrors s =  Error
            | otherwise = Warning
        ext s WarningNoError
            | ignoreAllWarnings s = Ignored 
            | otherwise = Warning
        ext s Error  
            | errorsAsFatal s = Fatal
            | otherwise = Error
        ext _ ErrorNoFatal = Error
        ext _ Fatal = Fatal


-- addNote :: Tags -> Rope -> Diagnostic -> Diagnostic

data DiagnosticLocale = DiagnosticLocale { 
    localeName :: String
    -- , localized_foo :: String
    -- , localized_foo :: argTypes -> String
#define DIAG(_a, _name, _class, _fmt) \
    , localized##_name :: String
#define DIAGS(_a,_name, _class, _fmt, _args, _argTypes) \
    , localized##_name :: _argTypes -> String
#include "Diagnostics.h"
#undef DIAGS
#undef DIAG
    }

-- foo_kind :: Kind; foo_kind = Kind "foo" (pack <$> tags) severity
#define DIAG(_tags,_name,_severity,_fmt)\
_name##_kind :: Kind; _name##_kind = Kind #_name (pack <$> _tags) _severity
#define DIAGS(_tags,_name,_severity,_fmt,_args,_types) \
_name##_kind :: Kind; _name##_kind = Kind #_name (pack <$> _tags) _severity
#include "Diagnostics.h"
#undef DIAGS
#undef DIAG

defaultLocale :: DiagnosticLocale
defaultLocale = DiagnosticLocale {
    localeName :: "en_US",
    -- , localized_foo = fmt
    -- , localized_foo = \ _args -> fmt
#define DIAG(_tags,_name,_severity, _fmt) \
    , localized_##_name = _fmt
#define DIAGS(_tags,_name,_severity, _fmt, _args, _types) \
    , localized_##_name = \ _args -> _fmt
#include "Diagnostics.h"
#undef DIAGS
#undef DIAG
    }

-- foo = diagnostic foo_kind localized_foo
-- foo args = diagnostic foo_kind (\locale -> localized_foo locale args) 
#define DIAG(_tags,_name,_severity,_fmt) \
_name :: Diagnostic; \
_name = diagnostic _name##_kind localized_##_name
#define DIAGS(_tags,_name,_severity,_fmt,_args,_types) \
_name :: _types -> Diagnostic; \
_name _args = diagnostic _name##_kind (\locale -> localized_##_name locale _args)
#include "Diagnostics.h"
#undef DIAGS
#undef DIAG


instance Error Diagnostic where
    noMsg = monad_no_msg
    strMsg = monad_fail
