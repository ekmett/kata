module Kata.Monad where

import Data.Rope

data QEnv = QEnv 

data DiagnosticState = DiagnosticState 
    { stateDiagnosticMapping :: Map ByteString Severity
    , warningsAsErrors :: Bool,
    , errorsAsFatal :: Bool,
    , ignoreAllWarnings :: Bool
    , stateErrorHasOccurred :: Bool
    , stateFatalErrorHasOccurred :: Bool
    , stateWarningsAsErrors :: Bool
    , stateErrorsAsFatal :: Bool
    , stateSuppressAllDiagnostics :: Bool
    }

data QState = QState 
    { diagnosticState :: DiagnosticState } 

getDiagnosticState :: Q DiagnosticState
getDiagnosticState = gets diagnosticState

putDiagnosticState :: DiagnosticState -> Q ()
putDiagnosticState ds = modify $ \s -> s { diagnosticState = ds } 

data Severity
    = Ignored
    | Warning
    | WarningNoError
    | Error
    | ErrorNoFatal
    | Fatal
    deriving (Show,Read,Eq,Ord)

data DiagnosticGroup
    = InternalGroup
    | PreprocessingGroup
    | MacroGroup
    | ParsingGroup
    | TypeCheckingGroup
    deriving (Show,Read,Eq,Ord)

failDiagnosticKind :: DiagnosticKind
failDiagnosticKind = DiagnosticKind (pack "monad-fail") Fatal

type DiagnosticTags = [ByteString]

newtype DiagnosticKind = DiagnosticKind DiagnosticTags Severity

-- -Werror              -- make all warnings into errors
-- -Werror=switch       -- make all warnings tagged 'switch' into errors
-- -Wno-error=switch    -- make all warnings tagged 'switch' into non-errors 

kindSeverity :: DiagnosticKind -> DiagnosticState -> Severity
kindSeverity (DiagnosticKind tags dflt) state = 
        ext state (go tags dflt (stateDiagnosticMapping state))
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

data Note = Note
    { noteTags :: DiagnosticTags
    , noteLocation :: Location
    , noteMessage :: Rope
    }

data Diagnostic = Diagnostic 
    { diagnosticKind :: DiagnosticKind
    , diagnosticLocation :: Location
    , diagnosticMessage :: Rope 
    , diagnosticNotes :: [(DiagnosticTags,Location, Rope)]
    }

setWarningsAsErrors :: Bool -> Q ()
setWarningsAsErrors =
    modify (\s -> s { warningsAsErrors :: 

addNote :: Tags -> Rope -> Diagnostic -> Diagnostic
addNote :: 

data Diagnostics = Diagnostics { getDiagnostics :: Seq Diagnostic } 
    deriving (Monoid)

data QError = QError Source Int 

newtype Q a = Q { runQ :: RWST QEnv Diagnostics QState (ErrorT Diagnostic IO) a }
    deriving 
        ( Functor
        , Applicative
        , Alternative
        , Monad, MonadPlus
        , MonadReader QEnv
        , MonadState QState
        , MonadWriter QLog
        , MonadError QError
        )

note :: Diagnostic -> Q ()
note = tell
warn :: Diagnostic -> Q ()
warn = tell 
error :: Diagnostic -> Q a
error = throw
fatal :: Diagnostic -> Q a 
fatal = throw 
