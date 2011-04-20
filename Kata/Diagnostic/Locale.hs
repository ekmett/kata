module Kata.Diagnostic.Locale 
    ( DiagnosticLocale(..)
    , defaultLocale
    ) where

data DiagnosticLocale = DiagnosticLocale { 
    localeName :: String
#define DIAG(_a, _name, _class, _fmt) \
    , _name :: Diagnostic
#define DIAGS(_a,_name, _class, _fmt, _args, _argTypes) \
    , _name :: argTypes -> Diagnostic
#include "Diagnostics.def"
#undef DIAGS
#undef DIAG
    }

#define DIAG(_tags,_name,_severity,_fmt)\
_name##_kind :: Kind; _name##_kind = Kind #_name (_pack <$> _tags) _severity
#define DIAGS(_tags,_name,_severity,_fmt,_args,_types) \
_name##_kind :: Kind; _name##_kind = Kind #_name (_pack <$> _tags) _severity
#include "Diagnostics.def"
#undef DIAGS
#undef DIAG

defaultLocale :: DiagnosticLocale
defaultLocale = DiagnosticLocale {
    localeName :: "en_US",
#define DIAG(_tags,_name,_severity, _fmt) \
    , _name = diagnostic _name##_kind (_fmt)
#define DIAGS(_tags,_name,_severity, _fmt, _args, _types) \
    , _name = \ _args -> diagnostic _name##_kind (_fmt)
#include "Diagnostics.def"
#undef DIAGS
#undef DIAG
    }
