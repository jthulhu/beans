/// # Summary
///
/// `ctry` is a similar macro to `?`, except it handles eventual warnings in
/// the `WResult` by adding the to the `WarningSet` given as argument.
#[macro_export]
macro_rules! ctry {
    ($obj: expr, $warnings: expr) => {{
        use crate::error::WResult::{WErr, WOk};
        match $obj {
            WOk(result, warnings) => {
                $warnings.extend(warnings);
                result
            }
            WErr(error) => return WErr(error),
        }
    }};
}

/// # Summary
///
/// `retrieve!` consumes a builder by fetching `resource` and returning owned.
#[macro_export]
macro_rules! retrieve {
    ($resource: expr) => {{
        use crate::error::{Error, WarningSet};
        use crate::wtry;
        let column = column!() as usize;
        let file = file!();
        let line = (line!() + 1) as usize;
        let result = wtry!($resource
            .ok_or(Error::new(
                Location::new(String::from(file), (line, column), (line, column)),
                ErrorType::InternalError(format!("{} missing", stringify!($resource))),
            ))
            .and_then(|x| Ok((x, WarningSet::empty())))
            .into());
        $resource = None;
        result
    }};
    ($resource: expr, $warnings: expr) => {{
        use crate::ctry;
        use crate::error::{Error, WarningSet};
        let column = column!() as usize;
        let file = file!();
        let line = (line!() + 1) as usize;
        let result = ctry!(
            $resource
                .ok_or(Error::new(
                    Location::new(String::from(file), (line, column), (line, column)),
                    ErrorType::InternalError(format!("{} missing", stringify!($resource))),
                ))
                .and_then(|x| Ok((x, WarningSet::empty())))
                .into(),
            $warnings
        );
        $resource = None;
        result
    }};
}

/// # Summary
///
/// `ask_case!` generates a warning when the given string does not comply
/// to the given case.
#[macro_export]
macro_rules! ask_case {
    ($string: expr, $case: ident, $warnings: expr) => {
        let string = &$string;
        use crate::{
            case::Case,
            error::{Warning, WarningType},
        };
        match Case::case(string) {
            Case::$case => {}
            c => $warnings.add(Warning::new(WarningType::CaseConvention(
                string.to_string(),
                c,
                Case::$case,
            ))),
        }
    };
}
