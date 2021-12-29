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

/// `int_err` generates an internal error.
#[macro_export]
macro_rules! int_err {
    () => {
	int_err!("anonymous internal error")
    };
    ($msg: literal, $($tks: tt),*) => {{
	let pos = (line!() as usize, column!() as usize);
	Error::new(
	    Location::new(file!(), pos, pos),
	    ErrorType::InternalError(format!($msg, $($tks),*))
	)
    }};
}

/// `retrieve!` consumes a builder by fetching `resource` and returning owned.
#[macro_export]
macro_rules! retrieve {
    ($resource: expr, $warnings: expr) => {{
        let column = column!() as usize;
        let file = file!();
        let line = (line!() + 1) as usize;
        let result = crate::ctry!(
            std::mem::replace(&mut $resource, None)
                .ok_or(crate::error::Error::new(
                    crate::location::Location::new(
                        std::rc::Rc::from(file),
                        (line, column),
                        (line, column)
                    ),
                    crate::error::ErrorType::InternalError(format!(
                        "{} missing",
                        stringify!($resource)
                    )),
                ))
                .and_then(|x| Ok((x, crate::error::WarningSet::empty())))
                .into(),
            $warnings
        );
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
        use crate::{
            case::{Case, CaseProcess},
            error::{Warning, WarningType},
        };
        use std::rc::Rc;
        match $string.case() {
            Case::$case => {}
            c => $warnings.add(Warning::new(WarningType::CaseConvention(
                Rc::from($string.as_str()),
                c,
                Case::$case,
            ))),
        }
    };
}
