/// `int_err` generates an internal error.
#[macro_export]
macro_rules! int_err {
    () => {
	int_err!("anonymous internal error")
    };
    ($msg: literal, $($tks: tt),*) => {{
	$crate::error::Error::InternalError {
	    message: format!($msg, $($tks),*)
	}
    }};
}

/// `retrieve!` consumes a builder by fetching `resource` and returning owned.
#[macro_export]
macro_rules! retrieve {
    ($resource: expr) => {{
        let result =
            std::mem::replace(&mut $resource, None).ok_or($crate::error::Error::InternalError {
                message: format!("{} missing", stringify!($resource)),
            })?;
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
