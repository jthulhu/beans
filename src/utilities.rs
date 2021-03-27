/// # Summary
///
/// `retrieve!` consumes a builder by fetching `resource` and returning owned.
#[macro_export]
macro_rules! retrieve {
    ($resource:expr) => {{
        let column = column!() as usize;
        let file = file!();
        let line = (line!() + 1) as usize;
        let result = $resource.ok_or(crate::error::Error::from((
            Location::new(String::from(file), (line, column), (line, column)),
            ErrorType::InternalError(format!("{} missing", stringify!($resource))),
        )))?;
        $resource = None;
        result
    }};
}

#[macro_export]
macro_rules! ask_case {
    ($string:expr, $case:ident, $warnings:expr) => {
	let string = &$string;
	use crate::{case::Case, error::WarningType};
	match Case::case(string) {
	    Case::$case => {}
	    c => $warnings.push(WarningType::CaseConvention(string.to_string(), c, vec![Case::$case]))
	}
    };
    ($string:expr, $cases:expr, $warnings:expr) => {
	let string = &$string;
	use crate::{Case, error::WarningType};
	let case = Case::case(string);
	if !$cases.contains(&case) {
	    $warnings.push(WarningType::CaseConvention(string.to_string(), case, $cases));
	}
    };
}
