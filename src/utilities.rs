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
        let result = std::mem::replace(&mut $resource, None).ok_or(
            $crate::error::Error::new(
                $crate::error::ErrorKind::InternalError {
                    message: format!("{} missing", stringify!($resource)),
                },
            ),
        )?;
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
        use std::rc::Rc;
        match $crate::case::CaseProcess::case($string) {
            $crate::case::Case::$case => {}
            c => $warnings.add($crate::error::Warning::new(
                $crate::error::WarningType::CaseConvention(
                    Rc::from($string.as_str()),
                    c,
                    $crate::case::Case::$case,
                ),
            )),
        }
    };
}

#[macro_export]
macro_rules! unwrap_or_continue {
    ($value:expr) => {
        if let Some(content) = $value {
            content
        } else {
            continue;
        }
    };
}

#[macro_export]
macro_rules! error {
    ($($tok:tt)*) => {{
	eprintln!(
	    "File \"{}\", line {}, character {}:\nInternal error:",
	    ::std::file!(),
	    ::std::line!(),
	    ::std::column!(),
	);
	eprintln!($($tok)*);
	::std::process::exit(2);
    }};
}

#[macro_export]
macro_rules! get {
    ($node:expr => $key:literal) => {{
        $node.attributes.remove($key).unwrap_or_else(|| {
            $crate::error!("expected to find child {}, got\n{:?}", $key, $node,)
        })
    }};
}

#[macro_export]
macro_rules! node {
    ($node:expr) => {
        if let $crate::parser::AST::Node {
            attributes, span, ..
        } = $node
        {
            $crate::parser::AstNode { attributes, span }
        } else {
            $crate::error!("expected to find node");
        }
    };
}

#[macro_export]
macro_rules! value {
    ($node:expr => $key:literal) => {
	if let $crate::parser::AST::Literal {
	    value: $crate::parser::Value::Str(result),
	    ..
	} = get!($node => $key) {
	    result
	} else {
	    error!("expected to find value, got\n{:?}", $node)
	}
    };
}
