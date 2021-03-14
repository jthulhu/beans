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
