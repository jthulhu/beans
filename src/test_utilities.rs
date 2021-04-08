/// # Summary
///
/// `rule!` parses a grammar definition of a single non-terminal
/// and returns a `Vec` of the rules of that non-terminal.
///
/// # Usage
/// Call this macro with the following syntax:
/// `rule!(NonTerminal ::= terminality token.type value@value ... <proxy> ...)`
/// **Note that this macro is only used to generate test units.**
#[macro_export]
macro_rules! rule {
    (@proxy insert $proxy: ident $key: ident bool $value: literal) => {
	$proxy.insert(stringify!($key).to_string(), Value::Bool($value));
    };
    (@proxy insert $proxy: ident $key: ident str $value: literal) => {
	$proxy.insert(stringify!($key).to_string(), Value::Str($value.to_string()));
    };
    (@proxy $($key: ident = $type: ident $value: literal)*) => {
	{
	    let mut proxy = Proxy::new();
	    $(
		rule!(@proxy insert proxy $key $type $value);
	    )*
		proxy
	}
    };
    (@key) => { None };
    (@key $key: ident) => { Some(stringify!($key).to_string()) };
    (@attribute) => { Attribute::None };
    (@attribute str $attribute: ident) => { Attribute::Named(stringify!($attribute).to_string()) };
    (@attribute idx $attribute: literal) => { Attribute::Indexed($attribute) };
    (@terminality t) => { TestElementType::Terminal };
    (@terminality n) => { TestElementType::NonTerminal };
    (@terminality) => { TestElementType::NonTerminal };
    (@element $(! $terminality: tt)? $name: ident $(. $type: ident $attribute: tt)? $(@ $key: tt)?) => {
	TestElement {
	    name: stringify!($name).to_string(),
	    attribute: rule!(@attribute $($type $attribute)?),
	    key: rule!(@key $($key)?),
	    element_type: rule!(@terminality $($terminality)?)
	}
    };
    (
	$name: ident ::= $(
	    $(
		$(! $terminality: tt)?
		$element: ident
		    $(
			. $attribute_type: ident $attribute: literal
		    )?
		$(
		    @ $key: tt
		)?
	    )*
		< $(
		    $proxy_key: ident = $proxy_type: ident $proxy_value: literal
		)* >
	)*
    ) => {
	{
	    let name = stringify!($name);
	    let mut result = Vec::new();
	    $(
		let mut elements = Vec::new();
		$(
		    elements.push(rule!(@element $(! $terminality)? $element $(. $attribute_type $attribute)? $(@ $key)?));
		)*;
		let proxy = rule!(@proxy $($proxy_key = $proxy_type $proxy_value)*);
		result.push(TestRule::new(name.to_string(), elements, proxy));
	    )*
		result
	}
    };
}

#[macro_export]
macro_rules! collect {
    ($($rules: expr),*) => {
	{
	    let mut result = Vec::new();
	    $(
		result.extend($rules);
	    )*
		result
	}
    };
}
