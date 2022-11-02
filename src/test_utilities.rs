#[macro_export]
macro_rules! rules {
    (@rule($grammar:expr) proxy insert $proxy: ident $key: ident bool $value: literal) => {
	$proxy.insert(stringify!($key).into(), $crate::parser::grammarparser::ValueTemplate::Bool($value));
    };
    (@rule($grammar:expr) proxy insert $proxy: ident $key: ident str $value: literal) => {
	$proxy.insert(
	    stringify!($key).into(),
	    $crate::parser::grammarparser::ValueTemplate::Str(
		Rc::from($value)
	    )
	);
    };
    (@rule($grammar:expr) proxy $($key: ident = $type: ident $value: literal)*) => {
	{
	    #[allow(unused_mut)]
	    let mut proxy = Proxy::new();
	    $(
		rules!(@rule($grammar) proxy insert proxy $key $type $value);
	    )*
		proxy
	}
    };
    (@rule($grammar:expr) key) => { None };
    (@rule($grammar:expr) key $key: ident) => { Some(::std::rc::Rc::from(stringify!($key))) };
    (@rule($grammar:expr) attribute) => { Attribute::None };
    (@rule($grammar:expr) attribute str $attribute: ident) => { Attribute::Named(stringify!($attribute).to_string()) };
    (@rule($grammar:expr) attribute idx $attribute: literal) => { Attribute::Indexed($attribute) };
    (@rule($grammar:expr) terminality t) => { TestElementType::Terminal };
    (@rule($grammar:expr) terminality n) => { TestElementType::NonTerminal };
    (@rule($grammar:expr) terminality) => { TestElementType::NonTerminal };
    (@rule($grammar:expr) element $(! $terminality: tt)? $name: ident $(. $type: ident $attribute: tt)? $(@ $key: tt)?) => {
	TestElement {
	    name: stringify!($name).to_string(),
	    attribute: rules!(@rule($grammar) attribute $($type $attribute)?),
	    key: rules!(@rule($grammar) key $($key)?),
	    element_type: rules!(@rule($grammar) terminality $($terminality)?)
	}
    };
    (($grammar:expr)
	$(
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
	);*
    ) => {
	{
	    let mut result = Vec::new();
	    $({
		let name = stringify!($name);
		$(
		    let mut elements = Vec::new();
		    $(
			elements.push(rules!(@rule($grammar) element $(! $terminality)? $element $(. $attribute_type $attribute)? $(@ $key)?));
		    )*
			let proxy = rules!(@rule($grammar) proxy $($proxy_key = $proxy_type $proxy_value)*);
		    result.push(TestRule::new(name, elements, proxy, $grammar));
		)*
	    })*
		result
	}
    };
}
