#[macro_export]
macro_rules! rules {
    (@rule proxy insert $proxy: ident $key: ident bool $value: literal) => {
	$proxy.insert(stringify!($key).to_string(), Value::Bool($value));
    };
    (@rule proxy insert $proxy: ident $key: ident str $value: literal) => {
	$proxy.insert(stringify!($key).to_string(), Value::Str($value.to_string()));
    };
    (@rule proxy $($key: ident = $type: ident $value: literal)*) => {
	{
	    #[allow(unused_mut)]
	    let mut proxy = Proxy::new();
	    $(
		rules!(@rule proxy insert proxy $key $type $value);
	    )*
		proxy
	}
    };
    (@rule key) => { None };
    (@rule key $key: ident) => { Some(stringify!($key).to_string()) };
    (@rule attribute) => { Attribute::None };
    (@rule attribute str $attribute: ident) => { Attribute::Named(stringify!($attribute).to_string()) };
    (@rule attribute idx $attribute: literal) => { Attribute::Indexed($attribute) };
    (@rule terminality t) => { TestElementType::Terminal };
    (@rule terminality n) => { TestElementType::NonTerminal };
    (@rule terminality) => { TestElementType::NonTerminal };
    (@rule element $(! $terminality: tt)? $name: ident $(. $type: ident $attribute: tt)? $(@ $key: tt)?) => {
	TestElement {
	    name: stringify!($name).to_string(),
	    attribute: rules!(@rule attribute $($type $attribute)?),
	    key: rules!(@rule key $($key)?),
	    element_type: rules!(@rule terminality $($terminality)?)
	}
    };
    (
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
			elements.push(rules!(@rule element $(! $terminality)? $element $(. $attribute_type $attribute)? $(@ $key)?));
		    )*
			let proxy = rules!(@rule proxy $($proxy_key = $proxy_type $proxy_value)*);
		    result.push(TestRule::new(name.to_string(), elements, proxy));
		)*
	    })*
		result
	}
    };
}
