use crate::{parser::{Value, AST, earley::EarleyGrammar}, error::{Result, Error}};
use ptree::{print_tree, TreeBuilder};

fn name(ast: &AST, grammar: &EarleyGrammar) -> String {
    match ast {
	AST::Node { nonterminal, attributes, .. } => {
	    let mut name = String::new();
	    name.push_str(&grammar.name_of(*nonterminal));
	    if let Some(AST::Literal { value: Value::Str(variant), .. }) = attributes.get("variant") {
		name.push_str("(");
		name.push_str(&variant);
		name.push_str(")");
	    }
	    name
	}
	AST::Literal { value: Value::Int(i), .. } => i.to_string(),
	AST::Literal { value: Value::Str(string), .. } => string.to_string(),
	AST::Literal { value: Value::Float(f), .. } => f.to_string(),
	AST::Literal { value: Value::Bool(b), .. } => b.to_string(),
	AST::Terminal(terminal) => terminal.name().to_string(),
    }
}

fn build_tree(tree: &mut TreeBuilder, ast: &AST, grammar: &EarleyGrammar) {
    if let AST::Node { attributes, .. } = ast {
        for (key, value) in attributes.iter() {
	    if &**key == "variant" {
		continue;
	    }
            tree.begin_child(format!("{key}: {}", name(value, grammar)));
            build_tree(tree, value, grammar);
            tree.end_child();
        }
    }
}

pub fn print_ast(ast: &AST, grammar: &EarleyGrammar) -> Result<()> {
    let mut tree = TreeBuilder::new(name(ast, grammar));
    build_tree(&mut tree, ast, grammar);
    let tree = tree.build();
    print_tree(&tree)
	.map_err(|error| Error::with_file(error, "<stdout>"))
}
