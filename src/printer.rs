use crate::parser::{AST, Value};
use ptree::{TreeBuilder, print_tree};

fn build_tree(tree: &mut TreeBuilder, ast: &AST) {
    match ast {
        AST::Node { attributes, .. } => {
            for (key, value) in attributes.iter() {
                tree.begin_child(key.to_string());
                build_tree(tree, value);
                tree.end_child();
            }
        }
        AST::Literal {
	    value: Value::Int(i),
	    ..
	} => {
            tree.add_empty_child(i.to_string());
        }
        AST::Literal {
	    value: Value::Str(string),
	    ..
	}=> {
            tree.add_empty_child(string.to_string());
        }
        AST::Literal {
	    value: Value::Float(f),
	    ..
	} => {
            tree.add_empty_child(f.to_string());
        }
        AST::Literal {
	    value: Value::Bool(b),
	    ..
	} => {
            tree.add_empty_child(b.to_string());
        }
        AST::Terminal(ter) => {
            tree.add_empty_child(ter.name().to_string());
        }
    }
}

pub fn print_ast(ast: &AST) -> std::io::Result<()> {
    let mut tree = TreeBuilder::new(String::from("AST"));
    build_tree(&mut tree, ast);
    let tree = tree.build();
    print_tree(&tree)
}
