use beans::{parser::AST, span::Span};
use cursive::{views::{TextView, LinearLayout}, View};

pub(crate) fn render_ast(ast: &AST) {
    let mut siv = cursive::default();
    siv.add_global_callback('q', |s| s.quit());
    siv.add_fullscreen_layer(
	LinearLayout::horizontal()
	    
    );
    siv.run();
}
