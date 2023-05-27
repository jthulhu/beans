#!/usr/bin/env bash
# shellcheck disable=SC2034,SC2016,SC2219
# author: jthulhu

GREEN=$'\e[1;32m'
ORANGE=$'\e[1;33m'
RED=$'\e[1;31m'
WHITE=$'\e[1;37m'
END=$'\e[0m'

ROOT=$(git rev-parse --show-toplevel)
SOURCE="$ROOT/src"
MIGR_DIR="$ROOT/.migration"
TARGET="$ROOT/target"
OUT="$ROOT/out"

GRAMMARS=(lexer/lexer parser/parser)

PARSER_EXT=gr
LEXER_EXT=lx
EXTS=(LEXER_EXT PARSER_EXT)

PREC_PARSER_EXT=cgr
PREC_LEXER_EXT=clx
PREC_EXTS=(PREC_PARSER_EXT PREC_LEXER_EXT)

AST_EXT=ast

MAX_STEP=2

usage() {
    echo 'migration -- modify the grammar of Beans'
    echo
    echo "${WHITE}Usage${END}: migration <command> [options]"
    echo
    echo 'Changing the grammars of Beans requires compiling both Beans and its grammars several'
    echo 'times, with a specific ordering and options. If you want to do a migration, please'
    echo 'start a new migration and follow the given instruction. DO NOT start modifying the'
    echo 'grammars before starting the migration, as it will fail.'
    echo 'In case you get weird errors, use `git restore` and restart everything.'
    echo 
    echo "${ORANGE}Important${END}:" '`migration` can only restore grammar-related files, and'\
	 'does not support (yet)'
    echo '           modifications in the Rust code. It will, however, try as best as it can to'\
	 'always leave you in a good'
    echo '           state. This means that most operations behave as if they were atomic,'\
	 'that is, if they fail, they'
    echo '           will restore everything as it was before the operation.'
    echo
    echo "${WHITE}Command${END}:"
    echo ' start         (re)-start the migration, removing any leftover traces if any'
    echo ' advance       advance the migration to the next step'
    echo ' status        give you information about the current migration'
    echo ' backtrack     back up the migration to the previous step'
    echo ' cancel        cancel the current migration, restoring it to its original state'
    echo ' end           end the migration once it has completed the final step, removing backup information'
    echo
    echo "${WHITE}Step${END}:"
    echo ' step 0        start the migration, giving you instructions about what to do'
    echo ' step 1        validate the modifications you have done to the syntax, and begin'
    echo '               implementing them'
    echo ' step 2        validate your implementation, and make them effective'
    echo
    echo "${WHITE}Options${END}:"
    echo ' -h --help     this message'
}

fatal() {
    echo "migration: $*" >&2
    exit 1
}

clean_file() {
    if [ -f "$1" ]; then
	if [[ "$2" == yes ]]; then
	    echo -n "${ORANGE}Warning${END}: precedent migration not properly ended, removing "
	    echo "leftover file: $1"
	fi
	rm -f "$1"
    fi
}

abort() {
    echo "${RED}error:${END} aborted due to previous error."
    backtrack
    status
    exit 1
}

step0_actions() {
    echo ' * Edit the lexer and parser grammars to expression the new syntax.'
    echo '   They should still be written in the old syntax.'
}

step1_actions() {
    echo ' * Edit the lexer and parser grammars so that they use the new syntax. You should not'
    echo '   add new tokens, modify the existing regex for tokens, add or remove non-terminals, or'
    echo '   modify existing rules for producing such non-terminals.'
    echo ' * Adjust the Rust driver code, if you modified the AST produced by the grammars.'
}

step1() {
    "$MIGR_DIR/step0/beans" parse -d src/lexer/lexer.c{lx,gr} src/lexer/lexer.lx || abort
    "$MIGR_DIR/step0/beans" parse -d src/parser/parser.c{lx,gr} src/lexer/lexer.gr || abort
    "$MIGR_DIR/step0/beans" parse -d src/lexer/lexer.c{lx,gr} src/parser/parser.lx || abort
    "$MIGR_DIR/step0/beans" parse -d src/parser/parser.c{lx,gr} src/parser/parser.gr || abort
    cargo build --release --features _from-ast || abort
    cp "$TARGET/release/beans" "$MIGR_DIR/step1/beans-migration"
    rm -f "$SOURCE"/{parser,lexer}/*."$AST_EXT"
}

step2_actions() {
    echo " * Ensure the new features you have introduced are well tested and documented."
    echo " * Update the tests so that they reflect the changes in the grammars."
    echo " * Launch all the tests to ensure you haven't broken anything."
    echo " * Bump the version, commit and publish."
}

step2() {
    "$MIGR_DIR/step1/beans-migration" compile lexer -o src/lexer/lexer.clx src/lexer/lexer.lx \
	|| abort
    "$MIGR_DIR/step1/beans-migration" compile parser -o src/lexer/lexer.cgr src/lexer/lexer.clx \
				   src/lexer/lexer.gr || abort
    "$MIGR_DIR/step1/beans-migration" compile lexer -o src/parser/parser.clx src/parser/parser.lx \
	|| abort
    "$MIGR_DIR/step1/beans-migration" compile parser -o src/parser/parser.cgr src/parser/parser.clx \
				   src/parser/parser.gr || abort
}

commit_new_step() {
    echo "$STEP" >|"$MIGR_DIR/step"
    mkdir -p "$MIGR_DIR/step$STEP"/{lexer,parser}
    for path in "${GRAMMARS[@]}"; do
	for extension in "${EXTS[@]}" "${PREC_EXTS[@]}"; do
	    cp "$SOURCE/$path.$extension" ".migration/step$STEP/$path.$extension"
	done
    done

    echo "Step ${WHITE}$STEP/$MAX_STEP${END}"
}

start() {
    STEP=0
    if [[ -e "$MIGR_DIR" ]]; then
	cancel
    fi
    mkdir "$MIGR_DIR"
    commit_new_step
    cd "$ROOT" || fatal "the root directory cannot be entered"
    make out/beans
    cp "$OUT/beans" "$MIGR_DIR/step0/"
    echo "Migration successfully started."
}

cancel() {
    STEP=0
    restore
    rm -rf .migration
}

get_step() {
    read -r STEP <.migration/step || fatal 'Migration has not started yet, try `make migration help`.'
}

status() {
    echo "You're currently at step ${WHITE}$STEP/$MAX_STEP${END}"
    echo "You can:"
    eval "step${STEP}_actions"
}

advance() {
    if [[ "$STEP" -eq "$MAX_STEP" ]]; then
	fatal 'You have already completed the final step of the migration, you should now end the
 migration with `migration end`'
    fi

    mkdir ".migration/step$STEP"

    let 'STEP++'
    eval "step$STEP"
    
    commit_new_step
}

restore() {
    for path in "$MIGR_DIR/step$STEP"/**/*; do
	target="$SOURCE"/${path#.migration/}
	cp "$path" "$target"
    done
}

backtrack() {
    if [[ "$STEP" -eq 0 ]]; then
	fatal 'Cannot backtrack before the start of the migration. You can cancel the migration with `make migrate-cancel`'
    fi

    rm -rf "$MIGR_DIR/step$STEP"
    
    let 'STEP--'

    restore
}

end_migration() {
    if [[ "$STEP" -eq "$MAX_STEP" ]]; then
	rm -rf "$MIGR_DIR"
    else
	fatal 'Migration has not reached the final step yet.'
    fi
}

if [ $# -eq 0 ]; then
    fatal "The argument <step> is required."
fi

while [ $# -gt 0 ]; do
    case "$1" in
	-h | --help )
	    usage
	    exit 0
	    ;;
	start )
	    start
	    exit 0
	    ;;
	advance )
	    get_step
	    advance
	    exit 0
	    ;;
	backtrace )
	    get_step
	    backtrack
	    exit 0
	    ;;
	end )
	    get_step
	    exit 0
	    ;;
	-* )
	    fatal 'Unknown options `$1`.'
	    ;;
	* )
	    fatal 'Unkown step `$1`. Please try with `--help` to see the available steps.'
	    ;;
    esac
done
