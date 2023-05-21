#!/usr/bin/env bash
# author: jthulhu

GREEN=$'\e[1;32m'
ORANGE=$'\e[1;33m'
RED=$'\e[1;31m'
WHITE=$'\e[1;37m'
END=$'\e[0m'

usage() {
    echo 'migration -- modify the grammar of Beans'
    echo
    echo '${WHITE}Usage${END}: migration <step> [options]'
    echo
    echo ' Changing the grammars of Beans requires compiling both Beans and its grammars several'
    echo ' times, with a specific ordering and options. If you want to do a migration, please'
    echo ' start a new migration and follow the given instruction. DO NOT start modifying the'
    echo ' grammars before starting the migration, as it will fail.'
    echo ' In case you get weird errors, use `git restore` and restart everything.'
    echo 
    echo "${ORANGE}Important${END}: this script should only be called at the toplevel of the Beans repository."
    echo
    echo '${WHITE}Step${END}:'
    echo ' start:        start the migration, giving you instructions about what to do'
    echo ' validate:     validate the modifications you have done to the syntax, and begin'
    echo '               implementing them'
    echo ' end:          validate your implementation, and make them effective'
    echo
    echo '${WHITE}Options${END}:'
    echo ' --help:       this message'
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

step1_actions() {
    echo ' * Edit the lexer and parser grammars to expression the new syntax.'
    echo '   They should still be written in the old syntax.'
    echo ' * Advance to the next step of the migration with `make migrate-step2`'
}

step1() {
    echo "Migrating toward step 1..."
    for file in out/beans-migration src/lexer/lexer.lx.ast src/lexer/lexer.gr.ast \
		src/parser/parser.lx.ast src/parser/parser.gr.ast src/lexer/lexer-new.clx \
		src/lexer/lexer-new.cgr src/parser/parser-new.clx src/parser/parser-new.cgr
    do
	clean_file "$file" yes
    done

    echo "${WHITE}MIGRATION${END}: ${GREEN}step 1${END}"
    echo "The migration has succesfully been started. You can now:"
    step1_actions
}

abort_step2() {
    for file in src/lexer/lexer.lx.ast src/lexer/lexer.gr.ast src/parser/parser.lx.ast \
		src/parser/parser.gr.ast
    do
	clean_file "$file" no
    done

    echo "${WHITE}MIGRATION${END}: ${RED}step 1${END}"
    echo "Advancing to step 2 has failed due to previous error. You can now:"
    step1_actions
    exit 1
}

step2_actions() {
    echo ' * Edit the lexer and parser grammars so that they use the new syntax. You should not'
    echo '   add new tokens, modify the existing regex for tokens, add or remove non-terminals, or'
    echo '   modify existing rules for producing such non-terminals.'
    echo ' * Adjust the Rust driver code, if you modified the AST produced by the grammars.'
    echo ' * Advance to the next step of the migration with `make migrate-step3`'
}

step2() {
    echo "Migrating toward step 2..."
    if [[ ! -f out/beans ]]; then
	make out/beans
    fi
    out/beans parse -d src/lexer/lexer.c{lx,gr} src/lexer/lexer.lx || abort_step2
    out/beans parse -d src/parser/parser.c{lx,gr} src/lexer/lexer.gr || abort_step2
    out/beans parse -d src/lexer/lexer.c{lx,gr} src/parser/parser.lx || abort_step2
    out/beans parse -d src/parser/parser.c{lx,gr} src/parser/parser.gr || abort_step2
    cargo build --release --features _from-ast || abort_step2
    cp target/release/beans out/beans-migration
    echo "${WHITE}MIGRATION${END}: ${GREEN}step 2${END}"
    echo "The migration has succesfully moved to step 2. You can now:"
    step2_actions
}

abort_step3() {
    for file in src/lexer/lexer-new.clx src/lexer/lexer-new.cgr src/parser/parser-new.clx \
		src/parser/parser-new.cgr
    do
	clean_file "$file" no
    done
    echo "${WHITE}MIGRATION${END}: ${RED}step 2${END}"
    echo "Advancing to step 3 has failed due to previous error. You can now:"
    step2_actions
    exit 1
}

step3_actions() {
    echo " * Ensure the new features you have introduced are well tested and documented."
    echo " * Update the tests so that they reflect the changes in the grammars."
    echo " * Launch all the tests to ensure you haven't broken anything."
    echo " * Bump the version, commit and publish."
}

step3() {
    echo "Migrating toward step 3..."
    if [[ ! -f out/beans-migration ]]; then
	echo "${RED}Error${END}: It looks like you haven't run all the previous step of the"
	echo "migrations. Please do that."
	exit 1
    fi

    out/beans-migration compile lexer -o src/lexer/lexer-new.clx src/lexer/lexer.lx \
	|| abort_step3
    out/beans-migration compile parser -o src/lexer/lexer-new.cgr src/lexer/lexer-new.clx \
			src/lexer/lexer.gr || abort_step3
    out/beans-migration compile lexer -o src/parser/parser-new.clx src/parser/parser.lx \
	|| abort_step3
    out/beans-migration compile parser -o src/parser/parser-new.cgr src/parser/parser-new.clx \
			src/parser/parser.gr || abort_step3
    mv src/lexer/lexer{-new,}.clx
    mv src/lexer/lexer{-new,}.cgr
    mv src/parser/parser{-new,}.clx
    mv src/parser/parser{-new,}.cgr
    cargo build --release
    cp target/release/beans out/beans
    rm out/beans-migration src/parser/parser.{lx,gr}.ast src/lexer/lexer.{lx,gr}.ast
    echo "${WHITE}MIGRATION${END}: ${GREEN}ended!${END}"
    echo "You should now:"
    step3_actions
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
	start | step1 )
	    step1
	    exit 0
	    ;;
	validate | step2 )
	    step2
	    exit 0
	    ;;
	end | step3 )
	    step3
	    exit 0
	    ;;
	-* )
	    fatal "Unknown options `$1`."
	    ;;
	* )
	    fatal "Unkown step `$1`. Please try with `--help` to see the available steps."
	    ;;
    esac
done
