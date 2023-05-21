SOURCES := $(shell find src -name '*.rs')
PARSER_GRAMMARS := $(wildcard src/parser/gmrs/*.gr)
LEXER_GRAMMARS := $(wildcard src/parser/gmrs/*.lx)
PARSER_GRAMMAR_TARGETS := $(patsubst %.gr,%.cgr,$(PARSER_GRAMMARS))
LEXER_GRAMMAR_TARGETS := $(patsubst %.lx,%.clx,$(LEXER_GRAMMARS))

DESTDIR ?=
PREFIX ?= /usr/local
BINDIR = $(DESTDIR)$(PREFIX)/bin

BUILD_FLAGS :=
ifdef RELEASE
TARGET := out/beans
ACTUAL_LOCATION := target/release/beans
BUILD_FLAGS += --release
else
TARGET := out/beans-debug
ACTUAL_LOCATION := target/debug/beans
endif

# This stinks.  I found those directories in
#     /usr/share/bash-completion/bash_completion
#   (search for '__load_completion' and 'compat_dir').
# The third case in particular is just tossing the dies in the air.
ifeq ($(PREFIX),/usr/local)
    BASH_COMPLETION_DIR := /usr/local/share/bash-completion/completions
else ifeq ($(PREFIX),$(HOME))
    BASH_COMPLETION_DIR := $(HOME)/.local/share/bash-completion/completions
else
    BASH_COMPLETION_DIR := $(PREFIX)/etc/bash_completion.d
endif

all: build
build: $(TARGET)

check:
	@cargo clippy

clean:
	cargo clean
	$(RM) out

grammars: $(PARSER_GRAMMAR_TARGETS) $(LEXER_GRAMMAR_TARGETS)

install: install-exe install-bash-completion

install-exe: out/beans
	install -D -m755 $< $(BINDIR)/beans

# Ahem.  install does not know how to both create the directories
#   *and* copy the file in there, unless you use -D and also specify
#   the name of the destination file.  Now, I do not want to specify
#   the destination filename, because it is already in the
#   prerequisites. Nor do I want to play with make functions in order
#   to extract the base name.
# install sucks.
install-bash-completion: bash/beans
	install -m755 -d $(BASH_COMP_DIR)
	install -m644 $< $(BASH_COMP_DIR)/

run: build
	@./$(TARGET)

test:
	@cargo test

out:
	@mkdir out

out/%: target/release/% | out
	@cp $< $@

out/%-debug: target/debug/% | out
	@cp $< $@

target/release/beans: $(SOURCES) Cargo.lock
	cargo build --release

target/debug/beans: $(SOURCES) Cargo.lock
	cargo build

%.clx: %.lx $(TARGET)
	$(TARGET) compile lexer -o $@ $<

%.cgr: %.gr %.clx $(TARGET)
	$(TARGET) compile parser -o $@ $(word 2,$^) $<

migrate-step1: out/beans
	@scripts/migration.sh step1

migrate-step2:
	@scripts/migration.sh step2

migrate-step3:
	@scripts/migration.sh step3

.PHONY: all ast build check clean grammars migrate-step1 migrate-step2 migrate-step3 run test
.SUFFIXES:
