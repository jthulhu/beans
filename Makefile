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

all: build
build: $(TARGET)

check:
	@cargo clippy

clean:
	cargo clean
	$(RM) out

grammars: $(PARSER_GRAMMAR_TARGETS) $(LEXER_GRAMMAR_TARGETS)

install: out/beans
	install -D -m755 $< $(BINDIR)/beans 

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

target/release/beans: $(SOURCES)
	cargo build --release

target/debug/beans: $(SOURCES)
	cargo build

src/parser/gmrs/%.clx: src/parser/gmrs/%.lx $(TARGET)
	$(TARGET) compile lexer -o $@ $<

src/parser/gmrs/%.cgr: src/parser/gmrs/%.gr src/parser/gmrs/%.clx $(TARGET)
	$(TARGET) compile parser --lexer $(word 2,$^) -o $@ $<

.PHONY: all build check clean grammars run test
.SUFFIXES:
