#!/usr/bin/python3
# -*- coding: utf-8 -*-

from .lexer import Lexer

class ASTNode:
    def __init__(self, name, attributes, location):
        self.name = name
        self.attributes = attributes
        self.location = location

class Parser:
    def __init__(self, grammar):
        self.grammar = grammar
    def compute_grammar(self):
        pass

class ASTBuilder:
    LEXER_GRAMMAR = "beansast/gmrs/lexer.gmr"
    PARSER_GRAMMAR = "beansast/gmrs/parser.gmr"
    def __init__(self, input, input_file="<stdin>"):
        self.input = input
        with open(self.LEXER_GRAMMAR) as f:
            self.lexer_grammar = f.read()
        with open(self.PARSER_GRAMMAR) as f:
            self.parser_grammar = f.read()
        self.lexer = Lexer(self.lexer_grammar, fn=input_file)
        self.parser = Parser(self.parser_grammar)
        self.lexer(self.input)
        self.parser(self.lexer)
    def get_ast(self):
        return self.parser.parse()
