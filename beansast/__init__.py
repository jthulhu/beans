#!/usr/bin/python3
# -*- coding: utf-8 -*-


__author__ = 'BlackBeans'

import sys, re
from .lexer import Lexer
from .parser import Parser

class ASTBuilder:
    def __init__(self, flux, grammars=None):
        self.grammars = grammars
        if not grammars:
            with open("beansast/gmrs/lexer.gmr") as f:
                lexer_grammar = f.read()
            with open("beansast/gmrs/parser.gmr") as f:
                parser_grammar = f.read()
            grammars = {
                "lexer": lexer_grammar,
                "parser": parser_grammar
            }
        self.lexer = Lexer(grammars["lexer"])
        self.lexer(flux)
        self.parser = Parser(grammars["parser"])
        self.parser(self.lexer)
    def update(self, grammars):
        self.lexer.update(grammars["lexer"])
        self.parser.update(grammars["parser"])
    def get_ast(self):
        return self.parser.parse()
