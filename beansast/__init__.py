#!/usr/bin/python3
# -*- coding: utf-8 -*-


__author__ = 'BlackBeans'

import sys, re
from .lexer import Lexer
from .parser import Parser
from .stdout import print_tree, print_times, print_times_r
import time

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

class ShellAST:
    def __init__(self):
        self.reload()
    def compute(self, flux):
        self.t1 = time.time()
        self.lexer(flux)
        self.t2 = time.time()
        self.parser(self.lexer)
        self.t3 = time.time()
        result = self.parser.parse()
        self.t4 = time.time()
        return result
    def print_compute(self, flux):
        try:
            print_tree(self.compute(flux), True)
        except SystemExit:
            pass
    def print_times_r(self):
        print_times_r(self.tt1,self.tt2,self.tt3)
    def print_times(self):
        print_times(self.t1,self.t2,self.t3,self.t4)
    def reload(self):
        with open("beansast/gmrs/lexer.gmr") as f:
            lexer_grammar = f.read()
        with open("beansast/gmrs/parser.gmr") as f:
            parser_grammar = f.read()
        self.tt1 = time.time()
        self.lexer = Lexer(lexer_grammar)
        self.tt2 = time.time()
        self.parser = Parser(parser_grammar)
        self.tt3 = time.time()
