#!/usr/bin/python3
# -*- coding: utf-8 -*-


__author__ = 'BlackBeans'

import sys, re
from .lexer import Lexer
from .parser import Parser

class Reader:
    def __init__(self, flux, grammars):
        self.grammars = grammars
        self.lexer = Lexer(grammars["lexer"])
        self.lexer(flux)
        self.parser = Parser(grammars["parser"])
        self.parser(self.lexer)
    def update(self, grammars):
        self.lexer.update(grammars["lexer"])
        self.parser.update(grammars["parser"])
    def get_node(self):
        context = self.lexer.tokenizers.copy()
        context.update(self.parser.nodizers)
        self.parser.parse(context)
