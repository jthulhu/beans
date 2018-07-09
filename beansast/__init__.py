#!/usr/bin/python3
# -*- coding: utf-8 -*-


__author__ = 'BlackBeans'

import sys, re
from .lexer import Lexer

class Reader:
    def __init__(self, flux, grammars):
        self.grammars = grammars
        self.lexer = Lexer(grammars["lexer"])
        self.lexer(flux)
    def update(self, grammars):
        self.lexer.update(grammars["lexer"])
    def get_node(self):
        self.parser.parse(self.lexer.lex)
