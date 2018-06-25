#!/usr/bin/python3
# -*- coding: utf-8 -*-


__author__ = 'BlackBeans'

import sys, re
from .primitives import primitives
from .tokenparser import StatementTokenParser
from .astnodes import *
from .rawparsers import raw_parsers

#############
### LEXER ###
#############

class Lexer:
    def __init__(self, flux):
        self.flux = list(flux) + ["EOF"]
        self.pos = 0
    def lex(self):
        while True:
            is_good = False
            for parser in raw_parsers:
                parsed = parser(self.flux, self.pos)
                if parsed.check_first():
                    parsed = parser(self.flux, self.pos)
                    is_good, pos, result = parsed.check_all()
                    if is_good:
                        self.pos = pos
                        if result:
                            return result
                        else:
                            break
            if is_good:
                continue                
            raise Exception(self.flux[self.pos] + str(self.pos))
    

##############
### PARSER ###
##############

class Parser:
    def __init__(self, lexer):
        self.flux = [lexer.lex()]
        while self.flux[-1].id != primitives["EOF"]:
            self.flux.append(lexer.lex())
        self.pos = 0
        self.AST = []
    def parse(self):
        while self.flux[self.pos].id != primitives["EOF"]:
            is_good, pos, node = StatementTokenParser(self.flux, self.pos).check()
            if not is_good:
                raise Exception("Token #%s not recognized - %s" % (pos, self.flux[self.pos]))
            self.AST.append(node)
            self.pos = pos
        return self.AST
