#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
Beansobject scripting language
Here follows the syntax

File ::= Statement *
Statement ::= Assignement|FunctionDefinition|ClassDefinition|FunctionCall
FunctionCall ::= VARIABLE LPAR ExpressionsList RPAR
FunctionDefinition ::= FUNCTION VARIABLE COLON ArgumentList Body
Body ::= LBRACE Statement * RBRACE
ClassDefinition ::= (FROM ClassNameList) ? CLASS VARIABLE COLON ArgumentList ClassBody
ClassBody ::= METHOD VARIABLE COLON ArgumentList Body
ArgumentList ::= (ARGUMENT (COMMA ARGUMENT) *) ?
Assignment ::= VARIABLE EQUALS Expression
ExpressionList ::= (Expression (COMMA Expression) *) ?
Expression ::= STRING|FLOAT|INT
"""

__author__ = 'BlackBeans'

import sys, re
from .primitives import primitives
from .tokenparser import StatementTokenParser
from .ASTnodes import *  # useless for now
from .rawparsers import raw_parsers

#############
### LEXER ###
#############

class Lexer:
    def __init__(self, flux):
        self.flux = flux
        self.pos = 0
    def lex(self):
        while True:
            is_good = False
            for parser in raw_parsers:
                parsed = parser(self.flux, self.pos)
                if parsed.check_first():
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
        while self.flux[-1].code != primitives["EOF"]["code"]:
            self.flux.append(lexer.lex())
        self.flux.append(lexer.lex()) # append also EOF so parser knows where to stop
        self.pos = 0
        self.AST = []
    def parse(self):
        while self.flux[self.pos].code != primitives["EOF"]["code"]:
            is_good, pos, node = StatementTokenParser(self.flux, self.pos).check()
            if not is_good:
                raise Exception("Token #%s not recognized - %s" % (pos, self.flux[self.pos]))
            self.AST.append(node)
            self.pos = pos
        return self.AST
