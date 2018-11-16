#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections

class ParsingError(Exception):
    pass

class ParsingSyntaxError(ParsingError):
    def __init__(self, excpected, token):
        msg = """Syntax error:
 file %s line %s character %s (token %s)
 excpected %s and got %s""" % (token.file, token.pos[1], token.pos[0], token.tkpos, excpected, token.name)
        self.args = (msg,)

from . import te, lxrgmrparser


class ASTNode:
    def __init__(self, name, attributes):
        self.name = name
        self.attributes = attributes
    def __repr__(self):
        return "<ASTNode named %s%s>" % (self.name, (" - %s" % str(self.attributes)) * int(bool(self.attributes)))

class ASTNodizer: # also called parser, but to not mess up with names
    def __init__(self, name, rule):
        self.name = name
        self.rule = te.compile(rule)
    def __call__(self, flux, pos):
        result = self.rule.match(flux[pos:])
    def __repr__(self):
        return "<ASTNodizer named %s with rule %s>" % (self.name, self.rule)

class ParserReader:
    def __init__(self, inp):
        self.inp = inp # beansast.lexer.Lexer self.inp = self.inp
        self.pos = 0
    def read(self):
        nodizers = collections.OrderedDict()
        while self.inp[self.pos].name != "EOF":
            
            self.pos, name = self.read_name(self.pos)
            self.pos = self.ignore_assignment(self.pos)
            self.pos, rule = self.read_rule(self.pos)
            nodizers[name] = ASTNodizer(name, rule)
        return nodizers
    def read_name(self, pos):
        if self.inp[pos].name == "ID":
            name = self.inp[pos]["name"]
            pos += 1
        else:
            raise ParsingSyntaxError("ID", self.inp[pos])
        return pos, name
    def ignore_assignment(self, pos):
        if self.inp[pos].name == "ASSIGNMENT":
            pos += 1
            return pos
        raise ParsingSyntaxError("ASSIGNMENT", self.inp[pos])
    def read_rule(self, pos):
        rule = []
        while self.inp[pos].name != "SEMICOLON":
            if self.inp[pos].name == "EOF":
                raise ParsingSyntaxError("SEMICOLON", self.inp[pos])
            rule.append(self.inp[pos])
            pos += 1
        eof = lxrgmrparser.Token("EOF", {})
        eof.pos = rule[-1].pos
        rule.append(eof)
        return pos + 1, rule
