#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections
from . import te
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
        self.inp = inp
        self.pos = 0
    def read(self):
        nodizers = collections.OrderedDict()
        maxsize = len(self.inp)
        self.pos = self.ignore_lines(self.pos)
        while self.pos < maxsize:
            self.pos, name = self.read_name(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos = self.ignore_assignment(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos, rule = self.read_rule(self.pos)
            self.pos = self.ignore_lines(self.pos)
            nodizers[name] = ASTNodizer(name, rule)
        return nodizers
    def read_name(self, pos):
        result = ""
        alphanumerics = set([chr(a) for a in range(ord("a"), ord("z") + 1)] + [chr(a) for a in range(ord("A"), ord("Z") + 1)] + [chr(a) for a in range(ord("0"), ord("9") + 1)])
        while self.inp[pos] in alphanumerics:
            result += self.inp[pos]
            pos += 1
        return pos, result
    def ignore_assignment(self, pos):
        if self.inp[pos:].startswith("::="):
            return pos+len("::=")
        raise SyntaxError("syntax is wrong at character %s of line %s" % pos2coords(pos, self.inp))
    def read_rule(self, pos):
        rule = ""
        while self.inp[pos] != ";":
            rule += self.inp[pos]
            pos += 1
        return pos + 1, rule
    def ignore_spaces(self, pos):
        maxsize = len(self.inp)
        while pos < maxsize and self.inp[pos] in {" ", "\t"}:
            pos += 1
        return pos
    def ignore_lines(self, pos):
        maxsize = len(self.inp)
        while pos < maxsize and self.inp[pos] in {" ", "\t", "\n"}:
            pos += 1
        return pos

def pos2coords(pos, flux):
    x = 1
    y = 1
    for char in flux[:pos]:
        if char == "\n":
            y += 1
            x = 1
        else:
            x += 1
    return x, y
