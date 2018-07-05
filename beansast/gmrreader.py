#!/usr/bin/python3
# -*- coding: utf-8 -*-

import re

class Token:
    def __init__(self, name, **attributes):
        self.name = name
        self.attributes = attributes

class Tokenizer:
    def __init__(self, name, rule, ignore=False):
        self.name = name
        self.rule = re.compile(rule)
        self.ignore = ignore
    def __call__(self, flux, pos):
        result = self.rule.match(flux[pos:])
        if result:
            if self.ignore: return True, result.end, None
            return True, result.end, Token(self.name, **result.groupdict())
        else:
            return False, pos, None
    def __repr__(self):
        return "<Tokenizer named %s with rule %s %s>" % (self.name, self.rule, "- ignored" * int(self.ignore))

class LexerReader:
    def __init__(self, inp):
        self.inp = inp
        self.pos = 0
    def read(self):
        tokenizers = {}
        while self.pos < len(self.inp):
            self.pos = self.ignore_lines(self.pos)
            self.pos, ignore = self.read_ignore(self.pos)
            self.pos, name = self.read_name(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos = self.ignore_assignment(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos, rule = self.read_rule(self.pos)
            tokenizers[name] = Tokenizer(name, rule, ignore)
        return tokenizers
            
    def read_ignore(self, pos):
        if self.inp[pos:].startswith("ignore "):
            return pos+len("ignore "), True
        else:
            return pos, False
    def read_name(self, pos):
        result = ""
        while self.inp[pos] not in {" ", "\t"}:
            result += self.inp[pos]
            pos += 1
        return pos, result
    def ignore_assignment(self, pos):
        if self.inp[pos:].startswith("::="):
            return pos+len("::=")
        raise SyntaxError("syntax is wrong at character %s of line %s" % pos2coords(pos, self.inp))
    def read_rule(self, pos):
        maxsize = len(self.inp) # in case it doesn't end with \n
        rule = ''
        while pos < maxsize and self.inp[pos] != '\n':
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
    y = 1
    x = 1
    for char in flux[:pos]:
        if char == '\n':
            y += 1
            x = 1
        else:
            x += 1
    return x, y
