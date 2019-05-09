#!/usr/bin/python3
# -*- coding: utf-8 -*-

import re, collections
from .stderr import Frame, raise_error, GrammarSyntaxError

class Token:
    """Standard token class
needs a name, which will be used by the parser, and attributes, a dict, which will be used to know what there was inside the token"""
    def __init__(self, name, attributes):
        self.name = name
        self.attributes = attributes
    def __repr__(self):
        return "<Token named %s%s>" % (self.name, (" - %s" % str(self.attributes)) * int(bool(self.attributes)))
    def __str__(self):
        return self.name
    def __getitem__(self, key):
        return self.attributes[key]
    def __eq__(self, right):
        # Token == Token means same .name, .attributes
        # Token == "name" means Token.name = "name"
        return (isinstance(right, type(self)) and right.name == self.name and right.attributes == self.attributes) or (isinstance(right, str) and right == str(self))
    def __hash__(self):
        return hash(self.name)
    def __bool__(self):
        return True

class Tokenizer:
    """Initialised with a name, a rule and an ignore flag
when called, it will match the string at given pos. If match, it will return True, then pos match ends, then Token generated or None if ignore flag was True. If not match, returns False, pos with which it was called, None"""
    def __init__(self, name, rule, ignore=False):
        self.name = name
        self.rule = re.compile(rule, re.M)
        self.ignore = ignore
    def __call__(self, flux, pos):
        result = self.rule.match(flux[pos:])
        if result:
            if self.ignore: return True, pos + result.end(), None
            return True, pos + result.end(), Token(self.name, result.groupdict())
        else:
            return False, pos, None
    def __repr__(self):
        return "<Tokenizer named %s with rule %s%s>" % (self.name, self.rule, " - ignored" * int(self.ignore))
    def __eq__(self, right):
        return hasattr(right, "name") and right.name == self.name and hasattr(right, "rule") and right.rule == self.rule and hasattr(right, "ignore") and right.ignore == self.ignore

class LexerReader:
    """Reads a grammar file and generates tokenizers from that file. You can precise the filename with the argument file. It will be used when ar error is raised, and is completly indipendent from the actual file reed."""
    def __init__(self, inp, file="<stdgmr>", helperr=False):
        self.helperr = helperr
        self.inp = inp
        self.pos = 0
        self.file = file
    def read(self):
        tokenizers = collections.OrderedDict()
        delete = []
        self.pos = self.ignore_lines(self.pos)
        while self.pos < len(self.inp):
            self.pos, del_ = self.read_del(self.pos)
            if del_:
                self.pos, name = self.read_name(self.pos)
                delete.append(name)
                self.pos = self.ignore_lines(self.pos)
                continue
            self.pos, ignore = self.read_ignore(self.pos)
            self.pos, name = self.read_name(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos = self.ignore_assignment(self.pos)
            self.pos = self.ignore_spaces(self.pos)
            self.pos, rule = self.read_rule(self.pos)
            tokenizers[name] = Tokenizer(name, rule, ignore)
            self.pos = self.ignore_lines(self.pos)
        return tokenizers, delete

    def read_del(self, pos):
        if self.inp[pos:].startswith("del "):
            return pos+len("del "), True
        return pos, False
    def read_ignore(self, pos):
        if self.inp[pos:].startswith("ignore "):
            return pos+len("ignore "), True
        else:
            return pos, False
    def read_name(self, pos):
        result = ""
        while self.inp[pos] not in {" ", "\t", "\n"}:
            result += self.inp[pos]
            pos += 1
        return pos, result
    def ignore_assignment(self, pos):
        if self.inp[pos:].startswith("::="):
            return pos+len("::=")
        raise_error(GrammarSyntaxError(Frame(self.file, *pos2coords(pos, self.inp)), "::="), helpmsg=self.helperr)
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
    x = 0
    for char in flux[:pos]:
        if char == '\n':
            y += 1
            x = 0
        else:
            x += 1
    return x, y
