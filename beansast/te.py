#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
TE: Tokens Expression (tokens' version of re)
"""

import json

def compile(rule):
    return PatternCompile(rule)

def match(rule, tokens, context):
    rule = PatternCompile(rule)
    return rule.match(tokens, context)
    

alphanumerics = set([chr(a) for a in range(ord("a"), ord("z") + 1)] + [chr(a) for a in range(ord("A"), ord("Z") + 1)] + [chr(a) for a in range(ord("0"), ord("9") + 1)])

class PatternCompile:
    def __init__(self, rule):
        rules = rule.split("*")
        self.crules = []
        for rule in rules:
            self.pos = 0
            self.rule = rule
            self.crules.append(self._read_rule())
        
    def _read_rule(self):
        self._ignore_spaces()
        tokens = []
        while self.rule[self.pos] != "<":
            tokens.append(TokenParser(*self._read_token()))
            print("",self.pos)
            self.pos += 1
            self._ignore_spaces()
        print(self.rule[self.pos], self.pos)
        proxy = self._read_proxy()
        return Rule(tokens, proxy)
    def _read_token(self):
        name = self._read_id()
        key = self._read_key()
        optional = self._read_optional()
        return name, key, optional
    def _read_proxy(self):
        self.pos += 1
        proxy = ""
        print(self.pos)
        while self.rule[self.pos] != ">":
            proxy += self.rule[self.pos]
            self.pos += 1
            print(self.pos)
        self.pos += 1
        proxy = [a.strip(" ").rstrip(" ") for a in proxy.split("+", 1)]
        return proxy
    def _read_id(self):
        id_ = ""
        while self.rule[self.pos] in alphanumerics:
            id_ += self.rule[self.pos]
            self.pos += 1
        return id_
    def _read_key(self):
        if self.rule[self.pos] == "@":
            self.pos += 1
            key = self._read_id()
            return key
        return None
    def _read_optional(self):
        if self.rule[self.pos] == "?":
            self.pos += 1
            return True
        return False
    def _ignore_spaces(self):
        print(self.rule[self.pos])
        while self.rule[self.pos] in {" ", "\t", "\n"}:
            self.pos += 1
    def match(self, tokens, context):
        for rule in self.crules:
            is_good, pos, result = rule(tokens, 0).check(context)
            if is_good:
                return result
        return None
    def __repr__(self):
        return 'te.compile(%s)' % " : ".join([repr(rule) for rule in self.crules])


class TokenParser:
    def __init__(self, name, key=None, optional=False):
        self.name = name
        self.key = key
        self.optional = optional
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
    def check(self, context):
        is_good, pos, result = context[self.name](flux, pos).check(context)
        is_good = max(is_good, self.optional) # same as ``is_good or self.optional''
        if self.key:
            result = {self.key: result}
        else:
            result = None
        return is_good, pos, result
    def __repr__(self):
        rpr = self.name
        if self.key:
            rpr += "@" + self.key
        if self.optional:
            rpr += "?"
        return rpr

class Rule:
    def __init__(self, tokens, proxy):
        self.tokens = tokens
        self.proxy = proxy
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
    def check(self, context):
        vars_ = {}
        for token in tokens:
            is_good, self.pos, result = token(self.flux, self.pos).check(context)
            if not is_good:
                return False, self.pos, None
            for proxy in self.proxy:
                vars_.update(evaluate(result, proxy))
        return True, self.pos, vars_
    def __repr__(self):
        return "<TE Rule %s with proxy <%s>>" % (" ".join([repr(token) for token in self.tokens]), " + ".join(self.proxy))
    
def evaluate(expr, vars_):
    if vars_ == ".":
        return expr
    else:
        return eval(vars_, expr)
