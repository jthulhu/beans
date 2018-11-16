#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
TE: Tokens Expression (tokens' version of re)
"""

import json
from .psrgmrparser import ParsingSyntaxError


def compile(rule):
    return PatternCompile(rule)

def match(rule, tokens, context):
    rule = PatternCompile(rule)
    return rule.match(tokens, context)
    


class PatternCompile:
    def __init__(self, rule):
        self.crules = []
        self.pos = 0
        while rule[self.pos].name != "EOF":
            self.rule = rule
            self.crules.append(self._read_rule())
        
    def _read_rule(self):
        self._ignore_colon()
        tokens = []
        while self.rule[self.pos].name != "LPROXY":
            tokens.append(TokenParser(*self._read_token()))
        proxy = self._read_proxy()
        return Rule(tokens, proxy)
    def _read_token(self):
        name = self._read_id()
        key = self._read_key()
        return name, key
    def _read_proxy(self):
        self.pos += 1
        proxy = []
        while self.rule[self.pos].name != "RPROXY":
            if self.rule[self.pos].name == "EOF":
                raise ParsingSyntaxError("DOT or ID", self.rule[self.pos])
            if self.rule[self.pos].name == "DOT":
                proxy.append(".")
            elif self.rule[self.pos].name == "ID":
                key = self.rule[self.pos]["name"]
                self.pos += 1
                if not self.rule[self.pos].name == "COLON":
                    raise ParsingSyntaxError("COLON", self.rule[self.pos])
                self.pos += 1
                if not self.rule[self.pos].name == "ID":
                    raise ParsingSyntaxError("ID", self.rule[self.pos])
                value = self.rule[self.pos]["name"]
                proxy.append("%s:%s" % (key, value))
            self.pos += 1
        self.pos += 1
        proxy = ",".join(proxy)
        return proxy
    def _read_id(self):
        id_ = []
        multi = self.rule[self.pos].name == 'LPAR'
        if multi:
            self.pos += 1
        elif self.rule[self.pos].name == "ID":
            self.pos += 1
            return self.rule[self.pos - 1]["name"]
        else:
            raise ParsingSyntaxError("ID or LPAR", self.rule[self.pos])
        if self.rule[self.pos].name == "ID":
            id_.append(self.rule[self.pos]["name"])
            self.pos += 1
        else:
            raise ParsingSyntaxError("ID", self.rule[self.pos])
        next_ = self.rule[self.pos]
        while next_.name == "LINE":
            self.pos += 1
            next_ = self.rule[self.pos]
            if next_.name == "ID":
                id_.append(next_["name"])
            else:
                raise ParsingSyntaxError("ID", next_)
            self.pos += 1
            next_ = self.rule[self.pos]
        if not next_.name == "RPAR": raise ParsingSyntaxError("RPAR", next_)
        self.pos += 1
            
        return id_
    
    def _read_key(self):
        if self.rule[self.pos].name == "AT":
            self.pos += 1
            if self.rule[self.pos].name == "ID":
                key =  self.rule[self.pos]["name"]
                self.pos += 1
            else:
                raise ParsingSyntaxError("ID", self.rule[self.pos])
            return key
        return None
    def _ignore_colon(self):
        if self.rule[self.pos].name == "COLON":
            self.pos += 1
        else:
            raise ParsingSyntaxError("COLON", self.rule[self.pos])
    def match(self, tokens, context):
        for rule in self.crules:
            is_good, pos, result = rule(tokens, 0).check(context)
            if is_good:
                return result
        return None
    def __repr__(self):
        return 'te.compile(%s)' % "\n : ".join([repr(rule) for rule in self.crules])
    def __getitem__(self, key):
        return self.crules[key]


class TokenParser:
    def __init__(self, name, key=None):
        self.multi = isinstance(name, list)
        if not self.multi:
            name = [name]
        self.name = name
        self.key = key
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
    def check(self, context):
        for name in self.name:
            is_good, pos, result = context[name](flux, self.pos).check(context)
            if is_good: break
        if self.key:
            result = {self.key: result}
        else:
            result = None
        return is_good, pos, result
    def __repr__(self):
        rpr = "|".join(self.name)
        if self.multi: rpr = '(' + rpr + ')'
        if self.key:
            rpr += "@" + self.key
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
        return "<TE Rule %s with proxy <%s>>" % (" ".join([repr(token) for token in self.tokens]), self.proxy)
    
def evaluate(expr, vars_):
    if vars_ == ".":
        return expr
    else:
        vars_ = vars_.split(":")
        return {vars_[0]: vars_[1]}
