#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections
from .stderr import raise_error, ParsingSyntaxError, token_to_frame
from . import lxrgmrparser

class Rule:
    def __init__(self, tokens, proxy):
        self.tokens = tokens
        self.proxy = proxy

class ASTNode:
    def __init__(self, name, attributes):
        self.name = name
        self.attributes = attributes
    def __repr__(self):
        return "<ASTNode named %s%s>" % (self.name, (" - %s" % str(self.attributes)) * int(bool(self.attributes)))
    def __str__(self):
        return self.name
    def __hash__(self):
        return str(self)
    def __eq__(self, right):
        return (isinstance(right, type(self)) and right.name == self.name and right.attributes == self.attributes) or (isinstance(right, str) and right == str(self))

class ASTNodizer: # also called parser, but to not mess up with names
    def __init__(self, name, rule, class_):
        self.name = name
        self.rule = rule
        self.class_ = class_
    def __call__(self, flux, pos):
        result, pos2 = self.rule.match(flux[pos:])
        pos += pos2
        if result:
            return True, pos, result
        return False, pos, None
    def __repr__(self):
        return "<ASTNodizer named %s with rule %s>" % (self.name, self.rule)

class ASTTokenNodizer(ASTNodizer):
    # ASTNodizer but to compile simple tokens e.g. ID is a token
    # so it will have an ASTTokenNodizer bound, so references
    # to ID in other rules will corrispond to an ASTNodizer
    def __init__(self, name):
        self.name = name
    def __call__(self, flux, pos):
        if flux[pos] == self.name:
            return True, flux[pos].attributes

class ParserReader:
    def __init__(self, inp, helperr=False):
        self.helperr = helperr
        self.inp = inp # beansast.lexer.Lexer self.inp = self.inp
    def read(self):
        nodizers = collections.OrderedDict()
        self.pos = 0
        metastmts = self.read_metastmts()
        with open("beansast/gmrs/plexer-r.gmr") as f:
            self.inp.update(f.read(), "beansast/gmrs/plexer-r.gmr")
            
        while not self.ahead_sgl_token("EOF"):
            name = self.read_name()
            if self.ahead_sgl_token("LBRACKET"):
                tok = self.read_sgl_token("ID")
                class_ = tok["value"]
                self.read_sgl_token("RBRACKET")
            else:
                class_ = None
            self.read_sgl_token("ASSIGNMENT")
            rules = self.read_rules()
            self.read_sgl_token("SEMICOLON")
            nodizers[name] = ASTNodizer(name, rules, class_)
        return metastmts, nodizers
    def aheadf_sgl_token(self, token):
        tok = self.ahead_sgl_token(token, step=False)
        if not tok: self.err_toks(token)
        else: return tok
    def ahead_sgl_token(self, token, step=True):
        if (type(token) == set and self.inp[self.pos] in token) or (type(token) != set and self.inp[self.pos] == token):
            pos = self.pos
            if step: self.pos += 1
            return self.inp[pos]
        else:
            return None
    def err_toks(self, token):
        last = (" or " + token.pop()) if type(token) == set else ""
        sent = (", ".join(token) if type(token) == set else token) + last
        raise_error(ParsingSyntaxError(token_to_frame(self.inp[self.pos]), sent, self.inp[self.pos]), helpmsg=self.helperr)
    def read_sgl_token(self, token):
        tok = self.ahead_sgl_token(token)
        if tok: return tok
        else: self.err_toks(token)
    def read_metastmt(self):
        tok = self.aheadf_sgl_token({"ASSOC", "CLASS", "FIRST", "END"})
        if tok == "END":
            self.read_end()
            args = ("end", None)
        elif tok == "ASSOC":
            assoc_rules = self.read_assoc()
            args = ("assoc", assoc_rules)
        elif tok == "CLASS":
            cls_rules = self.read_class()
            args = ("class", cls_rules)
        elif tok == "FIRST":
            first_rules = self.read_first()
            args = ("first", first_rules)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
        self.read_sgl_token("SEMICOLON")
        return args
    def read_class(self):
        self.read_sgl_token("CLASS")
        self.read_sgl_token("LBRACKET")
        name = self.read_sgl_token("ID")
        name = name["value"]
        self.read_sgl_token("RBRACKET")
        head_rule = self.read_sgl_token("ID")
        head_rule = head_rule["value"]
        instructions = {"sr": "shift", "d": {}, "p": {}}
        while self.ahead_sgl_token("COLON"):
            (key, value) = self.read_class_instruction()
            if key == "sr":
                instructions[key] = value
            else:
                instructions[key].update(value)
        return (name, head_rule, instructions)
    def read_class_instruction(self):
        tok = self.aheadf_sgl_token({"SHIFTREDUCE", "PRIORITY", "DEFAULT"})
        if tok == "SHIFTREDUCE":
            res = self.read_shiftreduce()
            args = ("sr", res)
        elif tok == "PRIORITY":
            res = self.read_priority()
            args = ("p", res)
        elif tok == "DEFAULT":
            res = self.read_default()
            args = ("d", res)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
        return args
    def read_first_instruction(self):
        tok = self.read_sgl_token({"META", "SINGLE", "UNIQUE"})
        if tok == "META":
            tok = self.read_sgl_token("BOOL")
            tok = {"True": True, "False": False}[tok["value"]]
            return ("meta", tok)
        elif tok == "SINGLE":
            tok = self.read_sgl_token("INT")
            tok = int(tok["value"])
            return ("single", tok)
        elif tok == "UNIQUE":
            tok = self.read_sgl_token("BOOL")
            tok = {"True": True, "False": False}[tok["value"]]
            return ("unique", tok)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
    def read_first(self):
        attributes = {"priority": None, "single": 0, "meta": False, "unique": False}
        self.read_sgl_token("FIRST")
        self.read_sgl_token("LBRACKET")
        tok = self.read_sgl_token("INT")
        attributes["priority"] = int(tok["value"])
        self.read_sgl_token("RBRACKET")
        tok = self.read_sgl_token("ID")
        root_token = tok["value"]
        
        while self.ahead_sgl_token("COLON"):
            (key, value) = self.read_first_instruction()
            attributes[key] = value
    
        return (root_token, attributes)
    def read_end(self):
        self.read_sgl_token("END")
        self.read_sgl_token("LBRACKET")
        self.read_sgl_token("RBRACKET")
    def read_assoc(self):
        self.read_sgl_token("ASSOC")
        tokens = self.read_tokens()
        tok = self.read_sgl_token({"RIGHT", "LEFT", "NONA"})
        if tok == "RIGHT":
            assoc = "right"
        elif tok == "LEFT":
            assoc = "left"
        elif tok == "NONA":
            assoc = "nona"
        else:
            # wtf...
            raise RuntimeError("This should not happen...")
        return {token: assoc for token in tokens}

    def read_metastmts(self):
        stmt, args = self.read_metastmt()
        metastmts = {
            "assoc": {},
            "class": {},
            "first": []
        }
        while stmt != "end":
            if stmt == "assoc":
                metastmts[stmt].update(args)
            elif stmt == "class":
                metastmts[stmt][args[0]] = args[1:3]
            elif stmt == "first":
                metastmts[stmt].append(args)
            else:
                # wtf...
                raise RuntimeError("This should not happen")
            (stmt, args) = self.read_metastmt()
        metastmts["first"].sort(key=lambda x: x[1]["priority"])
        return metastmts
    def read_tokens(self):
        tokens = []
        self.read_sgl_token("LBRACKET")
        tok = self.read_sgl_token("ID")
        tokens.append(tok["value"])
        while self.ahead_sgl_token("COMMA"):
            tok = self.read_sgl_token("ID")
            tokens.append(tok["value"])
        self.read_sgl_token("RBRACKET")
        return tokens
    def read_proxy_element(self):
        tok = self.read_sgl_token("ID")
        key = tok["value"]
        self.read_sgl_token("COLON")
        tok = self.read_sgl_token({"STRING", "INT", "FLOAT"})
        value = tok["value"]
        return (key, value)
        
    def read_proxy(self):
        self.read_sgl_token("LPROXY")
        if self.ahead_sgl_token("RPROXY"):
            return {}
        result = {}
        (key, value) = self.read_proxy_element()
        result[key] = value
        while self.ahead_sgl_token("COMMA"):
            (key, value) = self.read_proxy_element()
            result[key] = value
        self.read_sgl_token("RPROXY")
        return result
    def read_default(self):
        self.read_sgl_token("DEFAULT")
        defaults = self.read_proxy()
        return defaults
    def read_priority(self):
        self.read_sgl_token("PRIORITY")
        tokens = self.read_tokens()
        tok = self.read_sgl_token("INT")
        priority = int(tok["value"])
        return {token: priority for token in tokens}
    def read_shiftreduce(self):
        self.read_sgl_token("SHIFTREDUCE")
        self.read_sgl_token("LBRACKET")
        tok = self.read_sgl_token({"SHIFT", "REDUCE"})
        rule = "reduce" if tok == "REDUCE" else "shift"
        self.read_sgl_token("RBRACKET")
        return rule
    def read_name(self):
        tok = self.read_sgl_token("ID")
        name = tok["value"]
        return name
    def read_rule_token(self):
        token = self.read_sgl_token("ID")
        token = token["value"]
        if self.ahead_sgl_token("AT"):
            key = self.read_sgl_token("ID")
            key = key["value"]
        else:
            key = None
        return (token, key)
    def read_rule(self):
        tokens = []
        while self.ahead_sgl_token("ID", step=False):
            token = self.read_rule_token()
            tokens.append(token)
        proxy = self.read_proxy()
        return Rule(tokens, proxy)
    def read_rules(self):
        rules = []
        while self.ahead_sgl_token("COLON"):
            rule = self.read_rule()
            rules.append(rule)
        return rules
