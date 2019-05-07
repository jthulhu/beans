#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections
from .stderr import raise_error, ParsingSyntaxError, token_to_frame
from . import lxrgmrparser


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
        pos = 0
        pos, metastmts = self.read_metastmts(pos)
        with open("beansast/gmrs/plexer-r.gmr") as f:
            self.inp.update(f.read(), "beansast/gmrs/plexer-r.gmr")
            
        while self.inp[pos] != "EOF":
            pos, name = self.read_name(pos)
            if self.inp[pos] == "LBRACKET":
                pos += 1
                pos, tok = self.read_sgl_token(pos, "ID")
                class_ = tok["value"]
                pos, _ = self.read_sgl_token(pos, "RBRACKET")
            else:
                class_ = None
            pos, _ = self.read_sgl_token(pos, "ASSIGNMENT")
            pos, rule = self.read_rule(pos)
            nodizers[name] = ASTNodizer(name, rule, class_)
        return metastmts, nodizers
    def read_sgl_token(self, pos, token):
        if type(token) == set:
            if not self.inp[pos] in token:
                last = token.pop()
                sent = ", ".join(token) + " or " + last
                raise_error(ParsingSyntaxError(token_to_frame(self.inp[pos]), sent, self.inp[pos]), helpmsg=self.helperr)
            else:
                return pos + 1, self.inp[pos]
        else:
            if not self.inp[pos] == token:
                raise_error(ParsingSyntaxError(token_to_frame(self.inp[pos]), token, self.inp[pos]))
            else:
                return pos + 1, self.inp[pos]
    def read_metastmt(self, pos):
        _, tok = self.read_sgl_token(pos, {"ASSOC", "CLASS", "FIRST", "END"})
        if tok == "END":
            pos = self.read_end(pos)
            args = ("end", None)
        elif tok == "ASSOC":
            pos, assoc_rules = self.read_assoc(pos)
            args = ("assoc", assoc_rules)
        elif tok == "CLASS":
            pos, cls_rules = self.read_class(pos)
            args = ("class", cls_rules)
        elif tok == "FIRST":
            pos, first_rules = self.read_first(pos)
            args = ("first", first_rules)
        pos, _ = self.read_sgl_token(pos, "SEMICOLON")
        return pos, args
    def read_class(self, pos):
        pos, _ = self.read_sgl_token(pos, "CLASS")
        pos, _ = self.read_sgl_token(pos, "LBRACKET")
        pos, name = self.read_sgl_token(pos, "ID")
        name = name["value"]
        pos, _ = self.read_sgl_token(pos, "RBRACKET")
        pos, head_rule = self.read_sgl_token(pos, "ID")
        head_rule = head_rule["value"]
        instructions = {"sr": "shift", "d": {}, "p": {}}
        while self.inp[pos] == "COLON":
            pos += 1
            pos, (key, value) = self.read_class_instruction(pos)
            if key == "sr":
                instructions[key] = value
            else:
                instructions[key].update(value)
        return pos, (name, head_rule, instructions)
    def read_class_instruction(self, pos):
        _, tok = self.read_sgl_token(pos, {"SHIFTREDUCE", "PRIORITY", "DEFAULT"})
        if tok == "SHIFTREDUCE":
            pos, res = self.read_shiftreduce(pos)
            args = ("sr", res)
        elif tok == "PRIORITY":
            pos, res = self.read_priority(pos)
            args = ("p", res)
        elif tok == "DEFAULT":
            pos, res = self.read_default(pos)
            args = ("d", res)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
        return pos, args
    def read_first_instruction(self, pos):
        pos, tok = self.read_sgl_token(pos, {"META", "SINGLE"})
        if tok == "META":
            pos, tok = self.read_sgl_token(pos, "BOOL")
            tok = {"True": True, "False": False}[tok["value"]]
            return pos, ("meta", tok)
        elif tok == "SINGLE":
            pos, tok = self.read_sgl_token(pos, "INT")
            tok = int(tok["value"])
            return pos, ("single", tok)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
    def read_first(self, pos):
        attributes = {"priority": None, "single": 0, "meta": False}
        pos, _ = self.read_sgl_token(pos, "FIRST")
        pos, _ = self.read_sgl_token(pos, "LBRACKET")
        pos, tok = self.read_sgl_token(pos, "INT")
        attributes["priority"] = int(tok["value"])
        pos, _ = self.read_sgl_token(pos, "RBRACKET")
        pos, tok = self.read_sgl_token(pos, "ID")
        root_token = tok["value"]
        
        while self.inp[pos] == "COLON":
            pos += 1
            pos, (key, value) = self.read_first_instruction(pos)
            attributes[key] = value
    
        return pos, (root_token, attributes)
    def read_end(self, pos):
        pos, _ = self.read_sgl_token(pos, "END")
        pos, _ = self.read_sgl_token(pos, "LBRACKET")
        pos, _ = self.read_sgl_token(pos, "RBRACKET")
        return pos
    def read_assoc(self, pos):
        pos, _ = self.read_sgl_token(pos, "ASSOC")
        pos, tokens = self.read_tokens(pos)
        pos, tok = self.read_sgl_token(pos, {"RIGHT", "LEFT", "NONA"})
        if tok == "RIGHT":
            assoc = "right"
        elif tok == "LEFT":
            assoc = "left"
        elif tok == "NONA":
            assoc = "nona"
        else:
            # wtf...
            raise RuntimeError("This should not happen...")
        return pos, {token: assoc for token in tokens}

    def read_metastmts(self, pos):
        pos, (stmt, args) = self.read_metastmt(pos)
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
            pos, (stmt, args) = self.read_metastmt(pos)
        metastmts["first"].sort(key=lambda x: x[0])
        return pos, metastmts
    def read_tokens(self, pos):
        tokens = []
        pos, _ = self.read_sgl_token(pos, "LBRACKET")
        pos, tok = self.read_sgl_token(pos, "ID")
        tokens.append(tok["value"])
        while self.inp[pos] == "COMMA":
            pos += 1
            pos, tok = self.read_sgl_token(pos, "ID")
            tokens.append(tok["value"])
        pos, _ = self.read_sgl_token(pos, "RBRACKET")
        return pos, tokens
    def read_proxy_element(self, pos):
        pos, tok = self.read_sgl_token(pos, "ID")
        key = tok["value"]
        pos, _ = self.read_sgl_token(pos, "COLON")
        pos, tok = self.read_sgl_token(pos, {"STRING", "INT", "FLOAT"})
        value = tok["value"]
        return pos, (key, value)
        
    def read_proxy(self, pos):
        pos, _ = self.read_sgl_token(pos, "LPROXY")
        if self.inp[pos] == "RPROXY":
            pos += 1
            return pos, {}
        result = {}
        pos, (key, value) = self.read_proxy_element(pos)
        result[key] = value
        while self.inp[pos] == "COMMA":
            pos += 1
            pos, (key, value) = self.read_proxy_element(pos)
            result[key] = value
        pos, _ = self.read_sgl_token(pos, "RPROXY")
        return pos, result
    def read_default(self, pos):
        pos, _ = self.read_sgl_token(pos, "DEFAULT")
        pos, defaults = self.read_proxy(pos)
        return pos, defaults
    def read_priority(self, pos):
        pos, _ = self.read_sgl_token(pos, "PRIORITY")
        pos, tokens = self.read_tokens(pos)
        pos, tok = self.read_sgl_token(pos, "INT")
        priority = int(tok["value"])
        return pos, {token: priority for token in tokens}
    def read_shiftreduce(self, pos):
        pos, _ = self.read_sgl_token(pos, "SHIFTREDUCE")
        pos, _ = self.read_sgl_token(pos, "LBRACKET")
        pos, tok = self.read_sgl_token(pos, {"SHIFT", "REDUCE"})
        rule = "reduce" if tok == "REDUCE" else "shift"
        pos, _ = self.read_sgl_token(pos, "RBRACKET")
        return pos, rule
    def read_name(self, pos):
        pos, tok = self.read_sgl_token(pos, "ID")
        name = tok["value"]
        return pos, name
    def read_rule(self, pos):
        rule = []
        while self.inp[pos].name != "SEMICOLON":
            if self.inp[pos].name == "EOF":
                raise_error(ParsingSyntaxError(token_to_frame(self.inp[pos]), "SEMICOLON", self.inp[pos]), helpmsg=self.helperr)
            rule.append(self.inp[pos])
            pos += 1
        eof = lxrgmrparser.Token("EOF", {})
        eof.file = self.inp.file
        eof.tkpos = len(rule)
        eof.pos = rule[-1].pos
        rule.append(eof)
        return pos + 1, rule
