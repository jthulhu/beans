#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections, bisect, itertools, gzip, os
from .stderr import *
from . import lxrgmrparser
from carrot import *

proxy = OrderedDict(
    String(),
    Struct(
        String(),
        Bool(),
        Int(),
        String()
    )
)

rule_token = Struct(
    String(),
    String(),
    Bool(),
    String()
)

rule = Struct(
    String(),
    List(rule_token),
    proxy,
    Int()
)

trie_node = Struct(
    List(rule),
    Int(),
    Dict(
        rule_token,
        Int()
    )
)

trie = List(
    trie_node
)

FLAGS = {
    "PRIVATE": 0 << 0,
    "PUBLIC": 1 << 0,
    "PRIORITY": 1 << 1
}

class UUIDGen:
    def __init__(self):
        self.index = -1
    def __next__(self):
        self.index += 1
        return self.index

class ASTNode:
    def __init__(self, name, attributes, pos, file):
        self.name = name
        self.attributes = attributes
        self.pos = pos
        self.file = file
    def __repr__(self):
        return "<ASTNode named %s%s>" % (self.name, (" - %s" % str(self.attributes)) * int(bool(self.attributes)))
    def __getitem__(self, key):
        return self.attributes[key]
    def __str__(self):
        return self.name
    def __hash__(self):
        return hash(str(self) + str(attributes))
    def __eq__(self, right):
        return (isinstance(right, type(self)) and right.name == self.name and right.attributes == self.attributes) or (isinstance(right, str) and right == str(self))

class ASTNodizer: # also called parser, but to not mess up with names
    def __init__(self, name, rule):
        self.name = name
        self.rule = rule
    def __getitem__(self, key):
        return self.rule[key]
    def __len__(self):
        return len(self.rule)
    def __repr__(self):
        return "<ASTNodizer named %s with rule %s>" % (self.name, " ".join(repr(rule) for rule in self.rule))


    
class Proxy:
    def __init__(self):
        self.elements = collections.OrderedDict()
    def add_entry(self, key, value, list_append, flags, type):
        self.elements[key] = (value, list_append, flags, type)
    def __getitem__(self, key):
        return self.elements[key]
    def compile(self):
        return self.elements
    def items(self):
        return self.elements.items()
    @classmethod
    def load(cls, elements):
        proxy = cls()
        proxy.elements = elements
        return proxy
    
class RuleToken:
    def __init__(self, name, attribute, is_append, key, frame):
        self.name = name
        self.attribute = attribute
        self.is_append = is_append
        self.key = key
        self.frame = frame
    def __repr__(self):
        string = self.name
        if self.attribute:
            string += "." + self.attribute
        return string
    def __repr__(self):
        return self.name
    def __eq__(self, right):
        return self.name == right.name
    def __hash__(self):
        return hash(self.name)
    def compile(self):
        return (self.name, self.attribute, self.is_append, self.key)

class Rule:
    def __init__(self, name, tokens, proxy, priority):
        self.name = name
        self.tokens = tokens
        self.proxy = proxy
        self.class_ = None
        self.priority = priority
    @classmethod
    def load(cls, name, tokens, proxy, priority):
        tokens = tuple(RuleToken(*token,None) for token in tokens)
        proxy = Proxy.load(proxy)
        return cls(name, tokens, proxy, priority)
    def __getitem__(self, key):
        return self.tokens[key]
    def __repr__(self):
        return ": " + " ".join(repr(token) for token in self.tokens) + " <" + ", ".join(("[" if value[1] else "") + str(key) + ("]" if value[1] else "") + ": " + str(value[0]) for key, value in self.proxy.items()) + "> " + self.name
    def __len__(self):
        return len(self.tokens)
    def __contains__(self, token):
        return token in self.tokens
    def __bool__(self):
        return bool(self.tokens)
    def set_class(self, class_):
        self.class_ = class_
    def compile(self):
        return (self.name, tuple(token.compile() for token in self.tokens), self.proxy.compile(), self.priority)
    
class TrieNode:
    @classmethod
    def load(cls, rules, id, children, idgen=None, helpmsg=False):
        node = cls(id, idgen, helpmsg)
        node.rules = [Rule.load(*rule) for rule in rules]
        node.children = {RuleToken(*key,None): value for key, value in children.items()}
        return node
    def __init__(self, id, idgen, helpmsg=False):
        self.rules = []
        self.id = id
        self.idgen = idgen
        self.children = {}
        self.helpmsg = helpmsg
    def __getitem__(self, key):
        return self.children[key]
    def __setitem__(self, key, value):
        if len(key) == 0:
            if len(self.rules) > 0:
                raise_warning(error=GrammarAmbiguityError(value.tokens[0].frame, "two rules have the same reduction procedure"), helpmsg=self.helpmsg)
            self.rules.append(value)
        else:
            if key[0] not in self:
                self.children[key[0]] = TrieNode(next(self.idgen), self.idgen, self.helpmsg)
            self.children[key[0]][key[1:]] = value
    def __contains__(self, key):
        return key in self.children
    def compile(self):
        return (tuple(rule.compile() for rule in self.rules), self.id, {key.compile(): value.id for key, value in self.children.items()})

class Trie:
    def find_children(self):
        children = []
        todo = [self.root_node]
        while len(todo) > 0:
            current = todo.pop()
            for child in current.children.values():
                todo.append(child)
            children.append(current)
        return children
    def compile(self):
        result = []
        todo = [self.root_node]
        while len(todo) > 0:
            current = todo.pop()
            for children in current.children.values():
                todo.append(children)
            result.append(current.compile())
        
        return gzip.compress(trie.write(tuple(result)))
    @classmethod
    def from_compilation(cls, raw, helpmsg=False):
        pos, nodes = trie.read(gzip.decompress(raw))
        return cls.load(nodes,helpmsg)
    @classmethod
    def load(cls, nodes, helpmsg=False):
        inodes = {}
        for args in nodes:
            node = TrieNode.load(*args, helpmsg=helpmsg)
            inodes[node.id] = node
        for node in inodes.values():
            for key in list(node.children.keys()):
                node.children[key] = inodes[node.children[key]]
        trie = cls(helpmsg)
        trie.root_node = inodes[0]
        return trie
    def __init__(self, helpmsg):
        self.idgen = UUIDGen()
        self.root_node = TrieNode(next(self.idgen), self.idgen, helpmsg)
        self.helpmsg = helpmsg
    def add_rule(self, tokens, reduction):
        self.root_node[tokens] = reduction

class ParserReader:
    def __init__(self, inp, helperr=False):
        self.helpmsg = helperr
        self.inp = inp # beansast.lexer.Lexer self.inp = self.inp
        self.file = inp.fn
        if self.file.endswith(".gmr"):
            self.ofile = self.file[:-3] + "bo"
        else:
            self.ofile = self.file + ".bo"
    def compile(self, trie):
        with open(self.ofile, "wb") as f:
            f.write(trie.compile())
    def read(self):
        try:
            if os.path.getmtime(self.file) < os.path.getmtime(self.ofile):
                with open(self.ofile, 'rb') as f:
                    return Trie.from_compilation(f.read())
        except FileNotFoundError:
            pass
        trie = Trie(self.helpmsg)
        self.pos = 0
        metastmts = self.read_metastmts()
        with open("beansast/gmrs/plexer-r.gmr") as f:
            self.inp.update(f.read(), "beansast/gmrs/plexer-r.gmr")
        while not self.ahead_sgl_token("EOF"):
            name = self.read_name()
            if self.ahead_sgl_token("LBRACKET"):
                class_ = self.read_sgl_token_typed("ID")
                if class_ not in metastmts["class"]:
                    raise_error(GrammarClassError(token_to_frame(self.inp[self.pos-1]), class_))
                self.read_sgl_token("RBRACKET")
            else:
                class_ = None
            self.read_sgl_token("ASSIGNMENT")
            rules = self.read_rules(name)
            self.read_sgl_token("SEMICOLON")
            if class_:
                for rule in rules:
                    rule.set_class(class_)
            for rule in rules:
                trie.add_rule(rule.tokens[::-1], rule)
        self.compile(trie)
        return trie
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
        raise_error(ParsingSyntaxError(token_to_frame(self.inp[self.pos]), sent, self.inp[self.pos]), helpmsg=self.helpmsg)
    def read_sgl_token(self, token):
        tok = self.ahead_sgl_token(token)
        if tok: return tok
        else: self.err_toks(token)
    def read_sgl_token_typed(self, token):
        tok = self.read_sgl_token(token)
        if tok == "INT":
            if tok["value"].endswith("inf"):
                return float(tok["value"])
            return int(tok["value"])
        elif tok == "BOOL":
            return {"True": True, "False": False}[tok["value"]]
        elif tok == "STRING":
            return tok["value"]
        elif tok == "FLOAT":
            return float(tok["value"])
        elif tok == "ID":
            return tok["value"]
        else:
            return tok
            
    def read_metastmt(self):
        tok = self.aheadf_sgl_token({"ASSOC", "CLASS", "FIRST", "END"})
        if tok == "END":
            self.read_end()
            args = ("end", None)
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
        instructions = {"sr": "shift", "d": collections.OrderedDict(), "p": {}, "a": {}, "pop": None}
        while self.ahead_sgl_token("COLON"):
            (key, value) = self.read_class_instruction()
            if key == "sr":
                instructions[key] = value
            else:
                instructions[key].update(value)
        for key, value in instructions["d"].items():
            if bool(value[2] & FLAGS["PRIORITY"]):
                if instructions["pop"] != None:
                    raise_error(GrammarAmbiguityError(token_to_frame(self.inp[self.pos]), "multiple priority attribute have been defined for class %s" % name), helpmsg=self.helpmsg)
                else:
                    instructions["pop"] = key
        return (name, head_rule, instructions)
    def read_class_instruction(self):
        tok = self.aheadf_sgl_token({"SHIFTREDUCE", "PRIORITY", "DEFAULT", "ASSOC"})
        if tok == "SHIFTREDUCE":
            res = self.read_shiftreduce()
            args = ("sr", res)
        elif tok == "PRIORITY":
            res = self.read_priority()
            args = ("p", res)
        elif tok == "DEFAULT":
            res = self.read_default()
            args = ("d", res)
        elif tok == "ASSOC":
            assoc_rules = self.read_assoc()
            args = ("a", assoc_rules)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
        return args
    def read_first_instruction(self):
        tok = self.read_sgl_token({"META", "SINGLE", "UNIQUE"})
        if tok == "META":
            tok = self.read_sgl_token_typed("BOOL")
            return ("meta", tok)
        elif tok == "SINGLE":
            tok = self.read_sgl_token_typed("INT")
            return ("single", tok)
        elif tok == "UNIQUE":
            tok = self.read_sgl_token_typed("BOOL")
            return ("unique", tok)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
    def read_first(self):
        attributes = {"priority": None, "single": 0, "meta": False, "unique": False}
        self.read_sgl_token("FIRST")
        self.read_sgl_token("LBRACKET")
        tok = self.read_sgl_token_typed("INT")
        attributes["priority"] = tok
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
            "class": {},
            "first": []
        }
        while stmt != "end":
            if stmt == "class":
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
    def read_flags(self):
        flags = 0
        tok = self.ahead_sgl_token(set(FLAGS.keys()))
        while tok:
            flags |= FLAGS[str(tok)]
            tok = self.ahead_sgl_token(set(FLAGS.keys()))
        return flags
    def read_key(self):
        list_append = False
        flags = self.read_flags()
        tok  = self.ahead_sgl_token("LBRACKET")
        if tok:
            list_append = True
            key = self.read_sgl_token_typed("ID")
            self.read_sgl_token("RBRACKET")
        else:
            key = self.read_sgl_token_typed("ID")
        return list_append, key, flags
    def read_proxy_element(self):
        list_append, key, flags = self.read_key()
        self.read_sgl_token("COLON")
        if self.ahead_sgl_token("ID", step=False):
            type_ = "id"
        elif self.ahead_sgl_token("STRING", step=False):
            type_ = "string"
        elif self.ahead_sgl_token("INT", step=False):
            type_ = "int"
        elif self.ahead_sgl_token("FLOAT", step=False):
            type_ = "float"

        value = self.read_sgl_token_typed({"STRING", "INT", "FLOAT", "ID"})
        return ((list_append, key, flags), (value, type_))
        
    def read_proxy(self):
        self.read_sgl_token("LPROXY")
        result = Proxy()
        if self.ahead_sgl_token("RPROXY"):
            return result
        ((list_append, key, flags), (value, type_)) = self.read_proxy_element()
        result.add_entry(key, value, list_append, flags, type_)
        while self.ahead_sgl_token("COMMA"):
            ((list_append, key, flags), (value, type_)) = self.read_proxy_element()
            result.add_entry(key, value, list_append, flags, type_)
        self.read_sgl_token("RPROXY")
        return result
    def read_default(self):
        self.read_sgl_token("DEFAULT")
        defaults = self.read_proxy()
        return defaults
    def read_priority(self):
        self.read_sgl_token("PRIORITY")
        tokens = self.read_tokens()
        tok = self.read_sgl_token_typed("INT")
        priority = tok
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
        token_frame = token_to_frame(token)
        name = token["value"]
        if self.ahead_sgl_token("DOT"):
            origin = self.read_sgl_token("ID")
            origin = origin["value"]
        else:
            origin = ""
        if self.ahead_sgl_token("AT"):
            list_append, key, flags = self.read_key()
        else:
            list_append = False
            key = ""
        return RuleToken(name, origin, list_append, key, token_frame)
    def read_rule(self, name):
        tokens = []
        while self.ahead_sgl_token("ID", step=False):
            token = self.read_rule_token()
            tokens.append(token)
        proxy = self.read_proxy()
        priority = self.read_prior()
        return Rule(name, tokens, proxy, priority)
    def read_prior(self):
        priority = self.ahead_sgl_token("INT")
        return int(priority["value"]) if priority != None else 0
    def read_rules(self, name):
        rules = []
        while self.ahead_sgl_token("COLON"):
            rule = self.read_rule(name)
            rules.append(rule)
        return rules

from .stdout import *
