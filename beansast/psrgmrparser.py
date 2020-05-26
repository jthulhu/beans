#!/usr/bin/python3
# -*- coding: utf-8 -*-

import collections, bisect, itertools, gzip, os
from stderr import *
from . import lxrgmrparser
from carrot import *
from textwrap import dedent

proxy = OrderedDict(  # elements:
    String(),         #  key
    Struct(           #  entry:
        String(),     #   value
        String()      #   type
    )
)

axiom = String()

nonterminal = String()

nullable = String()

rule_token = Struct(
    String(),         # name
    String(),         # attribute
    String()          # key
)

rule = Struct(
    String(),         # name
    List(rule_token), # tokens: token
    proxy             # proxy
)

grammar = Struct(
    List(axiom),      # axioms: axiom
    List(rule),       # rules: rule
    List(nonterminal),# nontermianls: nonterminal
    List(nullable)    # nullables: nullable
)

class UUIDGen:
    def __init__(self):
        self.index = -1
    def __next__(self):
        self.index += 1
        return self.index

class Proxy:
    def __init__(self):
        self.elements = collections.OrderedDict()
    def add_entry(self, key, value, type):
        self.elements[key] = (value, type)
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
    def __init__(self, name, attribute, key, frame):
        self.name = name
        self.attribute = attribute
        self.key = key
        self.frame = frame
    def __repr__(self):
        string = self.name
        if self.attribute:
            string += "." + self.attribute
        if self.key:
            string += "@" + self.key
        return string
    def __repr__(self):
        return self.name
    def __eq__(self, right):
        return (isinstance(right, str) and self.name == right) or (self.name == right.name)
    def __hash__(self):
        return hash(self.name)
    def compile(self):
        return (self.name, self.attribute, self.key)

class Rule:
    def __init__(self, name, tokens, proxy):
        self.name = name
        self.tokens = tokens
        self.proxy = proxy
    @classmethod
    def load(cls, name, tokens, proxy):
        tokens = tuple(RuleToken(*token,None) for token in tokens)
        proxy = Proxy.load(proxy)
        return cls(name, tokens, proxy)
    def __len__(self):
        return len(self.tokens)
    def __iter__(self):
        return iter(self.tokens)
    def __getitem__(self, key):
        return self.tokens[key]
    def __repr__(self):
        return "{name} -> {tokens} <{proxy}>".format(
            tokens=" ".join(repr(token) for token in self.tokens),
            proxy=", ".join(str(key) + ": " + str(value[0]) for key, value in self.proxy.items()),
            name=self.name
        )
    def __len__(self):
        return len(self.tokens)
    def __contains__(self, token):
        return token in self.tokens
    def __bool__(self):
        return bool(self.tokens)
    def compile(self):
        return (self.name, tuple(token.compile() for token in self.tokens), self.proxy.compile())

class Grammar:
    __doc__ = dedent("""\
    Grammar(helpmsg=False) === G:=(NT, A, R)
     * represents a grammar without terminals
     * NT represents the non-ternimals
     * A the axioms (note that there may be multiple axioms)
     * R the production rules

    t in G := t in NT
    G[i]   := R[i]
    """)
    def __init__(self, helpmsg=False):
        self.axioms = []
        self.rules = []
        self.helpmsg = helpmsg
        self._indexes = {}
        self.nonterminals = set()
        self.nullables = set()
    def _is_rule_nullable(self, rule):
        for token in rule:
            if token.name not in self.nullables:
                return False
        return True
    def verify_nonproductive(self):
        nnprod_nt = self.nonterminals.copy()
        nnprod_rules = {i for i in range(len(self.rules))}
        edited = True
        while edited:
            edited = False
            nprod_rules = set()
            for i in nnprod_rules:
                for tok in self.rules[i]:
                    if tok in nnprod_nt:
                        break
                else:
                    edited = True
                    nprod_rules.add(i)
                    nt = self.rules[i].name
                    if nt in nnprod_nt:
                        nnprod_nt.remove(nt)
            nnprod_rules.difference_update(nprod_rules)
        return (nnprod_nt, nnprod_rules)
    def verify_unreachable(self):
        reachables = set(self.axioms)
        new_reachables = reachables.copy()
        edited = True
        while edited:
            edited = False
            found = set()
            for nt in new_reachables:
                for r in (rule for rule in self.rules if rule.name == nt):
                    for tok in r:
                        if tok.name in self.nonterminals and tok.name not in reachables:
                            edited = True
                            found.add(tok.name)
            reachables.update(new_reachables)
            new_reachables = found
        return self.nonterminals - reachables
    def verify_nullables(self):
        # build children
        children = {}
        for n in self.nullables:
            for rule_id in self.query_rules(n):
                if self._is_rule_nullable(self[rule_id]):
                    if n not in children:
                        children[n] = set()
                    for token in self[rule_id]:
                        children[n].add(token.name)
        for n in self.nullables:
            done = []
            todo = [n]
            while len(todo) > 0:
                current = todo.pop()
                if current in done:
                    return (False, done + [current])
                done.append(current)
                for child in children[current]:
                    todo.append(child)
        return (True,[])
    def compute_nullables(self):
        edited = True
        while edited:
            edited = False
            for rule in self.rules:
                if rule.name in self.nullables:
                    continue
                if self._is_rule_nullable(rule):
                    edited = True
                    self.nullables.add(rule.name)
    
    def add_axiom(self, axiom):
        self.axioms.append(axiom)
    def add_nonterminal(self, nonterminal):
        self.nonterminals.add(nonterminal)
    def _get_rules(self, name):
        for rule_id in self.query_rules(name):
            yield self[rule_id]
    def query_rules(self, name):
        yield from (i for i in range(len(self.rules)) if self.rules[i].name == name)
        return
        indexes = self._indexes.get(name, set())
        for i in indexes:
            yield i
    def add_rule(self, rule):
        if rule.name not in self._indexes:
            self._indexes[rule.name] = set()
        self._indexes[rule.name].add(len(self.rules))
        self.rules.append(rule)
    @classmethod
    def from_compilation(cls, raw, helpmsg=False):
        pos, (axioms, rules, nonterminals, nullables) = grammar.read(gzip.decompress(raw))
        return cls.load(axioms, rules, nonterminals, nullables, helpmsg=helpmsg)
    @classmethod
    def load(cls, axioms, rules, nonterminals, nullables, helpmsg=False):
        rules = [Rule.load(*rule) for rule in rules]
        self = cls(helpmsg)
        self.axioms = axioms
        for rule in rules:
            self.add_rule(rule)
        self.nonterminals = set(nonterminals)
        self.nullables = set(nullables)
        return self
    def compile(self):
        return gzip.compress(grammar.write((tuple(axiom for axiom in self.axioms), tuple(rule.compile() for rule in self.rules), self.nonterminals, self.nullables)))
    def __getitem__(self, key):
        return self.rules[key]
    def __contains__(self, token):
        return token.name in self.nonterminals

class ParserReader:
    def __init__(self, inp, helperr=False):
        self.helpmsg = helperr
        self.inp = inp # beansast.lexer.Lexer self.inp = self.inp
        self.file = inp.fn
        self._compile = bool(self.file)
        if self.file.endswith(".gmr"):
            self.ofile = self.file[:-3] + "bo"
        else:
            self.ofile = self.file + ".bo"
    def compile(self, grammar):
        with open(self.ofile, "wb") as f:
            f.write(grammar.compile())
    def read(self):
        try:
            if os.path.getmtime(self.file) < os.path.getmtime(self.ofile):
                with open(self.ofile, 'rb') as f:
                    return Grammar.from_compilation(f.read())
        except FileNotFoundError:
            pass
        grammar = Grammar(self.helpmsg)
        self.pos = 0
        metastmts = self.read_metastmts()
        with open("beansast/gmrs/plexer-r.gmr") as f:
            self.inp.update(f.read(), "beansast/gmrs/plexer-r.gmr")
        while not self.ahead_sgl_token("EOF"):
            name = self.read_name()
            self.read_sgl_token("ASSIGNMENT")
            rules = self.read_rules(name)
            self.read_sgl_token("SEMICOLON")
            grammar.add_nonterminal(name)
            for rule in rules:
                grammar.add_rule(rule)
        for token, _ in metastmts["first"]:
            grammar.add_axiom(token)
        grammar.compute_nullables()
        correct, stack = grammar.verify_nullables()
        if not correct:
            raise_warning (GrammarSyntaxError(Frame(self.file, "*", "*"), 'Grammar is bottomless (%s)' % (' -> '.join(stack))))
        nonprod_nonter, nonprod_rules = grammar.verify_nonproductive()
        if len(nonprod_nonter) > 0:
            raise_warning (GrammarSyntaxError(Frame(self.file, "*", "*"), 'Grammar contains %s nonproductive nonterminals:\n%s' % (len(nonprod_nonter), "\n".join(nonprod_nonter))))
        grammar.nonterminals.difference_update(nonprod_nonter)
        if len(nonprod_rules) > 0:
            raise_warning (GrammarSyntaxError(Frame(self.file, "*", "*"), 'Grammar contains %s nonproductive rules:\n%s' % (len(nonprod_rules), "\n".join(repr(grammar.rules[i]) for i in nonprod_rules))))
        grammar.rules = [grammar.rules[i] for i in range(len(grammar.rules)) if i not in nonprod_rules]
        unreachables = grammar.verify_unreachable()
        if len(unreachables) > 0:
            raise_warning (GrammarSyntaxError(Frame(self.file, "*", "*"), 'Grammar contains %s unreachable nonterminals:\n%s' % (len(unreachables), "\n".join(unreachables))))
        grammar.nonterminals.difference_update(unreachables)
        self.compile(grammar)
        return grammar
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
        raise_error(ParsingSyntaxError(token_to_frame(self.inp[self.pos]), sent), helpmsg=self.helpmsg)
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
        tok = self.aheadf_sgl_token({"FIRST", "END"})
        if tok == "END":
            self.read_end()
            args = ("end", None)
        elif tok == "FIRST":
            first_rules = self.read_first()
            args = ("first", first_rules)
        else:
            # wtf...
            raise RuntimeError("This should not happen")
        self.read_sgl_token("SEMICOLON")
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
    def read_metastmts(self):
        stmt, args = self.read_metastmt()
        metastmts = {
            "first": []
        }
        while stmt != "end":
            if stmt == "first":
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
    def read_key(self):
        key = self.read_sgl_token_typed("ID")
        return key
    def read_proxy_element(self):
        key= self.read_key()
        self.read_sgl_token("COLON")
        if self.ahead_sgl_token("ID", step=False):
            type_ = "id"
        elif self.ahead_sgl_token("STRING", step=False):
            type_ = "string"
        elif self.ahead_sgl_token("INT", step=False):
            type_ = "int"
        elif self.ahead_sgl_token("FLOAT", step=False):
            type_ = "float"
        elif self.ahead_sgl_token("BOOL", step=False):
            type_ = "bool"

        value = self.read_sgl_token({"STRING", "INT", "FLOAT", "ID", "BOOL"})["value"]
        return (key), (value, type_)
        
    def read_proxy(self):
        self.read_sgl_token("LPROXY")
        result = Proxy()
        if self.ahead_sgl_token("RPROXY"):
            return result
        (key, (value, type_)) = self.read_proxy_element()
        result.add_entry(key, value, type_)
        while self.ahead_sgl_token("COMMA"):
            (key, (value, type_)) = self.read_proxy_element()
            result.add_entry(key, value, type_)
        self.read_sgl_token("RPROXY")
        return result
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
            key = self.read_key()
        else:
            key = ""
        return RuleToken(name, origin, key, token_frame)
    def read_rule(self, name):
        tokens = []
        while self.ahead_sgl_token("ID", step=False):
            token = self.read_rule_token()
            tokens.append(token)
        proxy = self.read_proxy()
        return Rule(name, tokens, proxy)
    def read_rules(self, name):
        rules = []
        while self.ahead_sgl_token("COLON"):
            rule = self.read_rule(name)
            rules.append(rule)
        return rules

from stdout import *
