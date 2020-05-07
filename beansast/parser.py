#!/usr/bin/python3
# -*- coding: utf-8 -*-

from .lexer import Lexer
from .psrgmrparser import ParserReader
from .state import *
import stderr

class ASTNode:
    def __init__(self, name, frame, **kwargs):
        self.name = name
        self.frame = frame
        self.attributes = kwargs
    def items(self):
        return self.attributes.items()
    def __getitem__(self, key):
        return self.attributes[key]
    def __setitem__(self, key, value):
        self.attributes[key] = value
    def __contains__(self, key):
        return key in self.attributes

#################
### STATE SET ###
#################

class SortedSet:
    def __init__(self):
        self._set = []
        self._hash_table = {}
    def add(self, item, priority):
        if item in self:
            return
        
    def __contains__(self, item):
        h = hash(item)
        if h not in self._hash_table:
            return False
        for it in self._hash_table[h]:
            if item == it:
                return True
        return False
    
class DynamicSet:
    def __init__(self):
        self._set = []
        self._hash_table = {}
        self._size = 0
    def __len__(self):
        return self._size
    def __repr__(self):
        return '{%s}' % ', '.join(repr(i) for i in self._set)
    def __getitem__(self, key):
        return self._set[key]
    def add(self, item):
        h = hash(item)
        if h in self._hash_table:
            for i in self._hash_table[h]:
                if self._set[i] == item:
                    return
        else:
            self._hash_table[h] = set()
        self._hash_table[h].add(len(self._set))
        self._set.append(item)
        self._size += 1
    def __contains__(self, item):
        h = hash(item)
        if h not in self._hash_table:
            return False
        for i in self._hash_table[h]:
            if self._set[i] == item:
                return True
        return False
    def __iter__(self):
        return DSIterator(self)

class DSIterator:
    def __init__(self, ds):
        self._ds = ds
        self._pos = 0
    def __next__(self):
        if self._pos < len(self._ds):
            self._pos += 1
            return self._ds[self._pos-1]
        else:
            raise StopIteration()

####################
## EARLEY PARSING ##
####################

class EarleyItem:
    def __init__(self, rule_id, dot, start):
        self.rule_id = rule_id
        self.dot = dot
        self.start = start
    def __hash__(self):
        return hash((self.rule_id,self.dot,self.start))
    def __eq__(self, right):
        return self.rule_id == right.rule_id and self.dot == right.dot and self.start == right.start

class FinalEarleyItem:
    def __init__(self, rule_id, end):
        self.rule_id = rule_id
        self.end = end
    def __hash__(self):
        return hash((self.rule_id, self.end))
    def __eq__(self, right):
        return self.rule_id == right.rule_id and self.end == right.end
    
####################
### MAIN CLASSES ###
####################
    
class Parser:
    def __init__(self, grammar, gmrfile='beansast/gmrs/parser.gmr'):
        self.rawgrammar = grammar
        self.gmrfile = gmrfile
        self.compute_grammar()
    def compute_grammar(self):
        with open(PLEXERM_GRAMMAR) as f:
            self.lexedgrammar = Lexer(f.read(),file=PLEXERM_GRAMMAR)
        self.lexedgrammar(self.rawgrammar,fn=self.gmrfile)
        self.grammar = ParserReader(self.lexedgrammar).read()
    def __call__(self, input, file="<stdin>"):
        self.file = file
        self.input = input
    def parse(self):
        S = [DynamicSet()]
        for axiom in self.grammar.axioms:
            for rule in self.grammar.query_rules(axiom):
                S[0].add(EarleyItem(rule, 0, 0))
        i = 0
        reach_end = True
        while i == 0 or self.input[i-1] != 'EOF':
            if len(S[i]) == 0:
                reach_end = False
                break
            S.append(DynamicSet())
            for item in S[i]:
                rule = self.grammar[item.rule_id]
                if item.dot < len(rule):
                    if rule[item.dot] in self.grammar:
                        # Prediction
                        for rule_id in self.grammar.query_rules(rule[item.dot].name):
                           S[i].add(EarleyItem(rule_id, 0, i))
                        if rule[item.dot].name in self.grammar.nullables:
                            S[i].add(EarleyItem(item.rule_id, item.dot+1,i))
                    else:
                        # Scan
                        if rule[item.dot] == self.input[i]:
                            S[i+1].add(EarleyItem(item.rule_id, item.dot+1, item.start))
                else:
                    # Completition
                    j = item.start
                    for item2 in S[j]:
                        rule2 = self.grammar[item2.rule_id]
                        if item2.dot == len(rule2):
                            continue
                        if rule2[item2.dot].name == rule.name:
                            S[i].add(EarleyItem(item2.rule_id, item2.dot+1, item2.start))
            i += 1
        if len(S.pop()) != 0:
            # Some rule parsed AFTER the end of the input
            #  weird...
            print('Hum... weird... is there a rule containing EOF?')
        if not reach_end:
            # Could not parse the entire input!
            return (False, S)
        return (True, S)

class ASTBuilder:
    LEXER_GRAMMAR = "beansast/gmrs/lexer.gmr"
    PARSER_GRAMMAR = "beansast/gmrs/parser.gmr"
    def __init__(self):
        with open(self.LEXER_GRAMMAR) as f:
            self.lexer_grammar = f.read()
        with open(self.PARSER_GRAMMAR) as f:
            self.parser_grammar = f.read()
        self.lexer = Lexer(self.lexer_grammar, file=self.LEXER_GRAMMAR)
        self.parser = Parser(self.parser_grammar, gmrfile=self.PARSER_GRAMMAR)
    def _get_reversed_S(self):
        ended, S = self.parser.parse()
        rS = [DynamicSet() for i in range(len(S))]
        for i, state_set in enumerate(S):
            for item in state_set:
                if item.dot < len(self.parser.grammar[item.rule_id]):
                    continue
                j = item.start
                rS[j].add(FinalEarleyItem(item.rule_id, i))
        return ended, rS
    def _is_good_item(self, item, size):
        rule = self.parser.grammar[item.rule_id]
        return rule.name in self.parser.grammar.axioms and item.end == size
    def search_item(self, start, item, S, grammar, input):
        rule = grammar[item.rule_id]
        todo = [(0, start, [])]
        done = set()
        while len(todo) > 0:
            depth, curpos, items = todo.pop()
            # items: (start, token/item, isnonterminal)
            if curpos == item.end and depth == len(rule):
                # win
                break
            if depth >= len(rule):
                continue
            curtoken = rule[depth].name
            if curtoken in grammar.nonterminals:
                for item2 in S[curpos]:
                    rule2 =  grammar[item2.rule_id]
                    if rule2.name == curtoken and item2.end <= item.end and (depth+1,item2.end) not in done:
                        todo.append((depth+1, item2.end, items + [(curpos, item2, True)]))
                        done.add((depth+1,item2.end))
            else:
                if input[curpos].name == curtoken and (depth+1,curpos+1) not in done:
                    todo.append((depth+1, curpos+1, items + [(curpos, input[curpos], False)]))
                    done.add((depth+1,curpos+1))
        else:
            return False
        nodeattributes = {}
        for i, (start, item, isnonterminal) in enumerate(items):
            t = rule[i]
            if t.key == '':
                continue
            key = t.key
            if isnonterminal:
                node = self.search_item(start, item, S, grammar, input)
            else:
                node = item
            if t.attribute != '':
                value = node[t.attribute]
            else:
                value = node
            if key == 'this':
                for k, v in value.items():
                    nodeattributes[k] = v
            else:
                nodeattributes[key] = value
        for key, (value, type) in rule.proxy.items():
            if type == 'string':
                nodeattributes[key] = value
            elif type == 'float':
                nodeattributes[key] = float(value)
            elif type == 'int':
                nodeattributes[key] = int(value)
            elif type == 'id':
                nodeattributes[key] = eval(value, nodeattributes)
            elif type == 'bool':
                nodeattributes[key] = bool(value)
            else:
                print('Attribute type (%s) not understood...' % type)
        node = ASTNode(rule.name, stderr.token_to_frame(input[start]), **nodeattributes)
        return node
            
    def get_ast(self, input, input_file="<stdin>"):
        self.lexer(input, fn=input_file)
        self.parser(self.lexer)
        ended, S = self._get_reversed_S()
        if not ended:
            # Handling error
            token = self.lexer[len(S)-1]
            frame = stderr.token_to_frame(token)
            stderr.raise_error(stderr.SyntaxError(frame, 'Could not understand token `%s\'.' % token))
        if len(S) == 0:
            # Empty AST
            #  weird...
            print('Should not happen... weird...')
            return None
        for item in S[0]:
            if self._is_good_item(item, len(S)-1):
                return self.search_item(0, item, S, self.parser.grammar, self.parser.input)
        else:
            # Handling error
            frame = stderr.token_to_frame(self.lexer[len(S)-1])
            stderr.raise_error(stderr.SyntaxError(frame, "Input was not understood"))
