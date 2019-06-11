import queue
from collections import OrderedDict
from .psrgmrparser import ParserReader, ASTNode
from .lexer import Lexer
from .stderr import *

class ParserError(BaseException):
    pass

class Path:
    def __init__(self, parser):
        self.parser = parser

class Parser:
    def __init__(self, rgrammar, fn="<stdin>", helperr=False):
        self.helperr = helperr
        with open("beansast/gmrs/plexer-m.gmr") as f:
            grammar = Lexer(f.read(), file="beansast/gmrs/plexer.gmr", helperr=helperr)
        grammar(rgrammar, fn=fn)
        self.metastmts, self.nodizers = ParserReader(grammar, helperr=helperr).read()
    def __call__(self, flux, pos=-1):
        self.flux = flux
        self.file = flux.fn
    def shift(self, stack, pos):
        time = pos+1
        stack = stack.copy()
        stack.append(self.flux[time])
        return stack
    def reduce(self, stack, offset, name, args):
        stack = stack.copy()
        removed_tokens = []
        for i in range(offset):
            removed_tokens.append(stack.pop())
        if removed_tokens:
            frame = token_to_frame(removed_tokens[0])
            pos = (frame.char, frame.line)
            file = frame.file
        else:
            pos = "?"
            file = "?"
        new = ASTNode(name, args, pos, file)
        stack.append(new)
        return stack
    def parse(self):
        stacks = queue.LifoQueue()
        stacks.put_nowait((-1, []))
        done = []
        solutions = []
        while not stacks.empty():
            pos, next_ = stacks.get_nowait()
            is_solution = False
            if len(next_) == 1 and self.flux[pos+1] == "EOF":
                is_solution = True
            done.append(next_)
            possibilities = self.extend_stack(next_, pos)
            if possibilities["shift"]:
                stacks.put_nowait((pos+1, self.shift(next_, pos)))
            for offset, (name, args) in possibilities["reduce"]:
                stacks.put_nowait((pos, self.reduce(next_, offset, name, args)))
            if is_solution and next_[0] in (key for key, _ in self.metastmts["first"]):
                solutions.append(next_)
        if len(solutions) == 0:
            raise_error(NoPathSyntaxError(Frame(self.file, "*", "*")))
        elif len(solutions) > 1:
            raise_error(TooManyPathSyntaxError(Frame(self.file, "*", "*")))
        else:
            return solutions[0]
    def extend_stack(self, stack, pos):
        time = pos+1
        possibilities = {
            "shift": False,
            "reduce" : []
        }
        for offset in range(len(stack)+1):
            for name, nodizer in self.nodizers.items():
                for rule in nodizer:
                    if offset == 0:
                        back_view = []
                    else:
                        back_view = stack[-offset:]
                    if len(back_view) > len(rule):
                        continue
                    is_possible = True
                    args = OrderedDict()
                    if nodizer.class_ and nodizer.class_ in self.metastmts["class"]:
                        args.update(self.metastmts["class"][nodizer.class_][1]["d"])
                    for (token, origin, key), real_tok in zip(rule, back_view):
                        if token != real_tok:
                            is_possible = False
                            break
                        if key:
                            if origin:
                                args[key] = real_tok[origin]
                            else:
                                args[key] = real_tok
                    if "this" in args:
                        if len(args) > 1:
                            #raise_error(GrammarIdentityError(token_to_frame(self.flux[pos])), helpmsg=self.helperr)
                            pass
                        real_tok = args["this"]
                        args = real_tok.attributes.copy()
                        
                    for key, value in rule.proxy.items():
                        args[key] = value.format(args)
                    if is_possible and len(back_view) == len(rule):
                        possibilities["reduce"].append((offset, (name, args)))
                    elif is_possible and rule[len(back_view)][0] == self.flux[time]:
                        possibilities["shift"] = True
        return possibilities
    def update(self, grammar):
        nmetastmts, nnodizers = ParserReader(grammar, helperr=helperr).read()
        self.nodizers.update(nnodizers)
    def __eq__(self, r):
        return type(self) == type(r) and hasattr(r, "nodizers") and r.nodizers == self.nodizers and hasattr(r, "pos") and hasattr(r, "flux") and self.flux == r.flux
    def __repr__(self):
        fn = "" if not hasattr(self, "file") else (" instancied of file %s" % self.file)
        return "<Parser%s at %s>" % (fn, hex(id(self)))
            
