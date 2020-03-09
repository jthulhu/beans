import queue
from collections import OrderedDict
from .psrgmrparser import ParserReader, ASTNode, FLAGS
from .lexer import Lexer
from .stderr import *
from .stdout import print_tree

class ParserError(BaseException):
    pass

class Path:
    def __init__(self, parser):
        self.parser = parser

class ASTNodeAttribute:
    def __init__(self, value, flags, type_):
        self._value = value
        self._public = bool(FLAGS["PUBLIC"] & flags)
        self._private = not self._public
        self._priority = bool(FLAGS["PRIORITY"] & flags)
        self._flags = flags
        self._type_ = type_
    def __getattr__(self, attr):
        return getattr(self._value, attr)
    def __str__(self):
        return "ASTNodeAttributes(%s, %s, %s)" % (str(self._value), bin(self._flags), str(self._type_))
    def __repr__(self):
        return repr(self._value)
    def __eq__(self, right):
        return self._value == right
    def __ge__(self, right):
        return self._value >= right
    def __gt__(self, right):
        return self._value > right
    def __ne__(self, right):
        return self._value != right
    def __le__(self, right):
        return self._value <= right
    def __lt__(self, right):
        return self._value < right
    def __bool__(self):
        return bool(self._value)
        
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
    def shift(self, stack, pos, id_):
        time = pos+1
        stack = stack.copy()
        stack.append(self.flux[time])
        self.min_id += 1
        return stack
    def reduce(self, stack, offset, name, args, pos, id_):
        stack = stack.copy()
        self.min_id += 1
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
        self.min_id = 0
        while not stacks.empty():
            pos, next_ = stacks.get_nowait()
            next_id = self.min_id
            is_solution = False
            if len(next_) == 1 and self.flux[pos+1] == "EOF":
                is_solution = True
            done.append(next_)
            possibilities = self.extend_stack(next_, pos)
            if possibilities["shift"]:
                stacks.put_nowait((pos+1, self.shift(next_, pos, next_id)))
            for offset, (name, args) in possibilities["reduce"]:
                stacks.put_nowait((pos, self.reduce(next_, offset, name, args, pos, next_id)))
            if is_solution and next_[0] in (key for key, _ in self.metastmts["first"]):
                solutions.append(next_)
        if len(solutions) == 0:
            raise_error(NoPathSyntaxError(Frame(self.file, "*", "*")))
        elif len(solutions) > 1:
            for solution in solutions:
                print_tree(solution)
            raise_error(TooManyPathSyntaxError(Frame(self.file, "*", "*")))
        else:
            return solutions[0]
    def compute_typed_value(self, value, args, type_):
        if type_ == "string":
            return value.format(**args)
        elif type_ == "id":
            return args[value]
        else:
            return value
    def extend_stack(self, stack, pos):
        time = pos+1
        possibilities = {
            "shift": False,
            "reduce" : []
        }
        class_decision = {}
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
                        flags_inheirit = {}
                        for key, (value, list_append, flags, type_) in self.metastmts["class"][nodizer.class_][1]["d"].items():
                            
                            if list_append:
                                if key not in args:
                                    args[key] = []
                                args[key].append(ASTNodeAttribute(self.compute_typed_value(value, args, type_), flags, type_))
                            else:
                                args[key] = ASTNodeAttribute(self.compute_typed_value(value, args, type_), flags, type_)
                            flags_inheirit[key] = flags
                    for (token, origin, (list_append, key)), real_tok in zip(rule, back_view):
                        if token != real_tok:
                            is_possible = False
                            break
                        if key:
                            if origin:
                                new = real_tok[origin]
                            else:
                                new = real_tok
                            if real_tok == "ID":
                                type_ = "id"
                            elif real_tok == "STRING":
                                type_ = "string"
                            elif real_tok == "INT":
                                type_ = "int"
                            elif real_tok == "FLOAT":
                                type_ = "float"
                            else:
                                type_ = "astnode"
                            if key in flags_inheirit:
                                flags = flags_inheirit[key]
                            else:
                                flags = 0b01
                            args[key] = ASTNodeAttribute(new, flags, type_)
                    if not is_possible:
                        continue
                    if len(back_view) == len(rule):
                        if "this" in args:
                            if len(args) > 1:
                                #raise_error(GrammarIdentityError(token_to_frame(self.flux[pos])), helpmsg=self.helperr)
                                pass
                            real_tok = args["this"]
                            args = real_tok.attributes.copy()
                        for key, (value, list_append, flags, type_) in rule.proxy.items():
                            if key in flags_inheirit:
                                flags = flags_inheirit[key]
                            else:
                                flags = 0b01
                            if list_append:
                                if key not in args:
                                    args[key] = []
                                args[key].append(ASTNodeAttribute(self.compute_typed_value(value, args, type_), flags, type_))
                            else:
                                args[key] = ASTNodeAttribute(self.compute_typed_value(value, args, type_), flags, type_)
                    for key in [key for key in args.keys()]:
                        if key.startswith("_"):
                            del args[key]
                    if len(back_view) == len(rule):
                        action = "reduce"
                    elif rule[len(back_view)][0] == self.flux[time]:
                        action = "shift"
                    else:
                        continue
                    if action == "reduce":
                        if rule.class_ and self.metastmts["class"][rule.class_][1]["pop"]:
                            rule_priority = args[self.metastmts["class"][rule.class_][1]["pop"]]
                            if rule.class_ not in class_decision or class_decision[rule.class_][0] < rule_priority:
                                class_decision[rule.class_] = (rule_priority, "reduce", (offset, (name, args)))
                        else:
                            possibilities["reduce"].append((offset, (name, args)))
                    elif action == "shift":
                        if rule.class_ and self.metastmts["class"][rule.class_][1]["pop"]:
                            rule_priority = args[self.metastmts["class"][rule.class_][1]["pop"]]
                            if rule.class_ not in class_decision or class_decision[rule.class_][0] < rule_priority:
                                class_decision[rule.class_] = (rule_priority, "shift", rule)
                        else:
                            possibilities["shift"] = True
        for _, decision, args in class_decision.values():
            if decision == "shift":
                possibilities["shift"] = True
            else:
                possibilities["reduce"].append(args)
        size = len(possibilities["reduce"])
        if possibilities["shift"]: size += 1
        return possibilities
    def update(self, grammar):
        nmetastmts, nnodizers = ParserReader(grammar, helperr=helperr).read()
        self.nodizers.update(nnodizers)
        self.metastmts.update(nmetastmts)
    def __eq__(self, r):
        return type(self) == type(r) and hasattr(r, "nodizers") and r.nodizers == self.nodizers and hasattr(r, "pos") and hasattr(r, "flux") and self.flux == r.flux
    def __repr__(self):
        fn = "" if not hasattr(self, "file") else (" instancied of file %s" % self.file)
        return "<Parser%s at %s>" % (fn, hex(id(self)))
            
