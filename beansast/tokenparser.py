import types
from .primitives import primitives
from .rawparsers import Token
from .astnodes import *

class ProxyDict:
    def __init__(self, dict_={}):
        self.dict = dict_
    def __getitem__(self, key):
        return ItemProxy(self, key)
    def __delitem__(self, key):
        del self.dict[key]
    def __setitem__(self, key, value):
        self.dict[key] = value
    def __getattr__(self, attribute):
        return getattr(self.dict, attribute)

class ItemProxy:
    def __init__(self, dict_, key):
        self.dict = dict_
        self.key = key
    def __call__(self, *args, **kwargs):
        return self._get_item()(*args, **kwargs)
    def __getattr__(self, attribute):
        return self._get_item().__getattribute__(attribute)
    def _get_item(self):
        return self.dict.dict[self.key]
    def __add__(self, r):
        return self._get_item().__add__(r)

    
token_parsers = ProxyDict()

class TokenParserWithRepr(type):
    def __repr__(cls):
        return repr(cls.query)
    def __str__(cls):
        return str(cls.query)

class TokenParser(object, metaclass=TokenParserWithRepr): # Abstract base of all parsers
    result = False # shall what is returned by self.check() be stored and returned
    query = None # sum of parsers that define the current one
    ignore = False # True to delete all tokens that 
    def __init__(self, flux, pos):
        self.flux = flux
        self.pos = pos
        self.origin_pos = pos
    def next_token(self):
        if not self.ignore:
            is_ignored = False
            for ignored_token in ignored_tokens:
                is_good, pos, _ = ignored_token(self.flux, self.pos).check()
                if is_good:
                    is_ignored = True
                    self.pos = pos
                    break
            while is_ignored:
                is_ignored = False
                for ignored_token in ignored_tokens:
                    is_good, pos, _ = ignored_token(self.flux, self.pos).check()
                    if is_good:
                        is_ignored = True
                        self.pos = pos
                        break
        self.pos += 1
        return self.flux[self.pos - 1]
    def future_token(self, delay=1):
        return self.flux[self.pos + delay]
    @classmethod
    def __add__(cls, r):
        return AddTokenParser(cls, r)
    def check(self):
        # raise None object is not callable
        return self.query(self.flux, self.pos).check()
    def __repr__(self):
        return repr(self.query)
    def __str__(self):
        return str(self.query)

def variable(name):
    return Token("id", name)
    
def operator(op):
    return Token("op", op)

def space(size=1):
    return Token("space", size)

def EOL():
    return Token("EOL")

def integer(nb):
    return Token("int", nb)

def string(chrs):
    return Token("string", chrs)

    
class Wrapper: # Access to __add__ for classes
    def __init__(self, cls):
        self.cls = cls
    def __call__(self, *args, **kwargs):
        return self.cls(*args, **kwargs)
    def __add__(self, r):
        return self.cls.__add__(r)
    def __repr__(self):
        return repr(self.cls)
    def __str__(self):
        return str(self.cls)
    def __getattr__(self, attr):
        return getattr(self.cls, attr)

class Optional(TokenParser):
    def __init__(self, tp):
        self.tp = tp
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
        self.tp = self.tp(flux, pos)
        def __check__(self):
            is_good, pos, result = self.tp.check()
            if is_good:
                self.pos = pos
            return True, self.pos, result
        self.__check__ = types.MethodType(__check__)
    
class AddTokenParser(TokenParser):
    def __init__(self, l, r):
        self.l = l
        self.r = r
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
    
        def check(self):
            is_good, self.pos, rl = self.l(self.flux, self.pos).check()
            if not is_good:
                return (False, self.pos, None)
            is_good, self.pos, rr = self.r(self.flux, self.pos).check()
            if not is_good:
                return (False, self.pos, None)
            if type(rl) == MultipleValues:
                if self.r.result:
                    rl.append(rr)
                return rl
            else:
                result = MultipleValues()
                if self.l.result:
                    result.append(rl)
                if self.r.result:
                    result.append(rr)
                return result
                
        self.check = types.MethodType(check, self)
        return self
    def __repr__(self):
        return "%s + %s" % (repr(self.l), repr(self.r))
    def __str__(self):
        return "%s + %s" % (str(self.l), str(self.r))
    def __add__(self, r):
        return AddTokenParser(self, r)
            
class MultipleValues(list): pass # helps to make the difference between parsers that return lists and lists of what parsers return
        
class DictNotHashable:
    def __init__(self, value=[]):
        self.value = []
    def __setitem__(self, key, value):
        for place, element in enumerate(self.value):
            if element[0] == key:
                self.value.pop(place)
                break
        self.value.append((key, value))
    def __getitem__(self, item):
        for key, value in self.value:
            if key == item:
                return value
        raise KeyError(item)
    def __repr__(self):
        result = "{"
        for key, value in self.value:
            result += key.__repr__() + ": " + value.__repr__()
        result += "}"
        return result

        
@Wrapper
class AssignmentTokenParser(TokenParser):
    result = True
    @staticmethod
    @property
    def query():
        return token_parsers["variable"] + token_parsers["operator"]("=") + token_parsers["expression"]
    """
    def check(self):
        variable = self.next_token()
        if variable.id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        key = variable.value
        if self.next_token().id != primitives["EQUALS"]:
            return (False, self.origin_pos, None)
        is_good, pos, value = token_parsers["expression"](self.flux, self.pos).check()
        if is_good:
            return (True, pos, AssignmentNode(key, value))
        return (False, self.origin_pos, None)
    """
@Wrapper
class OperatorTokenParser(TokenParser):
    @property
    def query(self):
        return self.op
    def __init__(self, op):
        self.op = operator(op)
    def __call__(self, flux, pos):
        self.flux = flux
        self.pos = pos
        def check(self):
            return (self.next_token() == self.op, self.pos, None)
        self.check = types.MethodType(check)

@Wrapper
class SpaceTokenParser(TokenParser):
    ignore = True
    def check(self):
        spaces = self.next_token()
        return (spaces.id == space().id, self.pos, spaces.value)

class EOLTokenParser(TokenParser):
    ignore = True
    def check(self):
        eol = self.next_token()
        return (eol == EOL(), self.pos, None)
    
@Wrapper
class StatementTokenParser(TokenParser):
    def check(self):
        for parser in statement_token_parsers:
            parsed = parser(self.flux, self.pos)
            is_good, pos, node = parsed.check()
            if is_good:
                return (True, pos, node)
        return (False, self.origin_pos, node)
@Wrapper
class ImportAll: pass
   
@Wrapper
class ImportTokenParser(TokenParser):
    def check(self):
        next_ = self.next_token()
        
        if next_.id == primitives["FROM"]:
            beg_path = self.next_token()
            if beg_path.id != primitives["VARIABLES"]: return (False, self.origin_pos, None)
            next_ = self.next_token()
        if next_.id != primitives["IMPORT"]:
            return (False, self.origin_pos, None)
        if self.future_token().id == primitives["ASTERISK"]:
            self.pos += 1
            return (True, self.pos, ImportNode(beg_path, ImportAll()))
        is_good, pos, variables = token_parsers["variable_list"](self.flux, self.pos)
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        return (True, self.pos, ImportNode(beg_path, variables))
        

@Wrapper
class SimpleExpressionTokenParser(TokenParser):
    def check(self):
        for parser in expression_token_parsers:
            parsed = parser(self.flux, self.pos)
            is_good, self.pos, node = parsed.check()
            if is_good:
                is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()

                if is_good: self.pos = pos
                return (True, self.pos, node)
        return (False, self.pos, node)

@Wrapper
class ExpressionTokenParser(TokenParser):
    def check(self):
        # SimpleExpressionTokenParser + (OperatorTokenParser(".") + SimpleExpressionTokenParser) ?
        pass
    
@Wrapper
class FunctionCallTokenParser(TokenParser):
    def check(self):
        function = self.next_token()
        if function.id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        key = function.value
        if self.next_token().id != primitives["LPAR"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        exprs = []
        if next_.id != primitives["RPAR"]:
            is_good, pos, expr = token_parsers["simple_expression"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            exprs.append(expr)
            next_ = self.next_token()
            while next_.id != primitives["RPAR"]:
                if next_.id == primitives["EOF"]: # not useful
                                                        # but explicit
                    return (False, self.origin_pos, None)
                if next_.id != primitives["COMMA"]:
                    return (False, self.origin_pos, None)
                is_good, pos, expr = token_parsers["simple_expression"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                exprs.append(expr)
                next_ = self.next_token()
        return (True, self.pos, FunctionCallNode(key, exprs, child))

       
@Wrapper
class VariableTokenParser(TokenParser):
    @property
    def query(self):
        return "VAR"
    def check(self):
        var = self.next_token()
        if var != variable(var.value):
            return (False, self.origin_pos, None)
        return (True, self.pos, VariableNode(var.value))

@Wrapper
class VariableListTokenParser(TokenParser):
    def check(self):
        variables = []
        variables.append(self.next_token())
        if variables[-1].id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        pos = self.pos
        while self.next_token().id == primitives["COMMA"]:
            variables.append(self.next_token())
            if variables[-1].id != primitives["VARIABLE"]:
                return (False, self.origin_pos, None)
            pos = self.pos
        return (True, pos, variables)
    
@Wrapper
class FunctionDefinitionTokenParser(TokenParser):
    def check(self):
        if self.next_token().id != primitives["FUNCTION"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        name = name.value
        if self.next_token().id != primitives["COLON"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        if next_.id == primitives["EOF"]:
            return (False, self.origin_pos, None)
        args = []
        if next_.id != primitives["LBRACE"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.id != primitives["LBRACE"]:
                if next_.id == primitives["EOF"]:
                    return (False, self.origin_pos, None)
                if next_.id != primitives["COMMA"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        commands = []
        if next_.id != primitives["RBRACE"]:
            is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            commands.append(command)
            next_ = self.next_token()
            while next_.id != primitives["RBRACE"]:
                is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                commands.append(command)
                next_ = self.next_token()
        return (True, self.pos, FunctionDefinitionNode(name, args, commands))

@Wrapper
class MethodDefinitionTokenParser(TokenParser):
    def check(self):
        if self.next_token().id != primitives["METHOD"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        name = name.value
        next_ = self.next_token()
        if next_.id != primitives["COLON"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        if next_.id == primitives["EOF"]:
            return (False, self.origin_pos, None)
        args = []
        if next_.id != primitives["LBRACE"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.id != primitives["LBRACE"]:
                if next_.id == primitives["EOF"]:
                    return (False, self.origin_pos, None)
                if next_.id != primitives["COMMA"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        commands = []
        if next_.id != primitives["RBRACE"]:
            is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            commands.append(command)
            next_ = self.next_token()
            while next_.id != primitives["RBRACE"]:
                is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                commands.append(command)
                next_ = self.next_token()
        return (True, self.pos, MethodDefinitionNode(name, args, commands))

@Wrapper
class IfTokenParser(TokenParser):
    def check(self):
        pass

@Wrapper
class BoolExpressionTokenParser(TokenParser):
    def check(self):
        pass
    
@Wrapper
class ClassDefinitionTokenParser(TokenParser):
    def check(self):
        parents = []
        next_ = self.next_token()
        if next_.id == primitives["FROM"]:
            is_good, pos, parent = token_parsers["variable"](self.flux, self.pos).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            parents.append(parent)
            next_ = self.next_token()
            while next_.id != primitives["CLASS"]:
                if next_.id == primitives["EOF"]:
                    return (False, self.origin_pos, None)
                if next_.id != primitives["COMMA"]:
                    return (False, self.origin_pos, None)
                next_ = self.next_token()
                is_good, pos, parent = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                parents.append(parent)
                next_ = self.next_token()
        if next_.id != primitives["CLASS"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.id != primitives["VARIABLE"]:
            return (False, self.origin_pos, None)
        name = name.value
        
        next_ = self.next_token()
        if next_.id != primitives["COLON"]:
            return (False, self.origin_pos, None)
        args = []
        next_ = self.next_token()
        if next_.id != primitives["LBRACE"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.id != primitives["LBRACE"]:
                if next_.id == primitives["EOF"]:
                    return (False, self.origin_pos, None)
                if next_.id != primitives["COMMA"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["varialbe"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        methods = []
        if next_.id != primitives["RBRACE"]:
            is_good, pos, method = token_parsers["method_definition"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            methods.append(method)
            next_ = self.next_token()
            while next_.id != primitives["RBRACE"]:
                # HERE WE ARE !!!!!!!!!!!!!!!!!!
                is_good, pos, method = token_parsers["method_definition"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                methods.append(method)
                next_ = self.next_token()
        return (True, self.pos, ClassDefinitionNode(name, parents, args, methods))

# Built-in types    

@Wrapper
class StringTokenParser(TokenParser):
    def check(self):
        string = self.next_token()
        if string.id != primitives["STRING"]:
            return (False, self.origin_pos, None)
        return (True, self.pos, StringNode(string.value, child))

@Wrapper
class IntegerTokenParser(TokenParser):
    def check(self):
        number = self.next_token()
        if number.id != integer.id:
            return (False, self.origin_pos, None)
        
        return (True, self.pos, IntNode(number.value, child))

@Wrapper
class FloatTokenParser(TokenParser):
    def check(self):
        flt = self.next_token()
        if flt.id != primitives["FLOAT"]:
            return (False, self.origin_pos, None)
        return (True, self.pos, FloatNode(flt.value, child))

@Wrapper
class ListTokenParser(TokenParser):
    def check(self):
        if self.next_token().id != primitives["LBRACKET"]:
            return (False, self.origin_pos, None)
        is_good, pos, expressions = token_parsers["expression_list"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        if self.next_token().id != primitives["RBRACKET"]:
            return (False, self.origin_pos, None)
        return (True, self.pos, ListNode(expressions, child))
        

@Wrapper
class ExpressionListTokenParser(TokenParser):
    def check(self):
        expressions = []
        is_good, pos, expression = token_parsers["simple_expression"](self.flux, self.pos).check()
        if not is_good: return (True, self.pos, [])
        expressions.append(expression)
        self.pos = true_pos = pos
        next_ = self.next_token()
        while next_.id == primitives["COMMA"]:
            is_good, pos, expression = token_parsers["simple_expression"](self.flux, self.pos).check()
            if not is_good: return (False, self.origin_pos, None)
            expressions.append(expression)
            self.pos = pos
            true_pos = self.pos
            next_ = self.next_token()
        return (True, true_pos, expressions)

@Wrapper
class KeyAndValueTokenParser(TokenParser):
    def check(self):
        is_good, pos, key = token_parsers["simple_expression"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        next_ = self.next_token()
        if next_.id != primitives["COLON"]: return (False, self.origin_pos, None)
        is_good, pos, value = token_parsers["simple_expression"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        return (True, self.pos, (key, value))
    
@Wrapper
class ExpressionListSBCTokenParser(TokenParser):
    def check(self):
        expressions = DictNotHashable()
        is_good, pos, expression = token_parsers["key_and_value"](self.flux, self.pos).check()
        if not is_good: return (True, self.pos, {})
        expressions[expression[0]] = expression[1]
        """
        for place, element in enumerate(expressions):
            if element[0] == expression[0]:
                expression.pop(place)
                break
        expressions.append(expression)
        """
        self.pos = true_pos = pos
        next_ = self.next_token()
        while next_.id == primitives["COMMA"]:
            is_good, pos, expression = token_parsers["key_and_value"](self.flux, self.pos).check()
            if not is_good: return (False, self.origin_pos, None)
            expressions[expression[0]] = expression[1]
            """
            for place, element in enumerate(expressions):
                if element[0] == expression[0]:
                    expression.pop(place)
                    break
            expressions.append(expression)
            """
            self.pos = pos
            true_pos = self.pos
            next_ = self.next_token()
        return (True, true_pos, expressions)
    
@Wrapper
class DictTokenParser(TokenParser):
    def check(self):
        if self.next_token().id != primitives["LBRACE"]:
            return (False, self.origin_pos, None)
        is_good, pos, expressions = token_parsers["expression_list_sbc"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        if self.next_token().id != primitives["RBRACE"]:
            return (False, self.origin_pos, None)
        return (True, self.pos, DictNode(expressions, child))


class UpdatedList:
    def __init__(self):
        self._update()
        class Iteration:
            def __init__(self, value):
                self.value = value
                self.pos = 0
            def __next__(self):
                if len(self.value) <= self.pos:
                    raise StopIteration
                self.pos += 1
                return self.value[self.pos - 1]
        self._iter = Iteration
    def _update(self):
        self.items = [value for value in token_parsers.values() if value.ignore]
    def __iter__(self):
        self._update()
        return self._iter(self.items)
    def __getitem__(self, index):
        self._update()
        return self.items[index]
    



ignored_tokens = UpdatedList()
    
#token_parsers["simple_expression"] = SimpleExpressionTokenParser
token_parsers["expression"] = ExpressionTokenParser
token_parsers["assignment"] = AssignmentTokenParser
#token_parsers["statement"] = StatementTokenParser
#token_parsers["import"] = ImportTokenParser
#token_parsers["function_call"] = FunctionCallTokenParser
token_parsers["variable"] = VariableTokenParser
#token_parsers["variable_list"] = VariableListTokenParser
#token_parsers["function_definition"] = FunctionDefinitionTokenParser
#token_parsers["method_definition"] = MethodDefinitionTokenParser
#token_parsers["if"] = IfTokenParser
#token_parsers["bool_expression"] = BoolExpressionTokenParser
#token_parsers["class_definition"] = ClassDefinitionTokenParser
token_parsers["string"] = StringTokenParser
token_parsers["integer"] = IntegerTokenParser
#token_parsers["float"] = FloatTokenParser
#token_parsers["list"] = ListTokenParser
#token_parsers["expression_list"] = ExpressionListTokenParser
#token_parsers["key_and_value"] = KeyAndValueTokenParser
#token_parsers["expression_list_sbc"] = ExpressionListSBCTokenParser
#token_parsers["dict"] = DictTokenParser
token_parsers["operator"] = OperatorTokenParser
token_parsers["space"] = SpaceTokenParser
token_parsers["eol"] = EOLTokenParser

expression_token_parsers = [
#    token_parsers["function_call"],
    token_parsers["variable"],
    token_parsers["string"],
#    token_parsers["float"],
    token_parsers["integer"],
#    token_parsers["list"],
#    token_parsers["dict"],
]
statement_token_parsers = [
    token_parsers["class_definition"],
    token_parsers["function_definition"],
    token_parsers["function_call"],
    token_parsers["assignment"],
    token_parsers["simple_expression"]
]


