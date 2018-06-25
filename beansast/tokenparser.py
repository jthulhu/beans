from .primitives import primitives
from .ASTnodes import *

class TokenParser:
    def __init__(self, flux, pos):
        self.flux = flux
        self.pos = pos
        self.origin_pos = pos
    def next_token(self):
        self.pos += 1
        return self.flux[self.pos - 1]
    def future_token(self, delay=1):
        return self.flux[self.pos + delay]
    
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

        
    
class AssignmentTokenParser(TokenParser):
    def check(self):
        variable = self.next_token()
        if variable.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        key = variable.value
        if self.next_token().code != primitives["EQUALS"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, value = token_parsers["expression"](self.flux, self.pos).check()
        if is_good:
            return (True, pos, AssignmentNode(key, value))
        return (False, self.origin_pos, None)

class StatementTokenParser(TokenParser):
    def check(self):
        for parser in statement_token_parsers:
            parsed = parser(self.flux, self.pos)
            is_good, pos, node = parsed.check()
            if is_good:
                return (True, pos, node)
        return (False, self.origin_pos, node)

class ImportAll: pass
   
class ImportTokenParser(TokenParser):
    def check(self):
        next_ = self.next_token()
        
        if next_.code == primitives["FROM"]["code"]:
            beg_path = self.next_token()
            if beg_path.code != primitives["VARIABLES"]["code"]: return (False, self.origin_pos, None)
            next_ = self.next_token()
        if next_.code != primitives["IMPORT"]["code"]:
            return (False, self.origin_pos, None)
        if self.future_token().code == primitives["ASTERISK"]["code"]:
            self.pos += 1
            return (True, self.pos, ImportNode(beg_path, ImportAll()))
        is_good, pos, variables = token_parsers["variable_list"](self.flux, self.pos)
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        return (True, self.pos, ImportNode(beg_path, variables))
        
    
class ExpressionTokenParser(TokenParser):
    def check(self):
        for parser in expression_token_parsers:
            parsed = parser(self.flux, self.pos)
            is_good, pos, node = parsed.check()
            if is_good:
                return (True, pos, node)
        return (False, self.origin_pos, node)

class FunctionCallTokenParser(TokenParser):
    def check(self):
        function = self.next_token()
        if function.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        key = function.value
        if self.next_token().code != primitives["LPAR"]["code"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        exprs = []
        if next_.code != primitives["RPAR"]["code"]:
            is_good, pos, expr = token_parsers["expression"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            exprs.append(expr)
            next_ = self.next_token()
            while next_.code != primitives["RPAR"]["code"]:
                if next_.code == primitives["EOF"]["code"]: # not useful
                                                        # but explicit
                    return (False, self.origin_pos, None)
                if next_.code != primitives["COMMA"]["code"]:
                    return (False, self.origin_pos, None)
                is_good, pos, expr = token_parsers["expression"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                exprs.append(expr)
                next_ = self.next_token()
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()
        if is_good: self.pos = pos
        return (True, self.pos, FunctionCallNode(key, exprs, child))

class AttributeTokenParser(TokenParser):
    def check(self):
        if self.next_token().code != primitives["DOT"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["expression"](self.flux, self.pos).check()
        if is_good: return (True, pos, child)
        return (False, self.origin_pos, None)
        
    
class VariableTokenParser(TokenParser):
    def check(self):
        variable = self.next_token()
        if variable.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()
        if is_good: self.pos = pos
        return (True, self.pos, VariableNode(variable.value, child))

class VariableListTokenParser(TokenParser):
    def check(self):
        variables = []
        variables.append(self.next_token())
        if variables[-1].code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        pos = self.pos
        while self.next_token().code == primitives["COMMA"]["code"]:
            variables.append(self.next_token())
            if variables[-1].code != primitives["VARIABLE"]["code"]:
                return (False, self.origin_pos, None)
            pos = self.pos
        return (True, pos, variables)
    
class FunctionDefinitionTokenParser(TokenParser):
    def check(self):
        if self.next_token().code != primitives["FUNCTION"]["code"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        name = name.value
        if self.next_token().code != primitives["COLON"]["code"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        if next_.code == primitives["EOF"]["code"]:
            return (False, self.origin_pos, None)
        args = []
        if next_.code != primitives["LBRACE"]["code"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.code != primitives["LBRACE"]["code"]:
                if next_.code == primitives["EOF"]["code"]:
                    return (False, self.origin_pos, None)
                if next_.code != primitives["COMMA"]["code"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        commands = []
        if next_.code != primitives["RBRACE"]["code"]:
            is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            commands.append(command)
            next_ = self.next_token()
            while next_.code != primitives["RBRACE"]["code"]:
                is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                commands.append(command)
                next_ = self.next_token()
        return (True, self.pos, FunctionDefinitionNode(name, args, commands))

class MethodDefinitionTokenParser(TokenParser):
    def check(self):
        if self.next_token().code != primitives["METHOD"]["code"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        name = name.value
        next_ = self.next_token()
        if next_.code != primitives["COLON"]["code"]:
            return (False, self.origin_pos, None)
        next_ = self.next_token()
        if next_.code == primitives["EOF"]["code"]:
            return (False, self.origin_pos, None)
        args = []
        if next_.code != primitives["LBRACE"]["code"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.code != primitives["LBRACE"]["code"]:
                if next_.code == primitives["EOF"]["code"]:
                    return (False, self.origin_pos, None)
                if next_.code != primitives["COMMA"]["code"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        commands = []
        if next_.code != primitives["RBRACE"]["code"]:
            is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            commands.append(command)
            next_ = self.next_token()
            while next_.code != primitives["RBRACE"]["code"]:
                is_good, pos, command = token_parsers["statement"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                commands.append(command)
                next_ = self.next_token()
        return (True, self.pos, MethodDefinitionNode(name, args, commands))

class IfTokenParser(TokenParser):
    def check(self):
        pass

class BoolExpressionTokenParser(TokenParser):
    def check(self):
        pass
    
class ClassDefinitionTokenParser(TokenParser):
    def check(self):
        parents = []
        next_ = self.next_token()
        if next_.code == primitives["FROM"]["code"]:
            is_good, pos, parent = token_parsers["variable"](self.flux, self.pos).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            parents.append(parent)
            next_ = self.next_token()
            while next_.code != primitives["CLASS"]["code"]:
                if next_.code == primitives["EOF"]["code"]:
                    return (False, self.origin_pos, None)
                if next_.code != primitives["COMMA"]["code"]:
                    return (False, self.origin_pos, None)
                next_ = self.next_token()
                is_good, pos, parent = token_parsers["variable"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                parents.append(parent)
                next_ = self.next_token()
        if next_.code != primitives["CLASS"]["code"]:
            return (False, self.origin_pos, None)
        name = self.next_token()
        if name.code != primitives["VARIABLE"]["code"]:
            return (False, self.origin_pos, None)
        name = name.value
        
        next_ = self.next_token()
        if next_.code != primitives["COLON"]["code"]:
            return (False, self.origin_pos, None)
        args = []
        next_ = self.next_token()
        if next_.code != primitives["LBRACE"]["code"]:
            is_good, pos, arg = token_parsers["variable"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            args.append(arg)
            next_ = self.next_token()
            while next_.code != primitives["LBRACE"]["code"]:
                if next_.code == primitives["EOF"]["code"]:
                    return (False, self.origin_pos, None)
                if next_.code != primitives["COMMA"]["code"]:
                    return (False, self.origin_pos, None)
                is_good, pos, arg = token_parsers["varialbe"](self.flux, self.pos).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                args.append(arg)
                self.pos = pos
                next_ = self.next_token()
        next_ = self.next_token()
        methods = []
        if next_.code != primitives["RBRACE"]["code"]:
            is_good, pos, method = token_parsers["method_definition"](self.flux, self.pos - 1).check()
            if not is_good:
                return (False, self.origin_pos, None)
            self.pos = pos
            methods.append(method)
            next_ = self.next_token()
            while next_.code != primitives["RBRACE"]["code"]:
                # HERE WE ARE !!!!!!!!!!!!!!!!!!
                is_good, pos, method = token_parsers["method_definition"](self.flux, self.pos - 1).check()
                if not is_good:
                    return (False, self.origin_pos, None)
                self.pos = pos
                methods.append(method)
                next_ = self.next_token()
        return (True, self.pos, ClassDefinitionNode(name, parents, args, methods))

# Built-in types    

class StringTokenParser(TokenParser):
    def check(self):
        string = self.next_token()
        if string.code != primitives["STRING"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()

        if is_good: self.pos = pos
        return (True, self.pos, StringNode(string.value, child))

class IntegerTokenParser(TokenParser):
    def check(self):
        integer = self.next_token()
        if integer.code != primitives["INT"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()

        if is_good: self.pos = pos
        return (True, self.pos, IntNode(integer.value, child))

class FloatTokenParser(TokenParser):
    def check(self):
        flt = self.next_token()
        if flt.code != primitives["FLOAT"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()
        if is_good: self.pos = pos
        return (True, self.pos, FloatNode(flt.value, child))

class ListTokenParser(TokenParser):
    def check(self):
        if self.next_token().code != primitives["LBRACKET"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, expressions = token_parsers["expression_list"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        if self.next_token().code != primitives["RBRACKET"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()
        if is_good: self.pos = pos
        return (True, self.pos, ListNode(expressions, child))
        

class ExpressionListTokenParser(TokenParser):
    def check(self):
        expressions = []
        is_good, pos, expression = token_parsers["expression"](self.flux, self.pos).check()
        if not is_good: return (True, self.pos, [])
        expressions.append(expression)
        self.pos = true_pos = pos
        next_ = self.next_token()
        while next_.code == primitives["COMMA"]["code"]:
            is_good, pos, expression = token_parsers["expression"](self.flux, self.pos).check()
            if not is_good: return (False, self.origin_pos, None)
            expressions.append(expression)
            self.pos = pos
            true_pos = self.pos
            next_ = self.next_token()
        return (True, true_pos, expressions)

class KeyAndValueTokenParser(TokenParser):
    def check(self):
        is_good, pos, key = token_parsers["expression"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        next_ = self.next_token()
        if next_.code != primitives["COLON"]["code"]: return (False, self.origin_pos, None)
        is_good, pos, value = token_parsers["expression"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        return (True, self.pos, (key, value))
    
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
        while next_.code == primitives["COMMA"]["code"]:
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
    
class DictTokenParser(TokenParser):
    def check(self):
        if self.next_token().code != primitives["LBRACE"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, expressions = token_parsers["expression_list_sbc"](self.flux, self.pos).check()
        if not is_good: return (False, self.origin_pos, None)
        self.pos = pos
        if self.next_token().code != primitives["RBRACE"]["code"]:
            return (False, self.origin_pos, None)
        is_good, pos, child = token_parsers["attribute"](self.flux, self.pos).check()
        if is_good: self.pos = pos
        return (True, self.pos, DictNode(expressions, child))


class ItemProxy:
    def __init__(self, dict_, key):
        self.dict = dict_
        self.key = key
    def __call__(self, *args, **kwargs):
        return self._get_item()(*args, **kwargs)
    def __getattr__(self, attribute):
        return object.__getattribute__(self, "_get_item")().__getattribute__(attribute)
    def _get_item(self):
        return self.dict.dict[self.key]

class ProxyDict:
    def __init__(self, dict_={}):
        self.dict = dict_
    def __getitem__(self, key):
        return ItemProxy(self, key)
    def __delitem__(self, key):
        del self.dict[key]
    def __setitem__(self, key, value):
        self.dict[key] = value
    
token_parsers = ProxyDict({
    "expression": ExpressionTokenParser,
    "assignment": AssignmentTokenParser,
    "statement": StatementTokenParser,
    "import": ImportTokenParser,
    "function_call": FunctionCallTokenParser,
    "attribute": AttributeTokenParser,
    "variable": VariableTokenParser,
    "variable_list": VariableListTokenParser,
    "function_definition": FunctionDefinitionTokenParser,
    "method_definition": MethodDefinitionTokenParser,
    "if": IfTokenParser,
    "bool_expression": BoolExpressionTokenParser,
    "class_definition": ClassDefinitionTokenParser,
    "string": StringTokenParser,
    "integer": IntegerTokenParser,
    "float": FloatTokenParser,
    "list": ListTokenParser,
    "expression_list": ExpressionListTokenParser,
    "key_and_value": KeyAndValueTokenParser,
    "expression_list_sbc": ExpressionListSBCTokenParser,# Separated By
                                                        # Comma
    "dict": DictTokenParser
})

expression_token_parsers = [
    token_parsers["function_call"],
    token_parsers["variable"],
    token_parsers["string"],
    token_parsers["float"],
    token_parsers["integer"],
    token_parsers["list"],
    token_parsers["dict"],
    token_parsers["import"]
]
statement_token_parsers = [
    token_parsers["class_definition"],
    token_parsers["function_definition"],
    token_parsers["function_call"],
    token_parsers["assignment"],
    token_parsers["expression"]
]


