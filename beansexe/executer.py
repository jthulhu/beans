import carrot, stderr
from stderr import raise_error

class Type:
    type_repr = 'type'
    def __init__(self):
        self.ops = {
            "add": {},
            "sub": {},
            "mul": {},
            "pow": {},
            "div": {},
            "tdiv": {},
            "mod": {},
            "min": {},
            "pls": {},
            "eq": {},
            "lt": {},
            "gt": {},
            "com": {},
            "bool": {}
        }
    def __eq__(self, right):
        return self.type_repr == right.type_repr
    def __hash__(self):
        return hash(self.type_repr)
    def __repr__(self):
        return self.type_repr
    def add(self, left, rtype, right, frame):
        if type(rtype) in self.ops["add"]:
            ntype, op = self.ops["add"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["add"]:
            ntype, op = self.ops["add"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to add `%s' and `%s'"% (self, rtype)))
    def sub(self, left, rtype, right, frame):
        if type(rtype) in self.ops["sub"]:
            ntype, op = self.ops["sub"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["sub"]:
            ntype, op = self.ops["sub"]["*"]
            return ntype(self, rtype), op(left, right)
        else:

            raise_error (stderr.OpNotDefinedError(frame, "Tryed to subtrac `%s' and `%s'"% (self, rtype)))
    def mul(self, left, rtype, right, frame):
        if type(rtype) in self.ops["mul"]:
            ntype, op = self.ops["mul"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["mul"]:
            ntype, op = self.ops["mul"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to multiply `%s' and `%s'"% (self, rtype)))
    def pow(self, left, rtype, right, frame):
        if type(rtype) in self.ops["pow"]:
            ntype, op = self.ops["pow"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["pow"]:
            ntype, op = self.ops["pow"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to raise to power `%s' and `%s'" % (self, rtype)))
    def div(self, left, rtype, right, frame):
        if type(rtype) in self.ops["div"]:
            ntype, op = self.ops["div"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["div"]:
            ntype, op = self.ops["div"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to divide `%s' and `%s'"% (self, rtype)))
    def tdiv(self, left, rtype, right, frame):
        if type(rtype) in self.ops["tdiv"]:
            ntype, op = self.ops["tdiv"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["tdiv"]:
            ntype, op = self.ops["tdiv"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to true divede `%s' and `%s'"% (self, rtype)))
    def mod(self, left, rtype, right, frame):
        if type(rtype) in self.ops["mod"]:
            ntype, op = self.ops["mod"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["mod"]:
            ntype, op = self.ops["mod"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to take modulo `%s' and `%s'"% (self, rtype)))
    def min(self, left, frame):
        if None in self.ops["min"]:
            ntype, op = self.ops["min"][None]
            return ntype(self), op(left)
        elif "*" in self.ops["min"]:
            ntype, op = self.ops["min"]["*"]
            return ntype(self), op(left)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to take minus `%s'"% (self,)))
    def pls(self, left, frame):
        if None in self.ops["pls"]:
            ntype, op = self.ops["pls"][None]
            return ntype(self), op(left)
        elif "*" in self.ops["pls"]:
            ntype, op = self.ops["pls"]["*"]
            return ntype(self), op(left)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to take plus `%s' and `%s'"% (self,)))
    def ne(self, left, rtype, right, frame):
        if type(rtype) in self.ops["ne"]:
            ntype, op = self.ops["ne"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["ne"]:
            ntype, op = self.ops["ne"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to compare (ne) `%s' and `%s'"% (self, rtype)))
    def eq(self, left, rtype, right, frame):
        if type(rtype) in self.ops["eq"]:
            ntype, op = self.ops["eq"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["eq"]:
            ntype, op = self.ops["eq"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to compare (eq) `%s' and `%s'"% (self, rtype)))
    def lt(self, left, rtype, right, frame):
        if type(rtype) in self.ops["lt"]:
            ntype, op = self.ops["lt"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["lt"]:
            ntype, op = self.ops["lt"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to compare (lt) `%s' and `%s'"% (self, rtype)))
    def gt(self, left, rtype, right, frame):
        if type(rtype) in self.ops["gt"]:
            ntype, op = self.ops["gt"][type(rtype)]
            return ntype(self, rtype), op(left, right)
        elif "*" in self.ops["gt"]:
            ntype, op = self.ops["gt"]["*"]
            return ntype(self, rtype), op(left, right)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to compare (gt) `%s' and `%s'" % (self, rtype)))
    def bool(self, left, frame):
        if None in self.ops["bool"]:
            ntype, op = self.ops["bool"][None]
            return ntype(self), op(left)
        elif "*" in self.ops["bool"]:
            ntype, op = self.ops["bool"]["*"]
            return ntype(self), op(left)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to bool `%s'" % (self,)))
    def com(self, left, frame):
        if None in self.ops["com"]:
            ntype, op = self.ops["com"][None]
            return ntype(self), op(left)
        elif "*" in self.ops["com"]:
            ntype, op = self.ops["com"]["*"]
            return ntype(self), op(left)
        else:
            raise_error (stderr.OpNotDefinedError(frame, "Tryed to complement `%s'" % (self,)))
    def repr(self, left):
        return '(%s) %s' % (self, left)

class BnsStringType(Type):
    type_repr = 'string'
    def __init__(self):
        Type.__init__(self)
        self.ops.update({
            "add": {
                BnsStringType: (lambda ltype, rtype: ltype, lambda left, right: left+right)
            },
            "mul": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left*right)
            },
            "eq": {
                BnsStringType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left == right),
                "*": (lambda ltype, rtype: BnsBoolType(), lambda left, right: False)
            },
            "lt": {
                BnsStringType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left < right)
            },
            "gt": {
                BnsStringType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left > right)
            }
        })
        
class BnsIntType(Type):
    type_repr = 'int'
    def __init__(self):
        Type.__init__(self)
        self.ops.update({
            "add": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left+right)
            },
            "mul": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left*right),
                BnsStringType: (lambda ltype, rtype: rtype, lambda left, right: left*right)
            },
            "sub": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left-right)
            },
            #"div": {
            #    BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left/right)
            #},
        "tdiv": {
            BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left//right)
        },
            "eq": {
                BnsIntType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left==right),
                "*": (lambda ltype, rtype: BnsBoolType(), lambda left, right: False)
            },
            "ne": {
                BnsIntType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left!=right),
                "*": (lambda ltype, rtype: BnsBoolType(), lambda left, right: True)
            },
            "lt": {
                BnsIntType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left < right)
            },
            "gt": {
                BnsIntType: (lambda ltype, rtype: BnsBoolType(), lambda left, right: left > right)
            },
            "pow": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left**right)
            },
            "mod": {
                BnsIntType: (lambda ltype, rtype: ltype, lambda left, right: left%right)
            },
            "min": {
                None: (lambda ltype: ltype, lambda left: -left)
            },
            "plus": {
                None: (lambda ltype: ltype, lambda left: +left)
            },
            "bool": {
                None: (lambda ltype: BnsBoolType(), lambda left: left!=0)
            },
            "com": {
                None: (lambda ltype: ltype, lambda left: ~left)
            }
        })

class BnsBoolType(Type):
    type_repr = 'bool'
    def __init__(self):
        Type.__init__(self)
        self.ops.update({
            "bool": {
                None: (lambda ltype: ltype, lambda left: left)
            }
        })

class BnsFrameType(Type):
    def _init_constructor(self):
        pass
    def read(self, value):
        return value
    def write(self, value):
        return value

def char(string):
    if len(string) != 1:
        raise TypeError('Cannot recognize as char')
    return string

class Executer:
    def __init__(self):
        self.prevars = {}
        self.vars = {
            'string': (Type(), BnsStringType()),
            'int': (Type(), BnsIntType()),
            'bool': (Type(), BnsBoolType()),
        }
    def evaluate(self, node):
        if node["op"] == "builtin":
            if node["type"] == "int":
                return (self.get_variable("int", node.frame)[1], int(node["value"]))
            elif node["type"] == "string":
                return (self.get_variable("string", node.frame)[1], node["value"])
            elif node["type"] == "id":
                var = self.get_variable(node["value"], node.frame)
                return var
            elif node["type"] == "true":
                return (self.get_variable("bool", node.frame)[1], True)
            elif node["type"] == "false":
                return (self.get_variable("bool", node.frame)[1], False)
        elif node["op"] == "add":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.add(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "sub":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.sub(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "mul":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.mul(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "div":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.div(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "ge":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.ge(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "gt":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.gt(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "le":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.le(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "lt":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.lt(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "eq":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.eq(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "ne":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.ne(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "frame":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.add(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "com":
            ltype, lvalue = self.evaluate(node["right"])
            return ltype.com(lvalue, node.frame)
        elif node["op"] == "pls":
            ltype, lvalue = self.evaluate(node["right"])
            return ltype.pls(lvalue, node.frame)
        elif node["op"] == "min":
            ltype, lvalue = self.evaluate(node["right"])
            return ltype.min(lvalue, node.frame)
        elif node["op"] == "pow":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.pow(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "tdiv":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.tdiv(lvalue, rtype, rvalue, node.frame)
        elif node["op"] == "mod":
            ltype, lvalue = self.evaluate(node["left"])
            rtype, rvalue = self.evaluate(node["right"])
            return ltype.mod(lvalue, rtype, rvalue, node.frame)
        raise_error (stderr.OpNotFoundError(node.frame, "Node operator doesn't match the one supported by this interpreter"))
    def execute(self, node):
        if node["s"] == "eval":
            self.evaluate(node)
        elif node["s"] == 'assign':
            value = self.evaluate(node["value"])
            if node["type"] != "[None]":
                self.declare(node["key"], node["type"], node.frame)
            elif node["key"] not in self.prevars and node["key"] not in self.vars:
                self.prevars[node["key"]] = value[0]
            self.define(node["key"], value, node.frame)
        elif node["s"] == 'concatenate':
            a = self.execute(node["left"])
            if a != None:
                return a
            return self.execute(node["right"])
        elif node["s"] == 'declare':
            self.declare(node["key"], node["type"], node.frame)
        elif node["s"] == 'test':
            type, value = self.evaluate(node["test"])
            if type.bool(value, node.frame)[1]:
                return self.execute(node["then"])
            elif node["haselse"]:
                return self.execute(node["else"])
        elif node["s"] == 'do':
            type, value = self.evaluate(node["test"])
            while type.bool(value, node.frame)[1]:
                a = self.execute(node["do"])
                if a != None:
                    return a
                type, value = self.evaluate(node["test"])
        elif node["s"] == "return":
            return self.evaluate(node)
    def define(self, key, value, frame):
        if key not in self.prevars and key not in self.vars:
            raise_error (stderr.DeclarationNotFound(frame, 'Trying to define variable `%s\' without declaring it' % key))
        if key in self.prevars:
            type = self.prevars[key]
            del self.prevars[key]
            self.vars[key] = [type, None]
        else:
            type = self.vars[key][0]
        if type != value[0]:
            raise_error (stderr.DefineTypeError(frame, "Assigning value of type `%s' to variable of type `%s'" % (value[0], type)))
        self.vars[key][1] = value[1]
    def get_variable(self, key, frame):
        if key in self.prevars:
            raise_error (stderr.NotDefinedError(frame, 'Trying to retrieve not defined variable `%s\' value' % key))
        if key not in self.vars:
            raise_error (stderr.NotDeclaredError(frame, 'Trying to retrieve not declared variable `%s\' value' % key))
        return self.vars[key]
    def declare(self, key, type, frame):
        if key in self.prevars or key in self.vars:
            raise_error (stderr.DeclaredTwiceError(frame, 'Trying to declare already declared variable `%s\'' % key))
        self.prevars[key] = self.get_variable(type, frame)[1]
    def exec(self, node):
        return self.execute(node)
