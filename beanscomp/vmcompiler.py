import stderr, carrot
from stderr import raise_error

default = {
    "add": {
        ("int", "int"): "int",
        ("string", "string"):"string",
        ("bool", "bool"): "bool",
        "size": 2
    },
    "sub": {
        ("int", "int"): "int",
        "size": 2
    },
    "mul": {
        ("int", "int"): "int",
        ("bool", "bool"): "bool",
        ("string", "int"): "string",
        ("int", "string"): "string",
        "size": 2
    },
    "pow": {
        ("int", "int"): "int",
        "size": 2
    },
    "div": {
        "size": 2
    },
    "tdiv": {
        ("int", "int"): "int",
        "size": 2
    },
    "mod": {
        ("int", "int"): "int",
        "size": 2
    },
    "min": {
        ("int",): "int",
        "size": 1
    },
    "pls": {
        ("int",): "int",
        "size": 1
    },
    "ne": {
        ("bool", "bool"): "bool",
        ("string", "string"): "bool",
        ("int", "int"): "bool",
        "size": 2
    },
    "eq": {
        ("bool", "bool"): "bool",
        ("string", "string"): "bool",
        ("int", "int"): "bool",
        "size": 2
    },
    "lt": {
        ("bool", "bool"): "bool",
        ("int", "int"): "bool",
        ("string", "string"): "bool",
        "size": 2
    },
    "gt": {
        ("bool", "bool"): "bool",
        ("int", "int"): "bool",
        ("string", "string"): "bool",
        "size": 2
    },
    "com": {
        ("int",): "int",
        "size": 1
    },
    "bool": {
        ("bool",): "bool",
        ("int",): "bool",
        ("string",): "bool",
        "size": 1
    }
}

class VarHolder:
    def __init__(self):
        self.scopes = [{}]
        self._contains = {}
    def new_scope(self):
        self.scopes.append({})
    def remove_scope(self):
        for key in self.scopes.pop().keys():
            self._contains[key].pop()
            if len(self._contains[key])==0:
                del self._contains[key]
    def del_var(self, key):
        i = self.query_var(key)
        del self.scopes[i][key]
        self._contains[key].remove(i)
        if len(self._contains[key]) == 0:
            del self._contains[key]
    def declare_var(self, key, type):
        self.scopes[-1][key] = [type, False]
        if key not in self._contains:
            self._contains[key] = []
        self._contains[key].append(len(self.scopes)-1)
    def query_var(self, key):
        return self._contains[key][-1] if key in self._contains else -1
    def is_declared(self, key):
        return key in self
    def is_defined(self, key):
        return key in self and self.scopes[self.query_var(key)][key][1]
    def is_declared_current(self, key):
        return self.is_declared(key) and self.query_var(key) == len(self.scopes)-1
    def set_var(self, key):
        i = self.query_var(key)
        self.scopes[i][key][1] = True
    def __contains__(self, key):
        return key in self._contains
    def get_var(self, key):
        return self.scopes[self.query_var(key)][key][0]

class VMCompiler:
    def __init__(self):
        self.vars = VarHolder()
        self.types = {"int", "string", "bool"}
    def new_scope(self):
        self.vars.new_scope()
    def remove_scope(self):
        self.vars.remove_scope()
    def evaluate(self, node):
        if node["op"] == "builtin":
            if node["type"] in self.types:
                return node["type"]
            elif node["type"] == "true":
                return "bool"
            elif node["type"] == "false":
                return "bool"
            elif node["type"] == "id":
                return self.get_variable(node["value"], node.frame)
            else:
                raise_error (stderr.TypeNotFoundError(node.frame, "AST is corrupted: type `%s' is not recognized" % node["type"]))
        elif node["op"] in default:
            size = default[node["op"]]["size"]
            if size == 1:
                types = (self.evaluate(node["right"]),)
            else:
                types = (self.evaluate(node["left"]), self.evaluate(node["right"]))
            if types not in default[node["op"]]:
                if size == 1:
                    raise_error (stderr.OpNotDefinedError(node.frame, "%s on `%s' is not defined" % (node["op"], *types)))
                else:
                    raise_error (stderr.OpNotDefinedError(node.frame, "%s between `%s' and `%s' is not defined" % (node["op"], *types)))
            else:
                return default[node["op"]][types]
        else:
            raise_error (stderr.OpNotFoundError(node.frame, "AST is corrupted: operator `%s' is not recognized" % node["op"]))
    def execute(self, node):
        if node["s"] == "eval":
            self.evaluate(node)
        elif node["s"] == "assign":
            value = self.evaluate(node["value"])
            if node["type"] != "[None]":
                self.declare(node["key"], node["type"], node.frame)
            elif not self.vars.is_declared(node["key"]):
                self.declare(node["key"], value, node.frame)
            self.define(node["key"], value, node.frame)
        elif node["s"] == 'concatenate':
            self.execute(node["left"])
            self.execute(node["right"])
        elif node["s"] == 'declare':
            self.declare(node["key"], node["type"], node.frame)
        elif node["s"] == 'test':
            self.evaluate(node["test"])
            self.new_scope()
            self.execute(node["then"])
            self.remove_scope()
            if node["haselse"]:
                self.new_scope()
                self.execute(node["else"])
                self.remove_scope()
        elif node["s"] == 'do':
            self.evaluate(node["test"])
            self.new_scope()
            self.execute(node["do"])
            self.remove_scope()
        elif node["s"] == "return":
            self.evaluate(node)
    def define(self, key, value, frame):
        if not self.vars.is_declared(key):
            raise_error (stderr.DeclarationNotFound(frame, "Trying to defined variable `%s' without declaring it" % key))
        type = self.vars.get_var(key)
        if type != value:
            raise_error (stderr.DefineTypeError(frame, "Assigning value of type `%s' to variable of type `%s'" % (value, type)))
            
        self.vars.set_var(key)
    def get_variable(self, key, frame):
        if not self.vars.is_declared(key):
            raise_error (stderr.NotDeclaredError(frame, "Trying to retrieve not declared variable `%s' value" % key))
        if not self.vars.is_defined(key):
            raise_error (stderr.NotDefinedError(frame, "Trying to retrieve not defined variable `%s' value" % key))

        return self.vars.get_var(key)
    def declare(self, key, type, frame):
        if self.vars.is_declared_current(key):
            raise_error (stderr.DeclaredTwiceError(frame, "Trying to declare already declared variable `%s'" % key))
        self.vars.declare_var(key, type)
    def compile(self, tree):
        self.execute(tree)
