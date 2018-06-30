
class ASTNode:
    def __eq__(self, comp):
        return type(comp) == type(self) and {key: self.__getattribute__(key) for key in dir(self) if not key.startswith("__")} == {key: comp.__getattribute__(key) for key in dir(comp) if not key.startswith("__")}

class FunctionCallNode(ASTNode):
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments
        self.child = child
    def __repr__(self):
        return "FunctionCallNode(%s, %s, %s)" % (self.name, self.arguments, self.child)

class VariableNode(ASTNode):
    def __init__(self, name):
        self.name = name
        self.child = child
    def __repr__(self):
        return "VariableNode(%s, %s)" % (self.name, self.child)

class StringNode(ASTNode):
    def __init__(self, value):
        self.value = value
    def __repr__(self):
        return "StringNode(%s)" % self.value

class AssignmentNode(ASTNode):
    def __init__(self, name, value):
        self.name = name
        self.value = value
    def __repr__(self):
        return "AssignementNode(%s, %s)" % (self.name, self.value)

class FunctionDefinitionNode(ASTNode):
    def __init__(self, name, args, commands):
        self.name = name
        self.args = args
        self.commands = commands
    def __repr__(self):
        return "FunctionDefinitionNode(%s, %s, %s)" % (self.name, self.args, self.commands)

class MethodDefinitionNode(ASTNode):
    def __init__(self, name, args, commands):
        self.name = name
        self.args = args
        self.commands = commands
    def __repr__(self):
        return "MethodDefinitionNode(%s, %s, %s)" % (self.name, self.args, self.commands)

class ClassDefinitionNode(ASTNode):
    def __init__(self, name, parents, args, methods):
        self.name = name
        self.parents = parents
        self.args = args
        self.methods = methods
    def __repr__(self):
        return "ClassDefinitionNode(%s, %s, %s, %s)" % (self.name, self.parents, self.args, self.methods)

class ImportNode(ASTNode):
    def __init__(self, root_path, importobjs):
        self.root_path = root_path
        self.importobjs = importobjs
    def __repr__(self):
        return "ImportNode(%s, %s)" % (self.root_path, self.importobjs)

class BuiltInType(ASTNode):
    def __init__(self, value):
        self.value = value
        self.child = child
    
class StringNode(BuiltInType):
    def __repr__(self):
        return "StringNode(%s, %s)" % (self.value, self.child)

class IntNode(BuiltInType):
    def __repr__(self):
        return "IntNode(%s, %s)" % (self.value, self.child)

class FloatNode(BuiltInType):
    def __repr__(self):
        return "FloatNode(%s, %s)" % (self.value, self.child)

class ListNode(BuiltInType):
    def __repr__(self):
        return "ListNode(%s, %s)" % (self.value, self.child)

class DictNode(BuiltInType):
    def __repr__(self):
        return "DictNode(%s, %s)" % (self.value, self.child)
