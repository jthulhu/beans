import sys, traceback

class Executer:
    def __init__(self):
        self.vars = {}
        self.vars['vars'] = self.vars
    def evaluate(self, node):
        if node["op"] == "builtin":
            if node["type"] == "int":
                return int(node["value"])
            elif node["type"] == "string":
                return node["value"]
            elif node["type"] == "id":
                return self.vars[node["value"]]
        elif node["op"] == "add":
            return self.evaluate(node["left"]) + self.evaluate(node["right"])
        elif node["op"] == "sub":
            return self.evaluate(node["left"]) - self.evaluate(node["right"])
        elif node["op"] == "mul":
            return self.evaluate(node["left"]) * self.evaluate(node["right"])
        elif node["op"] == "div":
            return self.evaluate(node["left"]) / self.evaluate(node["right"])
        raise ValueError('expression not understood')
    def execute(self, node):
        if node["s"] == "eval":
            yield self.evaluate(node)
        elif node["s"] == 'assign':
            self.vars[node["key"]] = self.evaluate(node["value"])
        elif node["s"] == 'concatenate':
            yield from self.execute(node["left"])
            yield from self.execute(node["right"])
    def exec(self, node):
        try:
            yield from self.execute(node)
        except:
            traceback.print_exc()
