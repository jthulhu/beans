class Executer:
    def __init__(self):
        self.vars = {}
    def eval(self, node):
        if node["op"] == "builtin":
            if node["type"] == "int":
                return int(node["value"])
            elif node["type"] == "string":
                return node["value"]
            elif node["type"] == "id":
                return self.vars[node["value"]]
        elif node["op"] == "add":
            return self.eval(node["left"]) + self.eval(node["right"])
        elif node["op"] == "sub":
            return self.eval(node["left"]) - self.eval(node["right"])
        elif node["op"] == "mul":
            return self.eval(node["left"]) * self.eval(node["right"])
        elif node["op"] == "div":
            return self.eval(node["left"]) / self.eval(node["right"])
    def execute(self, node):
        if node["s"] == "eval":
            return self.eval(node)
        elif node["s"] == 'assign':
            self.vars[node["key"]] = self.eval(node["value"])
