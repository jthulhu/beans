#!/usr/bin/python3
from beansexe import executer
from beansast import ASTBuilder
import sys

parser = ASTBuilder()
ex = executer.Executer()

with open(sys.argv[1]) as f:
    ast = parser.get_ast(f.read(), sys.argv[1])

for type, r in ex.exec(ast):
    print(type.repr(r))
