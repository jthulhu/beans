#!/usr/bin/python3
from beansexe import executer
from beansast import ASTBuilder
from beanscomp import vmcompiler
import sys

parser = ASTBuilder()
ex = executer.Executer()
compiler = vmcompiler.VMCompiler()

with open(sys.argv[1]) as f:
    ast = parser.get_ast(f.read(), sys.argv[1])

#compiler.compile(ast)
    
a = ex.exec(ast)
if a != None:
    print("(%s) %s" % tuple(a))
