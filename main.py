#!/usr/bin/python3
import time
t1 = time.time()*1000
from beansexe import executer
from beansast import ASTBuilder
from beanscomp import vmcompiler
import sys
DEBUG = False

def debug(*args, **kwargs):
    if DEBUG:
        print(*args, **kwargs)

debug('Building grammar...', end=" ")
t_grammar_1 = time.time()*1000
parser = ASTBuilder()
t_grammar_2 = time.time()*1000
debug('done.')
ex = executer.Executer()
compiler = vmcompiler.VMCompiler()

debug('Building ast...', end=" ")
t_ast_1 = time.time()*1000
with open(sys.argv[1]) as f:
    ast = parser.get_ast(f.read(), sys.argv[1])
t_ast_2 = time.time()*1000
debug("done")

#compiler.compile(ast)
debug("Executing...", end=" ")
t_exec_1 = time.time()
a = ex.exec(ast)
t_exec_2 = time.time()
debug("done.")
if a != None:
    print("(%s) %s" % tuple(a))

t2 = time.time()*1000
debug('Total: %sms\n Grammar: %sms\n AST: %sms\n Exec: %sms' % (round(t2-t1), round(t_grammar_2 - t_grammar_1), round(t_ast_2 - t_ast_1), round(t_exec_2 - t_exec_1)))
