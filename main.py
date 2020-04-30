#!/usr/bin/python3

from beansexe import executer
from beansast import ASTBuilder
from beansast.stdout import print_tree
from color import *
from sys import exit

PS1 = BLUE + BOLD + "bns> " + ENDC

shell = ASTBuilder()
ex = executer.Executer()

def prompt():
    try:
        return input(PS1)
    except EOFError:
        print("quit")
        exit(0)



inp = prompt()
while inp not in {"q", "e", "quit", "exit"}:
    try:
        result = ex.execute(shell.get_ast(inp))
        if result:
            print(result)
    except SystemExit:
        pass
    inp = prompt()
