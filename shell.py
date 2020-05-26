#!/usr/bin/python3

from beansexe import executer
from beansast import ASTBuilder
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
        for type, result in ex.exec(shell.get_ast(inp)):
            print(type.repr(result))
    except SystemExit:
        pass
    inp = prompt()