#!/usr/bin/python3

from beansast import ASTBuilder
from beansast.stdout import print_tree
from color import *
from sys import exit

PS1 = BLUE + BOLD + "bns> " + ENDC

shell = ASTBuilder()

def prompt():
    try:
        return input(PS1)
    except EOFError:
        print("quit")
        exit(0)



inp = prompt()
while inp not in {"q", "e", "quit", "exit"}:
    try:
        print_tree(shell.get_ast(inp))
    except SystemExit:
        pass
    inp = prompt()
