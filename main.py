#!/usr/bin/python3

from beansast import ASTBuilder
from beansast.psrgmrparser import ASTNode
from beansast.stdout import print_tree
from beansast.color import *

PS1 = BLUE + BOLD + "bns> " + ENDC

inp = input(PS1)

while inp not in {"q", "e", "quit", "exit"}:
    try:
        print_tree(ASTBuilder(inp).get_ast())
    except SystemExit:
        pass
    inp = input(PS1)
