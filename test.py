#!/usr/bin/python3

from beansast import ASTBuilder
from beansast.stdout import print_tree

FILE = "utest_scripts/test1.bns"

with open(FILE) as f:
    inp = f.read()

print_tree(ASTBuilder(inp).get_ast())
