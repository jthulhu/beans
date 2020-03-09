#!/usr/bin/python3

import pdb
import beansast
from beansast.stdout import *
from beansast import ShellAST


shell = ShellAST()

inp = input('> ')

pdb.run("shell.compute(inp)")
