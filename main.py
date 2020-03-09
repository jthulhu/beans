#!/usr/bin/python3

from beansast import ShellAST
from beansast.stdout import print_tree
from beansast.color import *
from sys import exit

PS1 = BLUE + BOLD + "bns> " + ENDC

shell = ShellAST()

def prompt():
    try:
        return input(PS1)
    except EOFError:
        print("quit")
        exit(0)



inp = prompt()
while inp not in {"q", "e", "quit", "exit"}:
    if inp in {"r", "reload"}:
        shell.reload()
        inp = prompt()
        continue
    elif inp in {"r", "rtime", "rtimes"}:
        shell.print_times_r()
        inp = prompt()
        continue
    elif inp in {"t", "time", "times"}:
        shell.print_times()
        inp = prompt()
        continue
    try:
        print_tree(shell.compute(inp))
    except SystemExit:
        pass
    inp = prompt()
