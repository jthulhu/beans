#!/usr/bin/python3
# -*- coding: utf-8 -*-



def color_format(string):
    return string.replace("")

ENDC = '\033[0m'

BOLD = '\033[1m'
UNDERLINE = '\033[4m'
DIM = '\033[2m'
BLINK = '\033[5m'
REVERSE = '\033[7m'
HIDDEN = '\033[8m'

ENDFG = '\033[39m'
ENDBG = '\033[49m'

BLACK = '\033[30m'
RED = '\033[31m'
GREEN = '\033[32m'
YELLOW = '\033[33m'
BLUE = '\033[34m'
MAGENTA = '\033[35m'
CYAN = '\033[36m'

BBLACK = '\033[40m'
BRED = '\033[41m'
BGREEN = '\033[42m'
BYELLOW = '\033[43m'
BBLUE = '\033[44m'
BMAGENTA = '\033[45m'
BCYAN = '\033[46m'

WARNING = BOLD + RED
FATAL = BOLD + RED + BYELLOW
OK = BOLD + GREEN
IMPORTANT = UNDERLINE + BOLD + RED
CENSURED = BBLACK + BLACK

color_map = [
    ("ENDC", ENDC),
    ("BOLD", BOLD),
    ("UNDERLINE", UNDERLINE),
    ("DIM", DIM),
    ("BLINK", BLINK),
    ("REVERSE", REVERSE),
    ("HIDDEN", HIDDEN),
    ("ENDFG", ENDFG),
    ("ENDBG", ENDBG),
    ("BLACK", BLACK),
]
