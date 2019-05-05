# -*- coding: utf-8 -*-

import sys

__all__ = [
    "raise_error",
    "ParsingSyntaxError",
    "LexingSyntaxError",
    "GrammarSyntaxError",
    "Frame",
    "token_to_frame"
]

error_format = """Fatal error - {errno:d}.
 @{line}:{char} in {file}
 {errtype}: {errmsg}{helpmsg}"""

class Error:
    errtype = "Default root error"
    helpmsg = "This error doesn't hold any help message, sorry!"
    errmsg_mask = "{}"
    def __init__(self, frame, *args, **kwargs):
        self.frame = frame
        self.errmsg = self.errmsg_mask.format(*args, **kwargs)

class ParsingSyntaxError(Error):
    errtype = "Syntax error"
    helpmsg = "You tried to parse a file which doesn't respect the grammar rules."
    errmsg_mask = "excpected {} and got {}"

class LexingSyntaxError(Error):
    errtype = "Lexing syntax error"
    helpmsg = "You tried to lex a file which doesn't correspond to the defined tokens."
    errmsg_mask = "token not understood"

class GrammarSyntaxError(Error):
    errtype = "Grammar syntax error"
    helpmsg = "You tried to generate tokens from an invalid lexer grammar file."
    errmsg_mask = "excpected {}"

class Frame:
    def __init__(self, file_, line, char):
        self.file = file_
        self.line = line
        self.char = char

def token_to_frame(token):
    return Frame(token.file, token.pos[1], token.pos[0])
        
def raise_error(error=None, errno=1, helpmsg=False):
    if error:
        format_frame = {
            "errno": errno,
            "file": error.frame.file,
            "line": error.frame.line,
            "char": error.frame.char,
            "errtype": error.errtype,
            "errmsg": error.errmsg,
            "helpmsg": ("\n\n -> Tip: " + error.helpmsg) if (helpmsg and hasattr(error, "helpmsg")) else ""
        }
        print(error_format.format(**format_frame))
    else:
        print("Fatal error")
    #sys.exit(errno)
    raise 
