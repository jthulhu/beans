# -*- coding: utf-8 -*-
from .color import *
import sys

__all__ = [
    "raise_error",
    "ParsingSyntaxError",
    "GrammarIdentityError",
    "NoPathSyntaxError",
    "TooManyPathSyntaxError",
    "GrammarClassError",
    "LexingSyntaxError",
    "GrammarSyntaxError",
    "Frame",
    "token_to_frame"
]

error_format = """%sFatal error%s - {errno:d}.
 @%s{line}:{char}%s in %s{file}%s
 %s{errtype}%s: {errmsg}{helpmsg}""" % (FATAL, ENDC, BOLD, ENDC, BOLD, ENDC, BOLD, ENDC)

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
    errmsg_mask = "expected {} and got {}"

class GrammarAmbiguityError(Error):
    errtype = "Grammar error"
    helpmsg = "You tried to define a grammar which is ambiguous, in the sense that multiple lexer object could have been generated or that some attribute has been defined more than once."
    errmsg_mask = "Your grammar is ambiguous because {}"
    
class NoPathSyntaxError(Error):
    errtype = "Syntax error"
    helpmsg = "You tried to parse a file which cannot be parsed in anyway with the given rules (reach a `first' rule)."
    errmsg_mask = "cannot reach the first rule"

class TooManyPathSyntaxError(Error):
    errtype = "Syntax error"
    helpmsg = "Your grammar is too ambiguous or your input doesn't follow your grammar, but the `first' rules has been reached too many times and the parser cannot decide which one is the best (or how to merge them)."
    errmsg_mask = "first rule reached multiple times"

class GrammarClassError(Error):
    errtype = "Grammar error"
    helpmsg = "You defined a rule in your grammar which is a subrule of a class which doesn't exists. Try check again your class definitions and your inheiritence definition."
    errmsg_mask = "class {} does not exist"
    
class GrammarIdentityError(Error):
    errtype = "Grammar syntax-time error"
    helpmsg = "This error is a grammar error found during the syntax-parsing time. It occurs when you a rule has @this key but this key is not his only one."
    errmsg_mask = "multiple key found and @this withing them"
    
class LexingSyntaxError(Error):
    errtype = "Lexing syntax error"
    helpmsg = "You tried to lex a file which doesn't correspond to the defined tokens."
    errmsg_mask = "token not understood"

    
class GrammarSyntaxError(Error):
    errtype = "Grammar syntax error"
    helpmsg = "You tried to generate tokens from an invalid lexer grammar file."
    errmsg_mask = "expected {}"

class Frame:
    def __init__(self, file_, line, char):
        self.file = file_
        self.line = line
        self.char = char

        
def token_to_frame(token):
    return Frame(token.file, token.pos[1], token.pos[0])
        
def raise_error(error=None, errno=1, helpmsg=False, debugger=None):
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
    if debugger:
        print("debugger!")
        raise DebuggerSignal(debugger)
    else:
        sys.exit(errno)
