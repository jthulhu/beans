# -*- coding: utf-8 -*-
from color import *
import sys

error_format = """%sFatal error%s - {errno:d}.
 @%s{line}:{char}%s in %s{file}%s
 %s{errtype}%s: {errmsg}
""" % (FATAL, ENDC, BOLD, ENDC, BOLD, ENDC, BOLD, ENDC)

warning_format = """%sWarning%s - {errno:d}.
 @%s{line}:{char}%s in %s{file}%s
 %s{errtype}%s: {errmsg}
""" % (WARNING, ENDC, BOLD, ENDC, BOLD, ENDC, BOLD, ENDC)


class Error:
    errtype = "Default root error"
    def __init__(self, frame, message):
        self.frame = frame
        self.errmsg = message

class SyntaxError(Error):
    errtype = "Syntax error"
        
class ParsingSyntaxError(Error):
    errtype = "Syntax error"

class NoPathSyntaxError(Error):
    errtype = "Syntax error"

class GrammarIdentityError(Error):
    errtype = "Grammar syntax-time error"
    
class LexingSyntaxError(Error):
    errtype = "Lexing syntax error"

class GrammarSyntaxError(Error):
    errtype = "Grammar syntax error"

class NotDefinedError(Error):
    errtype = "Declaration error"
    
class DeclarationNotFound(Error):
    errtype = "Declaration error"

class DeclaredTwiceError(Error):
    errtype = "Declaration error"

class OpNotDefinedError(Error):
    errtype = "Illegal operation"

class OpNotFoundError(Error):
    errtype = "Illegal operation"

class DefineTypeError(Error):
    errtype = "Type error"
    
class Frame:
    def __init__(self, file_, line, char):
        self.file = file_
        self.line = line
        self.char = char

        
def token_to_frame(token):
    return Frame(token.file, token.pos[1], token.pos[0])

def raise_warning(error=None, errno=1, helpmsg=False, debugger=None):
    if error != None:
        format_frame = {
            "errno": errno,
            "file": error.frame.file,
            "line": error.frame.line,
            "char": error.frame.char,
            "errtype": error.errtype,
            "errmsg": error.errmsg
        }
        print(warning_format.format(**format_frame))
    else:
        print("Warning")

def raise_error(error=None, errno=1, helpmsg=False, debugger=None):
    if error != None:
        format_frame = {
            "errno": errno,
            "file": error.frame.file,
            "line": error.frame.line,
            "char": error.frame.char,
            "errtype": error.errtype,
            "errmsg": error.errmsg
        }
        print(error_format.format(**format_frame))
    else:
        print("Fatal error")
    sys.exit(errno)
