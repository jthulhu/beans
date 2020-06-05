from .lxrgmrparser import LexerReader
from .stdloc import Location
from stderr import *

class LexingError(Exception):
    pass

class Lexer:
    """Creates a lexer from a string grammar. You can precise the file name with the argument file. It won't affect the actual grammar string, it will only be used when an error is raised"""
    def __init__(self, grammar, file="<stdgmr>", helperr=False):
        self.tokenizers, self.delete = LexerReader(grammar, file=file, helperr=helperr).read()
        self.file = file
        self.helperr = helperr
        self.flux = None
    def __call__(self, flux, pos=0, fn="<stdin>"):
        self.flux = flux
        self._pos = pos
        self.tokens = []
        self.fn = fn
        self.prelex()
    def prelex(self):
        while True:
            is_good, value = self._lex()
            if not is_good:
                raise_error (LexingSyntaxError(Frame(self.fn, *pos2coords(self._pos, self.flux))), "Could not tokenize at a certain point... what have you done?")
            token = self.tokenize(value)
            self.tokens.append((token, self._pos))
            if token == 'EOF':
                break
    def tokenize(self, token):
        token.file = self.fn
        token.pos = pos2coords(self._pos, self.flux)[::-1]
        token.tkpos = len(self.tokens)
        return token
    def _lex(self):
        """lex one token. do not use directly, instead ask the n token through lexer[n]"""
        for tokenizer in self.tokenizers.values():
            is_good, pos, result = tokenizer(self.flux, self._pos, Location(self.fn, *pos2coords(self._pos, self.flux)))
            if is_good:
                self._pos = pos
                if not result:
                    is_good, result = self._lex()
                return is_good, result
        return False, None
    def __getitem__(self, key):
        if key >= len(self.tokens):
            return self.tokens[-1][0]
        else:
            return self.tokens[key][0]
    def copy(self):
        return Lexer._paste(self.tokenizers, self.flux, self._pos)
    @classmethod
    def _paste(cls, tokenizers, flux, pos):
        self = cls("")
        self.tokenizers = tokenizers
        self.flux = flux
        self._pos = pos
        return self
    def __eq__(self, r):
        return type(self) == type(r) and hasattr(r, "tokenizers") and r.tokenizers == self.tokenizers and hasattr(r, "_pos") and r._pos == self._pos and hasattr(r, "flux") and r.flux == self.flux
    def __repr__(self):
        fn = "" if not hasattr(self, "fn") else (" instancied of file %s" % self.fn)
        return "<Lexer%s at %s>" % (fn, hex(id(self)))


def pos2coords(pos, flux):
    x, y = 1, 1
    for char in flux[:pos]:
        if char == "\n":
            y += 1
            x = 1
        else:
            x += 1
    return y, x
