from .lxrgmrparser import LexerReader
from .stderr import *
class LexingError(Exception):
    pass

class Lexer:
    """Creates a lexer from a string grammar. You can precise the file name with the argument file. It won't affect the actual grammar string, it will only be used when an error is raised"""
    def __init__(self, grammar, file="<stdgmr>", helperr=False):
        self.tokenizers, self.delete = LexerReader(grammar, file=file, helperr=helperr).read()
        self.file = file
        self.helperr = helperr
    def __call__(self, flux, pos=0, fn="<stdin>"):
        self.flux = flux
        self._pos = pos
        self.tokens = []
        self.fn = fn
    def tokenize(self, token):
        token.file = self.fn
        token.pos = pos2coords(self._pos, self.flux)[::-1]
        token.tkpos = len(self.tokens)
        return token
    def _lex(self):
        """lex one token. do not use directly, instead ask the n token through lexer[n]"""
        for tokenizer in self.tokenizers.values():
            is_good, pos, result = tokenizer(self.flux, self._pos)
            if is_good:
                self._pos = pos
                if not result:
                    is_good, result = self._lex()
                return is_good, result
        return False, None
    def __getitem__(self, key):
        if key == -1:
            self._pos = 0
            self.tokens = []
            return None
        if key < 0:
            raise ValueError("Flux doesn't accept negative key. (-1 to reset)")
        while len(self.tokens) - 1 < key:
            is_good, value = self._lex()
            if not is_good:
                raise raise_error(LexingSyntaxError(Frame(self.fn, *pos2coords(self._pos, self.flux))))
            self.tokens.append((self.tokenize(value), self._pos))
        while len(self.tokens) - 1 > key:
            self.tokens.pop()
        self._pos = self.tokens[-1][1]
        return self.tokens[key][0]

    def update(self, grammar, file="<stdgmr>"):
        """update the lexer's grammar. same arguments used in the default constructor"""
        new_tokenizers, dels = LexerReader(grammar, file=file, helperr=self.helperr).read()
        for del_ in dels:
            if del_ in self.tokenizers.keys():
                del self.tokenizers[del_]
        self.tokenizers.update(new_tokenizers)
        self.file = file
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
