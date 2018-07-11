from .lxrgmrparser import LexerReader

class LexingError(Exception):
    pass

class Lexer:
    def __init__(self, grammar):
        self.tokenizers = LexerReader(grammar).read()
    def __call__(self, flux, pos=0):
        self.flux = flux
        self._pos = pos
        self.tokens = []
    def _lex(self):
        for tokenizer in self.tokenizers.values():
            is_good, pos, result = tokenizer(self.flux, self._pos)
            if is_good:
                self._pos = pos
                if not result:
                    is_good, result = self._lex()
                return is_good, result
        return False, None
    def __getitem__(self, key):
        if key < 0:
            raise ValueError("Flux doesn't accept negative key.")
        while len(self.tokens) - 1 < key:
            is_good, value = self._lex()
            if not is_good:
                raise LexingError("Unable to lex char %s" % self._pos)
            self.tokens.append((value, self._pos))
        while len(self.tokens) - 1 > key:
            self.tokens.pop()
        self._pos = self.tokens[-1][1]
        return self.tokens[key]

    def update(self, grammar):
        self.tokenizers.update(LexerReader(grammar).read())
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
        return "<Lexer%s at %s>" % ((" instancied") * int(hasattr(self, "flux")), str(id(self)))
