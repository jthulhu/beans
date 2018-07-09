from .lxrgmrparser import LexerReader

class Lexer:
    def __init__(self, grammar):
        self.tokenizers = LexerReader(grammar).read()
    def __call__(self, flux, pos=0):
        self.flux = flux
        self.pos = pos
    def lex(self):
        for tokenizer in self.tokenizers.values():
            is_good, pos, result = tokenizer(self.flux, self.pos)
            if is_good:
                self.pos = pos
                if not result:
                    is_good, result = self.lex()
                return is_good, result
        return False, None
    def update(self, grammar):
        self.tokenizers.update(LexerReader(grammar).read())
