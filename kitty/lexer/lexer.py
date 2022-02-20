import string

from kitty.lexer.errors import IllegalCharError
from kitty.lexer.position import Position
from kitty.token import Token, TokenType


class Lexer:
    text: str
    fname: str
    pos: Position
    cur_let: str

    def __init__(self, fname: str, text: str):
        self.text = text
        self.fname = fname
        self.pos = Position(-1, 0, -1, fname, text)
        self.cur_let = ""
        self.advance()

    def advance(self):
        self.pos.advance(self.cur_let)
        self.cur_let = (
            self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
        )

    def tokenize(self):
        tokens = []

        while self.cur_let != None:
            if self.cur_let in " \t\n":
                self.advance()

            elif self.cur_let == "+":
                tokens.append(Token(TokenType.ADD, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "-":
                tokens.append(Token(TokenType.SUB, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "*":
                tokens.append(Token(TokenType.MUL, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "/":
                tokens.append(Token(TokenType.DIV, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "(":
                tokens.append(Token(TokenType.L_BRC, pos_start=self.pos))
                self.advance()

            elif self.cur_let == ")":
                tokens.append(Token(TokenType.R_BRC, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "#":
                tokens.append(self.lex_comment())
                self.advance()

            elif self.cur_let in string.digits:
                tokens.append(self.lex_number())
                self.advance()

            else:
                pos_start = self.pos.copy()
                char = self.cur_let
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        return tokens, None

    def lex_number(self):
        pos_start = self.pos.copy()
        num_str = ""
        dot_count = 0

        while self.cur_let != None and self.cur_let in string.digits + ".":
            if self.cur_let == ".":
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += "."
            else:
                num_str += self.cur_let
            self.advance()

        if dot_count == 0:
            return Token(
                TokenType.NUM_INT,
                ctx=int(num_str),
                pos_start=pos_start,
                pos_end=self.pos,
            )
        else:
            return Token(
                TokenType.NUM_FLOAT,
                ctx=float(num_str),
                pos_start=pos_start,
                pos_end=self.pos,
            )

    def lex_comment(self):
        pos_start = self.pos.copy()
        self.advance()
        comment_str = ""

        while self.cur_let != None and self.cur_let != "\n":
            comment_str += self.cur_let
            self.advance()

        return Token(
            TokenType.INLINE_COMM,
            pos_start=pos_start,
            pos_end=self.pos,
            ctx=comment_str,
        )
