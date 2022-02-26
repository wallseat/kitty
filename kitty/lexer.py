import string

from kitty.errors import IllegalCharError
from kitty.position import Position
from kitty.symbol_table import SymTable
from kitty.token import Token, TokenType


class Lexer:
    text: str
    fname: str
    pos: Position
    cur_let: str
    symbol_table: SymTable

    def __init__(self, fname: str, text: str):
        self.text = text
        self.fname = fname
        self.pos = Position(-1, 0, -1, fname, text)
        self.symbol_table = SymTable()

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
            if self.cur_let in " \t":
                self.advance()

            elif self.cur_let == "\n":
                tokens.append(Token(TokenType.NLINE, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "+":
                tokens.append(Token(TokenType.ADD, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "-":
                tokens.append(self.lex_sub_or_r_arrow())

            elif self.cur_let == "*":
                tokens.append(Token(TokenType.MUL, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "/":
                tokens.append(self.lex_comment_or_div())
                self.advance()

            elif self.cur_let == "=":
                tokens.append(self.lex_assign_or_equal())

            elif self.cur_let == ">":
                tokens.append(self.lex_gt_or_gte())

            elif self.cur_let == "<":
                tokens.append(self.lex_lt_or_lte())

            elif self.cur_let == "!":
                start_pos = self.pos.copy()

                self.advance()
                if self.cur_let == "=":
                    tokens.append(
                        Token(TokenType.NEQ, pos_start=start_pos, pos_end=self.pos)
                    )
                    self.advance()
                else:
                    return [], IllegalCharError(start_pos, self.pos, "'!'")

            elif self.cur_let == "(":
                tokens.append(Token(TokenType.L_BRC, pos_start=self.pos))
                self.advance()

            elif self.cur_let == ")":
                tokens.append(Token(TokenType.R_BRC, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "{":
                tokens.append(Token(TokenType.S_BLOCK, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "}":
                tokens.append(Token(TokenType.E_BLOCK, pos_start=self.pos))
                self.advance()

            elif self.cur_let == ".":
                tokens.append(Token(TokenType.DOT, pos_start=self.pos))
                self.advance()

            elif self.cur_let == ":":
                tokens.append(Token(TokenType.COLON, pos_start=self.pos))
                self.advance()

            elif self.cur_let == ",":
                tokens.append(Token(TokenType.COMMA, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "#":
                tokens.append(self.lex_inline_comment())
                self.advance()

            elif self.cur_let in string.digits:
                tokens.append(self.lex_number())
                self.advance()

            elif self.cur_let in string.ascii_letters + "_":
                tokens.append(self.lex_id())

            else:
                pos_start = self.pos.copy()
                let = self.cur_let
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, f"'{let}'")

        return tokens, None

    def lex_number(self) -> Token:
        pos_start = self.pos.copy()
        num_str = ""
        dot_count = 0

        while self.cur_let != None and self.cur_let in string.digits + "." + "f":
            if self.cur_let == ".":
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += "."

            elif self.cur_let == "f":
                dot_count = 1
                break

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

    def lex_inline_comment(self) -> Token:
        pos_start = self.pos.copy()
        self.advance()
        comment_str = ""

        while self.cur_let != None and self.cur_let != "\n":
            comment_str += self.cur_let
            self.advance()

        return Token(
            TokenType.INLINE_COMMENT,
            pos_start=pos_start,
            pos_end=self.pos,
            ctx=comment_str,
        )

    def lex_comment_or_div(self) -> Token:
        pos_start = self.pos.copy()
        self.advance()

        if self.cur_let != "*":
            self.advance()
            return Token(TokenType.DIV, pos_start=pos_start, pos_end=self.pos)

        else:
            self.advance()

            comment_str = ""
            prev_let = ""
            while True:
                if self.cur_let == "/" and prev_let == "*":
                    comment_str = comment_str[:-1]
                    break

                else:
                    comment_str += self.cur_let

                prev_let = self.cur_let

                self.advance()

            return Token(
                TokenType.COMMENT,
                pos_start=pos_start,
                pos_end=self.pos,
                ctx=comment_str,
            )

    def lex_sub_or_r_arrow(self) -> Token:
        start_pos = self.pos.copy()

        self.advance()
        if self.cur_let == ">":
            tok = Token(TokenType.R_ARROW, pos_start=start_pos, pos_end=self.pos)
            self.advance()
            return tok

        else:
            return Token(TokenType.SUB, pos_start=start_pos, pos_end=self.pos)

    def lex_gt_or_gte(self) -> Token:
        start_pos = self.pos.copy()

        self.advance()
        if self.cur_let == "=":
            tok = Token(TokenType.GTE, pos_start=start_pos, pos_end=self.pos)
            self.advance()
            return tok

        else:
            return Token(TokenType.GT, pos_start=start_pos, pos_end=self.pos)

    def lex_lt_or_lte(self) -> Token:
        start_pos = self.pos.copy()

        self.advance()
        if self.cur_let == "=":
            tok = Token(TokenType.LTE, pos_start=start_pos, pos_end=self.pos)
            self.advance()
            return tok

        else:
            return Token(TokenType.LT, pos_start=start_pos, pos_end=self.pos)

    def lex_assign_or_equal(self) -> Token:
        start_pos = self.pos.copy()

        self.advance()
        if self.cur_let == "=":
            tok = Token(TokenType.EQ, pos_start=start_pos, pos_end=self.pos)
            self.advance()
            return tok

        else:
            return Token(TokenType.ASSIGN, pos_start=start_pos, pos_end=self.pos)

    def lex_id(self) -> Token:
        start_pos = self.pos.copy()
        identity = ""

        while self.cur_let != None and self.cur_let in string.ascii_letters + "_":
            identity += self.cur_let
            self.advance()

        if identity == "for":
            return Token(TokenType.FOR, pos_start=start_pos, pos_end=self.pos)

        elif identity == "if":
            return Token(TokenType.IF, pos_start=start_pos, pos_end=self.pos)

        elif identity == "while":
            return Token(TokenType.WHILE, pos_start=start_pos, pos_end=self.pos)

        elif identity == "ret":
            return Token(TokenType.RET, pos_start=start_pos, pos_end=self.pos)

        elif identity == "var":
            return Token(TokenType.VAR, pos_start=start_pos, pos_end=self.pos)

        elif identity == "func":
            return Token(TokenType.FUNC, pos_start=start_pos, pos_end=self.pos)

        elif identity == "and":
            return Token(TokenType.AND, pos_start=start_pos, pos_end=self.pos)

        elif identity == "or":
            return Token(TokenType.OR, pos_start=start_pos, pos_end=self.pos)

        elif identity == "not":
            return Token(TokenType.NOT, pos_start=start_pos, pos_end=self.pos)

        else:
            self.symbol_table.set(identity, None)
            return Token(
                TokenType.IDENTITY, pos_start=start_pos, pos_end=self.pos, ctx=identity
            )
