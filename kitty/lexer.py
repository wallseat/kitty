import string
from typing import List, Optional, Tuple

from kitty.errors import IllegalCharError
from kitty.position import Position
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

    def tokenize(self) -> Tuple[Optional[List[Token]], Optional[IllegalCharError]]:
        tokens = []

        while self.cur_let is not None:
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

            elif self.cur_let == "[":
                tokens.append(Token(TokenType.L_SQUARE, pos_start=self.pos))
                self.advance()

            elif self.cur_let == "]":
                tokens.append(Token(TokenType.R_SQUARE, pos_start=self.pos))
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

            elif self.cur_let == '"':
                tokens.append(self.lex_string())
                self.advance()

            elif self.cur_let == "'":
                start_pos = self.pos.copy()
                self.advance()
                char = self.cur_let

                if char == "\\":
                    self.advance()
                    if self.cur_let in "'":
                        char = self.cur_let
                    else:
                        char += self.cur_let

                self.advance()

                if self.cur_let != "'":
                    return [], IllegalCharError(
                        start_pos, self.pos.copy().advance(), f"'{self.cur_let}'"
                    )

                self.advance()
                tokens.append(
                    Token(
                        TokenType.CHAR, pos_start=start_pos, pos_end=self.pos, ctx=char
                    )
                )

            elif self.cur_let in string.ascii_letters + "_":
                tokens.append(self.lex_id())

            else:
                pos_start = self.pos.copy()
                let = self.cur_let
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, f"'{let}'")

        tokens.append(Token(TokenType.EOF, self.pos))

        return tokens, None

    def lex_number(self) -> Token:
        pos_start = self.pos.copy()
        num_str = ""
        dot_count = 0

        while self.cur_let is not None and self.cur_let in string.digits + "." + "f":
            if self.cur_let == ".":
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += "."

            elif self.cur_let == "f":
                dot_count = 1
                self.advance()
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

    def lex_string(self) -> Token:
        start_pos = self.pos.copy()
        self.advance()

        string = ""
        while self.cur_let is not None:
            if self.cur_let == "\\":
                self.advance()
                if self.cur_let == '"':
                    string += self.cur_let
                else:
                    string += "\\"
                    string += self.cur_let

            elif self.cur_let == '"':
                break

            else:
                string += self.cur_let

            self.advance()

        return Token(TokenType.STR, pos_start=start_pos, pos_end=self.pos, ctx=string)

    def lex_inline_comment(self) -> Token:
        pos_start = self.pos.copy()
        self.advance()
        comment_str = ""

        while self.cur_let is not None and self.cur_let != "\n":
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
            return Token(TokenType.DIV, pos_start=pos_start, pos_end=self.pos)

        else:
            self.advance()

            comment_str = ""
            prev_let = ""
            while self.cur_let is not None:
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

        while self.cur_let is not None and self.cur_let in string.ascii_letters + "_":
            identity += self.cur_let
            self.advance()

        if identity == "for":
            return Token(TokenType.FOR, pos_start=start_pos, pos_end=self.pos)

        elif identity == "if":
            return Token(TokenType.IF, pos_start=start_pos, pos_end=self.pos)

        elif identity == "elif":
            return Token(TokenType.ELIF, pos_start=start_pos, pos_end=self.pos)

        elif identity == "else":
            return Token(TokenType.ELSE, pos_start=start_pos, pos_end=self.pos)

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

        elif identity == "in":
            return Token(TokenType.IN, pos_start=start_pos, pos_end=self.pos)

        elif identity == "true":
            return Token(
                TokenType.BOOL, pos_start=start_pos, pos_end=self.pos, ctx=True
            )

        elif identity == "false":
            return Token(
                TokenType.BOOL, pos_start=start_pos, pos_end=self.pos, ctx=False
            )

        elif identity == "continue":
            return Token(TokenType.CONTINUE, pos_start=start_pos, pos_end=self.pos)

        elif identity == "break":
            return Token(TokenType.BREAK, pos_start=start_pos, pos_end=self.pos)

        else:
            return Token(
                TokenType.IDENTIFIER,
                pos_start=start_pos,
                pos_end=self.pos,
                ctx=identity,
            )
