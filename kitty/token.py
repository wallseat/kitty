from enum import Enum
from typing import Any

from kitty.position import Position


class TokenType(Enum):
    NLINE = "NLINE"  # \n

    ADD = "ADD"  # +
    SUB = "SUB"  # -
    MUL = "MUL"  # *
    DIV = "DIV"  # /
    ASSIGN = "ASSIGN"  # =

    EQ = "EQ"  # ==
    NEQ = "NEQ"  # !=
    GT = "GT"  # >
    LT = "LT"  # <
    GTE = "GTE"  # >=
    LTE = "LTE"  # <=

    L_BRC = "L_BRC"  # (
    R_BRC = "R_BRC"  # )
    S_BLOCK = "S_BLOCK"  # {
    E_BLOCK = "E_BLOCK"  # }
    L_SQUARE = "L_SQUARE"  # [
    R_SQUARE = "R_SQUARE"  # ]

    COMMA = "COMMA"  # ,
    DOT = "DOT"  # .
    COLON = "COLON"  # :
    R_ARROW = "R_ARROW"  # ->

    IF = "IF"  # if
    ELIF = "ELIF"  # elif
    ELSE = "ELSE"  # else
    WHILE = "WHILE"  # while
    FOR = "FOR"  # for
    RET = "RET"  # ret
    VAR = "VAR"  # var
    FUNC = "FUNC"  # func
    AND = "AND"  # and
    OR = "OR"  # or
    NOT = "NOT"  # not
    IN = "IN"  # in
    CONTINUE = "CONTINUE"  # continue
    BREAK = "BREAK"  # break

    IDENTIFIER = "IDENTIFIER"  # a | abc | ...

    INLINE_COMMENT = "INLINE_COMMENT"  # #
    COMMENT = "COMMENT"  # /* ctx */

    NUM_INT = "NUM_INT"  # 5
    NUM_FLOAT = "NUM_FLOAT"  # 5.0 or 5f

    STR = "STR"  # "string"
    CHAR = "CHAR"  # 'a'

    BOOL = "BOOL"  # true | false

    EOF = "EOF"  # end of file


class VarType(Enum):
    INT = "INT"
    FLOAT = "FLOAT"
    STR = "STR"
    CHAR = "CHAR"
    BOOL = "BOOL"

    GENERIC = "GENERIC"
    UNTYPED = "UNTYPED"


class Token:
    type_: TokenType
    ctx: Any

    pos_start: Position
    pos_end: Position

    def __init__(
        self,
        type_: TokenType,
        pos_start: Position,
        pos_end: Position = None,
        ctx: Any = None,
    ):
        self.type = type_
        self.ctx = ctx

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()  # type: ignore

        if pos_end:
            self.pos_end = pos_end.copy()

    def __repr__(self):
        if self.ctx:
            if isinstance(self.ctx, str):
                if len(self.ctx) <= 15 and not "\n" in self.ctx:
                    return f"{self.type}:'{self.ctx}'"
                else:
                    return f"{self.type}:[{len(self.ctx)} symbols]"
            else:
                return f"{self.type}:{self.ctx}"

        return f"{self.type}"


def identifier_to_var_type(token: Token, can_untyped: bool = False) -> VarType:
    if token.ctx == "int":
        return VarType.INT
    elif token.ctx == "float":
        return VarType.FLOAT
    elif token.ctx == "str":
        return VarType.STR
    elif token.ctx == "char":
        return VarType.CHAR
    elif token.ctx == "bool":
        return VarType.BOOL
    elif can_untyped and token.ctx in ('noret', 'untyped'):
        return VarType.UNTYPED
    else:
        raise NotImplementedError("Generic types is unsupported yet!")
