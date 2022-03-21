from enum import Enum
from typing import Any

from kitty.position import Position


class TokenType(Enum):
    NLINE = "NLINE"  # \n

    ADD = "add"  # +
    SUB = "sub"  # -
    MUL = "mul"  # *
    DIV = "div"  # /
    ASSIGN = "assign"  # =

    EQ = "eq"  # ==
    NEQ = "neq"  # !=
    GT = "gt"  # >
    LT = "lt"  # <
    GTE = "gte"  # >=
    LTE = "lte"  # <=

    L_BRC = "l_brc"  # (
    R_BRC = "r_brc"  # )
    S_BLOCK = "s_block"  # {
    E_BLOCK = "e_block"  # }
    L_SQUARE = "l_square"  # [
    R_SQUARE = "r_square"  # ]

    COMMA = "comma"  # ,
    DOT = "dot"  # .
    COLON = "colon"  # :
    R_ARROW = "r_arrow"  # ->

    IF = "if"  # if
    ELIF = "elif"  # elif
    ELSE = "else"  # else
    WHILE = "while"  # while
    FOR = "for"  # for
    RET = "ret"  # ret
    VAR = "var"  # var
    CONST = "const"  # const
    FUNC = "func"  # func
    AND = "and"  # and
    OR = "or"  # or
    NOT = "not"  # not
    IN = "in"  # in
    AS = "as"  # as
    CONTINUE = "continue"  # continue
    BREAK = "break"  # break

    IDENTIFIER = "identifier"  # a | abc | ...

    INLINE_COMMENT = "inline_comment"  # #
    COMMENT = "comment"  # /* ctx */

    NUM_INT = "num_int"  # 5
    NUM_FLOAT = "num_float"  # 5.0 or 5f
    STR = "str"  # "string"
    CHAR = "char"  # 'a'
    BOOL = "bool"  # true | false

    EOF = "eof"  # end of file


class VarType(Enum):
    INT = "int"
    FLOAT = "float"
    STR = "str"
    CHAR = "char"
    BOOL = "bool"

    GENERIC = "generic"
    UNTYPED = "untyped"


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
        self.type_ = type_
        self.ctx = ctx
        self.pos_start = None  # type: ignore
        self.pos_end = None  # type: ignore

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()  # type: ignore

        if pos_end:
            self.pos_end = pos_end.copy()

    def __repr__(self):
        if self.ctx is not None:
            if isinstance(self.ctx, str):
                if len(self.ctx) <= 15 and not "\n" in self.ctx:
                    return f"{self.type_}:'{self.ctx}'"
                else:
                    return f"{self.type_}:[{len(self.ctx)} symbols]"
            else:
                return f"{self.type_}:{self.ctx}"

        return f"{self.type_}"


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
    elif can_untyped and token.ctx in ("noret", "untyped"):
        return VarType.UNTYPED
    else:
        raise NotImplementedError("Generic types is unsupported yet!")
