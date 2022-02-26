from enum import Enum, auto
from typing import Any, Optional

from kitty.position import Position


class TokenType(Enum):
    NLINE = auto()  # \n

    ADD = auto()  # +
    SUB = auto()  # -
    MUL = auto()  # *
    DIV = auto()  # /
    ASSIGN = auto()  # =

    EQ = auto()  # ==
    NEQ = auto()  # !=
    GT = auto()  # >
    LT = auto()  # <
    GTE = auto()  # >=
    LTE = auto()  # <=

    L_BRC = auto()  # (
    R_BRC = auto()  # )

    COMMA = auto()  # ,
    DOT = auto()  # .
    COLON = auto()  # :
    R_ARROW = auto()  # ->

    IF = auto()  # if
    WHILE = auto()  # while
    FOR = auto()  # for
    RET = auto()  # ret
    VAR = auto()  # var
    FUNC = auto()  # func
    AND = auto()  # and
    OR = auto()  # or
    NOT = auto()  # not

    IDENTITY = auto()  # a | abc | ...

    S_BLOCK = auto()  # {
    E_BLOCK = auto()  # }

    INLINE_COMMENT = auto()  # #
    COMMENT = auto()  # /* ctx */

    NUM_INT = auto()  # 5
    NUM_FLOAT = auto()  # 5.0 or 5f


class Token:
    type_: TokenType
    ctx: Any

    pos_start: Optional[Position]
    pos_end: Optional[Position]

    def __init__(
        self,
        type_: TokenType,
        pos_start: Position = None,
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
