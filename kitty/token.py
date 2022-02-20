from enum import Enum, auto
from typing import Any, Optional

from kitty.lexer.position import Position


class TokenType(Enum):
    # SPACE = auto()
    # NLINE = auto()

    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()

    L_BRC = auto()
    R_BRC = auto()

    # S_BLOCK = auto()
    # E_BLOCK = auto()

    INLINE_COMM = auto()

    NUM_INT = auto()
    NUM_FLOAT = auto()


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
            return f"{self.type}:{self.ctx}"

        return f"{self.type}"
