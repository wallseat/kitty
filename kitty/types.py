from enum import Enum

from kitty.token import Token


class VarType(Enum):
    INT = "int"
    FLOAT = "float"
    STR = "str"
    CHAR = "char"
    BOOL = "bool"
    VOID = "void"

    UNTYPED = "untyped"

    GENERIC = "generic"


def identifier_to_var_type(token: Token) -> VarType:
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
    elif token.ctx == "void":
        return VarType.VOID
    else:
        return VarType.GENERIC
