from enum import Enum
from typing import Dict, Optional

from kitty.token import Token


class TypeName(Enum):
    INT = "int"
    FLOAT = "float"
    STR = "str"
    CHAR = "char"
    BOOL = "bool"
    VOID = "void"

    UNTYPED = "untyped"

    GENERIC_ARRAY = "array"
    GENERIC_STRUCT = "struct"


class Type_:
    type_name: TypeName

    def __init__(self, type_name: TypeName):
        self.type_name = type_name

    def __str__(self) -> str:
        return self.type_name.name

TYPE_INT = Type_(TypeName.INT)
TYPE_FLOAT = Type_(TypeName.FLOAT)
TYPE_STR = Type_(TypeName.STR)
TYPE_CHAR = Type_(TypeName.CHAR)
TYPE_BOOL = Type_(TypeName.BOOL)
TYPE_VOID = Type_(TypeName.VOID)
TYPE_UNTYPED = Type_(TypeName.UNTYPED)


class GenericType(Type_):
    pass


class GenericArrayType(GenericType):
    type_name = TypeName.GENERIC_ARRAY

    elem_type_name: Optional[TypeName]

    def __init__(self, elem_type_name: TypeName = None):
        self.elem_type_name = elem_type_name

class GenericStructType(GenericType):
    type_name = TypeName.GENERIC_STRUCT

    struct_args: Dict[str, Type_]

    def __init__(self, struct_args: Dict[str, Type_]):
        self.struct_args = struct_args


def identifier_to_type(token: Token, type_arg: Optional[Type_] = None) -> Type_:
    if token.ctx == TypeName.INT.value:
        return TYPE_INT
    elif token.ctx == TypeName.FLOAT.value:
        return TYPE_FLOAT
    elif token.ctx == TypeName.STR.value:
        return TYPE_STR
    elif token.ctx == TypeName.CHAR.value:
        return TYPE_CHAR
    elif token.ctx == TypeName.BOOL.value:
        return TYPE_BOOL
    elif token.ctx == TypeName.VOID.value:
        return TYPE_VOID
    elif token.ctx == 'array':
        return GenericArrayType(type_arg)
    else:
        raise NotImplementedError("User types not supported yet")
