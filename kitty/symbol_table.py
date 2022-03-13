from typing import Any, Dict, List, Optional, Tuple, Union

from kitty.token import VarType


class BaseSymbol:
    pass


class VarSymbol(BaseSymbol):
    name: str
    type_: VarType

    def __init__(self, name: str, type_: VarType):
        self.name = name
        self.type_ = type_


class FuncSymbol(BaseSymbol):
    name: str
    args: Optional[List[Tuple[str, VarType]]]
    ret_type: VarType

    def __init__(
        self,
        name: str,
        args: Optional[List[Tuple[str, VarType]]],
        ret_type: VarType,
    ):
        self.name = name
        self.args = args
        self.ret_type = ret_type


_T_Symbol = Union[VarSymbol, FuncSymbol]


class SymbolTable:
    table: Dict[str, _T_Symbol]
    outer_table: Optional["SymbolTable"]

    def __init__(self, outer_table: "SymbolTable" = None):
        self.outer_table = outer_table
        self.table = {}

    def get(self, _id: str, recursive: bool = True) -> Optional[_T_Symbol]:
        if (sym := self.table.get(_id, None)) is not None:
            return sym
        elif self.outer_table is not None and recursive:
            return self.outer_table.get(_id)
        else:
            return None

    def set(
        self,
        _id: str,
        value: _T_Symbol,
    ) -> bool:
        if self.table.get(_id, None) is None:
            self.table[_id] = value
            return True

        return False

    def delete(self, _id: str) -> Optional[_T_Symbol]:
        if (sym := self.table.get(_id, None)) is not None:
            del self.table[_id]
            return sym

        return None
