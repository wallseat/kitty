from typing import Dict, List, Optional, Tuple, Union

from kitty.ast import BaseNode, FuncNode, VarNode
from kitty.types import VarType


class BaseSymbol:
    name: str
    ref_node: BaseNode


class VarSymbol(BaseSymbol):
    type_: VarType
    ref_node: VarNode

    def __init__(self, name: str, type_: VarType, ref_node: VarNode):
        self.name = name
        self.type_ = type_
        self.ref_node = ref_node


class FuncSymbol(BaseSymbol):
    args: List[VarNode]
    ret_type: VarType
    ref_node: FuncNode

    def __init__(
        self,
        name: str,
        args: List[VarNode],
        ret_type: VarType,
        ref_node: FuncNode,
    ):
        self.name = name
        self.args = args
        self.ret_type = ret_type
        self.ref_node = ref_node


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
