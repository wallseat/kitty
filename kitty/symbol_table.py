from typing import Any, Dict, Optional


class SymTable:
    table: Dict[str, Any]
    outer_table: Optional["SymTable"]

    def __init__(self, outer_table: "SymTable" = None):
        self.outer_table = outer_table
        self.table = {}

    def get(self, _id: str) -> Optional[Any]:
        if (sym := self.table.get(_id, None)) is not None:
            return sym
        elif self.outer_table is not None:
            return self.outer_table.get(_id)
        else:
            return None

    def set(
        self,
        _id: str,
        value: Any,
    ) -> None:
        self.table[_id] = value
