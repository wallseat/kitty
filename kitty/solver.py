from typing import Any, Optional, Tuple, Union

from kitty.ast import BoolNode, CharNode, ExprNode, NumericNode, StrNode, ValueNode
from kitty.errors import IllegalOperation, OperationError
from kitty.position import Position
from kitty.token import Token, TokenType, VarType

_T_ValueOpRet = Tuple[Optional["Value"], Optional[OperationError]]


class Value:
    type_: VarType
    value: Optional[Any]

    pos_start: Position
    pos_end: Position

    def __init__(
        self, pos_start: Position, pos_end: Position, value: Optional[Any] = None
    ):
        self.value = value
        self.pos_start = pos_start
        self.pos_end = pos_end

    def add(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '+' not implemented for type '{self.type_}'",
        )

    def sub(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '-' not implemented for type '{self.type_}'",
        )

    def mul(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '*' not implemented for type '{self.type_}'",
        )

    def div(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '/' not implemented for type '{self.type_}'",
        )

    def eq(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '==' not implemented for type '{self.type_}'",
        )

    def neq(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '!=' not implemented for type '{self.type_}'",
        )

    def lt(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '<' not implemented for type '{self.type_}'",
        )

    def lte(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '<=' not implemented for type '{self.type_}'",
        )

    def gt(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '>' not implemented for type '{self.type_}'",
        )

    def gte(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation '>=' not implemented for type '{self.type_}'",
        )

    def and_(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation 'and' not implemented for type '{self.type_}'",
        )

    def or_(self, other: "Value") -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            other.pos_end,
            details=f"operation 'or' not implemented for type '{self.type_}'",
        )

    def not_(self) -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            self.pos_end,
            details=f"operation 'not' not implemented for type '{self.type_}'",
        )

    def minus(self) -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            self.pos_end,
            details=f"unary operation '-' not implemented for type '{self.type_}'",
        )

    def plus(self) -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            self.pos_end,
            details=f"unary operation '+' not implemented for type '{self.type_}'",
        )

    def bool_(self) -> _T_ValueOpRet:
        return None, OperationError(
            self.pos_start,
            self.pos_end,
            details=f"cast as bool not implemented for type '{self.type_}'",
        )


class NumericValue(Value):
    def __init__(
        self,
        pos_start: Position,
        pos_end: Position,
        type_: VarType,
        value: Optional[Union[int, float]] = None,
    ):
        self.type_ = type_
        super(NumericValue, self).__init__(pos_start, pos_end, value)

    def _get_out_type(self, other: "Value") -> VarType:
        out_type = (
            VarType.FLOAT
            if self.type_ == VarType.FLOAT or other.type_ == VarType.FLOAT
            else VarType.INT
        )

        return out_type

    def add(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'+' between '{self.type_}' and '{other.type_}'",
            )

        out_type = self._get_out_type(other)

        value = None
        if self.value is not None and other.value is not None:
            value = self.value + other.value

        return (
            NumericValue(self.pos_start, other.pos_end, out_type, value),
            None,
        )

    def sub(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'-' between '{self.type_}' and '{other.type_}'",
            )

        out_type = self._get_out_type(other)

        value = None
        if self.value is not None and other.value is not None:
            value = self.value - other.value

        return (
            NumericValue(self.pos_start, other.pos_end, out_type, value),
            None,
        )

    def div(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'/' between '{self.type_}' and '{other.type_}'",
            )

        out_type = VarType.FLOAT

        if other.value == 0:
            return None, OperationError(
                self.pos_start, other.pos_end, details="division by zero"
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value / other.value

        return (
            NumericValue(self.pos_start, other.pos_end, out_type, value),
            None,
        )

    def mul(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'*' between '{self.type_}' and '{other.type_}'",
            )

        out_type = self._get_out_type(other)

        value = None
        if self.value is not None and other.value is not None:
            value = self.value * other.value

        return (
            NumericValue(self.pos_start, other.pos_end, out_type, value),
            None,
        )

    def eq(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'==' between '{self.type_}' and '{other.type_}'",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value == other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def neq(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'!=' between '{self.type_}' and '{other.type_}'",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value != other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def lt(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'<' between '{self.type_}' and '{other.type_}'",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value < other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def lte(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'<=' between '{self.type_}' and '{other.type_}'",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value <= other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def gt(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'>' between '{self.type_}' and '{other.type_}''",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value > other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def gte(self, other: Value) -> _T_ValueOpRet:
        if not isinstance(other, NumericValue):
            return None, IllegalOperation(
                self.pos_start,
                other.pos_end,
                f"'>=' between '{self.type_}' and '{other.type_}'",
            )

        value = None
        if self.value is not None and other.value is not None:
            value = self.value >= other.value

        return (
            BoolValue(self.pos_start, other.pos_end, value),
            None,
        )

    def not_(self) -> _T_ValueOpRet:
        value = None
        if self.value is not None:
            value = not self.value

        return (
            BoolValue(self.pos_start, self.pos_end, value),
            None,
        )

    def minus(self) -> _T_ValueOpRet:
        value = None
        if self.value is not None:
            value = -self.value

        return (NumericValue(self.pos_start, self.pos_end, self.type_, value), None)

    def plus(self) -> _T_ValueOpRet:
        return self, None

    def bool_(self) -> _T_ValueOpRet:
        value = None
        if self.value is not None:
            value = bool(self.value)

        return BoolValue(self.pos_start, self.pos_end, value), None


class StringValue(Value):
    type_ = VarType.STR

    def __init__(
        self, pos_start: Position, pos_end: Position, value: Optional[str] = None
    ):
        super(StringValue, self).__init__(pos_start, pos_end, value)


class CharValue(StringValue):
    type_ = VarType.CHAR


class BoolValue(Value):
    type_ = VarType.BOOL

    def __init__(
        self, pos_start: Position, pos_end: Position, value: Optional[bool] = None
    ):
        super(BoolValue, self).__init__(pos_start, pos_end, value)

    def and_(self, other: Value) -> _T_ValueOpRet:
        other_as_bool, error = other.bool_()
        if error or not other_as_bool:
            return None, error

        value = None
        if self.value is not None and other_as_bool.value is not None:
            value = self.value and other_as_bool.value

        return (BoolValue(self.pos_start, other.pos_end, value), None)

    def or_(self, other: Value) -> _T_ValueOpRet:
        other_as_bool, error = other.bool_()
        if error or not other_as_bool:
            return None, error

        value = None
        if self.value is not None and other_as_bool.value is not None:
            value = self.value or other_as_bool.value

        return (BoolValue(self.pos_start, other.pos_end, value), None)

    def not_(self) -> _T_ValueOpRet:
        value = None
        if self.value is not None:
            value = not self.value

        return (BoolValue(self.pos_start, self.pos_end, value), None)

    def bool_(self) -> _T_ValueOpRet:
        return self, None


class NodeValueConverter:
    @staticmethod
    def node_to_value(node: ExprNode) -> "Value":
        if isinstance(node, NumericNode):
            if isinstance(node.token.ctx, int):
                return NumericValue(
                    node.pos_start, node.pos_end, VarType.INT, node.token.ctx
                )
            else:
                return NumericValue(
                    node.pos_start, node.pos_end, VarType.FLOAT, node.token.ctx
                )

        elif isinstance(node, StrNode):
            return StringValue(node.pos_start, node.pos_end, node.token.ctx)

        elif isinstance(node, CharNode):
            return CharValue(node.pos_start, node.pos_end, node.token.ctx)

        elif isinstance(node, BoolNode):
            return BoolValue(node.pos_start, node.pos_end, node.token.ctx)

        else:
            if node.type_ in (VarType.FLOAT, VarType.INT):
                return NumericValue(node.pos_start, node.pos_end, node.type_)
            elif node.type_ == VarType.STR:
                return StringValue(node.pos_start, node.pos_end)
            elif node.type_ == VarType.CHAR:
                return CharValue(node.pos_start, node.pos_end)
            elif node.type_ == VarType.BOOL:
                return BoolValue(node.pos_start, node.pos_end)
            else:
                raise Exception("Invalid type of node to value convert")

    @staticmethod
    def value_to_node(value: Value) -> ValueNode:
        node: ValueNode

        if isinstance(value, NumericValue):
            if value.type_ == VarType.INT:
                tok_type = TokenType.NUM_INT
            else:
                tok_type = TokenType.NUM_FLOAT

            node = NumericNode(
                Token(tok_type, value.pos_start, value.pos_end, value.value)
            )

        elif isinstance(value, StringValue):
            node = StrNode(
                Token(TokenType.STR, value.pos_start, value.pos_end, value.value)
            )

        elif isinstance(value, CharValue):
            node = CharNode(
                Token(TokenType.CHAR, value.pos_start, value.pos_end, value.value)
            )

        else:
            node = BoolNode(
                Token(TokenType.BOOL, value.pos_start, value.pos_end, value.value)
            )

        node.type_ = value.type_

        return node
