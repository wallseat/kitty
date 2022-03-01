from typing import Any, List, Optional, Tuple

from kitty.position import Position
from kitty.token import Token, VarType


class BaseNode:
    pos_start: Optional[Position]
    pos_end: Optional[Position]

    def __init__(
        self, pos_start: Optional[Position] = None, pos_end: Optional[Position] = None
    ):
        self.pos_start = pos_start
        self.pos_end = pos_end

    def as_struct(self) -> Tuple:
        return ()


class NumNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(NumNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("Numeric", self.token)

    def __repr__(self):
        return repr(self.token)


class BinOpNode(BaseNode):
    op_token: Token
    left_operand: BaseNode
    right_operand: BaseNode

    def __init__(
        self, left_operand: BaseNode, op_token: Token, right_operand: BaseNode
    ):
        self.op_token = op_token
        self.left_operand = left_operand
        self.right_operand = right_operand

        super(BinOpNode, self).__init__(left_operand.pos_start, right_operand.pos_end)

    def as_struct(self) -> Tuple[str, Any, Token, Any]:
        return (
            "BinaryOp",
            self.left_operand.as_struct(),
            self.op_token,
            self.right_operand.as_struct(),
        )

    def __repr__(self):
        return f"({self.left_operand} {self.op_token} {self.right_operand})"


class UnaryOpNode(BaseNode):
    op_token: Token
    node: BaseNode

    def __init__(self, op_token: Token, node: BaseNode):
        self.op_token = op_token
        self.node = node

        super(UnaryOpNode, self).__init__(self.op_token.pos_start, self.node.pos_end)

    def as_struct(self) -> Tuple[str, Any, Token]:
        return ("UnaryOp", self.node.as_struct(), self.op_token)

    def __repr__(self):
        return f"({self.op_tok}, {self.node})"


class VarNode(BaseNode):
    var_id_tok: Token
    value_node: Optional[BaseNode]
    var_type: VarType

    def __init__(
        self, var_id_tok: Token, var_type: VarType, value_node: Optional[BaseNode]
    ):
        self.var_id_tok = var_id_tok
        self.value_node = value_node
        self.var_type = var_type

        super(VarNode, self).__init__(
            var_id_tok.pos_start, value_node.pos_end if value_node else None
        )

    def as_struct(self) -> Tuple[str, Token, VarType, Optional[Any]]:
        return ("Variable", self.var_id_tok, self.var_type, self.value_node)

    def __repr__(self):
        if not self.value_node:
            return f"Var[name: {self.var_id_tok.ctx}, type: {self.var_type}]"
        else:
            return f"Var[name: {self.var_id_tok.ctx}, type: {self.var_type}, value: {self.value_node}]"


class StatementsNode(BaseNode):
    statements: List[BaseNode]

    def __init__(
        self, statements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.statements = statements

        super(StatementsNode, self).__init__(pos_start, pos_end)

    def as_struct(self) -> Tuple[str, List[Any]]:
        return ("Statements", [statement.as_struct() for statement in self.statements])

    def __repr__(self):
        return (
            "Statements[\n\t"
            + "\n\t".join(str(statement) for statement in self.statements)
            + "\n]"
        )


class CallNode(BaseNode):
    callable_node: BaseNode
    args: List[BaseNode]

    def __init__(self, callable_node: BaseNode, args: List[BaseNode]):
        self.callable_node = callable_node
        self.args = args

        super(CallNode, self).__init__(
            callable_node.pos_start,
            args[-1].pos_end if len(args) > 0 else callable_node.pos_end,
        )

    def as_struct(self) -> Tuple[str, Any, List[Any]]:
        return (
            "Call",
            self.callable_node.as_struct,
            [arg.as_struct() for arg in self.args],
        )

    def __repr__(self):
        return f"Call[callable: {self.callable_node}, args: [{self.args}]]"


class CharNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CharNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("Char", self.token)

    def __repr__(self):
        return f"Char[value: {self.token.ctx}]"


class StrNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(StrNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("String", self.token)

    def __repr__(self):
        return f"String[value: {self.token.ctx}]"


class VarAccessNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(VarAccessNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("Access", self.token)

    def __repr__(self):
        return f"Access[{self.token}]"


class ListNode(BaseNode):
    elements: List[BaseNode]

    def __init__(
        self, elements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.elements = elements

        super(ListNode, self).__init__(pos_start, pos_end)

    def as_struct(self) -> Tuple[str, List[Any]]:
        return ("ListExpr", [element.as_struct() for element in self.elements])

    def __repr__(self):
        return "ListExpr[" + ", ".join(str(element) for element in self.elements) + "]"


class CommentNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CommentNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("Comment", self.token)

    def __repr__(self):
        return str(self.token)


class BoolNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(BoolNode, self).__init__(token.pos_start, token.pos_end)

    def as_struct(self) -> Tuple[str, Token]:
        return ("Bool", self.token)

    def __repr__(self):
        return str(self.token)


class FuncDefNode(BaseNode):
    func_id_token: Token
    arg_id_tokens: List[Token]
    arg_type_tokens: List[Token]
    ret_type_token: Token
    body: BaseNode
    auto_ret: bool

    def __init__(
        self,
        func_id_token: Token,
        arg_id_tokens: List[Token],
        arg_type_tokens: List[Token],
        ret_type_token: Token,
        body: BaseNode,
        auto_ret: bool = False,
    ):
        self.func_id_token = func_id_token
        self.arg_id_tokens = arg_id_tokens
        self.arg_type_tokens = arg_type_tokens
        self.ret_type_token = ret_type_token
        self.body = body
        self.auto_ret = auto_ret

        super(FuncDefNode, self).__init__(func_id_token.pos_start, body.pos_end)

    def as_struct(
        self,
    ) -> Tuple[str, Token, List[Token], List[Token], Token, Any, bool]:
        return (
            "FuncDefNode",
            self.func_id_token,
            self.arg_id_tokens,
            self.arg_type_tokens,
            self.ret_type_token,
            self.body,
            self.auto_ret,
        )

    def __repr__(self):
        return (
            f"FuncDef[id: {self.func_id_token}\n"
            f"\targ_ids: {self.arg_id_tokens}\n"
            f"\targ_types: {self.arg_type_tokens}\n"
            f"\tret_type: {self.ret_type_token}\n"
            f"\tbody: {self.body}\n"
            f"\tauto_ret: {self.auto_ret}\n"
        )


class ReturnNode(BaseNode):
    ret_node: Optional[BaseNode]

    def __init__(
        self, ret_node: Optional[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.ret_node = ret_node

        super(ReturnNode, self).__init__(pos_start, pos_end)

    def __repr__(self):
        return f"Ret[{self.ret_node}]"
