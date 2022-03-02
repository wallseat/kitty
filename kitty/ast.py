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

    def pretty_repr(self, indent: int = 0) -> str:
        return ("\t" * indent) + "[" + self.__class__.__name__ + "]"


class NumNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(NumNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, indent: int = 0) -> str:
        return ("\t" * indent) + f"Numeric[{self.token}]"


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

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + "BinOp[\n"
            + ("\t" * indent)
            + f"\tleft: [\n"
            + f"\t{self.left_operand.pretty_repr(indent + 1)}\n"
            + ("\t" * indent)
            + "\t]\n"
            + ("\t" * indent)
            + f"\tright: [\n"
            + f"\t{self.right_operand.pretty_repr(indent + 1)}\n"
            + ("\t" * indent)
            + "\t]\n"
            + ("\t" * indent)
            + f"\toperation: {self.op_token}\n"
            + ("\t" * indent)
            + "]"
        )


class UnaryOpNode(BaseNode):
    op_token: Token
    node: BaseNode

    def __init__(self, op_token: Token, node: BaseNode):
        self.op_token = op_token
        self.node = node

        super(UnaryOpNode, self).__init__(self.op_token.pos_start, self.node.pos_end)


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

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + f"Var[\n"
            + ("\t" * indent)
            + f"\tid: {self.var_id_tok}\n"
            + ("\t" * indent)
            + f"\ttype: {self.var_type}\n"
            + ("\t" * indent)
            + (
                f"\tvalue: [\n"
                + f"{self.value_node.pretty_repr(indent + 2)}\n"
                + ("\t" * indent)
                + "\t]\n"
                if self.value_node
                else "\n"
            )
            + ("\t" * indent)
            + "]"
        )


class StatementsNode(BaseNode):
    statements: List[BaseNode]

    def __init__(
        self, statements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.statements = statements

        super(StatementsNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + "Statements[\n"
            + (",\n").join(
                statement.pretty_repr(indent + 1) for statement in self.statements
            )
            + "\n"
            + ("\t" * indent)
            + "]"
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

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + "Call[\n"
            + ("\t" * indent)
            + f"\tcallable: [\n{self.callable_node.pretty_repr(indent + 2)}\n"
            + ("\t" * indent)
            + "\t]\n"
            + (
                ("\t" * indent)
                + "\targs: [\n"
                + ",\n".join([node.pretty_repr(indent + 2) for node in self.args])
                + "\n"
                + ("\t" * indent)
                + "\t]\n"
                if self.args
                else ""
            )
            + ("\t" * indent)
            + "]"
        )


class CharNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CharNode, self).__init__(token.pos_start, token.pos_end)


class StrNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(StrNode, self).__init__(token.pos_start, token.pos_end)


class VarAccessNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(VarAccessNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, indent: int = 0) -> str:
        return ("\t" * indent) + f"Access[{self.token}]"


class ListNode(BaseNode):
    elements: List[BaseNode]

    def __init__(
        self, elements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.elements = elements

        super(ListNode, self).__init__(pos_start, pos_end)


class CommentNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CommentNode, self).__init__(token.pos_start, token.pos_end)


class BoolNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(BoolNode, self).__init__(token.pos_start, token.pos_end)


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

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + "FuncDef[\n"
            + ("\t" * indent)
            + f"\tid: {self.func_id_token}\n"
            + ("\t" * indent)
            + (
                (
                    f"\targ_ids: ["
                    + ("\n\t\t" + ("\t" * indent))
                    + ("\n\t\t" + ("\t" * indent)).join(
                        [str(token) for token in self.arg_id_tokens]
                    )
                    + "\n"
                    + ("\t" * indent)
                    + "\t]\n"
                )
                + ("\t" * indent)
                + (
                    f"\targ_types: ["
                    + ("\n\t\t" + ("\t" * indent))
                    + ("\n\t\t" + ("\t" * indent)).join(
                        [str(token) for token in self.arg_type_tokens]
                    )
                    + "\n"
                    + ("\t" * indent)
                    + "\t]\n"
                )
                + ("\t" * indent)
                if self.arg_id_tokens
                else ""
            )
            + f"\tret_type: {self.ret_type_token}\n"
            + ("\t" * indent)
            + (
                f"\tbody: [\n"
                + self.body.pretty_repr(indent + 2)
                + "\n"
                + ("\t" * indent)
                + "\t]\n"
                if self.body
                else ""
            )
            + ("\t" * indent)
            + f"\tauto_ret: {self.auto_ret}\n"
            + ("\t" * indent)
            + "]"
        )


class ReturnNode(BaseNode):
    ret_node: Optional[BaseNode]

    def __init__(
        self, ret_node: Optional[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.ret_node = ret_node

        super(ReturnNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, indent: int = 0) -> str:
        return (
            ("\t" * indent)
            + "Ret: "
            + (
                "[\n"
                + f"{self.ret_node.pretty_repr(indent + 1)}\n"
                + ("\t" * indent)
                + "]"
                if self.ret_node
                else "noret"
            )
        )
