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

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + "[" + self.__class__.__name__ + "]"


class NumNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(NumNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + f"Numeric[{self.token}]"


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

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "BinOp[\n"
            + (indent * ind_c)
            + f"{indent * 2}left: [\n"
            + f"{indent}{self.left_operand.pretty_repr(ind_c + 2, indent)}\n"
            + (indent * ind_c)
            + f"{indent * 2}]\n"
            + (indent * ind_c)
            + f"{indent * 2}right: [\n"
            + f"{indent}{self.right_operand.pretty_repr(ind_c + 2, indent)}\n"
            + (indent * ind_c)
            + f"{indent * 2}]\n"
            + (indent * ind_c)
            + f"{indent * 2}operation: {self.op_token}\n"
            + (indent * ind_c)
            + f"]"
        )


class UnaryOpNode(BaseNode):
    op_token: Token
    node: BaseNode

    def __init__(self, op_token: Token, node: BaseNode):
        self.op_token = op_token
        self.node = node

        super(UnaryOpNode, self).__init__(self.op_token.pos_start, self.node.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "Unary[\n"
            + (indent * ind_c)
            + f"{indent}operation: {self.op_token}\n"
            + (indent * ind_c)
            + f"{indent}node: [\n"
            + self.node.pretty_repr(ind_c + 2, indent)
            + "\n"
            + (indent * ind_c)
            + f"{indent}]"
        )


class VarNode(BaseNode):
    var_id_tok: Token
    value_node: Optional[BaseNode]
    var_type: VarType
    is_define: bool

    def __init__(
        self,
        var_id_tok: Token,
        var_type: VarType,
        value_node: Optional[BaseNode],
        is_define: bool,
    ):
        self.var_id_tok = var_id_tok
        self.value_node = value_node
        self.var_type = var_type
        self.is_define = is_define

        super(VarNode, self).__init__(
            var_id_tok.pos_start, value_node.pos_end if value_node else None
        )

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + f"Var[\n"
            + (indent * ind_c)
            + f"{indent}id: {self.var_id_tok},\n"
            + (indent * ind_c)
            + f"{indent}type: {self.var_type},\n"
            + (
                (indent * ind_c)
                + f"{indent}value: [\n"
                + f"{self.value_node.pretty_repr(ind_c + 2, indent)}\n"
                + (indent * ind_c)
                + f"{indent}],\n"
                if self.value_node
                else ""
            )
            + (indent * ind_c)
            + f"{indent}is_define: {self.is_define}\n"
            + (indent * ind_c)
            + "]"
        )


class StatementsNode(BaseNode):
    statements: List[BaseNode]

    def __init__(
        self, statements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.statements = statements

        super(StatementsNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "Statements[\n"
            + (",\n").join(
                statement.pretty_repr(ind_c + 1, indent)
                for statement in self.statements
            )
            + "\n"
            + (indent * ind_c)
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

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "Call[\n"
            + (indent * ind_c)
            + f"{indent}callable: [\n{self.callable_node.pretty_repr(ind_c + 2, indent)}\n"
            + (indent * ind_c)
            + f"{indent}]\n"
            + (
                (indent * ind_c)
                + f"{indent}args: [\n"
                + ",\n".join(
                    [node.pretty_repr(ind_c + 2, indent) for node in self.args]
                )
                + "\n"
                + (indent * ind_c)
                + f"{indent}]\n"
                if self.args
                else ""
            )
            + (indent * ind_c)
            + "]"
        )


class CharNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CharNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + f"Char[{self.token}]"


class StrNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(StrNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + f"Char[{self.token}]"


class VarAccessNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(VarAccessNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + f"Access[{self.token}]"


class ListNode(BaseNode):
    elements: List[BaseNode]

    def __init__(
        self, elements: List[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.elements = elements

        super(ListNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "ListExpr[\n"
            + ",\n".join(
                [node.pretty_repr(ind_c + 1, indent) for node in self.elements]
            )
            + "\n"
            + (indent * ind_c)
            + "]"
        )


class CommentNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(CommentNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "Comment["
            + (
                self.token.ctx[:30].replace("\n", "\\n").replace("    ", "\\t")
                + f"[{len(self.token.ctx) - 30} chars]"
                if len(self.token.ctx) > 30
                else self.token.ctx
            )
            + "]"
        )


class BoolNode(BaseNode):
    token: Token

    def __init__(self, token: Token):
        self.token = token

        super(BoolNode, self).__init__(token.pos_start, token.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + f"Bool[{self.token.ctx}]"


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

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "FuncDef: [\n"
            + (indent * ind_c)
            + f"{indent}id: {self.func_id_token}\n"
            + (indent * ind_c)
            + (
                (
                    f"{indent}arg_ids: ["
                    + (f"\n{indent * 2}" + (indent * ind_c))
                    + (f",\n{indent * 2}" + (indent * ind_c)).join(
                        [str(token) for token in self.arg_id_tokens]
                    )
                    + "\n"
                    + (indent * ind_c)
                    + f"{indent}]\n"
                )
                + (indent * ind_c)
                + (
                    f"{indent}arg_types: ["
                    + (f"\n{indent * 2}" + (indent * ind_c))
                    + (f",\n{indent * 2}" + (indent * ind_c)).join(
                        [str(token) for token in self.arg_type_tokens]
                    )
                    + "\n"
                    + (indent * ind_c)
                    + f"{indent}]\n"
                )
                + (indent * ind_c)
                if self.arg_id_tokens
                else ""
            )
            + f"{indent}ret_type: {self.ret_type_token}\n"
            + (indent * ind_c)
            + (
                f"{indent}body: [\n"
                + self.body.pretty_repr(ind_c + 2, indent)
                + "\n"
                + (indent * ind_c)
                + f"{indent}]\n"
                if self.body
                else ""
            )
            + (indent * ind_c)
            + f"{indent}auto_ret: {self.auto_ret}\n"
            + (indent * ind_c)
            + "]"
        )


class ReturnNode(BaseNode):
    ret_node: Optional[BaseNode]

    def __init__(
        self, ret_node: Optional[BaseNode], pos_start: Position, pos_end: Position
    ):
        self.ret_node = ret_node

        super(ReturnNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "Ret: "
            + (
                "[\n"
                + f"{self.ret_node.pretty_repr(ind_c + 1, indent)}\n"
                + (indent * ind_c)
                + "]"
                if self.ret_node
                else "noret"
            )
        )


class BreakNode(BaseNode):
    def __init__(self, pos_start: Position, pos_end: Position):
        super(BreakNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + "Break"


class ContinueNode(BaseNode):
    def __init__(self, pos_start: Position, pos_end: Position):
        super(ContinueNode, self).__init__(pos_start, pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (indent * ind_c) + "Continue"


class WhileNode(BaseNode):
    condition: BaseNode
    body: Optional[BaseNode]

    def __init__(self, condition: BaseNode, body: Optional[BaseNode]):
        self.condition = condition
        self.body = body

        super(WhileNode, self).__init__(
            condition.pos_start, body.pos_end if body else condition.pos_end
        )

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "WhileExpr[\n"
            + (indent * ind_c)
            + f"{indent}condition: [\n"
            + self.condition.pretty_repr(ind_c + 2, indent)
            + "\n"
            + (indent * ind_c)
            + f"{indent}]\n"
            + (
                (indent * ind_c)
                + f"{indent}body: [\n"
                + self.body.pretty_repr(ind_c + 2, indent)
                + "\n"
                + (indent * ind_c)
                + f"{indent}]\n"
                if self.body
                else "\n"
            )
            + (indent * ind_c)
            + "]"
        )


class ForNode(BaseNode):
    var_node: BaseNode
    iter_expr: BaseNode
    body: BaseNode

    def __init__(self, var_node: BaseNode, iter_expr: BaseNode, body: BaseNode):
        self.var_node = var_node
        self.iter_expr = iter_expr
        self.body = body

        super(ForNode, self).__init__(var_node.pos_start, body.pos_end)

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "ForExpr[\n"
            + (indent * ind_c)
            + f"{indent}var: [\n"
            + self.var_node.pretty_repr(ind_c + 2, indent)
            + "\n"
            + (indent * ind_c)
            + f"{indent}]\n"
            + (indent * ind_c)
            + f"{indent}iter_expr: [\n"
            + self.iter_expr.pretty_repr(ind_c + 2, indent)
            + "\n"
            + (indent * ind_c)
            + f"{indent}]\n"
            + (indent * ind_c)
            + f"{indent}body: [\n"
            + self.body.pretty_repr(ind_c + 2, indent)
            + "\n"
            + (indent * ind_c)
            + f"{indent}]\n"
            + (indent * ind_c)
            + "]"
        )


class IfNode(BaseNode):
    cases: List[Tuple[BaseNode, Optional[BaseNode]]]  # (condition, expr|statements)
    else_case: Optional[BaseNode]

    def __init__(
        self,
        cases: List[Tuple[BaseNode, Optional[BaseNode]]],
        else_case: Optional[BaseNode] = None,
    ):
        self.cases = cases
        self.else_case = else_case

        super(IfNode, self).__init__(
            cases[0][0].pos_start,
            else_case.pos_end
            if else_case
            else cases[-1][1].pos_end
            if cases[-1][1]
            else cases[-1][0].pos_end,
        )

    def pretty_repr(self, ind_c: int = 0, indent: str = "  ") -> str:
        return (
            (indent * ind_c)
            + "IfExpr[\n"
            + (indent * ind_c)
            + f"{indent}cases: [\n"
            + ",\n".join(
                [
                    (indent * ind_c)
                    + f"{indent * 2}case {idx}: [\n"
                    + (indent * ind_c)
                    + f"{indent * 3}condition: [\n"
                    + condition.pretty_repr(ind_c + 4, indent)
                    + "\n"
                    + (indent * ind_c)
                    + f"{indent * 3}],\n"
                    + (indent * ind_c)
                    + (
                        f"{indent * 3}body: [\n"
                        + body.pretty_repr(ind_c + 4, indent)
                        + "\n"
                        + (indent * ind_c)
                        + f"{indent * 3}]"
                        + "\n"
                        if body
                        else ""
                    )
                    + (indent * ind_c)
                    + f"{indent * 2}]"
                    for idx, (condition, body) in enumerate(self.cases, start=1)
                ]
            )
            + "\n"
            + (indent * ind_c)
            + f"{indent}]"
            + (
                ",\n"
                + (indent * ind_c)
                + f"{indent}else_case: [\n"
                + self.else_case.pretty_repr(ind_c + 2, indent)
                + "\n"
                + (indent * ind_c)
                + f"{indent}]\n"
                if self.else_case
                else "\n"
            )
            + (indent * ind_c)
            + f"]"
        )
