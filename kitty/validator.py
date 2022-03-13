from tkinter import E
from typing import Any, Generic, Optional, TypeVar, Union

from kitty.ast import (
    BaseNode,
    BinOpNode,
    BoolNode,
    BreakNode,
    CallNode,
    ContinueNode,
    ExprNode,
    ForNode,
    FuncNode,
    IfNode,
    ListNode,
    ReturnNode,
    StatementsNode,
    UnaryOpNode,
    VarAccessNode,
    VarNode,
    WhileNode,
)
from kitty.errors import Error, ValidationError
from kitty.solver import NodeValueConverter
from kitty.symbol_table import FuncSymbol, SymbolTable, VarSymbol
from kitty.token import TokenType, VarType

_T_AST = TypeVar("_T_AST")
_T_R_AST = TypeVar("_T_R_AST")


class ValidationResult(Generic[_T_AST]):
    ast: Optional[_T_AST]
    error: Optional[Error]

    def __init__(self):
        self.ast = None
        self.error = None

    def register_result(
        self, result: "ValidationResult[_T_R_AST]"
    ) -> Optional[_T_R_AST]:
        if result.error:
            self.error = result.error
            return None

        return result.ast

    def register_success(self, ast: _T_AST) -> "ValidationResult[_T_AST]":
        self.ast = ast
        return self

    def register_failure(self, error: Error) -> "ValidationResult":
        if not self.error:
            self.error = error

        return self


class Context:
    parent_ctx: Optional["Context"]

    sym_table: SymbolTable

    can_return: bool
    return_type: Optional[VarType]
    expr_type: Optional[VarType]

    can_continue: bool
    can_break: bool

    def __init__(
        self,
        parent_ctx: Optional["Context"] = None,
        can_return: bool = False,
        can_continue: bool = False,
        can_break: bool = False,
        return_type: Optional[VarType] = None,
        expr_type: Optional[VarType] = None,
    ):
        if parent_ctx is not None:
            self.parent_ctx = parent_ctx
            self.sym_table = SymbolTable(parent_ctx.sym_table)
            self.can_return = parent_ctx.can_return if not can_return else can_return
            self.return_type = (
                parent_ctx.return_type if not return_type else return_type
            )
            self.can_continue = (
                parent_ctx.can_continue if not can_continue else can_continue
            )
            self.can_break = parent_ctx.can_break if not can_break else can_break

        else:
            self.parent_ctx = None
            self.sym_table = SymbolTable()
            self.can_return = can_return
            self.return_type = return_type
            self.can_continue = can_continue
            self.can_break = can_break

        self.expr_type = expr_type


class Validator:
    ast: BaseNode

    def __init__(self, ast: BaseNode):
        self.sym_table = SymbolTable()
        self.ast = ast

    def validate(self) -> ValidationResult[StatementsNode]:
        res = ValidationResult[StatementsNode]()
        root_ctx = Context()

        if isinstance(self.ast, StatementsNode):
            return self.validate_statements(self.ast, root_ctx)

        else:
            return res.register_failure(
                ValidationError(
                    self.ast.pos_start,
                    self.ast.pos_end,
                    details="root node must be statements!",
                )
            )

    def validate_statements(
        self, ast: StatementsNode, parent_ctx: Context
    ) -> ValidationResult[StatementsNode]:
        res = ValidationResult[StatementsNode]()

        for idx in range(len(ast.statements)):
            stmt = ast.statements[idx]

            node: Union[FuncNode, VarNode, ExprNode, None]
            if isinstance(stmt, FuncNode):
                node = res.register_result(self.validate_func_def(stmt, parent_ctx))

            elif isinstance(stmt, VarNode):
                node = res.register_result(self.validate_var_def(stmt, parent_ctx))

            elif isinstance(stmt, ExprNode):
                node = res.register_result(self.validate_expr(stmt, parent_ctx))

            else:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start, ast.pos_end, details="invalid statement type"
                    )
                )

            if res.error or not node:
                return res

            ast.statements[idx] = node

        return res.register_success(ast)

    def validate_func_def(
        self, ast: FuncNode, parent_ctx: Context
    ) -> ValidationResult[FuncNode]:
        res = ValidationResult[FuncNode]()

        cur_ctx = Context(parent_ctx, can_return=True, return_type=ast.ret_type)

        args = [
            (name.ctx, type_) for (name, type_) in zip(ast.arg_id_tokens, ast.arg_types)
        ]

        func_name = ast.func_id_token.ctx

        if parent_ctx.sym_table.get(func_name, recursive=False):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{func_name}' is already defined",
                )
            )

        parent_ctx.sym_table.set(
            func_name,
            FuncSymbol(name=func_name, args=args, ret_type=ast.ret_type),
        )

        for var in args:
            cur_ctx.sym_table.set(var[0], VarSymbol(*var))

        node: Union[StatementsNode, ExprNode, None]
        if isinstance(ast.body, StatementsNode):
            node = res.register_result(self.validate_statements(ast.body, cur_ctx))
            if res.error or not node:
                return res

        elif isinstance(ast.body, ExprNode):
            cur_ctx.expr_type = ast.ret_type

            node = res.register_result(self.validate_expr(ast.body, cur_ctx))
            if res.error or not node:
                return res

            if node.type_ != cur_ctx.return_type:
                return res.register_failure(
                    ValidationError(
                        node.pos_start, node.pos_end, details="invalid expr type"
                    )
                )

        else:
            return res.register_failure(
                ValidationError(ast.pos_start, ast.pos_end, details="invalid func body")
            )

        ast.body = node

        return res.register_success(ast)

    def validate_var_def(
        self, ast: VarNode, parent_ctx: Context
    ) -> ValidationResult[VarNode]:
        res = ValidationResult[VarNode]()

        name: str = ast.var_id_tok.ctx

        if not ast.is_define:
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details="only can define a variable in this context",
                )
            )

        if parent_ctx.sym_table.get(name, recursive=False):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{name}' is already defined",
                )
            )

        if ast.value_node is not None:
            cur_ctx = Context(parent_ctx, expr_type=ast.var_type)

            node = res.register_result(self.validate_expr(ast.value_node, cur_ctx))

            if res.error or not node:
                return res

            if ast.var_type == VarType.UNTYPED:
                ast.var_type = node.type_

            elif ast.var_type != node.type_:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="the declared type of the variable does not match the data type",
                    )
                )

            ast.value_node = node

        parent_ctx.sym_table.set(ast.var_id_tok.ctx, VarSymbol(name, ast.var_type))
        return res.register_success(ast)

    def validate_expr(
        self, ast: ExprNode, parent_ctx: Context
    ) -> ValidationResult[ExprNode]:
        res = ValidationResult[ExprNode]()

        if isinstance(ast, ReturnNode):
            if not parent_ctx.can_return:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'return' outside function",
                    )
                )

            if ast.ret_node is not None:
                node = res.register_result(self.validate_expr(ast.ret_node, parent_ctx))

                if res.error or not node:
                    return res

                if node.type_ != parent_ctx.return_type:
                    return res.register_failure(
                        ValidationError(
                            ast.ret_node.pos_start,
                            ast.ret_node.pos_end,
                            details="return type does not match declared type",
                        )
                    )

                ast.ret_node = node
                ast.type_ = node.type_

        elif isinstance(ast, BreakNode):
            if not parent_ctx.can_break:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'break' outside loop",
                    )
                )

        elif isinstance(ast, ContinueNode):
            if not parent_ctx.can_break:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'continue' outside loop",
                    )
                )

        elif isinstance(ast, BinOpNode):
            node = res.register_result(self.validate_bin_op(ast, parent_ctx))

            if res.error or not node:
                return res

            ast = node

        elif isinstance(ast, UnaryOpNode):
            node = res.register_result(self.validate_unary_op(ast, parent_ctx))

            if res.error or not node:
                return res

            ast = node

        elif isinstance(ast, CallNode):
            identifier_name = ast.callable_node.token.ctx

            if not (func_sym := parent_ctx.sym_table.get(identifier_name)):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"name '{identifier_name}' is not defined",
                    )
                )

            if isinstance(func_sym, VarSymbol):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"'{func_sym.type_}' is not callable",
                    )
                )

            if func_sym.args:
                for idx, ((_, type_), node) in enumerate(zip(func_sym.args, ast.args)):
                    cur_ctx = Context(parent_ctx, return_type=type_)

                    node = res.register_result(self.validate_expr(node, cur_ctx))

                    if res.error or not node:
                        return res

                    if node.type_ != type_:
                        return res.register_failure(
                            ValidationError(
                                node.pos_start,
                                node.pos_end,
                                details=f"Expected type '{type_}', got type '{node.type_}'",
                            )
                        )

                    ast.args[idx] = node

            ast.type_ = func_sym.ret_type

        elif isinstance(ast, IfNode):
            raise NotImplementedError
            # TODO: if validation

        elif isinstance(ast, ForNode):
            raise NotImplementedError
            # TODO: for validation

        elif isinstance(ast, WhileNode):
            raise NotImplementedError
            # TODO: while validation

        elif isinstance(ast, ListNode):
            raise NotImplementedError
            # TODO: list validation

        elif isinstance(ast, VarAccessNode):
            raise NotImplementedError
            # TODO: var access validation

        return res.register_success(ast)

    def validate_bin_op(
        self, ast: BinOpNode, parent_ctx: Context
    ) -> ValidationResult[ExprNode]:
        res = ValidationResult[ExprNode]()

        if isinstance(ast.left_operand, ExprNode):
            left_operand = res.register_result(
                self.validate_expr(ast.left_operand, parent_ctx)
            )

            if res.error or not left_operand:
                return res

        else:
            left_operand = ast.left_operand

        if isinstance(ast.right_operand, ExprNode):
            right_operand = res.register_result(
                self.validate_expr(ast.right_operand, parent_ctx)
            )

            if res.error or not right_operand:
                return res

        else:
            right_operand = ast.right_operand

        left_value = NodeValueConverter.node_to_value(left_operand)
        right_value = NodeValueConverter.node_to_value(right_operand)

        solved_value = err = None

        if ast.op_token.type_ == TokenType.ADD:
            solved_value, err = left_value.add(right_value)

        elif ast.op_token.type_ == TokenType.SUB:
            solved_value, err = left_value.sub(right_value)

        elif ast.op_token.type_ == TokenType.MUL:
            solved_value, err = left_value.mul(right_value)

        elif ast.op_token.type_ == TokenType.DIV:
            solved_value, err = left_value.div(right_value)

        elif ast.op_token.type_ == TokenType.EQ:
            solved_value, err = left_value.eq(right_value)

        elif ast.op_token.type_ == TokenType.NEQ:
            solved_value, err = left_value.neq(right_value)

        elif ast.op_token.type_ == TokenType.LT:
            solved_value, err = left_value.lt(right_value)

        elif ast.op_token.type_ == TokenType.LTE:
            solved_value, err = left_value.lte(right_value)

        elif ast.op_token.type_ == TokenType.GT:
            solved_value, err = left_value.gt(right_value)

        elif ast.op_token.type_ == TokenType.GTE:
            solved_value, err = left_value.gte(right_value)

        elif ast.op_token.type_ == TokenType.AND:
            solved_value, err = left_value.and_(right_value)

        elif ast.op_token.type_ == TokenType.OR:
            solved_value, err = left_value.or_(right_value)

        if err is not None:
            return res.register_failure(err)

        if parent_ctx.expr_type == VarType.BOOL:
            solved_value, err = solved_value.bool_()  # type: ignore

        if err is not None:
            return res.register_failure(err)

        if solved_value.value is not None:  # type: ignore
            return res.register_success(
                NodeValueConverter.value_to_node(solved_value)  # type: ignore
            )

        else:
            ast.type_ = solved_value.type_  # type: ignore
            return res.register_success(ast)

    def validate_unary_op(
        self, ast: UnaryOpNode, parent_ctx: Context
    ) -> ValidationResult[ExprNode]:
        res = ValidationResult[ExprNode]()

        if isinstance(ast.node, ExprNode):
            node = res.register_result(self.validate_expr(ast.node, parent_ctx))

            if res.error or not node:
                return res

        else:
            node = ast.node

        value = NodeValueConverter.node_to_value(node)

        if ast.op_token.type_ == TokenType.NOT:
            solved_value, err = value.not_()

        elif ast.op_token.type_ == TokenType.SUB:
            solved_value, err = value.minus()

        elif ast.op_token.type_ == TokenType.ADD:
            solved_value, err = value.plus()

        if err:
            return res.register_failure(err)

        if solved_value.value is not None:  # type: ignore
            return res.register_success(NodeValueConverter.value_to_node(solved_value))  # type: ignore

        else:
            ast.type_ = solved_value.type_  # type: ignore
            return res.register_success(ast)

    def validate_if_expr(self, ast: IfNode, parent_ctx: Context) -> ValidationResult:
        raise NotImplementedError

    def validate_for_expr(self, ast: ForNode, parent_ctx: Context) -> ValidationResult:
        raise NotImplementedError

    def validate_while_expr(
        self, ast: WhileNode, parent_ctx: Context
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_list_expr(
        self, ast: ListNode, parent_ctx: Context
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_var_access(
        self, ast: VarAccessNode, parent_ctx: Context
    ) -> ValidationResult:
        raise NotImplementedError
