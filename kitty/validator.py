from typing import Callable, List, Optional

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
    ValueNode,
    VarAccessNode,
    VarNode,
    WhileNode,
)
from kitty.errors import Error, ValidationError
from kitty.solver import NodeValueConverter
from kitty.symbol_table import FuncSymbol, SymbolTable, VarSymbol
from kitty.token import TokenType, VarType


class ContextBlock:
    sym_table: SymbolTable
    node: Optional[BaseNode]
    child_blocks: List["ContextBlock"]

    def __init__(self, parent_sym_table: SymbolTable):
        self.sym_table = SymbolTable(parent_sym_table)
        self.node = None
        self.child_blocks = []


class ValidationResult:
    ctx_block: ContextBlock
    error: Optional[Error]

    def __init__(self, ctx_block: ContextBlock):
        self.ctx_block = ctx_block
        self.error = None

    def register_result(self, result: "ValidationResult") -> Optional[ContextBlock]:
        if result.error:
            self.error = result.error
            return None

        if result.ctx_block:
            self.ctx_block.child_blocks.append(result.ctx_block)

        return result.ctx_block

    def register_success(self, node: BaseNode) -> "ValidationResult":
        self.ctx_block.node = node
        return self

    def register_failure(self, error: Error) -> "ValidationResult":
        if not self.error:
            self.error = error

        return self


class ValidationContext:
    parent_ctx: Optional["ValidationContext"]

    can_return: bool
    return_type: Optional[VarType]
    expr_type: Optional[VarType]

    can_continue: bool
    can_break: bool

    def __init__(
        self,
        parent_ctx: Optional["ValidationContext"] = None,
        can_return: bool = False,
        can_continue: bool = False,
        can_break: bool = False,
        return_type: Optional[VarType] = None,
        expr_type: Optional[VarType] = None,
    ):
        if parent_ctx is not None:
            self.parent_ctx = parent_ctx
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

    def validate(self) -> ValidationResult:
        root_ctx = ValidationContext()
        root_sym_table = SymbolTable()

        if isinstance(self.ast, StatementsNode):
            return self.validate_statements(self.ast, root_ctx, root_sym_table)

        else:
            return ValidationResult(ContextBlock(root_sym_table)).register_failure(
                ValidationError(
                    self.ast.pos_start,
                    self.ast.pos_end,
                    details="root node must be statements!",
                )
            )

    def validate_statements(
        self,
        ast: StatementsNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:

        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        for idx in range(len(ast.statements)):
            stmt = ast.statements[idx]

            if isinstance(stmt, FuncNode):
                ctx_block = res.register_result(
                    self.validate_func_def(
                        stmt, parent_validation_ctx, cur_ctx_block.sym_table
                    )
                )

            elif isinstance(stmt, VarNode):
                if stmt.is_define:
                    ctx_block = res.register_result(
                        self.validate_var_def(
                            stmt, parent_validation_ctx, cur_ctx_block.sym_table
                        )
                    )
                else:
                    ctx_block = res.register_result(
                        self.validate_var_assign(
                            stmt, parent_validation_ctx, cur_ctx_block.sym_table
                        )
                    )

            elif isinstance(stmt, ExprNode):
                ctx_block = res.register_result(
                    self.validate_expr(
                        stmt, parent_validation_ctx, cur_ctx_block.sym_table
                    )
                )

            else:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start, ast.pos_end, details="invalid statement type"
                    )
                )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            ast.statements[idx] = ctx_block.node

        return res.register_success(ast)

    def validate_func_def(
        self,
        ast: FuncNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:

        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        cur_validation_ctx = ValidationContext(
            parent_validation_ctx, can_return=True, return_type=ast.ret_type
        )

        func_name = ast.func_id_token.ctx

        if parent_sym_table.get(func_name, recursive=False):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{func_name}' is already defined",
                )
            )

        for arg in ast.args:
            arg_name = arg.var_id_tok.ctx
            cur_ctx_block.sym_table.set(arg_name, VarSymbol(arg_name, arg.type_, ref_node=arg))  # type: ignore

        if isinstance(ast.body, StatementsNode):
            ctx_block = res.register_result(
                self.validate_statements(
                    ast.body, cur_validation_ctx, cur_ctx_block.sym_table
                )
            )
            if res.error or not ctx_block or ctx_block.node is None:
                return res

        elif isinstance(ast.body, ExprNode):
            cur_validation_ctx.expr_type = ast.ret_type

            ctx_block = res.register_result(
                self.validate_expr(
                    ast.body, cur_validation_ctx, cur_ctx_block.sym_table
                )
            )
            if res.error or not ctx_block or ctx_block.node is None:
                return res

            if ctx_block.node.type_ != cur_validation_ctx.return_type:  # type: ignore
                return res.register_failure(
                    ValidationError(
                        ctx_block.node.pos_start,
                        ctx_block.node.pos_end,
                        details=(
                            "invalid expr type, "
                            "expected type "
                            f"'{cur_validation_ctx.return_type.name}', "  # type: ignore
                            "got "
                            f"'{ctx_block.node.type_.name}'"  # type: ignore
                        ),
                    )
                )

        else:
            return res.register_failure(
                ValidationError(ast.pos_start, ast.pos_end, details="invalid func body")
            )

        ast.body = ctx_block.node

        parent_sym_table.set(
            func_name,
            FuncSymbol(
                name=func_name, args=ast.args, ret_type=ast.ret_type, ref_node=ast
            ),
        )

        return res.register_success(ast)

    def validate_var_def(
        self,
        ast: VarNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:

        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        var_name: str = ast.var_id_tok.ctx

        if not ast.is_define:
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details="only can define a variable in this context",
                )
            )

        if parent_sym_table.get(var_name, recursive=False):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{var_name}' is already defined",
                )
            )

        if ast.value_node is not None:
            cur_ctx = ValidationContext(parent_validation_ctx, expr_type=ast.type_)

            ctx_block = res.register_result(
                self.validate_expr(ast.value_node, cur_ctx, cur_ctx_block.sym_table)
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            if ast.type_ == VarType.UNTYPED:
                ast.type_ = ctx_block.node.type_  # type: ignore

            elif ast.type_ != ctx_block.node.type_:  # type: ignore
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="the declared type of the variable does not match the data type",
                    )
                )

            ast.value_node = ctx_block.node  # type: ignore

        parent_sym_table.set(
            ast.var_id_tok.ctx,
            VarSymbol(name=var_name, type_=ast.type_, ref_node=ast),
        )

        return res.register_success(ast)

    def validate_var_assign(
        self,
        ast: VarNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ):
        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        var_name: str = ast.var_id_tok.ctx

        if not (sym := parent_sym_table.get(var_name)):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{var_name}' is not defined",
                )
            )

        if not isinstance(sym, VarSymbol):
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=f"name '{var_name}' is not a variable",
                )
            )

        cur_ctx = ValidationContext(parent_validation_ctx, expr_type=ast.type_)

        ctx_block = res.register_result(
            self.validate_expr(ast.value_node, cur_ctx, cur_ctx_block.sym_table)  # type: ignore
        )

        if res.error or not ctx_block or ctx_block.node is None:
            return res

        if sym.type_ != ctx_block.node.type_:  # type: ignore
            return res.register_failure(
                ValidationError(
                    ast.pos_start,
                    ast.pos_end,
                    details=(
                        "the declared type "
                        f"'{sym.type_.name}' "
                        "of the variable does not match the data type "
                        f"'{ctx_block.node.type_.name}'"  # type: ignore
                    ),
                )
            )

        ast.value_node = ctx_block.node  # type: ignore

        sym.ref_node = ast

        return res.register_success(ast)

    def validate_expr(
        self,
        ast: ExprNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:
        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        if isinstance(ast, ReturnNode):
            if not parent_validation_ctx.can_return:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'return' outside function",
                    )
                )

            if ast.ret_node is not None:
                ctx_block = res.register_result(
                    self.validate_expr(
                        ast.ret_node, parent_validation_ctx, cur_ctx_block.sym_table
                    )
                )

                if res.error or not ctx_block or ctx_block.node is None:
                    return res

                if ctx_block.node.type_ != parent_validation_ctx.return_type:  # type: ignore
                    return res.register_failure(
                        ValidationError(
                            ast.ret_node.pos_start,
                            ast.ret_node.pos_end,
                            details=(
                                "return type does not match declared type, "
                                "expected "
                                f"'{parent_validation_ctx.return_type.name}' "  # type: ignore
                                "got "
                                f"'{ctx_block.node.type_.name}'"  # type: ignore
                            ),
                        )
                    )

                ast.ret_node = ctx_block.node  # type: ignore
                ast.type_ = ctx_block.node.type_  # type: ignore

        elif isinstance(ast, BreakNode):
            if not parent_validation_ctx.can_break:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'break' outside loop",
                    )
                )

        elif isinstance(ast, ContinueNode):
            if not parent_validation_ctx.can_continue:
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details="'continue' outside loop",
                    )
                )

        elif isinstance(ast, BinOpNode):
            ctx_block = res.register_result(
                self.validate_bin_op(ast, parent_validation_ctx, parent_sym_table)
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            ast = ctx_block.node  # type: ignore

        elif isinstance(ast, UnaryOpNode):
            ctx_block = res.register_result(
                self.validate_unary_op(ast, parent_validation_ctx, parent_sym_table)
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            ast = ctx_block.node  # type: ignore

        elif isinstance(ast, CallNode):
            identifier_name = ast.callable_node.token.ctx

            if not (func_sym := parent_sym_table.get(identifier_name)):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"name '{identifier_name}' is not defined",
                    )
                )

            if not isinstance(func_sym, FuncSymbol):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"'{func_sym.type_}' is not callable",
                    )
                )

            if func_sym.args:
                for idx, (func_arg, call_param) in enumerate(
                    zip(func_sym.args, ast.args)
                ):
                    cur_ctx = ValidationContext(
                        parent_validation_ctx, expr_type=func_arg.type_
                    )

                    ctx_block = res.register_result(
                        self.validate_expr(call_param, cur_ctx, cur_ctx_block.sym_table)
                    )

                    if res.error or not ctx_block or ctx_block.node is None:
                        return res

                    if ctx_block.node.type_ != func_arg.type_:  # type: ignore
                        return res.register_failure(
                            ValidationError(
                                call_param.pos_start,
                                call_param.pos_end,
                                details=f"Expected type '{func_arg.type_}', got type '{ctx_block.node.type_}'",  # type: ignore
                            )
                        )

                    ast.args[idx] = ctx_block.node  # type: ignore

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
            """
            TODO:
            нужно как-то проверять в каком контексте мы находимся.
            Если это контекст функции и производятся операции с ее аргументами,
            то значения для них еще не объявлены и мы проводим валидацию типов,
            если же это контекст присваивания в переменную,
            то мы не должны оставлять переменную без конкретного значения.
            Пример:

            var a: int
            var b = 5 * a # Ошибка. Значение для a не объявлено.

            """

            if not (sym := parent_sym_table.get(ast.token.ctx)):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"name {ast.token.ctx} does not exist",
                    )
                )

            if not isinstance(sym, VarSymbol):
                return res.register_failure(
                    ValidationError(
                        ast.pos_start,
                        ast.pos_end,
                        details=f"name {ast.token.ctx} is not a variable",
                    )
                )

            if isinstance(sym.ref_node.value_node, ValueNode):
                value_node = sym.ref_node.value_node.copy()  # type: ignore
                value_node.pos_start, value_node.pos_end = ast.pos_start, ast.pos_end
                ast = value_node
            else:
                ast = sym.ref_node  # type: ignore

        return res.register_success(ast)

    def validate_bin_op(
        self,
        ast: BinOpNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:

        cur_contex_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_contex_block)

        if isinstance(ast.left_operand, ExprNode):
            ctx_block = res.register_result(
                self.validate_expr(
                    ast.left_operand, parent_validation_ctx, parent_sym_table
                )
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            left_operand = ctx_block.node

        else:
            left_operand = ast.left_operand

        if isinstance(ast.right_operand, ExprNode):
            ctx_block = res.register_result(
                self.validate_expr(
                    ast.right_operand, parent_validation_ctx, parent_sym_table
                )
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            right_operand = ctx_block.node

        else:
            right_operand = ast.right_operand

        left_value = NodeValueConverter.node_to_value(left_operand)  # type: ignore
        right_value = NodeValueConverter.node_to_value(right_operand)  # type: ignore

        solved_value = err = None

        method = getattr(left_value, "op_" + ast.op_token.type_.value, None)
        if method is None:
            raise Exception(f"Operation {ast.op_token.type_} is not supported!")

        else:
            solved_value, err = method(right_value)  # type: ignore

            if err is not None:
                return res.register_failure(err)

        if parent_validation_ctx.expr_type == VarType.BOOL:
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
        self,
        ast: UnaryOpNode,
        parent_validation_ctx: ValidationContext,
        parent_sym_table: SymbolTable,
    ) -> ValidationResult:
        cur_ctx_block = ContextBlock(parent_sym_table)
        res = ValidationResult(cur_ctx_block)

        if isinstance(ast.node, ExprNode):
            ctx_block = res.register_result(
                self.validate_expr(
                    ast.node, parent_validation_ctx, cur_ctx_block.sym_table
                )
            )

            if res.error or not ctx_block or ctx_block.node is None:
                return res

            node = ctx_block.node

        else:
            node = ast.node

        value = NodeValueConverter.node_to_value(node)  # type: ignore

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

    def validate_if_expr(
        self, ast: IfNode, parent_ctx: ValidationContext
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_for_expr(
        self, ast: ForNode, parent_ctx: ValidationContext
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_while_expr(
        self, ast: WhileNode, parent_ctx: ValidationContext
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_list_expr(
        self, ast: ListNode, parent_ctx: ValidationContext
    ) -> ValidationResult:
        raise NotImplementedError

    def validate_var_access(
        self, ast: VarAccessNode, parent_ctx: ValidationContext
    ) -> ValidationResult:
        raise NotImplementedError
