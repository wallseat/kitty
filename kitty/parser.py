from typing import Callable, List, Optional, Tuple

from kitty.ast import (
    BaseNode,
    BinOpNode,
    BoolNode,
    CallNode,
    CharNode,
    CommentNode,
    ListNode,
    NumNode,
    StatementsNode,
    StrNode,
    UnaryOpNode,
    VarAccessNode,
    VarNode,
)
from kitty.errors import Error, InvalidSyntaxError, NotImplementedError
from kitty.symbol_table import SymTable
from kitty.token import Token, TokenType, VarType


class ParseResult:
    advancements: int
    last_registered_advancements: int

    node: Optional[BaseNode]
    error: Optional[Error]

    def __init__(self):
        self.ast = None
        self.error = None
        self.node = None

        self.advancements = 0
        self.last_registered_advancements = 0

    def advance(self):
        self.last_registered_advancements = 1
        self.advancements += 1

    def register_result(self, result: "ParseResult") -> Optional[BaseNode]:
        self.last_registered_advancements = result.advancements
        self.advancements += result.advancements
        if result.error:
            self.error = result.error
            return None

        return result.node

    def try_register_result(self, result: "ParseResult") -> Optional[BaseNode]:
        if result.error:
            self.last_registered_advancements = result.advancements
            return None

        return self.register_result(result)

    def register_success(self, node: BaseNode) -> "ParseResult":
        self.node = node
        return self

    def register_failure(self, error: Error) -> "ParseResult":
        if not self.error or self.last_registered_advancements == 0:
            self.error = error

        return self


class Parser:
    tokens: List[Token]
    symbol_table_stack: List[SymTable]
    cur_tok: Token
    idx: int

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.cur_tok = None  # type: ignore
        self.idx = -1

        self.symbol_table_stack = [SymTable()]

        self.advance()

    def advance(self, result: Optional[ParseResult] = None):
        self.idx += 1
        if result:
            result.advance()

        self.token_peek()

    def retreat(self, count: int):
        self.idx -= count
        self.token_peek()

    def token_peek(self):
        self.cur_tok = (
            self.tokens[self.idx] if 0 <= self.idx < len(self.tokens) else None
        )

    def parse(self) -> ParseResult:
        res = self.parse_statements()
        if not res.error and self.cur_tok.type != TokenType.EOF:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Token cannot appear after previous tokens",
                )
            )
        return res

    def parse_statements(self) -> ParseResult:
        res = ParseResult()
        statements: List[BaseNode] = []
        pos_start = self.cur_tok.pos_start.copy()

        more_statements = True
        while True:
            newline_count = 0
            while self.cur_tok.type in (TokenType.NLINE, TokenType.INLINE_COMMENT):
                self.advance(res)
                if statements:
                    newline_count += 1

            if (
                newline_count == 0
                and statements
                or not more_statements
                or self.cur_tok.type == TokenType.EOF
            ):
                break

            statement = res.register_result(self.parse_statement())
            if res.error:
                return res

            if not statement:
                self.retreat(res.last_registered_advancements)
                more_statements = False
                continue

            statements.append(statement)

        return res.register_success(
            StatementsNode(statements, pos_start, self.cur_tok.pos_end.copy())
        )

    def parse_statement(self) -> ParseResult:
        res = ParseResult()
        pos_start = self.cur_tok.pos_start.copy()

        if self.cur_tok.type == TokenType.RET:
            raise NotImplementedError

        elif self.cur_tok.type == TokenType.BREAK:
            raise NotImplementedError

        elif self.cur_tok.type == TokenType.CONTINUE:
            raise NotImplementedError

        expr = res.register_result(self.parse_expr())
        if res.error or not expr:
            return res

        return res.register_success(expr)

    def parse_expr(self) -> ParseResult:
        res = ParseResult()
        if self.cur_tok.type == TokenType.VAR:  # var assgin
            var_assign = res.register_result(self.parse_var_assign())
            if res.error:
                return res

            return res.register_success(var_assign)  # type: ignore

        else:
            node = res.register_result(
                self.parse_bin_op(self.parse_comp_expr, (TokenType.OR, TokenType.AND))
            )

            if res.error or not node:
                return res

            return res.register_success(node)

    def parse_var_assign(self) -> ParseResult:
        res = ParseResult()

        self.advance(res)

        if self.cur_tok.type != TokenType.IDENTIFIER:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected identity",
                )
            )

        var_id_tok = self.cur_tok
        self.advance(res)

        var_type = VarType.UNTYPED

        if self.cur_tok.type == TokenType.COLON:
            self.advance(res)

            if self.cur_tok.type == TokenType.IDENTIFIER:
                if self.cur_tok.ctx == "int":
                    var_type = VarType.INT
                elif self.cur_tok.ctx == "float":
                    var_type = VarType.FLOAT
                elif self.cur_tok.ctx == "str":
                    var_type = VarType.STR
                elif self.cur_tok.ctx == "char":
                    var_type = VarType.CHAR
                elif self.cur_tok.ctx == "bool":
                    var_type = VarType.BOOL
                else:
                    var_type = VarType.GENERIC

                self.advance(res)

            else:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        details="Expected identity (type-like)",
                    )
                )

        var_expr = None
        if self.cur_tok.type == TokenType.ASSIGN:
            self.advance(res)
            var_expr = res.register_result(self.parse_expr())

        elif var_type == VarType.UNTYPED:
            res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected assign ( = ) or type annotation ( : type )",
                )
            )

        # self.symbol_table_stack[-1].set(var_id_tok.ctx, (var_type, var_expr))

        return res.register_success(VarNode(var_id_tok, var_type, var_expr))  # type: ignore

    def parse_comp_expr(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type == TokenType.NOT:
            op_tok = self.cur_tok
            self.advance(res)

            node = res.register_result(self.parse_comp_expr())
            if res.error or not node:
                return res

            return res.register_success(UnaryOpNode(op_tok, node))

        node = res.register_result(
            self.parse_bin_op(
                self.parse_arith_expr,
                (
                    TokenType.EQ,
                    TokenType.NEQ,
                    TokenType.LT,
                    TokenType.GT,
                    TokenType.LTE,
                    TokenType.GTE,
                ),
            )
        )

        if res.error or not node:
            return res

        return res.register_success(node)

    def parse_arith_expr(self) -> ParseResult:
        return self.parse_bin_op(self.parse_term, (TokenType.ADD, TokenType.SUB))

    def parse_term(self) -> ParseResult:
        return self.parse_bin_op(self.parse_factor, (TokenType.MUL, TokenType.DIV))

    def parse_factor(self) -> ParseResult:
        res = ParseResult()
        tok = self.cur_tok

        if tok.type in (TokenType.ADD, TokenType.SUB):
            self.advance(res)

            factor = res.register_result(self.parse_factor())
            if res.error or not factor:
                return res

            return res.register_success(UnaryOpNode(tok, factor))

        return self.parse_call()

    def parse_call(self) -> ParseResult:
        res = ParseResult()
        atom = res.register_result(self.parse_atom())
        if res.error or not atom:
            return res

        if self.cur_tok.type == TokenType.L_BRC:
            self.advance(res)
            args = []

            if self.cur_tok.type == TokenType.R_BRC:
                self.advance(res)
            else:
                arg = res.register_result(self.parse_expr())
                if res.error or not arg:
                    return res

                args.append(arg)

                while self.cur_tok.type == TokenType.COMMA:
                    self.advance(res)

                    arg = res.register_result(self.parse_expr())
                    if res.error or not arg:
                        return res

                    args.append(arg)

                if self.cur_tok.type != TokenType.R_BRC:
                    return res.register_failure(
                        InvalidSyntaxError(
                            self.cur_tok.pos_start,
                            self.cur_tok.pos_end,
                            f"Expected ',' or ')'",
                        )
                    )

                self.advance(res)
            return res.register_success(CallNode(atom, args))

        return res.register_success(atom)

    def parse_atom(self) -> ParseResult:
        res = ParseResult()
        tok = self.cur_tok

        if tok.type in (TokenType.NUM_INT, TokenType.NUM_FLOAT):
            self.advance(res)

            return res.register_success(NumNode(tok))

        elif tok.type == TokenType.STR:
            self.advance(res)

            return res.register_success(StrNode(tok))

        elif tok.type == TokenType.CHAR:
            self.advance(res)

            return res.register_success(CharNode(tok))

        elif tok.type == TokenType.BOOL:
            self.advance(res)

            return res.register_success(BoolNode(tok))

        elif tok.type == TokenType.IDENTIFIER:
            self.advance(res)

            return res.register_success(VarAccessNode(tok))

        elif tok.type == TokenType.L_BRC:
            self.advance(res)
            expr = res.register_result(self.parse_expr())
            if res.error or not expr:
                return res

            if self.cur_tok.type == TokenType.R_BRC:
                self.advance(res)
                return res.register_success(expr)

            else:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected ')'",
                    )
                )

        elif tok.type == TokenType.L_SQUARE:
            list_expr = res.register_result(self.parse_list_expr())
            if res.error or not list_expr:
                return res

            return res.register_success(list_expr)

        elif tok.type == TokenType.COMMENT:
            self.advance(res)
            return res.register_success(CommentNode(tok))

        elif tok.type == TokenType.IF:
            if_expr = res.register_result(self.parse_if_expr())
            if res.error or not if_expr:
                return res

            return res.register_success(if_expr)

        elif tok.type == TokenType.FOR:
            for_expr = res.register_result(self.parse_for_expr())
            if res.error or not for_expr:
                return res

            return res.register_success(for_expr)

        elif tok.type == TokenType.WHILE:
            while_expr = res.register_result(self.parse_while_expr())
            if res.error or not while_expr:
                return res

            return res.register_success(while_expr)

        elif tok.type == TokenType.FUNC:
            func_def = res.register_result(self.parse_func_def())
            if res.error or not func_def:
                return res

            return res.register_success(func_def)

        return res.register_failure(
            InvalidSyntaxError(
                tok.pos_start,
                tok.pos_end,
                "Expected int, float, str, char, identifier, '+', '-', '(', '[', if', 'for', 'while', 'func'",
            )
        )

    def parse_bin_op(
        self,
        left_operand: Callable[[], ParseResult],
        operations: Tuple[TokenType, ...],
        right_operand: Optional[Callable[[], ParseResult]] = None,
    ) -> ParseResult:

        if right_operand == None:
            right_operand = left_operand

        res = ParseResult()
        left = res.register_result(left_operand())
        if res.error or not left:
            return res

        while self.cur_tok.type in operations:
            op_tok = self.cur_tok
            self.advance(res)

            right = res.register_result(right_operand())  # type: ignore
            if res.error or not right:
                return res

            left = BinOpNode(left, op_tok, right)

        return res.register_success(left)

    def parse_list_expr(self) -> ParseResult:
        res = ParseResult()
        elements: List[BaseNode] = []
        pos_start = self.cur_tok.pos_start.copy()

        if self.cur_tok.type != TokenType.L_SQUARE:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, f"Expected '['"
                )
            )

        self.advance(res)

        if self.cur_tok.type == TokenType.R_SQUARE:
            self.advance(res)
        else:
            while True:
                element = res.register_result(self.parse_expr())
                if res.error or not element:
                    if not elements:
                        return res.register_failure(
                            InvalidSyntaxError(
                                self.cur_tok.pos_start,
                                self.cur_tok.pos_end,
                                "Expected ...",
                            )
                        )
                    else:
                        return res

                elements.append(element)

                if not self.cur_tok.type == TokenType.COMMA:
                    break

                self.advance(res)

            if self.cur_tok.type != TokenType.R_SQUARE:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        f"Expected ',' or ']'",
                    )
                )

            self.advance(res)

        return res.register_success(
            ListNode(elements, pos_start, self.cur_tok.pos_end.copy())
        )

    def parse_if_expr(self) -> ParseResult:
        raise NotImplementedError

    def parse_for_expr(self) -> ParseResult:
        raise NotImplementedError

    def parse_while_expr(self) -> ParseResult:
        raise NotImplementedError

    def parse_func_def(self) -> ParseResult:
        raise NotImplementedError
