import logging
from typing import Callable, List, Optional, Tuple, Union

from kitty.ast import (
    BaseNode,
    BinOpNode,
    BoolNode,
    BreakNode,
    CallNode,
    CharNode,
    CommentNode,
    ContinueNode,
    ForNode,
    FuncNode,
    IfNode,
    ListNode,
    NumericNode,
    ReturnNode,
    StatementsNode,
    StrNode,
    UnaryOpNode,
    VarAccessNode,
    VarNode,
    WhileNode,
)
from kitty.errors import Error, InvalidSyntaxError
from kitty.token import Token, TokenType, VarType, identifier_to_var_type


class ParseResult:
    advancements: int
    last_registered_advancements: int

    node: Optional[BaseNode]
    error: Optional[Error]

    def __init__(self):
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

    def register_success(self, node: BaseNode) -> "ParseResult":
        self.node = node
        return self

    def register_failure(self, error: Error) -> "ParseResult":
        if not self.error or self.last_registered_advancements == 0:
            self.error = error

        return self


class Parser:
    tokens: List[Token]
    cur_tok: Token
    idx: int

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.cur_tok = None  # type: ignore
        self.idx = -1

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
        logging.debug("Start parsing")

        res = self.parse_statements()
        if not res.error and self.cur_tok.type_ != TokenType.EOF:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Token cannot appear after previous tokens",
                )
            )

        logging.debug("Parsed statements in parse")

        return res

    def parse_statements(self) -> ParseResult:
        logging.debug("Parse statements")

        res = ParseResult()
        statements: List[BaseNode] = []
        pos_start = self.cur_tok.pos_start.copy()

        more_statements = True
        while True:
            newline_count = 0
            while self.cur_tok.type_ in (TokenType.NLINE, TokenType.INLINE_COMMENT):
                self.advance(res)
                if statements:
                    newline_count += 1

            if (
                newline_count == 0
                and statements
                or not more_statements
                or self.cur_tok.type_ == TokenType.EOF
            ):
                break

            statement = res.register_result(self.parse_statement())
            if res.error:
                return res

            if not statement:
                self.retreat(res.last_registered_advancements)
                more_statements = False
                continue

            logging.debug(f"Parsed statement")

            statements.append(statement)

        return res.register_success(
            StatementsNode(statements, pos_start, self.cur_tok.pos_end.copy())
        )

    def parse_statement(self) -> ParseResult:
        logging.debug("Parse statement")

        res = ParseResult()

        if self.cur_tok.type_ == TokenType.FUNC:  # func define
            func_def = res.register_result(self.parse_func_def())
            if res.error or not func_def:
                return res

            logging.debug(f"Parsed func def, id: {func_def.func_id_token}")  # type: ignore

            return res.register_success(func_def)

        elif self.cur_tok.type_ == TokenType.VAR:  # var define
            var_assign = res.register_result(self.parse_var_def())
            if res.error:
                return res

            return res.register_success(var_assign)  # type: ignore

        expr = res.register_result(self.parse_expr())
        if res.error or not expr:
            return res

        return res.register_success(expr)

    def parse_expr(self) -> ParseResult:
        logging.debug("Parse expr")

        res = ParseResult()
        pos_start = self.cur_tok.pos_start.copy()

        if self.cur_tok.type_ == TokenType.RET:
            self.advance(res)

            expr = None
            if self.cur_tok.type_ != TokenType.NLINE:
                expr = res.register_result(self.parse_expr())
                if res.error:
                    return res

                if not expr:
                    self.retreat(res.last_registered_advancements)

            return res.register_success(
                ReturnNode(expr, pos_start, self.cur_tok.pos_end.copy())  # type: ignore
            )

        elif self.cur_tok.type_ == TokenType.BREAK:
            self.advance(res)
            return res.register_success(
                BreakNode(pos_start, self.cur_tok.pos_end.copy())
            )

        elif self.cur_tok.type_ == TokenType.CONTINUE:
            self.advance(res)
            return res.register_success(
                ContinueNode(pos_start, self.cur_tok.pos_end.copy())
            )

        node = res.register_result(
            self.parse_bin_op(self.parse_comp_expr, (TokenType.OR, TokenType.AND))
        )

        if res.error or not node:
            return res

        logging.debug(f"Parsed bin op node in expr")

        return res.register_success(node)

    def parse_var_def(self) -> ParseResult:
        logging.debug("Parse var assignment")

        res = ParseResult()

        if self.cur_tok.type_ != TokenType.VAR:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected var",
                )
            )

        self.advance(res)

        if self.cur_tok.type_ != TokenType.IDENTIFIER:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected identifier",
                )
            )

        var_id_tok = self.cur_tok
        self.advance(res)

        var_type = VarType.UNTYPED

        if self.cur_tok.type_ == TokenType.COLON:
            self.advance(res)

            if self.cur_tok.type_ == TokenType.IDENTIFIER:
                var_type = identifier_to_var_type(self.cur_tok)

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
        if self.cur_tok.type_ == TokenType.ASSIGN:
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

        return res.register_success(VarNode(var_id_tok, var_type, var_expr, True))  # type: ignore

    def parse_comp_expr(self) -> ParseResult:
        logging.debug("Parse comp expr")

        res = ParseResult()

        if self.cur_tok.type_ == TokenType.NOT:
            op_tok = self.cur_tok
            self.advance(res)

            node = res.register_result(self.parse_comp_expr())
            if res.error or not node:
                return res

            logging.debug(f"Parsed unary op node in comp expr")

            return res.register_success(UnaryOpNode(op_tok, node))  # type: ignore

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

        logging.debug(f"Parsed bin op in comp expr")

        return res.register_success(node)

    def parse_arith_expr(self) -> ParseResult:
        logging.debug("Parse arith expr")

        arith_expr = self.parse_bin_op(self.parse_term, (TokenType.ADD, TokenType.SUB))

        logging.debug(f"Parsed bin op node in arith expr")

        return arith_expr

    def parse_term(self) -> ParseResult:
        logging.debug("Parse term")

        term = self.parse_bin_op(self.parse_factor, (TokenType.MUL, TokenType.DIV))

        logging.debug(f"Parsed bin op node in term")

        return term

    def parse_factor(self) -> ParseResult:
        logging.debug("Parse factor")

        res = ParseResult()
        tok = self.cur_tok

        if tok.type_ in (TokenType.ADD, TokenType.SUB):
            self.advance(res)

            factor = res.register_result(self.parse_factor())
            if res.error or not factor:
                return res

            unary_op_node = UnaryOpNode(tok, factor)  # type: ignore

            logging.debug(f"Parsed unary op node in factor")

            return res.register_success(unary_op_node)

        call_node = res.register_result(self.parse_call())

        if res.error or not call_node:
            return res

        return res.register_success(call_node)

    def parse_call(self) -> ParseResult:
        logging.debug("Parse call")

        res = ParseResult()
        atom = res.register_result(self.parse_atom())
        if res.error or not atom:
            return res

        if self.cur_tok.type_ == TokenType.L_BRC:
            if not isinstance(atom, VarAccessNode):
                return res.register_failure(
                    InvalidSyntaxError(
                        atom.pos_start,
                        atom.pos_end,
                        details="Attempt to call non-identifier",
                    )
                )

            self.advance(res)
            args = []

            if self.cur_tok.type_ == TokenType.R_BRC:
                self.advance(res)
            else:
                arg = res.register_result(self.parse_expr())
                if res.error or not arg:
                    return res

                args.append(arg)

                while self.cur_tok.type_ == TokenType.COMMA:
                    self.advance(res)

                    arg = res.register_result(self.parse_expr())
                    if res.error or not arg:
                        return res

                    args.append(arg)

                if self.cur_tok.type_ != TokenType.R_BRC:
                    return res.register_failure(
                        InvalidSyntaxError(
                            self.cur_tok.pos_start,
                            self.cur_tok.pos_end,
                            f"Expected ',' or ')'",
                        )
                    )

                self.advance(res)

            logging.debug(f"Parsed call")

            return res.register_success(CallNode(atom, args))  # type: ignore

        logging.debug(f"Parsed atom")

        return res.register_success(atom)

    def parse_atom(self) -> ParseResult:
        logging.debug("Parse atom")

        res = ParseResult()
        tok = self.cur_tok

        if tok.type_ in (TokenType.NUM_INT, TokenType.NUM_FLOAT):
            self.advance(res)

            logging.debug(f"Parsed numeric")

            return res.register_success(NumericNode(tok))

        elif tok.type_ == TokenType.STR:
            self.advance(res)

            logging.debug(f"Parsed str")

            return res.register_success(StrNode(tok))

        elif tok.type_ == TokenType.CHAR:
            self.advance(res)

            logging.debug(f"Parsed char")

            return res.register_success(CharNode(tok))

        elif tok.type_ == TokenType.BOOL:
            self.advance(res)

            logging.debug(f"Parsed bool")

            return res.register_success(BoolNode(tok))

        elif tok.type_ == TokenType.IDENTIFIER:
            node = res.register_result(self.parse_var_access())
            if res.error or not node:
                return res

            logging.debug("Parsed var access/assign")

            return res.register_success(node)

        elif tok.type_ == TokenType.L_BRC:
            self.advance(res)
            expr = res.register_result(self.parse_expr())
            if res.error or not expr:
                return res

            if self.cur_tok.type_ == TokenType.R_BRC:
                self.advance(res)

                logging.debug(f"Parsed expr in atom")

                return res.register_success(expr)

            else:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected ')'",
                    )
                )

        elif tok.type_ == TokenType.L_SQUARE:
            list_expr = res.register_result(self.parse_list_expr())
            if res.error or not list_expr:
                return res

            logging.debug(f"Parsed list expr")

            return res.register_success(list_expr)

        elif tok.type_ == TokenType.COMMENT:
            self.advance(res)

            logging.debug(f"Parsed comment")

            return res.register_success(CommentNode(tok))

        elif tok.type_ == TokenType.IF:
            if_expr = res.register_result(self.parse_if_expr_a())
            if res.error or not if_expr:
                return res

            logging.debug(f"Parsed if expr")

            return res.register_success(if_expr)

        elif tok.type_ == TokenType.FOR:
            for_expr = res.register_result(self.parse_for_expr())
            if res.error or not for_expr:
                return res

            logging.debug(f"Parsed for expr")

            return res.register_success(for_expr)

        elif tok.type_ == TokenType.WHILE:
            while_expr = res.register_result(self.parse_while_expr())
            if res.error or not while_expr:
                return res

            logging.debug(f"Parsed while expr")

            return res.register_success(while_expr)

        elif tok.type_ == TokenType.E_BLOCK:
            return res

        return res.register_failure(
            InvalidSyntaxError(
                tok.pos_start.copy(),
                tok.pos_end.copy(),
                details="Expected expr (numeric, str, char, identifier, if, for, while or list expr)",
            )
        )

    def parse_bin_op(
        self,
        left_operand: Callable[[], ParseResult],
        operations: Tuple[TokenType, ...],
        right_operand: Optional[Callable[[], ParseResult]] = None,
    ) -> ParseResult:
        logging.debug("Parse bin op")

        if right_operand is None:
            right_operand = left_operand

        res = ParseResult()
        left = res.register_result(left_operand())
        if res.error or not left:
            return res

        while self.cur_tok.type_ in operations:
            op_tok = self.cur_tok
            self.advance(res)

            right = res.register_result(right_operand())  # type: ignore
            if res.error or not right:
                return res

            left = BinOpNode(left, op_tok, right)  # type: ignore

        return res.register_success(left)

    def parse_var_access(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.IDENTIFIER:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected identifier",
                )
            )

        var_id_tok = self.cur_tok

        self.advance(res)

        if self.cur_tok.type_ == TokenType.ASSIGN:
            assign_node = res.register_result(self.parse_var_assign(var_id_tok))

            if res.error or not assign_node:
                return res

            logging.debug("Parsed var assign")

            return res.register_success(assign_node)

        logging.debug("Parsed var access")
        return res.register_success(VarAccessNode(var_id_tok))

    def parse_var_assign(self, var_id_tok: Token) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.ASSIGN:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected '='",
                )
            )

        self.advance(res)

        expr = res.register_result(self.parse_expr())

        if res.error or not expr:
            return res

        return res.register_success(VarNode(var_id_tok, VarType.UNTYPED, expr, False))  # type: ignore

    def parse_list_expr(self) -> ParseResult:
        logging.debug("Parse list expr")

        res = ParseResult()
        elements: List[BaseNode] = []
        pos_start = self.cur_tok.pos_start.copy()

        if self.cur_tok.type_ != TokenType.L_SQUARE:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, f"Expected '['"
                )
            )

        self.advance(res)

        if self.cur_tok.type_ == TokenType.R_SQUARE:
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
                                "Expected elements (int, float, bool, str, char or expr)",
                            )
                        )
                    else:
                        return res

                elements.append(element)

                if not self.cur_tok.type_ == TokenType.COMMA:
                    break

                self.advance(res)

            if self.cur_tok.type_ != TokenType.R_SQUARE:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        f"Expected ',' or ']'",
                    )
                )

            self.advance(res)

        return res.register_success(
            ListNode(elements, pos_start, self.cur_tok.pos_end.copy())  # type: ignore
        )

    def parse_if_expr_a(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.IF:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'if'",
                )
            )

        self.advance(res)

        cases: List[Tuple[BaseNode, Optional[BaseNode]]] = []
        else_case: Optional[BaseNode] = None

        condition = res.register_result(self.parse_expr())

        if res.error or not condition:
            return res

        if self.cur_tok.type_ == TokenType.R_ARROW:  # inline like syntax
            self.advance(res)

            expr = res.register_result(self.parse_expr())

            if res.error or not expr:
                return res

            cases.append((condition, expr))

            while self.cur_tok.type_ == TokenType.ELIF:
                case_or_res = self.parse_if_expr_d()

                if isinstance(case_or_res, ParseResult):  # an error
                    res.register_result(case_or_res)
                    return res

                cases.append(case_or_res)

            if self.cur_tok.type_ == TokenType.ELSE:
                else_case = res.register_result(self.parse_if_expr_e())

                if res.error or not else_case:
                    return res

        elif self.cur_tok.type_ == TokenType.S_BLOCK:  # base syntax
            self.advance(res)

            statements = res.register_result(self.parse_statements())

            if res.error or not statements:
                return res

            cases.append((condition, statements))

            if self.cur_tok.type_ != TokenType.E_BLOCK:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        details="Expected '}'",
                    )
                )

            self.advance(res)

            while self.cur_tok.type_ == TokenType.ELIF:
                case_or_res = self.parse_if_expr_b()

                if isinstance(case_or_res, ParseResult):  # an error
                    res.register_result(case_or_res)
                    return res

                cases.append(case_or_res)

            if self.cur_tok.type_ == TokenType.ELSE:
                else_case = res.register_result(self.parse_if_expr_c())

                if res.error or not else_case:
                    return res

        return res.register_success(IfNode(cases, else_case))

    def parse_if_expr_b(self) -> Union[ParseResult, Tuple[BaseNode, BaseNode]]:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.ELIF:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'elif'",
                )
            )

        self.advance(res)

        condition = res.register_result(self.parse_expr())

        if res.error or not condition:
            return res

        if self.cur_tok.type_ != TokenType.S_BLOCK:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, details="Expected '{'"
                )
            )

        self.advance(res)

        statements = res.register_result(self.parse_statements())

        if res.error or not statements:
            return res

        if self.cur_tok.type_ != TokenType.E_BLOCK:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, details="Expected '}'"
                )
            )

        self.advance(res)

        return condition, statements

    def parse_if_expr_c(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.ELSE:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'else'",
                )
            )

        self.advance(res)

        if self.cur_tok.type_ != TokenType.S_BLOCK:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, details="Expected '{'"
                )
            )

        self.advance(res)

        statements = res.register_result(self.parse_statements())

        if res.error or not statements:
            return res

        if self.cur_tok.type_ != TokenType.E_BLOCK:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start, self.cur_tok.pos_end, details="Expected '}'"
                )
            )

        self.advance(res)

        return res.register_success(statements)

    def parse_if_expr_d(self) -> Union[ParseResult, Tuple[BaseNode, BaseNode]]:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.ELIF:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'elif'",
                )
            )

        self.advance(res)

        condition = res.register_result(self.parse_expr())

        if res.error or not condition:
            return res

        if self.cur_tok.type_ != TokenType.R_ARROW:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected '->'",
                )
            )

        self.advance(res)

        expr = res.register_result(self.parse_expr())

        if res.error or not expr:
            return res

        return (condition, expr)

    def parse_if_expr_e(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.ELSE:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'else'",
                )
            )

        self.advance(res)

        if self.cur_tok.type_ != TokenType.R_ARROW:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected '->'",
                )
            )

        self.advance(res)

        expr = res.register_result(self.parse_expr())

        if res.error or not expr:
            return res

        return res.register_success(expr)

    def parse_for_expr(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.FOR:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'for'",
                )
            )

        self.advance(res)

        with_brc = False

        if self.cur_tok.type_ == TokenType.L_BRC:
            self.advance(res)
            with_brc = True

        var_node = res.register_result(self.parse_var_def())

        if res.error or not var_node:
            return res

        if self.cur_tok.type_ != TokenType.IN:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'in'",
                )
            )

        self.advance(res)

        iter_expr = res.register_result(self.parse_expr())

        if res.error or not iter_expr:
            return res

        if with_brc:
            if self.cur_tok.type_ != TokenType.R_BRC:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        details="Expected ')'",
                    )
                )

            self.advance(res)

        if self.cur_tok.type_ == TokenType.R_ARROW:
            self.advance(res)

            body = res.register_result(self.parse_expr())

            if res.error or not body:
                return res

        elif self.cur_tok.type_ == TokenType.S_BLOCK:
            self.advance(res)

            body = res.register_result(self.parse_statements())

            if res.error or not body:
                return res

            if self.cur_tok.type_ != TokenType.E_BLOCK:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        details="Expected '}'",
                    )
                )

            self.advance(res)

        else:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected '->' or '{'",
                )
            )

        return res.register_success(ForNode(var_node, iter_expr, body))

    def parse_while_expr(self) -> ParseResult:
        res = ParseResult()

        if self.cur_tok.type_ != TokenType.WHILE:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected 'while'",
                )
            )

        self.advance(res)

        condition = res.register_result(self.parse_expr())
        body = None

        if res.error or not condition:
            return res

        if self.cur_tok.type_ == TokenType.R_ARROW:
            self.advance(res)

            body = res.register_result(self.parse_expr())

            if res.error or not body:
                return res

        elif self.cur_tok.type_ == TokenType.S_BLOCK:
            self.advance(res)

            body = res.register_result(self.parse_statements())

            if res.error or not body:
                return res

            if self.cur_tok.type_ != TokenType.E_BLOCK:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        details="Expected '}'",
                    )
                )

            self.advance(res)

        else:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    details="Expected '->' or '{'",
                )
            )

        return res.register_success(WhileNode(condition, body))

    def parse_func_def(self) -> ParseResult:
        logging.debug("Parse func def")

        res = ParseResult()

        if self.cur_tok.type_ != TokenType.FUNC:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    f"Expected 'func'",
                )
            )

        self.advance(res)

        if self.cur_tok.type_ == TokenType.IDENTIFIER:
            func_id_token = self.cur_tok
            self.advance(res)
            if self.cur_tok.type_ != TokenType.L_BRC:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected '('",
                    )
                )
        else:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Expected function identifier",
                )
            )

        self.advance(res)

        arg_id_tokens = []
        arg_types = []
        if self.cur_tok.type_ == TokenType.IDENTIFIER:  # Read arguments and their types
            arg_id_tokens.append(self.cur_tok)

            self.advance(res)

            if self.cur_tok.type_ != TokenType.COLON:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected ':'",
                    )
                )

            self.advance()

            if self.cur_tok.type_ != TokenType.IDENTIFIER:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected argument type identifier",
                    )
                )

            arg_types.append(identifier_to_var_type(self.cur_tok))

            self.advance(res)

            while self.cur_tok.type_ == TokenType.COMMA:
                self.advance(res)

                if self.cur_tok.type_ != TokenType.IDENTIFIER:
                    return res.register_failure(
                        InvalidSyntaxError(
                            self.cur_tok.pos_start,
                            self.cur_tok.pos_end,
                            "Expected argument identifier",
                        )
                    )

                arg_id_tokens.append(self.cur_tok)

                self.advance(res)

                if self.cur_tok.type_ != TokenType.COLON:
                    return res.register_failure(
                        InvalidSyntaxError(
                            self.cur_tok.pos_start,
                            self.cur_tok.pos_end,
                            "Expected ':'",
                        )
                    )

                self.advance(res)

                if self.cur_tok.type_ != TokenType.IDENTIFIER:
                    return res.register_failure(
                        InvalidSyntaxError(
                            self.cur_tok.pos_start,
                            self.cur_tok.pos_end,
                            "Expected argument type identifier",
                        )
                    )

                arg_types.append(identifier_to_var_type(self.cur_tok))

                self.advance(res)

            if self.cur_tok.type_ != TokenType.R_BRC:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        f"Expected ',' or ')'",
                    )
                )
        else:
            if self.cur_tok.type_ != TokenType.R_BRC:
                return res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected identifier or ')'",
                    )
                )

        self.advance()

        if self.cur_tok.type_ != TokenType.R_ARROW:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Expected '->'",
                )
            )

        self.advance(res)

        if self.cur_tok.type_ == TokenType.IDENTIFIER:
            ret_type = identifier_to_var_type(self.cur_tok, can_untyped=True)

        else:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Expected return type identifier",
                )
            )

        self.advance(res)

        auto_ret = False
        if self.cur_tok.type_ == TokenType.R_ARROW:
            self.advance(res)

            body = res.register_result(self.parse_expr())

            if res.error or not body:
                return res

            logging.debug("Parse expr in func def")

            auto_ret = True

        elif self.cur_tok.type_ == TokenType.S_BLOCK:
            self.advance(res)

            body = res.register_result(self.parse_statements())

            if res.error or not body:
                return res

            logging.debug("Parsed statements in func def")

            if self.cur_tok.type_ != TokenType.E_BLOCK:
                res.register_failure(
                    InvalidSyntaxError(
                        self.cur_tok.pos_start,
                        self.cur_tok.pos_end,
                        "Expected '}'",
                    )
                )

            self.advance(res)

        else:
            return res.register_failure(
                InvalidSyntaxError(
                    self.cur_tok.pos_start,
                    self.cur_tok.pos_end,
                    "Expected '->' or '{'",
                )
            )

        return res.register_success(
            FuncNode(
                func_id_token,
                arg_id_tokens,
                arg_types,
                ret_type,
                body,
                auto_ret=auto_ret,
            )
        )
