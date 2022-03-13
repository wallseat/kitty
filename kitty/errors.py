from kitty.position import Position


def string_with_arrows(text: str, pos_start: Position, pos_end: Position):
    result = ""

    idx_start = max(text.rfind("\n", 0, pos_start.idx), 0)
    idx_end = text.find("\n", idx_start + 1)
    if idx_end < 0:
        idx_end = len(text)

    line_count = pos_end.line - pos_start.line + 1
    for i in range(line_count):

        line = text[idx_start:idx_end]
        col_start = pos_start.column if i == 0 else 0
        col_end = pos_end.column if i == line_count - 1 else len(line) - 1

        result += line + "\n"
        result += " " * col_start + "^" * (col_end - col_start)

        idx_start = idx_end
        idx_end = text.find("\n", idx_start + 1)
        if idx_end < 0:
            idx_end = len(text)

    return result.replace("\t", "")


class Error:
    pos_start: Position
    pos_end: Position
    error_name: str
    details: str

    def __init__(
        self, pos_start: Position, pos_end: Position, error_name: str, details: str
    ):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def __repr__(self):
        result = f"{self.error_name}: {self.details}\n"
        result += f"File {self.pos_start.fname}, line {self.pos_start.line + 1}\n"
        result += string_with_arrows(self.pos_start.text, self.pos_start, self.pos_end)
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start: Position, pos_end: Position, details: str):
        super(IllegalCharError, self).__init__(
            pos_start, pos_end, "Illegal Character", details
        )


class InvalidSyntaxError(Error):
    def __init__(self, pos_start: Position, pos_end: Position, details: str = ""):
        super(InvalidSyntaxError, self).__init__(
            pos_start, pos_end, "Invalid Syntax", details
        )


class ValidationError(Error):
    def __init__(self, pos_start: Position, pos_end: Position, details: str = ""):
        super(ValidationError, self).__init__(
            pos_start, pos_end, "Validation Error", details
        )


class OperationError(Error):
    def __init__(self, pos_start: Position, pos_end: Position, details: str = ""):
        super(OperationError, self).__init__(
            pos_start, pos_end, "Operation Error", details
        )


class IllegalOperation(OperationError):
    def __init__(self, pos_start: Position, pos_end: Position):
        super(IllegalOperation, self).__init__(pos_start, pos_end, "illegal operation")
