from kitty.lexer.position import Position


class LexerError:
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

    def as_string(self):
        result = f"{self.error_name}: {self.details}\n"
        result += f"File {self.pos_start.fname}, line {self.pos_start.line + 1}"
        return result


class IllegalCharError(LexerError):
    def __init__(self, pos_start: Position, pos_end: Position, details: str):
        super().__init__(pos_start, pos_end, "Illegal Character", details)
