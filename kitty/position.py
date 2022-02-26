class Position:
    idx: int
    line: int
    column: int
    fname: str
    text: str

    def __init__(self, idx: int, line: int, column: int, fname: str, text: str):
        self.idx = idx
        self.line = line
        self.column = column
        self.fname = fname
        self.text = text

    def advance(self, cur_let: str = ""):
        self.idx += 1
        self.column += 1

        if cur_let == "\n":
            self.line += 1
            self.column = 0

        return self

    def copy(self):
        return Position(self.idx, self.line, self.column, self.fname, self.text)
