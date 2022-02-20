import argparse
from pprint import pprint

from kitty.lexer.lexer import Lexer

parser = argparse.ArgumentParser()

group = parser.add_mutually_exclusive_group(required=True)
group.add_argument(
    "-l", "--lex", action="store_true", help="lex input file and return token list"
)
group.add_argument(
    "-c", "--compile", action="store_true", help="compile the input file"
)
group.add_argument(
    "-s",
    "--simulate",
    action="store_true",
    help="run kitty interpretor with file as input",
)

parser.add_argument("file", type=str, help="run kitty interpretor with file as input")


if __name__ == "__main__":
    args = parser.parse_args()
    if args.compile:
        print("Compiling not implemented yet!")

    elif args.simulate:
        print("Simulating not implemented yet!")

    elif args.lex:
        with open(args.file, "r", encoding="utf-8") as f:
            text = f.read()

        lexer = Lexer(args.file, text)
        tokens, error = lexer.tokenize()

        if error:
            print(error)
        else:
            pprint(tokens)
