import argparse
import logging
from pprint import pprint

from kitty.lexer import Lexer
from kitty.parser import Parser

arg_parser = argparse.ArgumentParser()

group = arg_parser.add_mutually_exclusive_group(required=True)
group.add_argument(
    "-l", "--lex", action="store_true", help="lex input file and return token list"
)
group.add_argument(
    "-c", "--compile", action="store_true", help="compile the input file"
)
group.add_argument(
    "-p", "--parse", action="store_true", help="parse the input file and result AST"
)

group.add_argument(
    "-s",
    "--simulate",
    action="store_true",
    help="run kitty interpretor with file as input",
)

arg_parser.add_argument(
    "-d", "--debug", action="store_true", help="enable compiler debug mode"
)

arg_parser.add_argument(
    "file", type=str, help="run kitty interpretor with file as input"
)


if __name__ == "__main__":
    args = arg_parser.parse_args()

    if args.debug:
        logging.basicConfig(
            level=logging.DEBUG, format="%(filename)s : %(lineno)d : %(message)s"
        )

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

    elif args.parse:
        with open(args.file, "r", encoding="utf-8") as f:
            text = f.read()

        lexer = Lexer(args.file, text)
        tokens, error = lexer.tokenize()

        if error:
            print(error)
            exit(-1)

        parser = Parser(tokens)
        res = parser.parse()
        if res.error:
            print(res.error)
        elif res.node:
            print(res.node.pretty_repr())
        else:
            print("Empty file!")
