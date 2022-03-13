import argparse
import logging
from pprint import pprint

from kitty.lexer import Lexer
from kitty.parser import Parser
from kitty.validator import Validator

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
    "-p+v",
    "--parse+validate",
    action="store_true",
    help="parse the input file, validate and solve expressions if possible and result AST",
)

group.add_argument(
    "-s",
    "--simulate",
    action="store_true",
    help="run kitty interpreter with file as input",
)

arg_parser.add_argument(
    "-d", "--debug", action="store_true", help="enable compiler debug mode"
)

arg_parser.add_argument(
    "-p-indent", type=int, help="indent count while print AST", default=2
)

arg_parser.add_argument(
    "file", type=str, help="run kitty interpreter with file as input"
)


def lex(filename: str):
    with open(filename, "r", encoding="utf-8") as f:
        text = f.read()

    lexer = Lexer(args.file, text)
    return lexer.tokenize()


def parse(tokens):
    parser = Parser(tokens)
    return parser.parse()


def validate(ast):
    validator = Validator(ast)
    return validator.validate()


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

        tokens, error = lex(args.file)

        if error:
            print(error)
            exit(-1)

        pprint(tokens)

    elif args.parse:
        tokens, error = lex(args.file)

        if error:
            print(error)
            exit(-1)

        res = parse(tokens)

        if res.error:
            print(res.error)
        elif res.node:
            print(res.node.pretty_repr(indent=" " * args.p_indent))
        else:
            print("Empty file!")
            exit(0)

    elif args.__dict__["parse+validate"]:
        tokens, error = lex(args.file)

        if error:
            print(error)
            exit(-1)

        res = parse(tokens)

        if res.error:
            print(res.error)
        elif not res.node:
            print("Empty file!")
            exit(0)

        res = validate(res.node)

        if res.error:
            print(res.error)
        else:
            print(res.ast.pretty_repr(indent=" " * args.p_indent))
