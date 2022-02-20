import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-c", "--compile", action="store_true", help="compile the input file")
parser.add_argument("-s", "--simulate", action="store_true", help="run kitty interpretor with file as input")
parser.add_argument("file", type=str, help="run kitty interpretor with file as input")


if __name__ == '__main__':
    args = parser.parse_args()
    if args.compile and args.simulate:
        print("Invalid options -c with -s. See --help!")
        exit(-1)
        
    if args.compile:
        print("Compiling not implemented yet!")
    elif args.simulate:
        print("Simulating not implemented yet!")