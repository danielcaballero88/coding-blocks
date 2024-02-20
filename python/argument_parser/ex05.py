"""
Combining positional and optional arguments
(Note that the order does not matter,
either when setting the args or when using the program)

Help message:
    usage: ex05.py [-h] [-v] square

    positional arguments:
    square         display a square of a given number

    optional arguments:
    -h, --help     show this help message and exit
    -v, --verbose  increase output verbosity
""" 


import argparse


parser = argparse.ArgumentParser()

# Positional argument named square
parser.add_argument("square", type=int,
                    help="display a square of a given number")

# Optional argument named verbose
parser.add_argument("-v", "--verbose", action="store_true",
                    help="increase output verbosity")

args = parser.parse_args()

answer = args.square**2

if args.verbose:
    print("the square of {} equals {}".format(args.square, answer))
else:
    print(answer)