"""
Combining positional and optional arguments

a different way of playing with verbosity: action="count"
(en este caso es importante dar un default para el caso en
que el count de 0, porque en ese caso quedaria en None, y no
en 0 que seria mas logico, por defecto)

Help message:
    usage: ex05b.py [-h] [-v VERBOSITY] square

    positional arguments:
    square                display a square of a given number

    optional arguments:
    -h, --help            show this help message and exit
    -v VERBOSITY, --verbosity VERBOSITY
                            increase output verbosity
""" 


import argparse


parser = argparse.ArgumentParser()

# Positional argument named square
parser.add_argument("square", type=int,
                    help="display a square of a given number")

# Optional argument named verbose
parser.add_argument("-v", "--verbosity", 
                    action="count", default=0,
                    help="increase output verbosity")

args = parser.parse_args()

answer = args.square**2

if args.verbosity >= 2:
    print("the square of {} equals {}".format(args.square, answer))
elif args.verbosity >= 1:
    print("{}^2 == {}".format(args.square, answer))
else:
    print(answer)