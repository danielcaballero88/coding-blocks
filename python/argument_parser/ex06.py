"""
Conflicting options

Help message:
    usage: ex06.py [-h] [-v | -q] x y

    calculate x to the power of y

    positional arguments:
    x              the base
    y              the exponent

    optional arguments:
    -h, --help     show this help message and exit
    -v, --verbose
    -q, --quiet
""" 


import argparse

parser = argparse.ArgumentParser(description="calculate x to the power of y")

# Conflicting arguments -v and -q
group = parser.add_mutually_exclusive_group()
group.add_argument("-v", "--verbose", action="store_true")
group.add_argument("-q", "--quiet", action="store_true")

# Positional arguments
parser.add_argument("x", type=int, help="the base")
parser.add_argument("y", type=int, help="the exponent")

args = parser.parse_args()

answer = args.x**args.y

if args.quiet:
    print(answer)
elif args.verbose:
    print("{} to the power {} equals {}".format(args.x, args.y, answer))
else:
    print("{}^{} == {}".format(args.x, args.y, answer))