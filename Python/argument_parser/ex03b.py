"""
Positional arguments

Flag arguments (no value needed)

Help message:
    usage: ex03b.py [-h] [--verbose]

    optional arguments:
    -h, --help  show this help message and exit
    --verbose   increase output verbosity
"""


import argparse


parser = argparse.ArgumentParser()

parser.add_argument("--verbose", help="increase output verbosity",
                    action="store_true")

args = parser.parse_args()

if args.verbose:
    print("verbosity turned on")