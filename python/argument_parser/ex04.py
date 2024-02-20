"""
Short options

Help message:
    usage: ex04.py [-h] [-v]

    optional arguments:
    -h, --help     show this help message and exit
    -v, --verbose  increase output verbosity
"""


import argparse


parser = argparse.ArgumentParser()

parser.add_argument("-v", "--verbose", help="increase output verbosity",
                    action="store_true")

args = parser.parse_args()

if args.verbose:
    print("verbosity turned on")