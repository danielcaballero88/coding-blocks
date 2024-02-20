""" 
positional argument with a useful message 
AND 
most importantly! 
with a type, otherwise it is treated as a string
besides, the program will exit with error on bad input, instead of proceeding
"""


import argparse


parser = argparse.ArgumentParser()

parser.add_argument("square", help="display a square of a given number",
                    type=int)

args = parser.parse_args()

print(args.square**2)