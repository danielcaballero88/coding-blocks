"""
Positional arguments

Help message:
    usage: ex03.py [-h] [--verbosity VERBOSITY]

    optional arguments:
    -h, --help            show this help message and exit
    --verbosity VERBOSITY
                            increase output verbosity

The program is written so as to display something 
when --verbosity is specified and display nothing when not.

Importante! 
El flag es --verbosity y luego viene el valor VERBOSITY,
este ultimo es un string asi que poco vale poner 0 o False
(pues va a considerarse un string no vacio entonces, 
en el if, va a tomarse como True)

Ademas, al ser un argumento opcional, si no se da, va a 
existir igualmente un argumento en args, de nombre
verbosity, pero cuyo valor va a ser None
"""


import argparse


parser = argparse.ArgumentParser()

parser.add_argument("--verbosity", help="increase output verbosity")

args = parser.parse_args()

if args.verbosity:
    print("verbosity turned on")