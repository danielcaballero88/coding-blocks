""" 
parser with no arguments listed
it only accepts -h or --help and returns a 
default message 

The --help option, which can also be shortened to -h, 
is the only option we get for free 
(i.e. no need to specify it). 
Specifying anything else results in an error. 
But even then, we do get a useful usage message, 
also for free.
"""

import argparse

parser = argparse.ArgumentParser()
parser.parse_args()