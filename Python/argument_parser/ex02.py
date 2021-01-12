""" positional arguments """ 


import argparse

# Init parser
print("1")
parser = argparse.ArgumentParser()

# Add an positional argument named echo
print("2")
parser.add_argument("echo", help="echo the string you use here")

# Parse args
print("3")
args = parser.parse_args()

# Print args
print("4")
print(args.echo)