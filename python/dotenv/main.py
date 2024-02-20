"""
This script shows how to load configuration from a settings.py file
(just the simplest way to do it, it can be better)

settings.py has variables that are needed for something,
so I just need to import settings and then the variables are
present in the module's namespace
"""
import settings

my_email = settings.email
my_pass = settings.passw

print("My email is: ", my_email)
print("My password is: ", my_pass)