import sqlite3
from sqlite3 import Error
import os

HERE = os.path.dirname(__file__)

# ---------------------------------------------------
# Main

def main():
    dbPath = os.path.join(HERE, 'test_db.sqlite')
    check_if_db_exists(dbPath)
    # connection = create_connection(dbPath) # this creates the db if it doesn't exists

# ---------------------------------------------------
# Methods

def create_connection(dbFilePath):
    print(f'Connecting to database (file), dbFilePath={dbFilePath}')
    conn = None
    try:
        conn = sqlite3.connect(dbFilePath)
        print('Connection established')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    return conn

def check_if_db_exists(dbFilePath):
    print(f'Checking if db exists at filepath={dbFilePath}')
    exists = False
    try:
        exists = os.path.isfile(dbFilePath)
        print(f'Success! exists={exists}')
    except Exception as e:
        print(f'An exception occurred: {e}')
    #
    return exists

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()