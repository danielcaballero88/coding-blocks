import sqlite3
from sqlite3 import Error
import os

HERE = os.path.dirname(__file__)

# ---------------------------------------------------
# Main

def main():
    dbPath = os.path.join(HERE, 'test_db.sqlite')
    connection = create_connection(dbPath) # this creates the db if it doesn't exists

# ---------------------------------------------------
# Methods

def create_connection(dbFilePath):
    print(f'Connecting to database (file), dbFilePath={dbFilePath}')
    conn = None
    try:
        conn = sqlite3.connect(dbFilePath)
        print('Connectin established')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    return conn

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()