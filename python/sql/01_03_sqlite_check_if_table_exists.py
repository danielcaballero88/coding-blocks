import sqlite3
from sqlite3 import Error
import os

HERE = os.path.dirname(__file__)

# ---------------------------------------------------
# Main

def main():
    dbPath = os.path.join(HERE, 'test_db.sqlite')
    if check_if_db_exists(dbPath):
        connection = create_connection(dbPath) # this creates the db if it doesn't exists
        check_if_table_exists(connection, 'test')
    
# ---------------------------------------------------
# Methods

def create_connection(dbFilePath):
    print('---')
    print(f'Connecting to database (file), dbFilePath={dbFilePath}')
    conn = None
    try:
        conn = sqlite3.connect(dbFilePath)
        print('Connection established')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    print('--')
    return conn

def check_if_db_exists(dbFilePath):
    print('---')
    print(f'Checking if db exists at filepath={dbFilePath}')
    exists = False
    try:
        exists = os.path.isfile(dbFilePath)
        print(f'Success! db exists={exists}')
    except Exception as e:
        print(f'An exception occurred: {e}')
    #
    print('--')
    return exists


def check_if_table_exists(dbConn, tableName):
    print('---')
    print(f'Checking if a table with name={tableName} exists in db={dbConn}')
    exists = False
    try:
        cursor =dbConn.cursor()
        cursor.execute(f'SELECT count(*) FROM sqlite_master WHERE type="table" AND name="{tableName}";')
        a = cursor.fetchall()
        if a[0][0]:
            exists = True
        print(f'Success! table exists={exists}')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    print('--')
    return exists
# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()