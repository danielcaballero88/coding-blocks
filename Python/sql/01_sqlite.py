import sqlite3
from sqlite3 import Error
import os

HERE = os.path.dirname(__file__)

def create_connection(path):
    conn = None
    try:
        conn = sqlite3.connect(path)
        print('Connectin established')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    return conn

def main():
    dbPath = os.path.join(HERE, 'test_db.sqlite')
    connection = create_connection(dbPath)

if __name__ == '__main__':
    main()