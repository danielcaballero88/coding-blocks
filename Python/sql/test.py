import sqlite3
from sqlite3 import Error
import os
import pandas as pd

HERE = os.path.dirname(__file__)

# ---------------------------------------------------
# Main

def main():
    dbPath = os.path.join(HERE, 'test_db.sqlite')
    dbConn = create_connection(dbPath) # this creates the db if it doesn't exists
    df = pd.DataFrame({
        'a': [0,1,2],
        'b': [3,4,5]
    })
    df.to_sql('df', dbConn)

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