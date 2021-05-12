import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main

def main():
    #
    dbName = 'test'
    tableName = 'test_table'
    #
    serverConnection = connect_to_server('localhost', 'root', 'asdd')
    dbExists = check_if_db_exists(serverConnection, dbName)
    if dbExists:
        dbConnection = connect_to_database('localhost', 'root', 'asdd', dbName)    
        # Check if table exists
        tableExists = check_if_table_exists(dbConnection, 'test_table')
        # The following code searches for the tables of the entire server
        if False:
            try:
                cursor = serverConnection.cursor()
                cursor.execute('SELECT * FROM information_schema.tables WHERE TABLE_NAME="test_table"')
                result = cursor.fetchall()
                print(result)
            except Error as e:
                print('Error occurred: %s' % e)

# ---------------------------------------------------
# Methods

def connect_to_server(hostName, userName, userPassword):
    print(f'Trying to connect to server {hostName} using credentials: user={userName}, pass={userPassword}')
    connection = None
    try:
        connection = mysql.connector.connect(
            host=hostName,
            user=userName,
            passwd=userPassword
        )
        print('Connection to MySQL DB successful')
    except Error as e:
        print(f'The error "{e}" occurred')
    #
    return connection

def connect_to_database(hostName, userName, userPassword, dbName):
    print(f'Trying to connect to database "{dbName}" using credentials: user={userName}, pass={userPassword}')
    connection = None
    try:
        connection = mysql.connector.connect(
            host=hostName,
            user=userName,
            passwd=userPassword,
            database=dbName
        )
        print('Connection successful')
    except Error as e:
        print(f'The error "{e}" occurred')
    #
    return connection

def check_if_db_exists(serverConn, dbName):
    print('---')
    print('Checking if db %s exists' % dbName)
    query = (
        "SELECT count(*) "
        "FROM INFORMATION_SCHEMA.SCHEMATA "	
        f"WHERE SCHEMA_NAME = '{dbName}'"
    )
    cursor = serverConn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()[0]
        print(query)
        print('Success')
        dbExists = result[0]
        print('db exists? %s' % bool(dbExists))
    except Error as e:
        print(f'The error {e} occurred')
    #
    return dbExists

def check_if_table_exists(dbConn, tableName):
    print('---')
    print('Checking if table "%s" exists in database "%s"' % (tableName, get_database_name(dbConn)))
    query = (
        "SELECT count(*) "
        "FROM INFORMATION_SCHEMA.TABLES "	
        f"WHERE TABLE_NAME = '{tableName}'"
    )
    cursor = dbConn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()[0]
        print(query)
        print('Success')
        tableExists = result[0]
        print('Table exists? %s' % bool(tableExists))
    except Error as e:
        print(f'The error {e} occurred')
    #
    return tableExists

def get_database_name(dbConn):
    # print('---')
    # print('Trying to get database name for dbConn=%s' % dbConn)
    query = 'SELECT database()'
    cursor = dbConn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()
        dbName = result[0][0]
        # print('Success, dbName=%s' % dbName)
    except Error as e:
        print('Error occurred: %s' % e)
    else:
        return dbName

def create_database(server_conn, db_name):
    print(f'Trying to create db of name: {db_name}')
    cursor = server_conn.cursor()
    try:
        cursor.execute(f'CREATE DATABASE {db_name}')
        print('Database created successfully')
    except Error as e:
        print(f'The error "{e}" occurred')
    
# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()