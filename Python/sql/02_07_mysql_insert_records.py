import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main



def main():
    # Settings
    dbName = 'test'
    tableName = 'test_table'
    fields = [
        'id INT AUTO_INCREMENT PRIMARY KEY',
        'a VARCHAR(100)',
        'b INT'
    ]
    fieldNames = '(a, b)'
    fieldValues = [
        ('hola', 1),
        ('chau', 2)
    ]
    #
    serverConnection = connect_to_server('localhost', 'root', 'asdd')
    dbExists = check_if_db_exists(serverConnection, dbName)
    if not dbExists:
        print('Cannot continue, database "%s" does not exist' % dbName)
    else:
        dbConnection = connect_to_database('localhost', 'root', 'asdd', dbName)    
        # Check if table exists
        tableExists = check_if_table_exists(dbConnection, tableName)
        #
        if tableExists:
            drop_table(dbConnection, tableName)
        #
        create_table(dbConnection, tableName, fields)
        #
        insert_records(dbConnection, tableName, fieldNames, fieldValues)
        insert_many_records(dbConnection, tableName, fieldNames, fieldValues)

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
    print(f'Trying to connect to database {dbName} using credentials: user={userName}, pass={userPassword}')
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
        result = cursor.fetchone()
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
    print('Checking if table %s exists in database %s' % (tableName, get_database_name(dbConn)))
    query = (
        "SELECT count(*) "
        "FROM INFORMATION_SCHEMA.TABLES "	
        f"WHERE TABLE_NAME = '{tableName}'"
    )
    cursor = dbConn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchone()
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

def create_database(serverConn, dbName):
    print(f'Trying to create db of name: {dbName}')
    cursor = serverConn.cursor()
    try:
        cursor.execute(f'CREATE DATABASE {dbName}')
        print('Database created successfully')
    except Error as e:
        print(f'The error "{e}" occurred')

def create_table(dbConn, tableName, fields):
    print('---')
    print('Trying to create table "%s" in database "%s"' % (tableName, get_database_name(dbConn)))
    #
    query = f'CREATE TABLE {tableName} (\n' + ',\n'.join(field for field in fields) + ');'
    print(query)
    #
    cursor = dbConn.cursor()
    try:
        cursor.execute(query)
        print('Success!')
    except Error as e:
        print('Error occurred: %s' % e)

def drop_table(dbConn, tableName):
    print('---')
    print('Trying to drop table "%s" in database "%s"' % (tableName, get_database_name(dbConn)))
    query = 'DROP TABLE %s' % tableName
    try:
        cursor = dbConn.cursor()
        cursor.execute(query)
        print('Success!')
    except Error as e:
        print('Error occurred: %s' % e)

def insert_records(dbConn, tableName, fieldNames, records):
    print('---')
    print('Trying to insert records into table "%s" in database "%s"' % (tableName, get_database_name(dbConn)))
    print('Records:')
    print(fieldNames)
    for rec in records:
        print(rec)
    #
    fieldValuesStr = ',\n'.join([str(rec) for rec in records])
    query = f'INSERT INTO {tableName} {fieldNames}\nVALUES\n{fieldValuesStr};'
    print(query)
    #
    try:
        cursor = dbConn.cursor()
        cursor.execute(query)
        dbConn.commit()
        print('Success!')
    except Error as e:
        print('Error occurred: %s' % e)

def insert_many_records(dbConn, tableName, fieldNames, records):
    print('---')
    print('Trying to insert many records (using executemany) into table "%s" in database "%s"' % (tableName, get_database_name(dbConn)))
    print('Records:')
    print(fieldNames)
    for rec in records:
        print(rec)
    #
    query = f'INSERT INTO {tableName} {fieldNames} VALUES (%s, %s)'
    print(query)
    #
    try:
        cursor = dbConn.cursor()
        cursor.executemany(query, records)
        dbConn.commit()
        print('Success!')
    except Error as e:
        print('Error occurred: %s' % e)

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()