import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main

def main():
    server_connection = connect_to_server('localhost', 'root', 'asdd')    
    db_name = 'test'
    dbExists = check_if_db_exists(server_connection, db_name)
    if not dbExists:
        create_database(server_connection, db_name)

# ---------------------------------------------------
# Methods

def connect_to_server(host_name, user_name, user_password):
    print(f'Trying to connect to server using credentials: user={user_name}, pass={user_password}')
    connection = None
    try:
        connection = mysql.connector.connect(
            host=host_name,
            user=user_name,
            passwd=user_password
        )
        print('Connection to MySQL DB successful')
    except Error as e:
        print(f'The error "{e}" occurred')

    return connection

def check_if_db_exists(server_conn, db_name):
    print('---')
    print('Checking if db %s exists' % db_name)
    query = (
        "SELECT count(*) "
        "FROM INFORMATION_SCHEMA.SCHEMATA "	
        f"WHERE SCHEMA_NAME = '{db_name}'"
    )
    cursor = server_conn.cursor()
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