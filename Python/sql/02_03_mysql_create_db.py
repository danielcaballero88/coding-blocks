import mysql.connector
from mysql.connector import Error

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
    print(f'Querying to check if there are databases with the name {db_name}')
    #
    query = (
        'SELECT SCHEMA_NAME '
        'FROM INFORMATION_SCHEMA.SCHEMATA '	
        f'WHERE SCHEMA_NAME = "{db_name}"'
    )
    #
    cursor = server_conn.cursor()
    #
    try:
        cursor.execute(query)
        result = cursor.fetchall()
        print('Success')
        if result:
            print('There are databases witht the given name')
        else:
            print('No databases with the given name')
    except Error as e:
        print(f'The error "{e}" occurred')
    #
    return result

def create_database(server_conn, db_name):
    print(f'Trying to create db of name: {db_name}')
    cursor = server_conn.cursor()
    try:
        cursor.execute(f'CREATE DATABASE {db_name}')
        print('Database created successfully')
    except Error as e:
        print(f'The error "{e}" occurred')
    
def main():
    server_connection = connect_to_server('localhost', 'root', 'asdd')    
    db_name = 'test'
    db_list = check_if_db_exists(server_connection, db_name)
    if not db_list:
        create_database(server_connection, db_name)
    

if __name__ == '__main__':
    main()