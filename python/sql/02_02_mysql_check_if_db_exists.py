import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main

def main():
    server_connection = connect_to_server("localhost", "root", "asdd")    
    db_name = "test"
    get_all_databases(server_connection)
    check_if_db_exists(server_connection, db_name)

# ---------------------------------------------------
# Methods

def connect_to_server(host_name, user_name, user_password):
    print('---')
    print('Connecting to MySQL server %s, with credentials %s:%s' % (host_name, user_name, user_password))
    connection = None
    try:
        connection = mysql.connector.connect(
            host=host_name,
            user=user_name,
            passwd=user_password
        )
        print("Connection successful")
    except Error as e:
        print(f"The error '{e}' occurred")

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

def get_all_databases(server_conn):
    print('---')
    print('Getting the list of all databases')
    query = (
        "SELECT SCHEMA_NAME "
        "FROM INFORMATION_SCHEMA.SCHEMATA "	
    )
    # query = 'SHOW DATABASES' # another option (simpler)
    cursor = server_conn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()
        print(result)
    except Error as e:
        print('The error %s occurred' % e)

# ---------------------------------------------------
# Execute (only if called explicitely as main)    

if __name__ == "__main__":
    main()