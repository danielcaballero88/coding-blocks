import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main

def main():
    server_connection = connect_to_server("localhost", "root", "asdd")    
    db_name = "test"
    check_if_db_exists(server_connection, db_name)

# ---------------------------------------------------
# Methods

def connect_to_server(host_name, user_name, user_password):
    connection = None
    try:
        connection = mysql.connector.connect(
            host=host_name,
            user=user_name,
            passwd=user_password
        )
        print("Connection to MySQL DB successful")
    except Error as e:
        print('connect_to_server')
        print(f"The error '{e}' occurred")

    return connection

def check_if_db_exists(server_conn, db_name):
    query = (
        "SELECT SCHEMA_NAME "
        "FROM INFORMATION_SCHEMA.SCHEMATA "	
        f"WHERE SCHEMA_NAME = '{db_name}'"
    )
    cursor = server_conn.cursor()
    try:
        cursor.execute(query)
        result = cursor.fetchall()
        print(query)
        print('Success')
        print(result)
    except Error as e:
        print('check_if_db_exists')
        print(f'The error {e} occurred')

# ---------------------------------------------------
# Execute (only if called explicitely as main)    

if __name__ == "__main__":
    main()