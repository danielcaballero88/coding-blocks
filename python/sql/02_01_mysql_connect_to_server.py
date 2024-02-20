import mysql.connector
from mysql.connector import Error

# ---------------------------------------------------
# Main

def main():
    connection = create_connection('localhost', 'root', 'asdd')

# ---------------------------------------------------
# Methods

def create_connection(host_name, user_name, user_password):
    conn = None
    try:
        conn = mysql.connector.connect(
            host=host_name,
            user=user_name,
            passwd=user_password
        )
        print('Connection Established')
    except Error as e:
        print(f'An error occurred: {e}')
    #
    return conn

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()