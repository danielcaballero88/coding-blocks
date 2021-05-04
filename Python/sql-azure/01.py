import pyodbc
import os 
server = 'dc-testing-db-server.database.windows.net'
database = 'dc-testing-db'
username = 'danielcaballero88'
password = 'Qwer!234'
driver= '{ODBC Driver 17 for SQL Server}'
# driver = '/home/dancab/Temp/libmsodbcsql-17.7.so.2.1'
# driver = os.path.join('/', 'home', 'dancab', 'Temp', 'libmsodbcsql-17.7.so.2.1')

configString = (
    f'DRIVER={driver};'
    f'SERVER={server};'
    'PORT=1433;'
    f'DATABASE={database};'
    f'UID={username};'
    f'PWD={password}'
)

# ls = os.listdir('/opt/microsoft/msodbcsql17/lib64')
# print(ls)
# ls = os.listdir('/home/dancab/Temp')
# print(ls)
# print(os.path.exists(driver))

# with pyodbc.connect('DRIVER='+driver+';SERVER='+server+';PORT=1433;DATABASE='+database+';UID='+username+';PWD='+ password) as conn:
#     with conn.cursor() as cursor:
#         cursor.execute("SELECT TOP 3 name, collation_name FROM sys.databases")
#         row = cursor.fetchone()
#         while row:
#             print (str(row[0]) + " " + str(row[1]))
#             row = cursor.fetchone()

with pyodbc.connect(configString) as conn:
    with conn.cursor() as cursor:
        cursor.execute('SELECT TOP 3 name, collation_name FROM sys.databases')
        row = cursor.fetchone()
        while row:
            print (str(row[0]) + ' ' + str(row[1]))
            row = cursor.fetchone()