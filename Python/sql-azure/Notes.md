# Notes on Python + Azure SQL Database

1. Create and configure the databse online in Azure: https://docs.microsoft.com/en-us/azure/azure-sql/database/connect-query-python
2. Install the odbc driver: https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver15
3. Query the database: https://docs.microsoft.com/en-us/azure/azure-sql/database/connect-query-python#create-code-to-query-your-database
   * The code is in file `01.py`. It works fine running from a terminal, but does not work running within vscode.
   * It should return the database name and collatino (set of characters allowed)
4.   