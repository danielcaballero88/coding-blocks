import pandas as pd
import logging
import mysql_methods

logger = logging.getLogger(__name__)

class Server:
    def __init__(self, host):
        logger.info('New instance of Server: host=%ss', host)
        self.host = host

    # ---------------------------------------
    # Basic/Essential Methods: connect and execute

    def connect(self, userName, userPasswd):
        logger.info('Connecting to server %s as user %s', self.host, userName)
        connection = mysql_methods.connect_to_server(self.host, userName, userPasswd)
        return connection

    def execute(self, connection, query, after=None, close='all'):
        logger.debug('Executing query at server connection level')
        # Execute given query
        result = mysql_methods.execute(connection, query, after, close)
        # Done
        return result

    # ---------------------------------------
    # Predefined methods for common operations

    def exists_database(self, connection, dbName):
        logger.info('Checking if db %s exists', dbName)
        query = (
            "SELECT count(*) "
            "FROM INFORMATION_SCHEMA.SCHEMATA "	
            f"WHERE SCHEMA_NAME = '{dbName}'"
        )
        # Execute query
        result = self.execute(connection, query, after='fetchone')
        dbExists = result[0]
        logger.debug('DataBase exists: %s', bool(dbExists))
        # Done
        return dbExists

    def create_database(self, connection, dbName, after=None, close='all'):
        logger.info('Trying to create DataBase of name: %s', dbName)
        query = f'CREATE DATABASE {dbName};'
        self.execute(connection, query, after, close)
    
    def drop_database(self, connection, dbName, ifExists=True, after=None, close='all'):
        logger.info('Droping DataBase %s', dbName)
        #
        if ifExists:
            query = 'DROP DATABASE IF EXISTS %s' % dbName
        else:
            query = 'DROP DATABASE %s' % dbName
        #
        self.execute(connection, query, after, close)

class DataBase:
    def __init__(self, server, name):
        logger.info('New instance of DataBase object %s', name)
        self.server = server
        self.name = name

    # ---------------------------------------------
    # Basic/Essential Methods: connect and execute

    def connect(self, userName, userPasswd):
        logger.info('Connecting to database %s as user %s', self.name, userName)
        connection = mysql_methods.connect_to_database(self.server.host, userName, userPasswd, self.name)
        return connection

    def execute(self, connection, query, after=None, close='cursor'):
        logger.debug('Executing query: %s', query)
        # Execute given query
        result = mysql_methods.execute(connection, query, after, close)
        # Done
        return result

    # ---------------------------------------------
    # Predefined methods for common operations

    def exists_table(self, connection, tableName, after=None, close='cursor'):
        logger.info('Checking if table %s exists in database %s', tableName, self.name)
        query = (
            'SELECT count(*) '
            '  FROM INFORMATION_SCHEMA.TABLES '	
            '  WHERE TABLE_NAME = "%s"'
            '    AND TABLE_SCHEMA = "%s"'
        ) % (tableName, self.name)
        # Execute query
        result = self.execute(connection, query, after='fetchone', close=close)
        tableExists = result[0]
        logger.debug('Table exists: %s', bool(tableExists))
        # Done
        return tableExists

    def create_table(self, connection, tableName, fields, after=None, close='cursor'):
        logger.info('Trying to create Table of name "%s" in DataBase "%s"', tableName, self.name)
        logger.debug('Fields: %s', fields)
        #
        fields = ', '.join('%s' % f for f in fields)
        query = 'CREATE TABLE %s (%s);' % (self.name, fields)        
        self.execute(connection, query, after, close)
    
    def drop_table(self, connection, tableName, ifExists=True, after=None, close='cursor'):
        logger.info('Droping Table "%s" from DataBase "%s"', tableName, self.name)
        #
        if ifExists:
            query = 'DROP TABLE IF EXISTS %s;' % tableName
        else:
            query = 'DROP TABLE %s;' % tableName
        #
        self.execute(connection, query, after, close)

if __name__ == '__main__':
    try:
        # Config logger
        format='%(asctime)s %(name)-12s %(funcName)-12s %(levelname)-8s %(message)s'
        logging.basicConfig(level='DEBUG', format=format)
        #
        # Create Server object
        server = Server('localhost')
        if True:
            serverConn = server.connect('root', 'asdd')
            server.drop_database(serverConn, 'test1', close=None)
            server.create_database(serverConn, 'test1', close='all')
        # 
        # Create a DataBase object
        db = DataBase(server, 'test1')
        dbConn = db.connect('root', 'asdd')
        #
        # Create a table in the database
        db.exists_table(dbConn, 'test1', after='keep_connection')
        db.drop_table(dbConn, 'test', after='keep_connection')
        #
        fields = [
            'id INT AUTO_INCREMENT PRIMARY KEY',
            'name VARCHAR(100) NOT NULL',
            'age DECIMAL(4,1)'
        ]
        db.create_table(dbConn, 'test', fields, close='all')
    except Exception as e:
        logger.critical(e.__repr__())