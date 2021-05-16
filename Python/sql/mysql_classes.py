import pandas as pd
import logging
import mysql_methods
from dotenv import load_dotenv
import os

logger = logging.getLogger(__name__)
HERE = os.path.dirname(__file__)

class User:
    def __init__(self, name, password):
        self.name = name
        self.password = password

    @classmethod
    def from_dotenv(cls, dotenvPath=os.path.join(HERE, '.env')):
        load_dotenv(dotenv_path=dotenvPath)
        name = os.environ.get('USER_NAME')
        password = os.environ.get('USER_PASSWORD')
        return cls(name, password)

class Server:
    def __init__(self, host):
        logger.info('New instance of Server: host=%ss', host)
        self.host = host

    # ---------------------------------------
    # Basic/Essential Methods: connect and execute

    def connect(self, user):
        logger.info('Connecting to server %s as user %s', self.host, user.name)
        connection = mysql_methods.connect_to_server(self.host, user.name, user.password)
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

    def connect(self, user):
        logger.info('Connecting to database %s as user %s', self.name, user.name)
        connection = mysql_methods.connect_to_database(self.server.host, user.name, user.password, self.name)
        return connection

    def execute(self, connection, query, **kwargs):
        logger.debug('DataBase.execute() in db="%s"', self.name)
        logger.debug('Executing query: %s', query)
        # Execute given query
        result = mysql_methods.execute(connection, query, **kwargs)
        # Done
        return result

    # ---------------------------------------------
    # Predefined methods for common operations

    def exists_table(self, connection, tableName, close='cursor'):
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

    def create_table(self, connection, tableName, fields, **kwargs):
        logger.info('Trying to create Table of name "%s" in DataBase "%s"', tableName, self.name)
        logger.debug('Fields: %s', fields)
        #
        fields = ', '.join('%s' % f for f in fields)
        query = 'CREATE TABLE %s (%s);' % (tableName, fields)
        self.execute(connection, query, **kwargs)
    
    def drop_table(self, connection, tableName, ifExists=True, **kwargs):
        logger.info('Droping Table "%s" from DataBase "%s"', tableName, self.name)
        #
        if ifExists:
            query = 'DROP TABLE IF EXISTS %s;' % tableName
        else:
            query = 'DROP TABLE %s;' % tableName
        #
        self.execute(connection, query, **kwargs)

class Table:
    def __init__(self, db, name):
        self.db = db
        self.name = name
    
    def insert(self, dbConn, fields, records):
        logger.info('Trying to insert records into table "%s" in database "%s"', self.name, self.db.name)
        logger.debug('Records: %s', records)
        #
        if isinstance(records, tuple):
            self.__insert_one(dbConn, fields, records)
        elif isinstance(records, list):
            self.__insert_many(dbConn, fields, records)
        else:
            raise ValueError
        #

    def __insert_one(self, dbConn, fields, record, **kwargs):
        logger.debug('Inserting record using __insert_one() method')
        #
        query = 'INSERT INTO %s %s VALUES %s' % (self.name, fields, str(record))
        #
        self.db.execute(dbConn, query, after='commit', **kwargs)

    def __insert_many(self, dbConn, fields, records, **kwargs):
        logger.debug('Inserting records using __insert_many() method')
        #
        query = 'INSERT INTO %s %s VALUES (%%s, %%s)' % (self.name, fields)
        #
        self.db.execute(dbConn, query, many=True, params=records, after='commit', **kwargs)

if __name__ == '__main__':
    try:
        # Config logger
        format='%(asctime)s %(name)-12s %(funcName)-12s %(levelname)-8s %(message)s'
        logging.basicConfig(level='DEBUG', format=format)
        # ---
        # USER
        user = User.from_dotenv()
        # ---
        # SERVER ACTIONS
        # Create Server object
        server = Server(host='localhost')
        # Create a database in the server
        if True:
            serverConn = server.connect(user)
            server.drop_database(serverConn, 'test1', close=None)
            server.create_database(serverConn, 'test1', close='all')
        # ---
        # DATABASE ACTIONS
        # Create a DataBase object
        db = DataBase(server=server, name='test1')
        dbConn = db.connect(user)
        #
        # Create a table in the database
        db.exists_table(dbConn, 'test1')
        db.drop_table(dbConn, 'test')
        #
        fields = [
            'id INT AUTO_INCREMENT PRIMARY KEY',
            'name VARCHAR(100) NOT NULL',
            'age DECIMAL(4,1)'
        ]
        db.create_table(dbConn, 'test', fields, close='all')
        # ---
        # TABLE ACTIONS
        # Create a Table object
        table = Table(db=db, name='test')
        # Insert some records
        dbConn = db.connect(user)
        singleRecord = ('Dani', 32)
        table.insert(dbConn, fields='(name, age)', records=singleRecord)
        manyRecords = [('Luli', 31), ('Oscar', 61), ('Mama', 65.5555)]
        table.insert(dbConn, fields='(name, age)', records=manyRecords)
    
    except Exception as e:
        logger.critical(e.__repr__())