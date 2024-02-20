from mysql.connector import connect, Error
import logging

logger = logging.getLogger(__name__)

# ------------------------------------------
# MySQL Methods

def connect_to_server(host, userName, userPasswd):
    """ Create a connection to a server """
    logger.debug('Connecting to server %s as user %s', host, userName)
    try:
        serverConnection = connect(
            host=host, 
            user=userName,
            passwd=userPasswd
        )
        logger.debug('Success')
    except Exception as e:
        logger.error(e)
        raise
    # Done
    return serverConnection

def connect_to_database(host, userName, userPasswd, dbName):
    """ Create a connection to a database """
    logger.debug('Connecting to database %s in server %s as user %s' % (dbName, host, userName))
    try:
        dbConnection = connect(
            host=host, 
            user=userName,
            passwd=userPasswd,
            database=dbName
        )
        logger.debug('Success')
    except Exception as e:
        logger.error(e)
        raise
    # Done
    return dbConnection

def execute(connection, query, after=None, close='cursor', many=False, params=[]):
    """ Helper function to encapsulate exception handling in a single method """
    try:
        logger.debug('Executing query: %s', query)
        # Get Cursor
        cursor = connection.cursor()
        # Execute given query
        if many:
            cursor.executemany(query, params)
        else:
            cursor.execute(query)
        logger.debug('cursor.execute(): Success')
        # Perform after action
        logger.debug('Executing after action: %s', after)
        result = __after(connection, cursor, after)
        logger.debug('__after(): Success')
        # Close (or not) connections
        logger.debug('Closing connections on close=%s', close)
        __close(connection, cursor, close)
        logger.debug('__close(): Success')
    except Exception as e:
        logger.error(e)
        raise
    # Done
    return result

def __after(connection, cursor, after):
    """ Perform actions after executing query: none, commit or fetch """
    # Get the result
    result = None
    if after:
        f = {
            'commit': connection.commit,
            'fetchone': cursor.fetchone,
            'fetchall': cursor.fetchall,
            'get_cursor': lambda: cursor,
        }.get(after, None)
        if f:
            result = f()
    # Done
    return result

def __close(connection, cursor, close):
    # Close connections (or not)
    if close:
        if close == 'cursor':
            cursor.close()
        elif close == 'all':
            cursor.close()
            connection.close()