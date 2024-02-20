from sqlalchemy import create_engine
from sqlalchemy import text
import os

# ---------------------------------------------------
# Main

HERE = os.path.dirname(__file__)

def main():
    url = assemble_url('sqlite', 'pysqlite', '/:memory:')
    print('---')
    print('Connecting to: %s' % url)
    #
    engine = create_engine(url, echo=True, future=True)
    #
    # commit as you go
    print('---')
    print('commit as you go')
    with engine.connect() as conn:
        conn.execute(text(
            'CREATE TABLE some_table (x int, y int)'
        ))
        conn.execute(
            text(
            'INSERT INTO some_table (x, y) VALUES (:x, :y)'
            ),
            [{'x': 1, 'y': 1}, {'x': 2, 'y': 4}]
        )
        conn.commit()
    #
    # begin once
    print('---')
    print('begin once')
    with engine.begin() as conn:
        conn.execute(
            text(
            'INSERT INTO some_table (x, y) VALUES (:x, :y)'
            ),
            [{'x': 6, 'y': 8}, {'x': 9, 'y': 10}]
        )
    print('---')

# ---------------------------------------------------
# Methods

def assemble_url(dialect, driver, database):
    url = dialect
    if driver:
        url += '+' + driver
    url += '://'
    url += database
    return url 

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()