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
    # Repeat step 02
    create_some_table(engine)
    insert_some_values_into_some_table(engine)
    # fetch rows sending parameters (bound parameters)
    with engine.connect() as conn:
        result = conn.execute(
            text('SELECT x, y FROM some_table WHERE y > :y'), # note the bound parameter sintax with the ':'
            {'y': 2} # and here is the bound parameter value that is passed
        )
        for row in result:
            print(f'x: {row.x}, y: {row.y}')

# ---------------------------------------------------
# Methods

def assemble_url(dialect, driver, database):
    url = dialect
    if driver:
        url += '+' + driver
    url += '://'
    url += database
    return url 

def create_some_table(engine):
    with engine.connect() as conn:
        conn.execute(text(
            'CREATE TABLE some_table (x int, y int)'
        ))

def insert_some_values_into_some_table(engine):
    # commit as you go
    with engine.connect() as conn:
        conn.execute(
            text(
            'INSERT INTO some_table (x, y) VALUES (:x, :y)'
            ),
            [{'x': 1, 'y': 1}, {'x': 2, 'y': 4}]
        )
        conn.commit()
    # begin once
    with engine.begin() as conn:
        conn.execute(
            text(
            'INSERT INTO some_table (x, y) VALUES (:x, :y)'
            ),
            [{'x': 6, 'y': 8}, {'x': 9, 'y': 10}]
        )

# ---------------------------------------------------
# Execute (only if called explicitely as main)

if __name__ == '__main__':
    main()