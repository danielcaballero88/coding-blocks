from sqlalchemy import create_engine
from sqlalchemy import text
import os

# ---------------------------------------------------
# Main

HERE = os.path.dirname(__file__)

def main():
    url = assemble_url('sqlite', 'pysqlite', '/:memory:')
    print('Connecting to: %s' % url)
    engine = create_engine(url, echo=True, future=True)
    with engine.connect() as conn:
        result = conn.execute(text('select "hello world"'))
        print(result.all())

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