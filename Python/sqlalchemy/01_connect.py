"""
https://docs.sqlalchemy.org/en/14/tutorial/engine.html#tutorial-engine
"""

from sqlalchemy import create_engine
from sqlalchemy import text

engine = create_engine('sqlite+pysqlite:///:memory:', echo=True, future=True)

with engine.connect() as conn:
    result = conn.execute(text('select "hello world"'))
    print(result.all())