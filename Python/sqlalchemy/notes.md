# Notes about SQLAlchemy

These are very brief notes because, otherwise, it would be the same as reading the docs
or following the [tutorial][1].  
Only very small fragments of text and code snippets with brief comments should appear here.

[1]: https://docs.sqlalchemy.org/en/14/tutorial/index.html#unified-tutorial

## Concept

SQLAlchemy is a ORM (Object Relational Mapper) which means that is relates a database and its tables to Python objects.

It's composed of two distinct APIs, a *core* API and a *ORM* API.
The idea is to use mostly the ORM, but access to the core is still provided, I think.

What is important is that it masks the SQL so the same code can be used with different types of databases.  
It's only needed to know Python, and some SQL surely.

## The Engine

The engine is the central point of connection to the database.

```
>>> from sqlalchemy import create_engine
>>> engine = create_engine("sqlite+pysqlite:///:memory:", echo=True, future=True)
```

The string form of the URL is `dialect[+driver]://user:password@host/dbname[?key=value..]`, 
where dialect is a database name such as mysql, oracle, postgresql, etc., 
and driver the name of a [DBAPI][2], such as psycopg2, pyodbc, cx_oracle, etc. 
Alternatively, the URL can be an instance of [URLclass][3].

In this case, our URL includes the phrase `/:memory:`, 
which is an indicator to the sqlite3 module that we will be using an in-memory-only database.

[2]: https://www.oreilly.com/library/view/python-in-a/0596100469/ch11s04.html
[3]: https://docs.sqlalchemy.org/en/14/core/engines.html#sqlalchemy.engine.URL.

## Working with Transactions and the DBAPI

### Getting a Connection

As the Connection represents an open resource against the database, 
we want to always limit the scope of our use of this object to a specific context.

Textual SQL is emitted using a construct called [`text()`][4].
This function has some advantages over using a plain SQL string.


```
>>> from sqlalchemy import text

>>> with engine.connect() as conn:
...     result = conn.execute(text("select 'hello world'"))
...     print(result.all())
BEGIN (implicit)
select 'hello world'
[...] ()
[('hello world',)]
ROLLBACK
```
The transaction is **not committed automatically**.

[4]: https://docs.sqlalchemy.org/en/14/core/sqlelement.html#sqlalchemy.sql.expression.text

### Committing Changes

Two ways:
1. "Commit as you go": Use `Engine.connect()` -> do some work -> `Connection.commit()` -> repeat or not
2. "Begin once": Use `Engine.begin()` -> it does the connection and commits at the end

```
# "commit as you go"
>>> with engine.connect() as conn:
...     conn.execute(text("CREATE TABLE some_table (x int, y int)"))
...     conn.execute(
...         text("INSERT INTO some_table (x, y) VALUES (:x, :y)"),
...         [{"x": 1, "y": 1}, {"x": 2, "y": 4}]
...     )
...     conn.commit()
BEGIN (implicit)
CREATE TABLE some_table (x int, y int)
[...] ()
<sqlalchemy.engine.cursor.CursorResult object at 0x...>
INSERT INTO some_table (x, y) VALUES (?, ?)
[...] ((1, 1), (2, 4))
<sqlalchemy.engine.cursor.CursorResult object at 0x...>
COMMIT
```

```
# "begin once"
>>> with engine.begin() as conn:
...     conn.execute(
...         text("INSERT INTO some_table (x, y) VALUES (:x, :y)"),
...         [{"x": 6, "y": 8}, {"x": 9, "y": 10}]
...     )
BEGIN (implicit)
INSERT INTO some_table (x, y) VALUES (?, ?)
[...] ((6, 8), (9, 10))
<sqlalchemy.engine.cursor.CursorResult object at 0x...>
COMMIT
```

In both cases we are using "bind parameters":
Bind parameters are specified by name, using the format `:name`. 

### Basics of Statement Execution

#### Fetching Rows

This illustrates the [`Result`][5] object.

```
>>> with engine.connect() as conn:
...     result = conn.execute(text("SELECT x, y FROM some_table"))
...     for row in result:
...         print(f"x: {row.x}  y: {row.y}")
BEGIN (implicit)
SELECT x, y FROM some_table
[...] ()
x: 1  y: 1
x: 2  y: 4
x: 6  y: 8
x: 9  y: 10
ROLLBACK
```

Above, the “SELECT” string we executed selected all rows from our table. 
The object returned is called `Result` and represents an iterable object of result rows.

The [`Row`][6] objects themselves are intended to act like Python [named tuples][7].

[5]: https://docs.sqlalchemy.org/en/14/core/connections.html#sqlalchemy.engine.Result
[6]: https://docs.sqlalchemy.org/en/14/core/connections.html#sqlalchemy.engine.Row
[7]: https://docs.python.org/3/library/collections.html#collections.namedtuple

#### Sending Parameters

The `Connection.execute()` method therefore also accepts parameters, 
which are referred towards as *bound parameters*[8].
They are converted to proper SQL as question marks.

```
>>> with engine.connect() as conn:
...     result = conn.execute(
...         text("SELECT x, y FROM some_table WHERE y > :y"),
...         {"y": 2}
...     )
...     for row in result:
...        print(f"x: {row.x}  y: {row.y}")
BEGIN (implicit)
SELECT x, y FROM some_table WHERE y > ?
[...] (2,)
x: 2  y: 4
x: 6  y: 8
x: 9  y: 10
ROLLBACK
```

[8]: https://docs.sqlalchemy.org/en/14/glossary.html#term-bound-parameters

#### Sending Multiple Parameters

We can send multi params to the Connection.execute() method by passing
a list of dictionaries instead of a single dictionary.

```
>>> with engine.connect() as conn:
...     conn.execute(
...         text("INSERT INTO some_table (x, y) VALUES (:x, :y)"),
...         [{"x": 11, "y": 12}, {"x": 13, "y": 14}]
...     )
...     conn.commit()
BEGIN (implicit)
INSERT INTO some_table (x, y) VALUES (?, ?)
[...] ((11, 12), (13, 14))
<sqlalchemy.engine.cursor.CursorResult object at 0x...>
COMMIT
```

Behind the scenes, the Connection objects uses a DBAPI feature known as [cursor.executemany()][9].

[9]: https://www.python.org/dev/peps/pep-0249/#id18