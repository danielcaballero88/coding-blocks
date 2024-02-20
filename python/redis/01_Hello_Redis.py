import redis
import sys

# Create connection to server
# (ensure that redis server is running)
try:
    conn = redis.Redis()
except redis.exceptions.ConnectionError:
    print('Error: cannot connect to server')
    sys.exit()

# Store a key/value: 'hello': 'world'
conn.set('hello', 'world')

# Get the value
value = conn.get('hello')
# value = b'world'
print('hello: ', value)


