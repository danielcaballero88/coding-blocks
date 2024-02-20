# Redis

## Types and common operations

These commands correspond to the Redis command line (`redis-cli`).

### Strings

1. Set a new key/value pair
`set key value`
Returns `OK` or raises an error.
2. Delete a key/value pair
`del key`
Returns the number of deleted items (typically 1 or 0)
3. Get a key/value pair value
`get key`
Returns the value or `nil` if not present.

### Lists

1. Push an element to the right of the list (append).
(note: if the key is not assigned, the list is created)
`rpush key item`
Returns an integer (number of items in the list)
2. Idem to the left (preppend):
`lpush key item`
Returns an integer (number of items in the list)
3. Get an item:
`lindex key idx`
Returns the value ir `nil` if that index is out of range.
4. Pop an element:
`rpop key`
`lpop key`
Returns the value

### Sets

1. Add an element (create the set if not exists):
`sadd key item`
Returns the number of new items (typically 1 or 0)
2. List all members:
`smembers key`
Returns the set items as a list.
3. Remove an item:
`srem key item`
Returns the number of deleted items.
4. Check if an item is present in the set:
`sismember key item`
Returns 1 or 0.

### Hashes

Hashes are hashed lists, like dictionaries or sub-Redis.

1. Add a new key/value:
`hset key subkey value`
Returns the number of new items (typically 1 or 0)
2. Get all pais:
`hgetall key`
Returns a list of alternating keys and values.
3. Delete a pair:
`hdel key subkey`
Returns the number of deleted items.
4. Get a value:
`hget key subkey`
Returns the value.

### ZSets (Sorted sets)

Like Redis HASHes, ZSETs also hold a type of key and value. The keys
(called members) are unique, and the values (called scores) are limited to
floating-point numbers.

1. Add a pair:
`zadd key score member`
2. Get all items:
`zrange key 0 -1 [withscores]`
If `withscores` options is given, it returns an alternating list of members and
scores, else, only the members.
3. Get members filtering by score:
`zrangebyscore key score1 score2`
Returns all members with score between `score1` and `score2`.
