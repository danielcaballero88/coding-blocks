# Matrix given in a list of lists 
rows = [
    ['a','b','c'],
    [1,2,3],
    [4,5,6],
    [7,8,9]
]
print('Data in rows: ')
for row in rows:
    print(row)

# Extract header 
names = rows[0]
rows = rows[1:]
print('Header: ')
print(names)

# Transpose
columns = [list(tuple_column) for tuple_column in zip(*rows)]
print('Transpose (Data in columns): ')
for name,col in zip(names,columns):
    print(f'{name}: ', col)