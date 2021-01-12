import csv 

rows = []
with open('example_write_csv.csv', 'r') as file:
    reader = csv.reader(file)
    header = next(reader)
    print(header)
    for row in reader:
        print(row)
        rows.append(row)

# transpose of list of lists
print('Transpose:')
columns = [list(tuple_column) for tuple_column in zip(*rows)]
for name,col in zip(header,columns):
    print(name, col)