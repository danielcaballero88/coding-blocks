import csv 

n = 7
col1 = [1,2,3,4,5,6,7] 
col2 = ['a', 'b', 'c', 'd', 'e', 'f', 'g'] 
cols = [col1,col2]
header = ['col1','col2']

# writing one row at a time
with open('example_write_csv.csv', 'w') as file: 
    writer = csv.writer(file) 
    writer.writerow(header)
    for i in range(n): 
        row = [col[i] for col in cols]
        writer.writerow(row)
    
# transposing cols into rows and writing all rows at once
rows = [list(tuple_row) for tuple_row in zip(*cols)]
with open('example_write_csv_2.csv', 'w') as file: 
    writer = csv.writer(file)
    writer.writerow(header)
    writer.writerows(rows)