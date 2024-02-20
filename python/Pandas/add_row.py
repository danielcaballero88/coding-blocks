import numpy as np
import pandas as pd

df = pd.DataFrame(
    {
        'a':[1,1,1,1,2,2,2,2],
        'b':[1,1,1,2,2,2,3,3],
        'c':[1,1,2,2,3,3,4,4],
        'd':[1,2,3,4,5,6,7,8]
    }
)

print(df)

# add new row indicating a new index value
new_row = ['1','2','3','4']
df.loc['new_row'] = new_row 
print(df)

# add new row appending a series
new_row = [5,6,7,8]
df = df.append(new_row)
print(df)
