import pandas as pd
import numpy as np 

c1 = np.arange(10)
c2 = c1 / 2

df = pd.DataFrame({'c1':c1, 'c2':c2}) 
print(df)

# rename column into a new dataframe
df2 = df.rename(columns={'c1':'col1'})
print(df)
print(df2)

# rename in place
df.rename(columns={'c1':'col1'}, inplace=True)
print(df)

# rename a series
s = df['col1']
print(s) 
s.rename('s', inplace=True)
print(s)
