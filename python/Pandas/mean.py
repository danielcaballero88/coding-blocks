import pandas as pd 
import numpy as np 

v = np.arange(9).reshape(3,3)
print(v)

df = pd.DataFrame(v, columns=['a','b','c'])
print(df)

# mean of each column
print(df.mean())

# mean of each row 
print(df.mean(axis=1))
