import pandas as pd
import numpy as np 

c1 = np.arange(10)
c2 = c1 / 2

df = pd.DataFrame({'c1':c1, 'c2':c2}) 
print(df)

df2 = pd.concat([df,df]) 
print(df2)
