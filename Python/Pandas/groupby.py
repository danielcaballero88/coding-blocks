import pandas as pd
import numpy as np 
import random

a = np.arange(10) 
b = a*1.5
c = ['a']*3 + ['b']*4 + ['c']*3

df = pd.DataFrame(index=a, data={'col1':b, 'col2':c})
print('df: ')
print(df)

df_grouped = df.groupby('col2')
print('df_grouped: ')
print(df_grouped) 
print('df_grouped.groups: ')
print(df_grouped.groups)

df_grouped_count = df_grouped.count()
print('df_grouped.count(): ')
print(df_grouped.count())

df_grouped_count = df_grouped.mean()
print('df_grouped.mean(): ')
print(df_grouped.mean())
