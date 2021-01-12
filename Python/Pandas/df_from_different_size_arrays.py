#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Fri May  1 08:53:38 2020

@author: dancab
"""

import numpy as np
import pandas as pd 

# Two arrays of different sizes
a = np.arange(3) 
b = np.arange(4)

# Create two dataframes 
dfa = pd.DataFrame(data={'a':a})
dfb = pd.DataFrame(data={'b':b})
print("dfa y dfb")
print(dfa)
print(dfb)

# Combine into one dataframe
df = pd.concat([dfa,dfb], axis=1)
print("df concatenado")
print(df)

# Now replace all NaN with 0 (optional) 
df = df.replace(np.nan, 0)
print("df sin Nan")
print(df)

# Now compute sum over each row 
df['c'] = df.loc[:,'a':'b'].sum(axis=1) # Note that label slicing is inclusive
print("df con operacion en nueva columna") 
print(df)