import pandas as pd
import numpy as np


d = {
    11: {'a': 1, 'b': 2.0}, 
    12: {'a': 4, 'b': np.nan}, 
    13: {'a': 3, 'b': np.nan}
}

print(d)


df = pd.DataFrame(data=d.values(), index=d.keys())

print(df)

df = pd.DataFrame.from_dict(d, orient="index")

print(df)