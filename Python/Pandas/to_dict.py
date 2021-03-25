import pandas as pd
import numpy as np


df = pd.DataFrame(
    data={
        "id": [11, 12, 13],
        "a": [1, 4, 3],
        "b": [2, np.nan, np.nan]
    }
)

df = df.set_index(keys="id", drop=True)

print(df)

d = df.to_dict(orient="index")

print(d)