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

# groupby a -> single index
print( df.groupby('a').count() )

# groupby a -> a not as index
print( df.groupby('a', as_index=False).count() )

# groupby a,b -> MultiIndex
print( df.groupby(['a','b']).count() )

# groupby a,b,c -> MultiIndex (3 indices) 
print( df.groupby(['a','b','c']).count() )

# cancel multiindex option
# groupby a,b -> MultiIndex
print( df.groupby(['a','b'], as_index=False).count() )


# get values for an index 
df2 = df.groupby(['a','b']).count()
print(df2)
print( df2.index.get_level_values('b') )
