import random
import pandas as pd 

a = list(range(20))
b = [random.randint(1, 10) for item in a]
c = [random.choice(['a', 'b', 'c']) for item in a]

df = pd.DataFrame(data={'a':a, 'b':b, 'c':c})
print(df)
print('---')

print(df.sort_values(by='b', ascending=False).head(5))

print('---')

print(df.sort_values(by=['b', 'a'], ascending=False).head(5))

print('---')

print(df.nlargest(5, ['b', 'a']))