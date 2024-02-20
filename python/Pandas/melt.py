import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns


df = pd.DataFrame({'X_Axis':[1,3,5,7,10,20],
                   'col_2':[.4,.5,.4,.5,.5,.4],
                   'col_3':[.7,.8,.9,.4,.2,.3],
                   'col_4':[.1,.3,.5,.7,.1,.0],
                   'col_5':[.5,.3,.6,.9,.2,.4]})
print(df)
print('\n')


df = df.melt(id_vars=['X_Axis'], var_name='cols',  value_name='vals')
print(df)
print('\n')

sns.set()
sns.catplot(x="X_Axis", y="vals", hue='cols', data=df, kind='point')
plt.show()