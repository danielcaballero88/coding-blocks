"""
Date and datetime functionality in Pandas
"""
import pandas as pd
from datetime import datetime, timedelta
# 1. Dates as tring

## 1.1 Conversion of a single date
date0 = '01-02-2010'
date0 = pd.to_datetime(date0)
"""
>>> print(date0)
2010-01-02 00:00:00
"""

### Note that it takes the first value as the month, that can be changed:
date0 = '01-02-2010'
date0 = pd.to_datetime(date0, dayfirst=True)
"""
>>> print(date0)
2010-02-01 00:00:00
>>> print(type(date0))
<class 'pandas._libs.tslibs.timestamps.Timestamp'>
>>> print(isinstance(date0, pd.datetime))
True
>>> print(isinstance(date0, datetime))
True
"""

## 1.2 Convertion of a list of dates (as strings)
dates = [
    '01-02-2010',
    '04-08-1988',
    '06-12-2020',
    '27-11-2019',
    '06-02-2015'
]

### As an index
dates = pd.to_datetime(dates)
"""
>>> print(dates)
DatetimeIndex(['2010-01-02', '1988-04-08', '2020-06-12', '2019-11-27',
               '2015-06-02'],
              dtype='datetime64[ns]', freq=None)
"""

### As a series
dates = pd.to_datetime(pd.Series(dates))
"""
>>> print(dates)
0   2010-01-02
1   1988-04-08
2   2020-06-12
3   2019-11-27
4   2015-06-02
dtype: datetime64[ns]
"""

## 2. Dates as datetime objects
date0 = datetime(1988, 8, 4)
"""
>>> print(date0)
1988-08-04 00:00:00
>>> print(type(date0))
<class 'datetime.datetime'>
"""

### 2.1 Convert the dates BEFORE going to pandas
dates = [
    '01-02-2010',
    '04-08-1988',
    '06-12-2020',
    '27-11-2019',
    '06-02-2015'
]
dates = [datetime.strptime(date, '%d-%m-%Y') for date in dates]
"""
>>> print(dates)
[
    datetime.datetime(2010, 2, 1, 0, 0),
    datetime.datetime(1988, 8, 4, 0, 0),
    datetime.datetime(2020, 12, 6, 0, 0),
    datetime.datetime(2019, 11, 27, 0, 0),
    datetime.datetime(2015, 2, 6, 0, 0)
]
"""

### 2.2 Use the datetime dates to create the series
dates = pd.Series(dates)
print(dates[0])
print(type(dates[0]))
"""
>>> print(dates)
0   2010-02-01
1   1988-08-04
2   2020-12-06
3   2019-11-27
4   2015-02-06
dtype: datetime64[ns]
>>> print(dates[0])
2010-02-01 00:00:00
>>> print(type(dates[0]))
<class 'pandas._libs.tslibs.timestamps.Timestamp'>
"""

# 2. Timedeltas

## Prepare a dataframe with a date column
df = pd.DataFrame({
    'id': range(len(dates)),
    'date': pd.to_datetime(dates)
 })
"""
>>> print(df)
   id       date
0   0 2010-02-01
1   1 1988-08-04
2   2 2020-12-06
3   3 2019-11-27
4   4 2015-02-06
"""

## Add a timedelta to the date
df['date'] = df['date'] + pd.Timedelta(2, 's')
"""
>>> print(df)
   id                date
0   0 2010-02-01 00:00:02
1   1 1988-08-04 00:00:02
2   2 2020-12-06 00:00:02
3   3 2019-11-27 00:00:02
4   4 2015-02-06 00:00:02
"""

df['date'] = df['date'] + timedelta(minutes=1)
"""
>>> print(df)
   id                date
0   0 2010-02-01 00:01:02
1   1 1988-08-04 00:01:02
2   2 2020-12-06 00:01:02
3   3 2019-11-27 00:01:02
4   4 2015-02-06 00:01:02
"""

# 3. datetime index

## Set it
df = df.set_index('date')
"""
>>> print(df)
                     id
date
2010-02-01 00:01:02   0
1988-08-04 00:01:02   1
2020-12-06 00:01:02   2
2019-11-27 00:01:02   3
2015-02-06 00:01:02   4
>>> print(df.index)
DatetimeIndex(['2010-02-01 00:01:02', '1988-08-04 00:01:02',
               '2020-12-06 00:01:02', '2019-11-27 00:01:02',
               '2015-02-06 00:01:02'],
              dtype='datetime64[ns]', name='date', freq=None)
"""

## Now we can get records by only the year
"""
>>> print(df.loc['2019'])
                     id
date
2019-11-27 00:01:02   3
>>> print(df.loc[:'2010'])
                     id
date
2010-02-01 00:01:02   0
1988-08-04 00:01:02   1
"""
## Or the year and the month
"""
>>> print(df.loc[:'2019 10'])
                     id
date
2010-02-01 00:01:02   0
1988-08-04 00:01:02   1
2015-02-06 00:01:02   4
"""