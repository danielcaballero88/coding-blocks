"""
Let's get cover_url
"""
from imdb import IMDb

ia =  IMDb()

# Let's do a search
movies = ia.search_movie('a')
movie0 = movies[0]

for movie in movies:
    print(movie['cover url'] or "No url")