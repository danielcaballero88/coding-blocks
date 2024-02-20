"""
Let's check how this api retrieves movies and how to access movie data
"""
from imdb import IMDb

ia =  IMDb()

movies = ia.search_movie('matrix')
for movie in movies:
    print(movie)
# The Matrix
# Matrix
# Matrix
# The Matrix 4
# The Matrix Reloaded
# The Matrix Revolutions
# The Matrix Revisited
# Matrix xxx parody
# The Animatrix
# The Matrix
# A Glitch in the Matrix
# Enter the Matrix
# The Matrix
# Threat Matrix
# Escape the Matrix
# The Matrix Online
# A glitch in the Matrix
# The Matrix: Reborn
# The Matrix: Path of Neo
# Matrixx


movie0 = movies[0]
for key in movie0.keys():
    print(key)
# title
# kind
# year
# cover url
# canonical title
# long imdb title
# long imdb canonical title
# smart canonical title
# smart long imdb canonical title
# full-size cover url
