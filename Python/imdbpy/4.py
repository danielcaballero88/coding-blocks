"""
Let's print info about a single film
Spoiler: by default is very little (only 'main', 'plot', and 'synopsis')
"""
from imdb import IMDb

ia =  IMDb()

# Let's get "The Matrix (1999)""
movie = ia.get_movie('0133093')

for item in movie.current_info:
    print(item)
# main
# plot
# synopsis

for key in movie.infoset2keys:
    val = movie.infoset2keys[key]
    print("="*30)
    print(f"{key}: {val}")
print("="*30)

# ==============================
# main: ['original title', 'cast', 'genres', 'runtimes', 'countries', 'country codes', 'language codes', 'color info', 'aspect ratio', 'sound mix', 'box office', 'certificates', 'original air date', 'rating', 'votes', 'cover url', 'imdbID', 'plot outline', 'languages', 'title', 'year', 'kind', 'directors', 'writers', 'producers', 'composers', 'cinematographers', 'editors', 'editorial department', 'casting directors', 'production designers', 'art directors', 'set decorators', 'costume designers', 'make up department', 'production managers', 'assistant directors', 'art department', 'sound department', 'special effects', 'visual effects', 'stunts', 'camera department', 'animation department', 'casting department', 'costume departmen', 'location management', 'music department', 'script department', 'transportation department', 'miscellaneous', 'akas', 'writer', 'director', 'top 250 rank', 'production companies', 'distributors', 'special effects companies', 'other companies']
# ==============================
# plot: ['plot', 'synopsis']
# ==============================

# Let's print the synopsis and cast
cast = movie['cast']

for person in cast:
    print(type(person), person)