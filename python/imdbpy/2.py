"""
Let's print a nice list of movies
"""
from imdb import IMDb

ia =  IMDb()

# Let's do a search
movies = ia.search_movie('matrix')
# this retrieves a list of imdb.Movie objects

for movie in movies:
    print("="*30)
    print(f"{movie['title']} ({movie['year']}) - {movie['kind']} - {movie.movieID}" )
print("="*30)

# ==============================
# The Matrix (1999) - movie - 0133093
# ==============================
# Matrix (2020) - movie - 11749868
# ==============================
# Matrix (1993) - tv series - 0106062
# ==============================
# The Matrix 4 (2021) - movie - 10838180
# ==============================
# The Matrix Reloaded (2003) - movie - 0234215
# ==============================
# The Matrix Revolutions (2003) - movie - 0242653
# ==============================
# The Matrix Revisited (2001) - video movie - 0295432
# ==============================
# Matrix xxx parody (2020) - movie - 13502978
# ==============================
# The Animatrix (2003) - video movie - 0328832
# ==============================
# The Matrix (2016) - short - 9642498
# ==============================
# A Glitch in the Matrix (2020) - movie - 9847360
# ==============================
# Enter the Matrix (2003) - video game - 0277828
# ==============================
# The Matrix (2004) - short - 9851526
# ==============================
# Threat Matrix (2003) - tv series - 0364888
# ==============================
# Escape the Matrix (2012) - video movie - 2579522
# ==============================
# The Matrix Online (2005) - video game - 0390244
# ==============================
# A glitch in the Matrix (2021) - tv miniseries - 13285880
# ==============================
# The Matrix: Reborn (2020) - tv movie - 12355912
# ==============================
# The Matrix: Path of Neo (2005) - video game - 0451118
# ==============================
# Matrixx (2001) - short - 8793748
# ==============================