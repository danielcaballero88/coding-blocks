"""
Let's check the Movie object information
"""

"""
Let's print a nice list of movies
"""
from imdb import IMDb

ia =  IMDb()

# Let's do a search
movies = ia.search_movie('matrix')
# this retrieves a list of imdb.Movie objects
# one of these is
movie0 = movies[0]

# After a search (instead of a get) there is no much information
# I need to fetch some by using the update method
ia.update(movie0)
print("Current info:")
for item in movie0.current_info:
    print(item)
# main
# plot
# synopsis

# Those were infosets, let's see the actual info and keys to access that info

# And this is a LOT of info
for key in movie0.infoset2keys:
    print("="*30)
    print(f"{key}: {movie0.infoset2keys[key]}")
print("="*30)
# ==============================
# main: ['original title', 'cast', 'genres', 'runtimes', 'countries', 'country codes', 'language codes', 'color info', 'aspect ratio', 'sound mix', 'box office', 'certificates', 'original air date', 'rating', 'votes', 'cover url', 'imdbID', 'plot outline', 'languages', 'title', 'year', 'kind', 'directors', 'writers', 'producers', 'composers', 'cinematographers', 'editors', 'editorial department', 'casting directors', 'production designers', 'art directors', 'set decorators', 'costume designers', 'make up department', 'production managers', 'assistant directors', 'art department', 'sound department', 'special effects', 'visual effects', 'stunts', 'camera department', 'animation department', 'casting department', 'costume departmen', 'location management', 'music department', 'script department', 'transportation department', 'miscellaneous', 'akas', 'writer', 'director', 'top 250 rank', 'production companies', 'distributors', 'special effects companies', 'other companies']
# ==============================
# plot: ['plot', 'synopsis']
# ==============================

# We can access that information using the keys
plots = movie0["plot"]
# In the case of "plot", there are several apparently
# I'm guessing the first is the good one
for k, plot in enumerate(plots):
    print("="*30)
    print(f"Plot {k}: {plot}")
print("="*30)
# ==============================
# Plot 0: When a beautiful stranger leads computer hacker Neo to a forbidding underworld, he discovers the shocking truth--the life he knows is the elaborate deception of an evil cyber-intelligence.
# ==============================
# Plot 1: Thomas A. Anderson is a man living two lives. By day he is an average computer programmer and by night a hacker known as Neo. Neo has always questioned his reality, but the truth is far beyond his imagination. Neo finds himself targeted by the police when he is contacted by Morpheus, a legendary computer hacker branded a terrorist by the government. As a rebel against the machines, Neo must confront the agents: super-powerful computer programs devoted to stopping Neo and the entire human rebellion.::redcommander27
# ==============================
# Plot 2: Have you ever had a dream that you were so sure was real? What if you couldn't awaken? How would you know the difference between dream and reality? When a beautiful stranger (Carrie-Anne Moss) leads computer hacker Neo (Keanu Reeves) to a forbidding underworld, he discovers the shocking truth--the life he knows is the elaborate deception of an evil cyber-intelligence. Neo joins legendary and dangerous rebel warrior Morpheus (Laurence Fishburne in the battle to destroy the illusion enslaving humanity.
# ==============================
# Plot 3: During the year 1999, a man named Thomas Anderson (also known as Neo), lives an ordinary life. A software techie by day and a computer hacker by night, he sits alone at home by his monitor, waiting for a sign, a signal - from what or whom he doesn't know - until one night, a mysterious woman named Trinity seeks him out and introduces him to that faceless character he has been waiting for: Morpheus. A messiah of sorts, Morpheus presents Neo with the truth about his world by shedding light on the dark secrets that have troubled him for so long.::Anthony Pereyra {hypersonic91@yahoo.com}
# ==============================