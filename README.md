10 mins to build and run the dissapointing Hello, wold! program.
Sign in to gitlab with github? interesting

scala cheatsheet for the win

cool photos - farrah - patrick's burrito

force tail recursion to protect against regular recursion


# Design thoughts
## Setup phase
 #. calculate score index for each site (how???) rowan
 #. identify features/disjoint networks?
 #. sort mines by potential value (matt)
 
## Starting site (adon)
 #. Pick mine with highest starting value
 #. Pick most recently visited site from history with a path to next most valuable mine
 #. Pick most recently visited site from history with an available river
 #. give up and pick anything

## Path selectioN
 #. Follow shortest path to nearest unconnected mine
 #. Lazily evaluate highest value rivers and take them (scott)

## Fallback

