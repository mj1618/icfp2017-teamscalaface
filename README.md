10 mins to build and run the dissapointing Hello, wold! program.
Sign in to gitlab with github? interesting

scala cheatsheet for the win

cool photos - farrah - patrick's burrito

force tail recursion to protect against regular recursion

# Design Thoughts 2
At the moment we are getting all the futures then going for mines

I think if we planted futures along all the paths to the mines would be good, and then:
get all the futures to a mine, then get the mine, then get the futures to the next mine, then get the mine
this is an easy change I think
i will keep the toggle so we can turn it off if it turns out to hurt us
Also: we should assess our targetSites on a per-go basis and skip ahead sites which are easier to reach
e.g. If it is twice as easy to get to target site 2 than target site 1 we should do it
this is the per-go optimization we need


# Design thoughts - Mostly Done
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

