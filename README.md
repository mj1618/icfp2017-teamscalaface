https://icfpcontest2017.github.io/

./build
./run {port} {nbrains}
./kill {port}

- make sure we can handle surge requests from the server
- test what our largest map ability is and then cull large maps
- test disconnected graphs
- test we work in the VM
- optimise tryFindFurthestTarget

10 mins to build and run the dissapointing Hello, wold! program.
Sign in to gitlab with github? interesting

scala cheatsheet for the win

cool photos - farrah - patrick's burrito

force tail recursion to protect against regular recursion

# Design 3 - Todo (weighted stats on future selection)

Basically on setup predict if futures will succeed based on likelihood of claims to fail on a map.
Use basic stats/magic numbers to let predictor work out claim failure rate as a percentage
Inputs:
 * Rivers / Players - higher should increase claim success
 * Mines - higher should increase claim success

Output: claimSucessRate = A * (Rivers/Players) + B * Mines + c

A, B are likely to be positive

Probably as a Double, e.g. 0.8 means 80% of claims will succeed

if claimSuccessRate > 0.5 (or some other arbitrary success rate?) we should turn on futures?

# Design Thoughts 2 - Implemented
At the moment we are getting all the futures then going for mines

I think if we planted futures along all the paths to the mines would be good, and then:
get all the futures to a mine, then get the mine, then get the futures to the next mine, then get the mine
this is an easy change I think
i will keep the toggle so we can turn it off if it turns out to hurt us
Also: we should assess our targetSites on a per-go basis and skip ahead sites which are easier to reach
e.g. If it is twice as easy to get to target site 2 than target site 1 we should do it (depending on the value of these)
this is the per-go optimization we need


# Design thoughts - Mostly Done
## Setup phase
1. calculate score index for each site (how???) rowan
2. identify features/disjoint networks?
3. sort mines by potential value (matt)
 
## Starting site (adon)
1. Pick mine with highest starting value
2. Pick most recently visited site from history with a path to next most valuable mine
3. Pick most recently visited site from history with an available river
4. give up and pick anything

## Path selectioN
1. Follow shortest path to nearest unconnected mine
2. Lazily evaluate highest value rivers and take them (scott)

## Fallback

