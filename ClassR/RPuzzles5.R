##EXERCISE 5 ###

# scale the matrix coloumn 0 to 1
m <- matrix(1:12, 3,4)

## Rescale each column to range between 0 and 1
# first way
maxs <- apply(m, 2, max) # 2 indicates coloumns
mins <- apply(m, 2, min) # 2 indicates coloumns
scale(m, center = mins, scale = maxs - mins)

# second way
apply(m, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
