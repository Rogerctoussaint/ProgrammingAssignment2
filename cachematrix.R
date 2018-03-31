

## Function to create the matrix object
## Returned list cotnains functions to set x, get x, set the inverse of x, and get the inverse
makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    set<- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(z) inverse <<- z
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse of matrix object x
## Checks if the inverse has already been calculated and cached. If so, it reutnrs that value
cacheSolve <- function(x, ...)
{
    i <- x$getInverse()
    if(!is.null(i))
    {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}