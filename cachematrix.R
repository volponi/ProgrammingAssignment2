## #######################
##
## Program cache computing and caching of the inverse matrix given
## Part of the Programming Assignment 2 of the Coursera course "R Programming"
##
## Based on the examples given on the Assignment 2
##
## #######################


## Create a cache for the inverse matrix given. 
## This is necessary because sometimes the computed value is expensive to do everytime, 
## so we will store the result for reuse (caching)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## in this case, we are caching the SOLVE function (inverse matrix)
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()       
        
        ## if there is a cached version, just return it
        if(!is.null(m)){
                message("getting cached data for inverse matrix")
                return(m)
        }
        
        # if not, compute the inverse matrix and store it in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}