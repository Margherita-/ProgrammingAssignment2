## This functions create and manage special object that suport chached data 
# in order to reduce the overall computational time for recurrent operations
# (the inverse of a matrix in this case)


## create a special object that stores a matrix and cache's its inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # save the data in a different enviroment
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    
    # bild the returned list
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)

}


## Calculates the inverse of the special matrix. Checks to see if the iverse 
# has already been calculated, if so it gets data from the cache, otherwise 
# calculates it and sets the value in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    
    # check the cache and eventually returns it
    if(!is.null(i)) {
        message("getting cached inverted matrix")
        return(i)
    }
    
    # calculates the inverse and store it in the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}