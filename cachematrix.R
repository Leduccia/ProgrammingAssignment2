## The following functions create the inverse of a given matrix and cache it
## to spare computational power 

## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## The return value of this function is a list whose elements are functions.

makeCacheMatrix <- function(x = matrix()) {
    # set the inverse to NULL as a placeholder for future values
    m <- NULL 
    # define a function to set the matrix, x, to a new matrix, y,
    # and resets the mean, m, to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # return the matrix
    get <- function() x
    # set the inverse, m, to "solve"
    setinverse <- function(solve) m <<- solve
    # return the inverse, m
    getinverse <- function() m
    #return the 'special matrix' containing the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by 
## "makeCacheMatrix". If the inverse has already been calculated and the matrix 
## has not changed, then the following function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # assign to m the value from getinverse(m) that is NULL
    m <- x$getinverse()
    # if the stored inverse is not NULL, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if the inverse is not stored, assign to data the matrix x
    data <- x$get()
    # calculate the inverse and assign it to m
    m <- solve(data, ...)
    # store the inverse
    x$setinverse(m)
    # return it
    m
}
