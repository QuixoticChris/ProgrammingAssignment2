## The function 'makeCacheMatrix' stores a matrix and it's internal functions can be called 
## to set the inverse of the matrix. get and getinverse can also be called for the matrix to return
## it's value and it's inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function calls out to makeCacheMatrix's internal functions to see
## if x has an inverse set. If set, it returns the inverse value. If it's
## not set, it gets the matrix through the method above and sets the inverse
## value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If no inverse matrix found, get the matrix, solve for the inverse
        ## set it 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
