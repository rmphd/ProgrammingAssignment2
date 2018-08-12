## Put comments here that give an overall description of what your
## functions do

## This creates a matrix and caches that matrix within the same function
## or can be used to cache an existing matrix
## the specific functions called within this function are used by cacheSolve
## to retrive or set the inverted matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        ##initialize the value of m to be NULL
        m <- NULL
        ##create the matrix in the working environment by
        ##setting y equal to x in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##get the value of the matrix
        get <- function() x
        ##invert the matrix and reassign m the value of the
        ##inverse to replace the NULL value originally assigned
        setinv <- function(solve) m <<- solve
        ##retreive the inverted matrix from the cache
        getinv <- function() m
        #set named elements to call from the working environment
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}

##This function calculates the inverse of the matrix created in makeCacheMatrix
##If the inverted matrix is not in the cache, the inverse is calculated
#and stored in the cache by this function

cacheSolve <- function(x, ...) {
        ## Retrieve a matrix that is the inverse of x if it exists
        m <- x$getinv()
        ##If the inverse does not exist, create the inverse matrix in
        ##the working environment
        ##Return the cached inverse matrix and the message below
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##Create the inverse matrix since it does not exist
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}