## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix uses the <<- operator to create a 
## special function that is stored in a separate environment.
## A special function is created in that separate environment which is a list
## containing a function to set/get the value of the specified matrix and 
## calculate the inverse of that matrix using the solve() function.

## Usage Example:
## p <- matrix(c(1,3,4,5,6,78,99,88,39,10,22,25,52,57,198,111), nrow=4, ncol=4)
## a <- makeCacheMatrix(p)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
    

}


## Write a short comment describing this function
## cacheSolve is a function that inverts the matrix created by the function
## makeCacheMatrix (see above).  The function cacheSolve first checks to see
## if the inversion of the specified matrix has already been created and cached
## in the separate envinronemnt.  If so, cacheSolve returns the value of the 
## already inverted matrix as well as a message indicating the information
## returned is from the cached version.  Otherwise, cacheSolve invokes the
## solve() function on the specified matrix, caches that value in the
## separate environment, and returns the value of the inverted matrix to the
## console.

## Usage Example (based on the example above):
## First Invocation (caculates the inverted matrix and stores in cache):
## > cacheSolve(a)
## [,1]        [,2]         [,3]          [,4]
## [1,] -0.220934397 -0.57994977 -0.193989677  7.473485e-01
## [2,]  0.005787523  0.04013473  0.001948949 -2.679750e-02
## [3,]  0.033248593  0.02327973 -0.007026765 -1.499614e-02
## [4,] -0.002124728 -0.01093785  0.008775765 -3.295856e-05
## Second Invocation (retreives previous caculation from cache)
## > cacheSolve(a)
## getting cached data
## [,1]        [,2]         [,3]          [,4]
## [1,] -0.220934397 -0.57994977 -0.193989677  7.473485e-01
## [2,]  0.005787523  0.04013473  0.001948949 -2.679750e-02
## [3,]  0.033248593  0.02327973 -0.007026765 -1.499614e-02
## [4,] -0.002124728 -0.01093785  0.008775765 -3.295856e-05


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinvert(m)
    m
}
