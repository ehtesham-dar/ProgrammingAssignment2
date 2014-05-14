##  This  function  consists   of   four  nested functions,
##  The following are the functionality of these functions
##      1- 'set' function sets the value of a matrix
##      2- 'get' function gets the value of a matrix
##      3- 'setInverse' function set the value of a matrix which is inversed
##      4- 'getInverse' fucntion get the value of a matrix which is inversed
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## This function set the value of Matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ##This function get the value of the Matrix
    get <- function() x

    ##This function sets the special Matrix for caching
    setInverse <- function(inverse) m <<- inverse

    ##This function get the special Matrix
    getInverse <- function() m

    ##This list cosists of all the nested functions defined above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse  of  the special "matrix" 
## created with the  makeCacheMatrix  function.  However,  it first checks
## that the inverse of the matrix has already  calculated.  If so, it gets 
## the  calcuated  invers  of  the  matrix   from  the cache and skips the 
## computation.  Otherwise, it  calculates  the  inverse  of  the data and 
## sets  the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Try to get the special "matrix" from makeCacheMatrix function
    m <- x$getInverse()
    
    ## Check  that  value of m is null or not,  if  value of 
    ## m is not null it means matrix is inveresed and cached.
    ## so return this cached matrix 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if matrix is not inversed then we get the matrix from 
    ## makeCacheMatrix function and calculate its inverse by 
    ## using solve() function and we will cache the inverted 
    ## matrix
    data <- x$get()
    m <- try(solve(data))
    x$setInverse(m)
    
    ## We will return Return a matrix that is the inverse of 'x'
    m
}
