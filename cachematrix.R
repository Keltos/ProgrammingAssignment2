## Purpose of these functions are to cache potential time consuming inverse marix calculations.
## Example x<-makeCacheMatrix(matrix(c(4,3,3,2),2,2)) with its inverse y<-matrix(c(-2,3,3,-4),2,2). 
#--------------------------------------------------------------------------------------------------
#get the matrix: x$get() 
##  solve its inverse xinv<-cacheSolve(x)
##  check its inverse is cached: x$getinv()
## set the matrix to an arbitrary matrix for instance its inverse:  x$set(matrix(c(-2,3,3,-4),2,2))
## set the inverse matrix: x$setinv(y)
#--------------------------------------------------------------------------------------------------

## creates a special matrix containg list of functions to: set/get value/inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        # returns a "cacheable" matrix
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        
        setinv <- function(inversematrix) xinv <<- inversematrix
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
            getinv = getinv)
        
}


## A function which solves the inverse of the matrix and sets it in the cache. 
## If inverse is already been calculated, it retrieves it from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
}
