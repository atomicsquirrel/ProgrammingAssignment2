## Matrix inversion is a time-consuming computation. If the same matrix needs 
## to be inverted numerous times, it is beneficial to calculate the inverse of 
## the matrix once and cache the result for later use, rather than computing it 
## repeatedly. 

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## x: a square, invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    
    inv <- NULL
    set <- function(y) {
            # use `<<-` to assign a value to an object in an environment 
            # different from the current environment. 
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
	# This list provides four functions:
	# x <- makeCacheMatrix(matrix)
    # x$set(newmatrix) # to change the matrix
    # x$get # to retrieve the matrix
    # x$setInv # to set the inverse of the matrix
    # x$getInv # to get the inverse of the matrix

}


## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves the result from the cache.

cacheSolve <- function(x, ...) {
	# retrieve inverse of matrix. It will be NULL if it has not 
	# been calculated
    inv <- x$getinv()
        
    # check if the inverse has already been calculated
    if (!is.null(inv)){
		# if yes, get it from the cache and skip the computation. 
        message("getting cached data")
        return(inv)
    }
        
    # if no, calculate the inverse 
    m <- x$get() # get the matrix data
    inv <- solve(m, ...) # calculate the inverse
        
    # set the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
        
    return(inv)
}
