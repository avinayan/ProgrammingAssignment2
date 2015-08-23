################################################################################
## ** "makeCacheMatrix" stores the input matrix to the cache and the inverse  ##
## matrix to the cache.                                                       ##
## ** It also fetches both the stored cache values.                           ##
## ** If cache value cannot be used it returns a NULL on the invMatrix,       ##
## which can be used as a flag to determine if the inverse of the input       ##
## matrix should be computed again                                            ##
##                                                                            ##
## -- "cacheSolve" computes the inverse of a matrix stores it to the cache    ##
## -- If the "makeCacheMatrix" indicates that a cached values is already      ##
## available, this function used the cached value directly instead of         ##
## computing for the inverse of the matrix                                    ##
##                                                                            ##
## Reference: Prof. Roger Peng's code for computing vector means using        ##
##            cached values.                                                  ##
################################################################################

################################################################################
## Function to receive the input and transform it as:                         ##
## 1. Using "setMatrix" function commit the input as the Matrix cache.        ##
##    If the input matches cache, invMatrix is not null and the inverse       ##
##    computed and stored in the cache is used again.                         ##
## 2. Using "getMatrix" returns the stored invMatrix (using lexical scoping)  ##
## 3. Using "setInvMatrix" commits the calculated inverse matrix to the cache.##
## 4. Using "getInvMatrix" returns the stored inverse matrix value.           ##
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    getMatrix <- function() x
    setInvMatrix <- function(inverse) invMatrix <<- inverse
    getInvMatrix <- function() invMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


################################################################################
## Function to use the input matrix and compute its inverse by:               ##
## 1. If the input matrix is same as the input matrix in the cache, the       ##
##    the inverse matrix computed and stored in the cache is returned         ##
## 2. If the input matrix is not same as the input matrix in the cache, new   ##
##    matrix inverse is computed and returned. The cache is also updated      ##
##    with the newly computed matrix inverse.                                 ##
################################################################################

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix)){
        message("using cached data for inverse matrix")
        return(invMatrix)
    }
    matrix <- x$getMatrix()
    invMatrix <- solve(matrix, ...)
    x$setInvMatrix(invMatrix)
}
