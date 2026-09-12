# Matrix inversion is generally a computationally expensive operation, so it can be
# beneficial to cache the inverse of a matrix instead of recalculating it repeatedly.
# The following two functions are used to create and manage a cached matrix inverse.
# makeCacheMatrix creates a list containing functions to:
# - Set the value of the matrix
# - Get the value of the matrix
# - Set the cached inverse of the matrix
# - Get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set,
get = get,
setinverse = setinverse,
getinverse = getinverse)
}

# The following function returns the inverse of the matrix.
# It first checks whether the inverse has already been calculated.
# If the inverse is available in the cache, it returns the cached result
# without recalculating it.
# If the inverse is not available, it calculates the inverse and stores
# the result in the cache using the setinverse function.
# This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
i <- x$getinverse()
if (!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
