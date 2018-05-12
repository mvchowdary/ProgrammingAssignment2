# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse 
    <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}


##OutPut

# Matrix Creation

my_matrix <- makeCacheMatrix(matrix(4:7, 2, 2))

# Initial Matrix
my_matrix$get()
[,1] [,2]
[1,]    4    6
[2,]    5    7

# First Time Inverse
cacheSolve(my_matrix)
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
# Second  Time Inverse Proof is Getting Cashed Data
cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2