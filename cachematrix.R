## The functions are designed to provide facility for caching inverse matrix
## Flow/usage:
### 1) Create object that would hold source matrix, its inverse matrix and methods to access them.
###      The step # is accomplished by calling makeCacheMatrix function with source matrix as an input
### 2) Call cacheSolve function with object created in step 1. If the function is called first time inverse matrix would be
###        calculated. Any subsequent times the inverse matrix will be retrieved from cache
### Example: 
# src_mat = matrix(c(4,3,3,2),2,2)
# s_m=makeCacheMatrix(src_mat)
# cacheSolve(s_m)


# Function to create source matrix object with methods. When object first created, inverse matrix variable is NULL
makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse = function(solve) m <<- solve
    getinverse = function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Function to calculate or retrieve inverse matrix. When called first with any object, it will calculate inverse matrix.
# When called second any subsequent time, the inverse matrix is being returned from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getinverse()
    if(!is.null(m)) {
        #message("getting cached inverse matrix")
        return(m)
    }
    src_matrix = x$get()
    m = solve(src_matrix, ...)
    x$setinverse(m)
    m
}
