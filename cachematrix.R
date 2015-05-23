# These two functions implement the computation of the inverse of a matrix,
# reusing a previous calculation if available.
#
# The makeCacheMatrix() function takes a numeric matrix x as input
# and returns four functions: if f <- makeCacheMatrix(x), then:
#
# f$set() is a function that takes a numeric matrix y as input
# and assigns x <<- y and i <<- NULL, both of which are defined outside its scope
# (they belong to the makeCacheMatrix function's environment).
# f$set() is used to input the matrix whose inverse is to be computed,
# and to make makeCacheMatrix() aware that a new matrix has been brought in,
# one whose inverse is yet to be computed (hence i <<- NULL).
#
# f$get() is a function without arguments, that simply returns x,
# the matrix whose inverse we wish to compute.
#
# f$setinv() is a function that takes a number, inv, as input,
# and assigns i <<- inv, which lies outside its scope
# (i belongs to the makeCacheMatrix() function's environment).
# This function makes makeCacheMatrix() aware that the inverse has been computed.
#
# f$getinv is a function without arguments, that simply returns i,
# the inverse of the matrix x. If the inverse is yet to be computed, it returns NULL.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

# The cacheSolve function takes a list f comprised of four functions,
# which is the output of the makeCacheMatrix() function, as input
# (and additional ... arguments to be passed to the solve function),
# and computes the inverse of the matrix it contains, only if it has not been computed before.
cacheSolve <- function(f, ...) {
    # First, check if the inverse has already been computed.
    i <- f$getinv()
    # If the inverse is there, i.e., if i != NULL, then use it!
    # cacheSolve returns i and exits giving a message
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # This portion of the function's code only gets executed if the inverse is not available.
    # First, get the data. The f$get() function simpy returns the numeric matrix
    # which was the input to makeCacheMatrix()
    data <- f$get()
    # Compute the inverse, using any additional arguments passed to the cacheSolve() function
    i <- solve(data, ...)
    # Use the computed value, i, to set the inverse via the f$setinv() function.
    # This call makes makeCacheMatrix() aware that the inverse has been computed,
    # because now i != NULL
    f$setinv(i)
    # Return the inverse
    i
}
