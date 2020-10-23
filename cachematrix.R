## Author: JohnBoldt499
## Last edited: 23/10/2020

## This is the second programming assignment in the course "R programming".
## The assignment consists in building/adapting two functions to keep the inverse //
## of a matrix in cache, in order to keep the result in case its needed again.

## makeCacheMatrix builds a "special" matrix object, which contains 4 "methods"  //
## to set and get the matrix "x" and its inverse "m".
## "m" is set to NULL and "x" is set as an empty matrix by default.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(s = solve(get())) m <<- s
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## cacheSolve looks for the inverse of a matrix "x" (which MUST be a special matrix //
## built with makeCacheMatrix).
## If the inverse hasn't already been calculated, cacheSolve will calculate it, set //
## it into the object and return it. Otherwise it will just return the inverted     //
## matrix already stored in the object.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
