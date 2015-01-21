## The function take an input matrix, create intern method that can set and get the matrix 

## comment describing makeCacheMatrix function

## inverse is my 'inverse matrix' and it's reset to NULL every time makeCacheMatrix is called
## the set function defined inside the makeCacheMatrix environment takes an input matrix and saves 
## the input by super-assignment and resets the inverse to NULL 
## get function just returns the value of the original matrix
## the set_inverse function is called by cacheSolve() during the first 
## cacheSolve() access and it stores the value of the inverse using super-assignment
## the set inverse function returns the cached value to cacheSolve() on subsequent accesses
## makeCacheMatrix function returns a list of the internal functions

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inverse <<- solve
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}		

