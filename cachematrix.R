# The code uses makeCacheMatrix() function to create objects, then access those objects with cacheSolve() function.
# If the inverse of the matrix has not yet been calculated cacheSolve() function calculates 
	# the inverse matrix and stores it in the object created by makeCacheMatrix() function, then returns the inverse matrix.  
# If the inverse matrix has been  calculated earlier then cacheSolve simply fetches it and returns the inverse matrix value 


## comments describing makeCacheMatrix() function

# the function take matrix as an input
# inverse is my 'inverse matrix' and it is reset to NULL every time makeCacheMatrix() function is called
# the set function defined inside the makeCacheMatrix() function environment takes an input matrix and saves 
# the input by super-assignment and resets the inverse to NULL 
# get function just returns the value of the original matrix
# the set_inverse() function is called by cacheSolve() during the first 
# cacheSolve() access and it stores the value of the inverse using super-assignment
# the get_inverse() function returns the cached value to cacheSolve() function on subsequent accesses
# makeCacheMatrix function finally returns a list of the objects (functions) it has created

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set_matrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(solve) inverse <<- solve
        get_inverse <- function() inverse
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## comment describing cacheSolve() function

# the input 'x' of this function is an object created by makeCacheMatrix() function
# The function first accesses the object 'x' and gets the value of the inverse matrix
# Then if inverse matrix was already cached (not NULL) ...
	# then the message "getting cached data" is sent to the console
	# and the inverse matrix is returned and the program ends
# Otherwise (if x$get_inverse returned NULL) the function accesses the object 'x' again 
	# and gets the value of the matrix 
	# and calculate the inverse
# Then the calculated inverse matrix is stored in 'x'
# Finally the cacheSolve() function returns the inverse matrix

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        my_matrix <- x$get_matrix()
        inverse <- solve(my_matrix, ...)
        x$set_inverse(inverse)
        inverse
}		

