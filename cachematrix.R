## This file contains a set of functions that cache the inverse of a matrix
## instead of computing it repeatedly.
## It is assumed any supplied matrix is always invertible.


## This function creates a special "matix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse matrix by creating a 1x1 matix with no data
        mat_inv <- matrix()
        
        ## Define the set() function for the object
        set <- function(y) {
                ## Assign input argument to the x object in the parent environment
                x <<- y
                ## Assign the matrix inverse in the parent environment to a 1x1 matrix with no data
                ## Thus clearning any previously cached inverse matrix
                mat_inv <<- matrix()
        }
        
        ## Define the get() function for the object
        get <- function() x
        
        ## Define the setter function for the matrix inverse
        ## Using the <<- operator because mat_inv is defined in the parent environment
        set_mat_inv <- function(inv) mat_inv <<- inv 
        
        ## Define the getter function for the matrix inverse
        get_mat_inv <- function() mat_inv
        
        ## Return the getter and setter functions to the parent environment as a list of named elements
        list(set = set, get = get, set_mat_inv = set_mat_inv, get_mat_inv = get_mat_inv)
}


## This function computes the inverse of a special "matrix" returned 
## by the makeCachMatrix function.
## If the inverse has already been calculated and has not changed, 
## the function retrieves the inverse from the cache instead of re-calculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Start by trying to get the inverse of the matrix
        mat_inv <- x$get_mat_inv()
        
        ## If the inverse is cached (has data), return the cached value
        if(!any(is.na(mat_inv))) {
                message("getting cached inverse data")
                return(mat_inv)
        }
        
        ## If the inverse is not cached, calculate the inverse
        mat <- x$get()
        mat_inv <- solve(mat)
        x$set_mat_inv(mat_inv)
        mat_inv
}
