## Put comments here that give an overall description of what your functions do
## The objective of the funtion is to save processing time and effort by storing the results 
## the calculation and using the same subsequently instead of recalculation 

## Write a short comment describing this function
## This fuction uses the scoping rules of R . Lexical scoping allows R to access defining environment and objects 
## in the environment which is the Key rational in developing the functions below 

makeCacheMatrix <- function(x_data = matrix()) {

        mat_inverse <- NULL
        set <- function(y) {
                x_data <<- y
                solve_mat <<- NULL
        }
        get  <- function () x_data
        set_inverse <- function(inverse) mat_inverse <<- inverse
        get_inverse <- function() mat_inverse
        list (set = set ,get = get, set_inverse = set_inverse , get_inverse = get_inverse)
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if (!is.null(inverse)) {
                message ("getting cached data")
                return (inverse)
                
        }
        
        data <- x$get()
        inverse <- solve(data,...)
        x$set_inverse(inverse)
        inverse
}

## this is a test to see if GIT has been set up correctly
x <- matrix (1:4,nrow=2 ,ncol=2)
x_cache <-makeCacheMatrix(x)
cacheSolve(x_cache) # calling the first time -calculation done and stored
cacheSolve(x_cache) # cached data returned without calculation 


