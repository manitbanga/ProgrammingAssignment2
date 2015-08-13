
## The objective of the funtion is to save processing time and effort by storing the results 
## post calculation of inverse of the matrix and using the same subsequently instead of recalculation 
## This fuction uses the scoping rules of R . Lexical scoping allows R to access defining environment  objects 
## the Key rational used in the  functions


# The function makeCacheMatrix returns a list containing 4 functions ; it has two variables including one 
# argument which is used to store the matrix and the calculated inverse 

makeCacheMatrix <- function(x_data = matrix()) {

        mat_inverse <- NULL # variable to cache the inverse 
        set <- function(y) {  # function to reset the values for data matrix and inverse
                x_data <<- y
                solve_mat <<- NULL
        }
        get  <- function () x_data  # function to fetch matrix 
        set_inverse <- function(inverse) mat_inverse <<- inverse # function to cache the value of inverse
        get_inverse <- function() mat_inverse # function to fetch the value of the inverse 
        list (set = set ,get = get, set_inverse = set_inverse , get_inverse = get_inverse) # the list containing the 4 function is returned by the function
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        inverse <- x$get_inverse()  # gets the value of inverse from the object environment 
        if (!is.null(inverse)) {
                message ("getting cached data")
                return (inverse) # if inverse exist the value is returned from the cache 
                
        }
        
        data <- x$get() # if it the first run value for the matrix is fetched from the defining object environment 
        inverse <- solve(data,...) # inverse is calculated 
        x$set_inverse(inverse) # the value of the inverse is updated in the object environment 
        inverse # the funtion returns the inverse 
}

## this is a test to see if GIT has been set up correctly
x <- matrix (1:4,nrow=2 ,ncol=2)
x_cache <-makeCacheMatrix(x)
cacheSolve(x_cache) # calling the first time -calculation done and stored
cacheSolve(x_cache) # cached data returned without calculation along with message 


