##
## sample usage
## > mx <- makeCacheMatrix(matrix(1:4, 2, 2))
## > mx_inverse <- cacheSolve(mx)
## > mx_inverse
## > mx_inverse <- cacheSolve(mx)
## > mx_inverse
##

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
        ## inverseMatrix is an object that is accessed in different environments that contains
        ## the inverse of a matrix object
		inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                
                ## inverseMatrix is set to NULL if matrix is changed 
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
        
		getInverseMatrix <- function() inverseMatrix
        
        ## return a list that contains getter and setter functions to a matrix object
        ## 1. get and set the value of the matrix
        ## 2. get and set the inverse of the matrix
        list (
            set = set, get = get,
            setInverseMatrix = setInverseMatrix,
            getInverseMatrix = getInverseMatrix
        )
}

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverseMatrix()
        
        ## check if the inverse of matrix is cached or not
        if(!is.null(inverseMatrix)) {
                message("Getting cached inverse matrix!")
                
                ## return cached inverse matrix.
                return(inverseMatrix)
        }
        
        ## otherwise, get the matrix object and compute its inverse matrix
		## assume that the matrix supplied is always invertible
        data <- x$get()                    
        inverseMatrix <- solve(data, ...)
        
		## cache the inverse of matrix 
        x$setInverseMatrix(inverseMatrix)
        
        ## return the inverse of a matrix object
        inverseMatrix
}
