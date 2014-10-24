## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.



## This function take a matrix as argument and return a spical matrix which is a list of function ,getInv to read the inverse of th
##of the matrix and and setInv to set it ,get to read the matrix ,and set to assign value to it.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) Inv <<- Inverse
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The following function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv<- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}
