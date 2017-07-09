## makeCacheMatrix and cacheSolve are functions that cache the inverse of a matrix 

## makeCacheMatrix function defines a matrix object which contains variables and functions that can be
## accessed using the $ sign and changed when necessary. getinverse returns NULL unless cacheSolve function was run.

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) { ## defining the setter function which sets initial values of variables x and inv
                x <<- y 
                inv <<- NULL ## clears any previous values of inv
        }
        get <- function() x ## defining the getter function
        setinverse <- function(solve) inv <<- solve ## applying solve() to inv to get the inverse of the matrix
                                                    ## and saving its value to parent directory.
        getinverse <- function() inv                
        list(set = set, get = get,    ## saving and naming the functions in a list
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function checks for already calculated values for the inverse given 
## as argument x and returns it if available; if not, then it returns a newly calculated value of the inverse.

cacheSolve <- function(x, ...){
        inv <- x$getinverse() ## trying to retrieve a previous value of the inverse  
        if(!is.null(inv)) { ## if inv is not NULL, a cached value for the inverse can be returned
                message("getting cached data")
                return(inv) 
        }
        data <- x$get()
        inv <- solve(data, ...) ## if inv is NULL, the function calculates the inverse
        x$setinverse(inv)
        inv
}
