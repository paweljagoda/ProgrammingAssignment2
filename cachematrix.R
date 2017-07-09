#makeCacheMatrix and cacheSolve are functions that cache the inverse of a matrix 

#makeCacheMatrix function defines a matrix object which contains variables and functions that can be
#accessed using the $ sign and changed when necessary. getinverse returns NULL unless cacheSolve function was run.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) { #defining the setter function which sets initial values of variables x and m
                x <<- y 
                m <<- NULL #clears any previous values of m
        }
        get <- function() x #defining the getter function
        setinverse <- function(solve) m <<- solve #applying solve() to m to get the inverse of the matrix
                                                  #and saving its value to parent directory.
        getinverse <- function() m                
        list(set = set, get = get,    #saving and naming the functions in a list
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve function checks for already calculated values for the inverse given 
#as argument x and returns it if available; if not, then it returns a newly calculated value of the inverse.

cacheSolve <- function(x, ...){
        m <- x$getinverse() #trying to retrieve a previous value of the inverse  
        if(!is.null(m)) { #if m is not NULL, a cached value for the inverse can be returned
                message("getting cached data")
                return(m) 
        }
        data <- x$get()
        m <- solve(data, ...) #if m is NULL, the function calculate the inverse
        x$setinverse(m)
        m
}
