## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function:

## take x as argument, return a list of functions with names set, get, setInverse and getInverse, serving as cache
## upon initialization, i is set to NULL
## use deep assignment arrow to set the value of the matrix and its inverse, the objects whose names are found in the parent environment, not in the current environment 
## take no argument and return x, the value of the matrix
## set the value of i to the calculated inverse from the function below  

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(data) i <<- data
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function:

## calculate the inverse of the matrix created with the above function
## check if the inverse has already been calculated
## if so, get the inverse from the cache and return it
## if not, pass the matrix to data
## and calculate the inverse of data
## and set the value of the inverse in the cache via the setInverse function
## finally, return i, a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) { 
                message("getting cached data")  
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
