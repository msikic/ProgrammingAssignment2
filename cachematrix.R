## R programming course - Programming Assignment 2
## These function can be used to create an object that stores a numeric matrix and caches
## it inverse value

## Function creates an object that keeps matrix and its inverse
## The object consists of the matrix, its inverse and the following functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get=get, setinverse = setinverse, getinverse = getinverse)
    
}


## Function calculates the inverse of the matrix created with above functions
## if this value has not been already cached

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
