## the code consists of 2 functions. First o, we make the matrix reversible, 
##then we get the inverse.

## creating a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
}

## compute the inverse matrix of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        
        m <- x$getinverse()
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
                
        }
        
        data <- x$get()
        m <- inverse(data,...)
        x$setinverse(m)
        m
        
}
