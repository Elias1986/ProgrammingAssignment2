## Both functions let you evaluate any matrix, save it in the cache and then run the inverse of it

## makeCacheMatrix has as an input the configuration of a matrix that will be saved in the cache

makeCacheMatrix <- function(g = matrix()) {
       
        mi <- NULL
        set <- function(m) {
                g <<- m
                inv <<- NULL
                
        }
        
        get <- function() g
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve will be the fuction that will run the inverse of whatever finds in the cache

cacheSolve <- function(g, ...) {
       
        mi <- g$getinverse()
        if(!is.null(mi)) {
                return(mi)
        
        }
        
        data <- g$get()
        mi <- solve(data)
        g$setinverse(mi)
        mi

}
