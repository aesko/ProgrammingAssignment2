## These functions are used to store a matrix in the cache, calculate its
## inverse matrix and store it in the cache as well.

## Store and retrieve a matrix and  its inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
        #store NULL in i to signal cache empty
        i <- as.null()
        
        #set and get functions for storing and retrieving the matrix
        set <- function(y) {
                x <<- y 
                i <<- as.null()
        }
        get <- function() x
        
        #setINV and getINV for storing and retriving inverse matrix
        setINV <- function(INV) i <<- INV
        getINV <- function() i
        
        #list of subfunctions
        list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)   
}


## Retrieve inverse matrix from cache. If not yet in cache,
## retrieve original matrix, calculate inverse and store in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #retrieve and temporarily store inverse matrix
        i <- x$getINV()
        
        #condition and for returning cached inverse matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #retrieve and calculation of inverse matrix if not in cache
        data <- x$get()
        i <- solve(data, ...)
        x$setINV(i)
        i
}
