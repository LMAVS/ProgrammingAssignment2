# special matrix object type, that holds both the matrix and its inverse

# -------------------------

# create a special list object that holds both a matrix and its inverse
# if it has been calculated
# assumes matrix is invertible

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL						

        set <- function(y) {			
                x <<- y
                inv <<- NULL
        }
        get <- function() x				
        setinv <- function(solve) inv <<- solve	
        getinv <- function() inv

        list(set = set, get = get, 		
             setinv = setinv,
             getinv = getinv)
}


# calculate and stores the matrix inverse, if it has not been done before

cacheSolve <- function(x, ...) {

        inv <- x$getinv()
		
        if(!is.null(inv)) {
                message("returning cached data")
                return(inv)
        }
		
        data <- x$get()		
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
