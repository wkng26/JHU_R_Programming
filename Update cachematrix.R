## The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
        set <- function(y) {
                ## setting the value of the matrix
                x <<- y
                inv <<- NULL
        }
        
        ## getting the value of the matrix
        get <- function() x
        
        ## setting the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ## getting the value of the inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix" created with the first function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function


cachesolve <- function(x, ...) {
	
	## getting the value of the inverse of x and assigning it to variable inv
        inv <- x$getinverse()
	
	## checking whether the inverse has already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
	
	## calculating the inverse of the data
        inv <- solve(data, ...)
	
	## setting the value of the inverse in the cache 
        x$setinverse(inv)
	
        inv
}

