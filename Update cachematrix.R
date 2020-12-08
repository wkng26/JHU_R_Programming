## The first function, makeCacheMatrix creates a special "CacheMatrix", which is a list containing a function to
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
             getinverse = inverse)
}
