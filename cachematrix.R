#The following is a set of function that can cache & compute a matrix. 
#This typiccally time intensive process can be made easier with these functions. 


#set will set the value of the matrix.
#get will get the value of the matrix.
#setinverse will set the inverse value for the matrix.
#getinverse will get the inverse value for the matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


# This function computes the inverse of the special matrix returnd by 
# makeCacheMatrix. If the inverse has already been calculated, the cachesolve will retrive 
# the inverse from  the cashe.  

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
