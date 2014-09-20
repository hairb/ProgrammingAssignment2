## These pair functions provide the functionality of computing the 
##    inverse of a matrix while using caching to improve performance and
##    reduce computations.
##
## The functions assumes that the matrix supplied is a 
##    square invertiable matrix.
## 

## MakeCacheMatrix - This function creates an object of a square matrix 
##    in which in addition to the matrix itself - contains its inverse, for 
##    caching purpose
##    i - the inverse of the matrix (will be NULL, if not yet calculated)


makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y){
        x <<-y
        i <<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - This function returns the inverse of the matrix obtained
##    in makeCacheMatrix (the function above). If the inverse of the matrix 
##    is already calculated - It returns the existing calculation. If the 
##    inverse is not yet calculated (NULL was returned from getinverse() )- 
##    It calculated it, sets it in the "matrix" of makeCacheMatrix (using 
##    setinverse(i)), and returns it.

cacheSolve <- function(x, ...) {
  
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached inverse data")
        return (i)
    }
    data <- x$get()
    message("calculating the inverse")
    i <- solve(data,...)
    x$setinverse(i)
    i
}
