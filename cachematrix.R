### This function, makeCacheMatrix, creates a special matrix which is a list containing
### a function to set the value of the matrix, get the value of the matrix,
### set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse 
      getinverse <- function() m
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


### This function, cacheSolve, returns the inverse of the matrix. If the matrix
### has already been calculated, then it does not calculate it, but displays the
### inverse from the cache to save computation resources

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
          message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}

### Testing on sample data using the code below shows that the functions
### work as expected and the cacheSolve does not duplicate the calculation
### but instead retrieves from the cache if the matrix inverse was already calculated

x = rbind(c(5,10),c(-.5,6))
x
matrix = makeCacheMatrix(x)
matrix$get()
cacheSolve(matrix)
cacheSolve(matrix)
