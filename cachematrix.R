# use the same logic for caching the  mean of a vector, but do it for the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
      # we need to:
      # 1. set the value of the matrix
      # 2. get the value of the matrix
      # 3. set the value of the inverse
      # 4. get the value of the inverse
      
      i <- NULL # this variable is for the inverse value
      set <- function(y) {
            # use the << operator to assign values to x and i different from the current environment
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# use the same logic as the caching the mean of a vector function
cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      
      i <- x$getinv()
      if(!is.null(i)) { 
            # if the inverse is not null, then get the cached inverse data
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...) #use the solve function to compute the inverse of the square matrix
      x$setinv(i)
      return(i)
}
