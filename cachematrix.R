## The makeCache function creates a matrix object and caches the inverse of the same 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                          #Set new data as input to matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           #Return/Get new data into matrix 
  setinv <- function(solve) inv <<- solve       #Set the cached inverse value returned from the cacheSolve function
  getinv <- function() inv                      #Return/Get the cached inverse value 
  list(set = set, get = get,                    #List of functions 
       setinv = setinv,
       getinv = getinv)
}


## The CacheSolve function below computes the inverse of the matrix which is returned from the above cache matrix function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                            #Get the cached inverse of matrix
  if(!is.null(inv)) {                          #Determine if cached data can be used based on a prior computation of sam einput
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                              #Get the new set of data for the matrix
  inv <- solve(data,...)                       #Determine new value of inverse to be cached
  x$setinv(inv)                                #Set the new inverse in cache function
  inv                                          #Return a matrix that is the inverse of 'x'
        
}
