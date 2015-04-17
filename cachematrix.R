## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	  set <- function(y) {
		x <<- y
		inv <<- NULL
	  }
	  get <- function() x
	  set.inv.matrix <- function(inv.matrix) inv <<- inv.matrix
	  get.inv.matrix <- function() inv
	  list(set = set, get = get,
		   set.inv.matrix = set.inv.matrix,
		   get.inv.matrix = get.inv.matrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	inv.matrix <- x$get.inv.matrix()
	  
	  if(!(is.null(inv.matrix))){
		message("getting the cached data")
		return (inv.matrix)
	  }
	  
	  data.matrix <- x$get()
	  inv.matrix <- solve(data.matrix)
	  
	  x$set.inv.matrix(inv.matrix)
	  inv.matrix
}
