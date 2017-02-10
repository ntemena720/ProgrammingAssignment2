## Functions will solve the inverse of a Matrix and store it for efficiency

## Function creates the following task: create, reset, set and retrive the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL # clear inverse value
      set <- function(y){ # get and set new matrix y  
          x <<- y # make y the same as value of x (parent env)
          inv <<- NULL #clear inv value (parent env)
        }
      get <- function() x # provide matrix x value
      setinverse <- function(inversee) inv <<- inversee # set value for inverse matrix
      getinverse <- function() inv #retrive stored inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function checks inverse matric value, if null it will proceed with getting the inverse matrix, if its not nullit will show inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv1 <- x$getinverse() # get stored inverse value
      if(!is.null(inv1)){ # check stored value and show
        message("getting cached data")
        return(inv1)   # return stored value and exit function
      }
      data <- x$get() # if value is null get data
      inv1 <- solve(data,...) # process X 
      x$setinverse(inv1) # store x
      inv1 # return processed value
}
