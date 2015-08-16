

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##The purposed function returns the inverse of the matrix. 
##To begin with, it checks whether the inverse has been accessed or not.
##If so, the results will be printed. If not, it would calculate the inverse of matrix and set the value in the cache.


cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}


## Exemplifying.

#x = rbind(c(2, -1/2), c(-1/2, 6))
#s <- makeCacheMatrix(x)
#s$get()
#[,1] [,2]
#[1,]  2.0 -0.5
#[2,] -0.5  6.0

#cacheSolve(s)
 #          [,1]       [,2]
#[1,] 0.51063830 0.04255319
#[2,] 0.04255319 0.17021277
