#dummy matrix

x <- matrix(1:4, nrow=2, ncol=2)

#cache matrix function

makeCacheMatrix <- function(x = matrix()) {
  
#set Matrix
  
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  
#get Matrix
  
  get <- function() x
  
# set inverse function
setinverse <- function(inverse) inv <<- inverse


# get inverse function
getinverse <- function() inv

# Encapsulate into a list
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)	
}

#Cache solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # Simply return the computed inverse		
    message("computing cached matrix")
    return(inv)
  }
  
  # Inverse matrix
  data_inv <- x$get()
  
  # Find the inverse
  inv <- solve(data_inv)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # print inverse
  print(inv)    
}


#output 

m <- makeCacheMatrix(x)
s <- cacheSolve(m)
