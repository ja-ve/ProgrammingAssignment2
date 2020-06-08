#the function makecacheMatrix creates an object consisting of functions which allow you to:
#set the value of the matrix, get the value, set its inverse once computed and retrieve the inverse if it already has been computed. 

makecacheMatrix <- function( x = matrix() ) {
  i <- NULL
  set <- function(y) { #sets the value of the matrix to y
    x <<- y
    i <<- NULL
  }
  get <- function() x #gets the value of the matrix
  setinverse <- function(inverse) i <<- inverse #sets the inverse
  getinverse <- function() i #gets the inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  #creates a list of the four functions defining makecacheMatrix in the parent environment
}

#cacheSolve finds and returns the inverse of a matrix created using makecacheMatrix by either: 
#looking for a cached computation that had been done earlier, or just computing the inverse.

#The result will be stored in the matrix object created by makecacheMatrix.

cacheSolve <- function(x) {
  i <- x$getinverse() #get the inverse from makecacheMatrix object, which is null if it hasn't been computed yet
  if(!is.null(i)){ #check whether the inverse has already been computed
    message("retrieving cached calculation")
    return(i) #return the inverse if it was already computed
  }
  matrix <- x$get() #get the matrix from makecacheMatrix
  i <- solve(matrix) #find the inverse of the matrix
  x$setinverse(i) #store the result in makecacheMatrix
  i
}

#Example use: 
#To compute the inverse of matrix A:

#myMatrix <- makeCacheMatrix()
#myMatrix$set(A)
#cacheSolve(myMatrix)
