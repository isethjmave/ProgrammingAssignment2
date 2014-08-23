## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

## function to create matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initialize inv to empty
  inv<-NULL
  
  #function to set the value of the matrix
  set<-function(y){
    
    #search the environment until the variable 'x' and 'inv' is encountered. 
    #once found assign y and NULL respectively.
    assign(x, y, inherits=TRUE)
    assign(inv, NULL, inherits=TRUE)
  }
  
  #function to get the matrix
  get <- function() x
  
  
  #calculate the inverse value of the matrix store in a lobal variable inv
  setinverse <- function(solve) {
    #search the environment until the variable 'inv' is encountered. 
    #once found assign value solve.
    assign(inv, solve, inherits=TRUE)
    
  }
  
  #get the value of the inv
  getinverse<-function() inv
  
  #assign names to each function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## calculates the inverse value of the matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  #If the inverse has already been calculated (and the matrix has not changed)
  #get the the inverse from the cache
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<-x$get()
  
  #perform 'solve' to get the inverse of the data
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv
}
