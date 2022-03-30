## Put comments here that give an overall description of what your
## functions do

## make matrix assign to variable x, and initialize inv to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){       ## if user want to reset matrix 
    x<<-y                 ## reassign "new" matrix to x
    inv<<-NULL            ## reinitialize inv to NULL
  }
  get<-function() x
  
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)) {        ## if user had calculated the same matrix before
    message("getting cached data")
    return(inv)              ## return old result(inv) directly
  }
  data<-x$get()               ## otherwise, get the uncalculated matrix
  inv<-solve(data,...)        ## calculate the inverse matrix
  x$setinverse(inv)           ## reassign inverse matrix
  inv                         ## print the inverse matrix  
}
