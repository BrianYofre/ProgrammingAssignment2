## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inve<-NULL
  set<-function(y){
    x<<-y
    inve<<-NULL
  }
  get<- function() {x} #get matrix x
  setInverse<- function(inverse){inve<<-inverse}
  getInverse<- function(){inve} #inverse of matrix
  list(set=set,get = get,setInverse= setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inve<-x$getInverse()
  if(!is.null(inve)){
    message("getting cached data...")
    return(inve)
  }
  data <- x$get()
  inve<- solve(data,...)    #calculate inverse
  x$setInverse(inve)
  inve ## Return a matrix that is the inverse of 'x'
}
