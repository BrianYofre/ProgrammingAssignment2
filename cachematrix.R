## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Set, get and save data 
makeCacheMatrix <- function(x = matrix()) {
  inve<-NULL
  set<-function(y){
    x<<-y
    inve<<-NULL
  }
  #get matrix x
  get<- function() {x} 
  setInverse<- function(inverse){inve<<-inverse}
  #inverse of matrix
  getInverse<- function(){inve} 
  list(set=set,get = get,setInverse= setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## Function to get the cache data
cacheSolve <- function(x, ...) {
  inve<-x$getInverse()
  if(!is.null(inve)){
    message("getting cached data...")
    return(inve)
  }
  data <- x$get()
  #calculate inverse
  inve<- solve(data,...)    
  x$setInverse(inve)
  inve
  ## Return a matrix that is the inverse of 'x'
}
