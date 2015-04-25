## Put comments here that give an overall description of what your
## functions do 
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-MULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function()m
  list(set=set,get=get,setmean=setmean,getmean=getmean)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m))
  {
    message("getting the cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse()
  m
}
