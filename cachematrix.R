## Put comments here that give an overall description of what your
## functions do
## The defined set of functions is used to find the inverse of a 
## matrix and cache it,later when the inverse of same matrix is 
## required it is then retrived from the cache file

## Write a short comment describing this function
## "makeCacheMatrix" is a function which is used to store and 
## retrive the data, that is it is used to store the matrix and 
## the precalculated inverse value and then retrive it if the same 
## value is required again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinv <- function(inv) m <<- inv 
  getinv <- function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function
## "cacheSolve" is a function which is used to search for the cached 
## value, if it exists then retrive it or else calculate the inverse 
## and cache it for future purpose.
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
  

##--------------------sample usage of function------------------------
## >m<-matrix(rnorm(16),4,4)
## >m1<-makeCacheMatrix(m)
## >cacheSolve(m1)

#           [,1]      [,2]      [,3]       [,4]
#[1,]  2.3605498 -9.399904 -9.380738  22.507979
#[2,] -2.0578459  4.680838  3.082898  -9.028224
#[3,]  0.3022307  0.841626  2.175312  -2.103137
#[4,] -1.1066555  8.598724  8.924224 -20.044345

## >cacheSolve(m1)
#getting cached data
#           [,1]      [,2]      [,3]       [,4]
#[1,]  2.3605498 -9.399904 -9.380738  22.507979
#[2,] -2.0578459  4.680838  3.082898  -9.028224
#[3,]  0.3022307  0.841626  2.175312  -2.103137
#[4,] -1.1066555  8.598724  8.924224 -20.044345
  

