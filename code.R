## the f() below provide a way to cache potentially time consuming
## computations.

##makeCacheMatrix creates a list of 4 functions to
## 1. set | changes the matrix stored in the main f()
## 2. get | returns the matrix stored in the main f()
## 3. setmatrix | stores input into the main f()
## 4. getmatrix | gets the input from the main F()

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##cacheSolve uses the list from makeCacheMatrix 
## first it checks to see if the the inverse of matrix exists
## if yes, then the f() will pass back the existing results
## if no, the f() will use solve() to determine the inverse

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setmatrix(m)
  m
}
