## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix and cacheSolve are functions required to do faster operations with a inverse matrix m precomputed   
#
##
# makeCacheMatrix is a function to instance object matrix whith a persistence object matrix m (caching) and 
# basic operations  to assign and obtain the  matrix object m.
# 
#This fuction, makeCacheMatrix creates a instance matrix object with a basic functions to
# set the value to the matrix
# get the value to the matrix
# assigned the value inverse to matrix
# obtain the value inverse to matrix
#
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  assigned<-function(matriz)m<<-matriz
  obtain<-function() m
  list(set=set,get=get,assigned=assigned,obtain=obtain)


}


## Write a short comment describing this function
# cacheSolve is a fuctions to assigned or get a persistent objetc matrix m with the inverse of a square argument matrix x
# 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	m<-x$obtain()
  if(!is.null(m)){
    message("getting cached matrix")
    return (m)
  }
  matriz<-x$get()
  m<-solve(matriz)
  x$assigned(m) 
  m
}
