
#this code is prepared by M_S_Islam
#Nov_15, 2020
#this code will cache the inverse matrix of a square matrix and it's
#calculated

makeCacheMatrix <- function(x=matrix()) {     #anniuncing the function
  va<-NULL           # va = inverse function
  nf<- function(y) {     #nf = for y function
    x<<-y
    va<<- NULL
  }
  get<-function(){x}    #getting the function of x
  sinv<-function(inverse){va<<-inverse}   #setting inverse
  getinv<-function() {va}                 #getting inverse
  list(nf = nf, get = get, sinv=sinv, getinv = getinv) 
}

cacheSolve<- function(x, ...){
  va<- x$getinv()   #inverse function
  if (!is.null(va)){       #making statement
    message("catching the cached data")
    return(va)
  }
  mat<-x$get() #new matrix
  va<-solve(mat, ...) 
  x$sinv(va) # inverse matrix
  va
}
