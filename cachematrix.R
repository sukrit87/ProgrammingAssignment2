##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1> set the value of the vector
## 2> get the value of the vector
## 3> set the value of the mean
## 4> get the value of the mean
## The next function, cacheSolve draws value from the makeCacheMatrix, checkes whether the inverse of the matrix has been cached. If not then it computes the inverse and returns the value
## makeCacheMatrix does the act of caching the data and cacheSolve is the computing function which draws value from the former to compute the inverse.
  
makeCacheMatrix <- function(x=matrix()){
   inv<-NULL
   set<-function(y){
      x<<-y
      inv<<-NULL
   }
   get<-function() {x}
   setinv<-function(z){
      inv<<-z
   }
   getinv<-function(){inv}
   list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cacheSolve<-function(x){
   inverse<-x$getinv()
   if(!is.null(inverse)){
      print("Getting cached Data")
      return(inverse)
   }
      data<-x$get()
      inverse<-solve(data)
      x$setinv(inverse)
      return(inverse)
}
