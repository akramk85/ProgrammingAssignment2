## This package gets a matrix and calculates its inverse and it chaches
## the inverse of the matrix


## The "makeCacheMatrix" function takes the matrix and returns a list of functions 
## that they set and get the matrix and set and get the inverse of matrix respectivley

makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     set<-function(y){
          x<<-y
          inv<<-NULL
     }
     get<-function() x
     setinv<-function(solve) inv<<-solve
     getinv<-function()inv
     list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## cachSolve calls the function and if the inverse of matrix is NULL, it calculates 
## the inverse, if not it returns cache.


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
        
}
