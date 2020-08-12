## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
  
    #1. set the value of the matrix
    set<-function(y){x<<-y;inv<<-NULL}
    #2. get the value of the matrix
    get<-function() x
    #3. set the value of the inverse
    setinv<-function(inverse_val) inv<<-inverse_val  
    #4. get the value of the inverse
    getinv<-function() inv
    
    #return the list of the result
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #x is the result of the makCacheMatrix funciton 
        #... is the extra option about the solve function
    inv<-x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
      }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    
    return(inv)
}
