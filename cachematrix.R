## Put comments here that give an overall description of what your
## get(): returns the matrix
## cacheSolve(): Return the inverse

## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        
        set<-function(y){
                x<<-y
                i<<-NULL
        }        
        get<-function() x
        
        setinverse<-function(solve) i<<- solve
        getinverse<-function() i
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: Compute teh inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        matrix<-x$get()
        i<-solve(matrix,...)
        x$setinverse(i)
        
        #Return Inverse
        i
}

