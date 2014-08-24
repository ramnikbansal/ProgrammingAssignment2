## Objective:
##-----------
## The purpose of the two functions below is to save computation
## time by caching computation already performed.
## the first function makeCacheMatrix is generic in nature
## and can be applied to any matrix for caching a computation
## where input is a matrix. The second function cacheSolve 
## is used to either give back the cached value of inverse of
## a matrix or compute the inverse of a matrix. Actually function can
## be modified to perform any desired computation on an input 
## which is a special Matrix created using "makeCacheMatrix" function.
## -------
## Benefit:
## -------
## This method of creating a special Matrix has benefit in
## saving computation time when large data is involved and
## thus computation time is high, especially when same computed
## value is required multiple times.


## ------------------------
## Function makeCacheMatrix
## -------(code written on 24-Aug-2014 by Ramnik Bansal)
## This function creates a Special Matrix that also
## saves the inverse of the matrix for future use,
## computed using another function "cacheSolve"
##
##----Return Value
## The function returns a LIST of 4 functions associated with input matrix
## The Set,Get,SetInverse and GetInverse functions.
## The function uses the concept of "<<-" assignment operator.
makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        setf<-function(y = matrix()){
                x<<-y
                inverse<<-NULL
        }
        getf<-function() {x}
        
        setinversef<-function(invmatrix) inverse<<-invmatrix
        getinversef<-function() inverse
        list(set = setf, get =getf, setinverse=setinversef,getinverse=getinversef)
}


## -----
## Function cacheSolve :
## Input: a special matrix created as output of makeCacheMatrix
## Limitation: This function works on a matrix which is Invertible.
##
cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        if(is.null(x$getinverse())){
                print("Computing Inverse Matrix...")
                invmatrix<-solve(x$get())
                x$setinverse(invmatrix)
                return(invmatrix)
        }        
        else {
                invmatrix<-x$getinverse()
                print("Returning Cached Inverse Matrix...")
                return(invmatrix)
        }
}
