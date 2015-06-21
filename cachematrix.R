
## These set of codes calculate the inverse of any non-singular square matrix and use the value in cache memory  
## the next time it is necessary to find the inverse of the same matrix.  

## The function makeCacheMatrix creates a list of functions:
##      setMatrix(x): sets the value of the matrix to 'x'
##      getMatrix() : outputs the matrix to be inverted
##      setInv(y)   : sets the inverse to 'y'
##      getInv()    : outputs the inverse of the matrix (to be calculated in the seperate function, cacheSolve)

## makeCacheMatrix can be used in the following ways:
##      n<-makeCacheMatrix([define or input matrix])
##      n<-makeCacheMatrix() .
## In the latter case, a null matrix is inputted and hence
## you will have to use n$setMatrix([define or input matrix]) to set the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        setMatrix <- function(y) {
                x <<-y
                Inv<<-NULL
        }
        getMatrix <- function() x
        setInv <- function(invMat) inv <<- invMat
        getInv <- function() inv
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)

}


## This function returns the inverse of the inputted matrix.
## It first looks if this has already been calculated and stored in cache
#     If so, it just uses that value
#     If not, it calculates the inverse and stores this value in cache using  
#     the setInvfunction

cacheSolve <- function(x) {
        inv<-x$getInv()
        if(!is.null(inv)) {
                print("Getting cached data")
                return(inv)
        }
        mat <- x$getMatrix()
        inv <- solve(mat)
        x$setInv(inv)
        inv     ## Returns a matrix that is the inverse of 'x'      
}
