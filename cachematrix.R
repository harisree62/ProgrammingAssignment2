## aim is to writer two functions to inverse a matrix

## makeCacheMatrix is a function we are creating an object to store the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {

        inversed<- NULL
        set<- function(a){
        x<<-a
                inversed<<- NULL
                
     }
        get <- function() x
        setInverse <- function(inverse) inversed<<- inverse
        getInverse <- function() inversed
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



## cacheSolve function is written to compute the inverse of the matrix returned by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getInverse()
        if (!is.null(inversed)){
                message("retreiving inverse calculated")
        return(inversed)
        }
matr<- x$get()
inversed<- solve(matr, ...)
x$setInverse(inversed)
        inversed
}



## executed result
##>matd<-makeCacheMatrix(matrix(1:4,2,2))
## >matd$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## >matd$getInverse()
##NULL
## >cacheSolve(matd)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## >cacheSolve(matd)
##retreiving inverse calculated
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
