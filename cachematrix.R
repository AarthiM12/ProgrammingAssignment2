## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this makecachematrix function takes the input matrix
##Here the input matix inversed

makeCacheMatrix <- function(x = matrix()) {
             invmat<-NULL    ##invmat will hold the values of the matrix
        set <- function(y){
              x<<-y              ##value of matrix in parent environment
                invmat<<-NULL
        }
        get <- function() x    ## defines the arguments of x
        
        setinverse <- function(inverse) invmat<<-inverse
        getinverse <- function() invmat             ##gets the value of the invmat
        list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
        
}


## cachesolve will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        invmat<-x$getinverse()
        if(!is.null(invmat)){                   ## if invmat not null then it will return invmat
        message("getting the chache")
                return(invmat)
        }
        data<-x$get()
        invmat<-solve(data,...)
        x$setinverse(invmat)
        invmat
        ## Return a matrix that is the inverse of 'x'
}
