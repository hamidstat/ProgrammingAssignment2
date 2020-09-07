## caching the inverse of a matrix
## functions do

## Making cache matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse)i<<-inverse
        getInverse<-function()i
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## computing the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        mat<-x$get()
        i<-solve(mat,...)
        x$setInverse(i)
        i
}
