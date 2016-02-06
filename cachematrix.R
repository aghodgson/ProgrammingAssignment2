## These functions cache the inverse of a matrix

## Caches inverse with four functions

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y) {
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setmi<-function(solve) i<<-solve
        getmi<-function() i
        list(set=set, get=get,
             setmi=setmi,
             getmi=getmi)
}
        
## Computes and returns inverse

cacheSolve <- function(x, ...) {
        i<-x$getmi()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setmi(i)
        i
