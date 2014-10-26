## Put comments here that give an overall description of what your
## functions do

##
## Matrix Cache constructor function
##
makeCacheMatrix <- function(x = matrix()) {
    ## Intialize the inverse
    m<-NULL
    
    ## set the matrix; assign matrix contents, initialize inverse
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    ## get the matrix contents
    get<-function() x
    
    ## set the inverse of the matrix
    setmatrix<-function(solve) m<<- solve
    
    ## get the inverse of the matrix
    getmatrix<-function() m
    
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

##
## Cache the inverse of a matrix 'x'
##
cacheInverse <- function(x=matrix(), ...) {
    ## get the cached inverse
    m <-x$getmatrix()
    
    ## if exist return
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## if not exist get the matrix itself and calculate the inverse
    matrix<-x$get()
    m<-solve(matrix, ...)
    
    ## cache the calculated inverse
    x$setmatrix(m)
    
    ## return the result
    m
}
