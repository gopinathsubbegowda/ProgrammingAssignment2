## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The below functions achieve it in two levels (functions)
## 1.makeCacheMatrix(): Creates a special "matrix" that can cache its inverse.
## 2.cacheSolve():computes the inverse of the special "matrix" returned by makeCacheMatrix.
##                If the inverse has already been calculated, then the cachesolve should 
##                retrieve the inverse from the cache.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL ## Initiates the value of m to NULL
        set<-function(y){
                x<<-y ## caches the inputted matrix so that cacheSolve can check whether it has changed
                m<<-NULL ## Initiates the caches value of m to NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        ## Four functions are added to a list
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Is the matrix same as above
        m<-x$getmatrix()
        ## Get value from cache
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() 
        ## Compute the inverse of matrix
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m ## Return the inverse
}
