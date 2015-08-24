## The following functions will COMPUTE and CACHE the inverse of a given matrix. 
## The first time makeCache() is called it will compute the inverse of the matrix. 
## Everytime after this it will retrieve the cached inverse rather that recalculating it. 
## Assumptions: the input matrix is invertible and square. 

## The function makeCacheMatrix() takes a matrix (x) as its argument
## Returns a list of 4 functions: set, get, setinv and getinv, and can cache the inverse of the matrix.
## These functions get and set both the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y        ## Use <<- to assign an environment that is different from the current environment. 
                inv <<- NULL   
        }
        get = function() x     
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) ## This list is used by cacheSolve()
}


## The function cacheSolve() takes function makeCacheMatrix() as its argument.
## It first checks to see if the inverse has been calculated. 
## If it has then it will retrieve the inverse from the cache and skip the steps to recompute it.  
## If it has not it calculates the inverse using solve() and caches the result. 

cacheSolve <- function(x, ...) {
        inv = x$getinv()        ## Assign getinv from the makeCacheMatrix() list to inv
        if (!is.null(inv)){      ## Check to see if the inverse has already been calculated (i.e. is not NULL)
                message("getting cached data")          ## If it has then the message is printed
                return(inv)                             ## and the value for inverse is returned from the cache.  
        }
        Matrix_data = x$get()           
        inv = solve(Matrix_data, ...)  ## Solves the inverse of the matrix using solve()
        x$setinv(inv)    ## Sets the inverse of the matrix in the cache   
        return(inv)     ## Returns the inverse of matrix x
}
