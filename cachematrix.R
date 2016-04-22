## Objective: Avoid to compute the same matrix inversion twice.
## Explicit is better than implicit

## Objective: Check if the matrix invertion has been already done before
## Input: A square matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    get <- function(){
        return(x)
    } 
    
    setinv <- function(inverted){
        return(inv <<- inverted)
    }
    
    getinv <- function(){
        return(inv)
    } 
    
    return(list(get = get, setinv = setinv, getinv = getinv))
}


## Calculate the matrix invertion or return the value already calculated before

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting Cached Data ...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(print(inv))
}
