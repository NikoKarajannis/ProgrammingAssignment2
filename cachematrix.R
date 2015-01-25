## The first function creates an environment with two variables and four functions; these build a parent environment for the second function

## get is the function that gives you x, the original matrix; setm is the function that creates the inverse of m; getm is the function that takes the cached inverse of the matrix; in the end, all the four functions are sored in a list

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x   <<- y
        m <<- NULL
    }
    get <- function() x
    setm <- function (solve) m <<- solve
    getm <- function() m
    list(set = set, get = get,
         setm = setm,
         getm = getm)
}


## The cacheSolve-function uses the functions created in makeCacheMatrix, that is it uses and assigns the values from that environment. If the two matrices are not identical and m is not null, it takes the value from the cache; if not, it takes the inverse of m and gives it as an result

cacheSolve <- function(x, ...) {
    m <- x$getm()
    if (!is.null(m) && !identical(x, m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setm(m)
    m
}
