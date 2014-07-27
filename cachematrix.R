## Douglas R. Briggs
## Coursera: R Programming
## July 2014

## test invertable matrix, m
m <- matrix(c(1, 5, 9, 5, 2, 56, 29, 6, 3, 7, 3, 7, 4, 8, 4, 8), 
            4, ## rows
            4, ## cols
            byrow = TRUE)  ##fill direction, TRUE indicates by rows

## makeCacheMatrix is a function that accepts an invertible matrix as input (x)
## it returns a list which is a function to set the value of the vector
## and its inverse and to get the value of the vector and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve is a function that inverts the input matrix if it has not already been
## calculated or returns the stored inverse if it has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
