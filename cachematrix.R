##This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y){
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mat_inv <<- inverse
        getinverse <- function() mat_inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x , ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinverse()
        if(!is.null(mat_inv)) {
                message("Getting cached data")  ##the inverse is from cache, not calculated
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data,...)
        x$setinverse(mat_inv)
        mat_inv
        
}



##to test this code you can execute the following commands

##my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##my_matrix$get()
##my_matrix$getinverse()  ##there is no cached value
##cacheSolve(my_matrix)
##cacheSolve(my_matrix)   ##the value is from cache, not calculated
##my_matrix$getinverse()

##my_matrix$set(matrix(matrix(2:5, 2, 2), 2, 2))
##my_matrix$get()
##my_matrix$getinverse()
##cacheSolve(my_matrix)
##cacheSolve(my_matrix)
##my_matrix$getinverse()