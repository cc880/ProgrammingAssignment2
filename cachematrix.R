## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
                
        ## The `makeCacheMatrix()` is a funtion to create a list including 4 functions
        ## (get(), set(), setinverse(), getinverse())
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        
        get <- function() x
        
        ## When `set_ivs(matrix_inverse)` run, the `matrix_inverse`(a matrix) will be stored as `m`.
        set_ivs <- function(matrix_inverse) m <<- matrix_inverse 
        
        ## `get_ivs()` will return the stored matrix, signed as  `m` above.
        get_ivs <- function() m
        
        
        
        ##print the list,can be cited with "$"
        list(set = set, get = get,
             setinverse = set_ivs,
             getinverse = get_ivs)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        
        ## Judgement:There is an `m`(refer to the stored matrix) have been contained. GET THE `m`.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        ## Judgement: There is no `m` stored. 
        ## Set the m. 
        data <- x$get()
        #calculate the inveres of the x, signed as m
        m <- solve(data, ...)
        # stored the `m`, so that `m` can be get from the `getinverse()` function
        x$setinverse(m)
        
## Return `m` , wthich is the inverse of 'x'
        m
        

}
