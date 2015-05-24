## The following functions cache an inverse of a matrix
## Note that the matrix passed is assumed to have an inverse

## This function caches an inverse of x where is x is a matrix that has an inverse
## It returns a list of functions that set and get the values of x; and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invX <- matrix()
        
        ##set values of x
        set <- function (y) {
                
                x <<- y
                invX <- matrix()
        }
        
        ##get values of x       
        get <- function() x
        
        ##set inverse of x
        setInv <- function(inverse) invX <<- inverse

        ##retrive inverse of x
        getInv <- function() invX
        
        ##return the list of functions
        list (set = set, get=get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of x if already cached otherwise it first calculates it, caches it, and
## returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInv()
        
        ##if inverse of x is not empty then return the inverse of x
        if (!is.na(invX)) {
                
                message("getting cached data")
                return(invX)
        }
        
        ##if inverse of x is empty then calculate it
        data <- x$get()
        invX <- solve(data,...)
        
        ##then cache it
        x$setInv(invX)
        
        ##and return it
        invX
        
        
}
