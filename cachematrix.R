# These 2 functions create a special Matrix that enables to calculate the
# inverse of a matrix and store it into the cache. if stored, the cached value
# get extracted in case the inverse is calculated again. it assumes that
# the matrix supplied is always invertible (square matrix)
# In Mathematica ....  inv[M_] := inv[M] = Inverse[M] 


makeCacheMatrix <- function(x = matrix()) {
        # creates a special "matrix", which is a
        # list containing a function to:
        #
        # set the value of the matrix 
        # get the value of the matrix 
        # set the value of the inverse matrix in the cache
        # get the value of the inverse matrix from the cache
        
        invx<-NULL
        set<- function(y){
                x<<-y
                invx<<-NULL
        }
        get<- function() x
        setinv<- function(inverseM) invx<<- inverseM
        getinv<- function() invx
        list(set=set,get=get,setinv=setinv,getinv=getinv)        
}


# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse stored in the cache and
# skips the computation. Otherwise, it calculates the inverse of the matrix
# using solve and sets its value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invx<-x$getinv()
        if (!is.null(invx)){
                message("....getting cached data....")
                return(invx)
                }
        matr<-x$get()
        invx<- solve(matr,...)
        x$setinv(invx)
        invx
        
        }

