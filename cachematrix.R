## Creating a set of functions that checks to see if the inverse for a matrix has
## been calculated and stored in cache

## Creates a special matrix that sets/gets value of a matrix and sets/gets inverse

makeCacheMatrix <- function(x = matrix()) {
		
		m<-NULL
		
		set<- function (y){
			x<<-y
			m<<- NULL
		}
		
		get <- function(){
		x
		}
		
		setInverse <-function(inverse){
			m<<-inverse
		}
		
		getInverse <-function(){
		m
		}
		
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse of a special "matrix", but first checks if the answer
## is stored in cache (provided that the matrix didn't change)

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
		data <- x$get()
        
		m <- solve(data)
        
		x$setInverse(m)
        
		return(m)
}
