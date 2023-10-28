# This home work consists of 3 sections: 
# Section 1 Defining function makeCacheMatrix
# Section 2 Defining function cacheSolve
# Section 3 Test cases (5 in total) confirming both functions function as intended

########### Part 1
# ## makeCacheMatrix is a function that can take a matrix as argument, check if it can be inversed, store the inversed value if there is one
# 

makeCacheMatrix <- function(x = matrix()) {
  
  inversed_x <- NULL # variable in the function to store inversed value
  
  setMatrix <- function(y) { # setMatrix function to read in the original matrix and clear the inversed matrix
    
    if (is.matrix(y)) { #first to check if the input is matrix
      x <<- y             #reset x in the parent environment
      inversed_x <<- NULL  #clear inversed matrix value
    } else {
      print("The input is not a matrix.")
      return()
    }
    
  }
  
  getMatrix <- function() { # getMatrix only to return the stored input matrix
    return(x)
  }
  
  set_inversed_Matrix <- function() { # set the inversed matrix
    
    if (det(x) != 0) {    # check if the matrix can be inversed
      inversed_x <<- solve(x)
    } else {
      print("The input matrix can not be inversed.")
      inversed_x <<- NULL
    }
  }
  
  get_inversed_Matrix <- function() { # return the stored inversed matrix
    return(inversed_x)
  }
  
  return(list(setMatrix = setMatrix, getMatrix = getMatrix, set_inversed_Matrix = set_inversed_Matrix, get_inversed_Matrix = get_inversed_Matrix))
}


########### Part 2 Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, special_object = makeCacheMatrix()) {
  # x is an input matrix, # special_oject should exist in the parent environment as created by special_object <- makeCacheMatrix() previously
  
  #check is x is a matrix
  if (!is.matrix(x)) {
    print("The input is not a matrix.")
    return()
  }
  
  
  #retrieve stored inversed matrix
  inverse_matrix_stored <- special_object$get_inversed_Matrix()
  
  #check if x exists in the special object
  if (identical(x, special_object$getMatrix())) {
    if(!is.null(inverse_matrix_stored)){
      message("getting cached data")
      return(inverse_matrix_stored)
    }
  }
  
  # for x not stored in the special object  
  special_object$setMatrix(x) # update the stored matrix value in the special matrix in the parent environment
  special_object$set_inversed_Matrix() # update the stored inversed matrix value in the parent environment
  return(special_object$get_inversed_Matrix())
  special_object$getMatrix()
}


########### Part 3 Test cases to ensure both functions work as intended

# Initialization: make a special object 

special_object <- makeCacheMatrix() 

# Test case 1: check initial values
special_object$getMatrix()
# Expected output: 1x1 matrix with the value NA
special_object$get_inversed_Matrix()
# Expected output: NULL


# Test case 2: initialize the speical object
cacheSolve(matrix(c(1, 2, 3, 4), nrow = 2), special_object)
# Expected output: a matrix equal to matrix(c(-2, 1, 1.5, -0.5), nrow = 2)
special_object$getMatrix()
# Expected output: matrix(c(1, 2, 3, 4), nrow = 2)
special_object$get_inversed_Matrix()
# Expected output: matrix(c(-2, 1, 1.5, -0.5), nrow = 2)


# Test case 3: repeat the above step
cacheSolve(matrix(c(1, 2, 3, 4), nrow = 2), special_object)
# Expected output: message in console "getting cached data" and the identical output as case 1
special_object$getMatrix()
# Expected output: matrix(c(1, 2, 3, 4), nrow = 2)
special_object$get_inversed_Matrix()
# Expected output: matrix(c(-2, 1, 1.5, -0.5), nrow = 2)



# Test case 4: check values in
cacheSolve(matrix(1:9, nrow = 3), special_object)
# Expected output: message in console "The input matrix can not be inversed." and NULL
special_object$getMatrix()
# Expected output: input matrix
special_object$get_inversed_Matrix()
# Expected output: NULL


# Test case 5: non matrix input
cacheSolve(1:4, special_object)
# Expected output: message in console "The input is not a matrix." and NULL
special_object$getMatrix()
# Expected output: input matrix in test case 4, no update
special_object$get_inversed_Matrix()
# Expected output: NULL

