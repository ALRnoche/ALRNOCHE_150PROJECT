##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Polynomial Regression
# October 30, 2023
##########################################
source("../ALRNOCHE_150PROJECT/NocheGJE_project.R") #Import GJE code
PolynomialRegression <- function(data, degree, estimated_x) #regression polynomial function
{ #dependent, independent, and degree variables
  independent = data[[1]] #access independent and dependent data
  dependent = data[[2]]
  x = length(independent) #length of dependent and independent
  y = length(dependent)
  est_x = estimated_x #reassign to new variable
  
  augCoeff_matrix = matrix(NA, nrow = degree + 1, ncol = degree + 2, byrow = T) #initialize augmented coeff matrix
  m = nrow(augCoeff_matrix) #get num of rows and cols
  n = ncol(augCoeff_matrix)
  
  row_names = c()
  count = 0
  
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      if(j != n) #checking if RHS. If it is not, execute line of code
        augCoeff_matrix[i,j] = sum(independent^((i-1)+(j-1))) #last column value (raised to (i-1)+(j-1))
      
      else #Execute line of code below if RHS. Multiply previous value to current value(raised to i-1)
        augCoeff_matrix[i,j] = sum(dependent * (independent^(i-1)))
    }
  }
  
  orig_aug_mat = augCoeff_matrix #get resulting augmented coeff matrix
  coeffs = Gauss_Jordan_Elimination_v2(augCoeff_matrix)$solSet #importing solution set from gauss jordan elimination
  gauss = Gauss_Jordan_Elimination_v2(augCoeff_matrix)$matrix
  reg_poly = "function (x) " #regression polynomial string
  
  for(i in 1:m)
  {
    if(i != m) #if i is not equal to no. of rows
    {
      if(i == 1) # and i is 1,
        reg_poly = paste(reg_poly, coeffs[i], " + ", sep = "") #update and concatenate reg_poly string. disregards power of 0
      
      else # if i not equal to 1,
        reg_poly = paste(reg_poly, coeffs[i], " * x ^ ", i - 1, " + ", sep = "") #update and concatenate reg_poly string
    }
    
    else # if i is equal to no. of rows
      reg_poly = paste(reg_poly, coeffs[i], " * x ^ ", i - 1, sep = "") #update and concatenate reg_poly string. last element of string
    
    row_names = c(row_names, paste("a", count, sep = "", collapse = NULL))
    count = count + 1
  }
  
  rownames(orig_aug_mat) = seq(nrow(orig_aug_mat)) #rename col and row names
  colnames_except_rhs = ifelse(is.na(coeffs), NA, seq(n - 1))
  colnames(orig_aug_mat) = c(colnames_except_rhs, "RHS")
  
  rownames(gauss) = c(row_names) #rename col name
  reg_func = eval(parse(text = reg_poly)) #evaluate expression(reg_poly string already converted)

  regression_estimate = reg_func(est_x) #plug in x to be estimated
  result = list(polynomial_string = reg_poly, polynomial_function = reg_func, estimate = regression_estimate)  #collating required data as list
  return(result) #output labelled list result
}
