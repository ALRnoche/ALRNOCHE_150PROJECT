##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Quadratic Spline Interpolation
# December 04, 2023
##########################################
source("../ALRNOCHE_150PROJECT/NocheACM_project.R") #import ACM and GJE codes
source("../ALRNOCHE_150PROJECT/NocheGJE_project.R")

poly.quadspline <- function(data, unknown)
{
  x = data[[1]] #extract all x and f(x)
  y = data[[2]]
  unk = unknown #recall unknown var
  
  n = length(x) - 1 #n intervals
  num_eqns = 3 * n #number of 3n unknown equations
  
  first_cond = c() #function(a1, a2, a3, ...., cn) header
  first_cond_base = c() #" 1 * a1 + 2 * b1 + ... + -cn body
  for(i in 2:n)#first condition
  {
    if((i-1) == 1)
    { #automatically remove a1 from the equation as condition 4 is a1 = 0
      head_knot1 = paste("function (b", i - 1, ", c", i - 1, ")", sep = "", collapse = NULL)
      int_knot1 = paste(x[i]," * b", i - 1, " + 1 * c", i - 1, " + ", -(y[i]), sep = "", collapse = NULL) #still LHS
    }
    
    else
    { #if it equation does not have a1, start from a
      head_knot1 = paste("function (a", i - 1, ", b", i - 1, ", c", i - 1, ")", sep = "", collapse = NULL)
      int_knot1 = paste((x[i])^2, " * a", i - 1, " + ", x[i]," * b", i - 1, " + 1 * c", i - 1, " + ", -(y[i]), sep = "", collapse = NULL) #still LHS
    }
    #2nd equation for condition 1 where there is no a1
    head_knot2 = paste("function (a", i, ", b", i, ", c", i, ")", sep = "", collapse = NULL)
    int_knot2 = paste((x[i])^2, " * a", i, " + ", x[i]," * b", i, " + 1 * c", i, " + ", -(y[i]), sep = "", collapse = NULL) #still LHS
    
    first_cond <- c(first_cond, head_knot1, head_knot2) #continuously iterate head strings until n intervals
    first_cond_base <- c(first_cond_base, int_knot1, int_knot2) #continuously iterate body strings until n intervals
  }
  
  head_L = paste("function (b1, c1)", sep = "", collapse = NULL) #already omit a1 in second condition in both head and body strings
  endpt_L = paste(x[1], " * b1 + 1 * c1 + ", -(y[1]), sep = "", collapse = NULL)
  
  head_U = paste("function (a", n, ", b", n, ", c", n, ")", sep = "", collapse = NULL) #get last endpoint head and body strings
  endpt_U = paste((x[length(x)])^2, " * a", length(x) - 1, " + ", x[length(x)], " * b", length(x) - 1, " + 1 * c", length(x) - 1, " + ", -(y[length(x)]), sep = "", collapse = NULL)
  
  second_cond <- c(head_L, head_U) #append head and base to their respective vectors.
  second_cond_base <- c(endpt_L, endpt_U)
  
  third_cond = c() #initialize vectors, boolean, and counters
  third_cond_base = c()
  valid = 0
  ifa1 = 0
  
  for(i in 2:n)
  {
    if((i-1) == 1) #omit a1 in equation
    {
      dx_0_int_knot = paste("1 * b", i - 1, sep = "", collapse = NULL)
      ifa1 = 1 #counter to check if a1 exists
    }
    
    else
    {
      dx_0_int_knot = paste(2 * (x[i]), " * a", i - 1, " + 1 * b", i - 1, sep = "", collapse = NULL)
      ifa1 = 0
    }
    
    temp_dx_0 = paste(2 * (x[i]), " * a", i - 1, " + 1 * b", i - 1, sep = "", collapse = NULL) #derivative in left hand sign
    dx_1_int_knot = paste(2 * (x[i]), " * a", i, " + 1 * b", i, sep = "", collapse = NULL) #derivative on right hand sign
    
    if(nchar(temp_dx_0) == nchar(dx_1_int_knot)) #double check if derivatives are equal by length of character
    {
      if(ifa1 == 0) #generate function head if no a1
        head_knot = paste("function (a", i - 1, ", b", i - 1, ", a", i, ", b", i,")", sep = "", collapse = NULL)
      
      else #if a1 exists, generate function head
        head_knot = paste("function (b", i - 1, ", a", i, ", b", i,")", sep = "", collapse = NULL)
      
      #paste 3rd condition equation along with transposed RHS
      merged_knot = paste(dx_0_int_knot, " + ", -(2 * (x[i])), " * a", i, " + -1 * b", i, " + 0", sep = "", collapse = NULL)
      
      third_cond <- c(third_cond, head_knot) #continuously iterate head of equations
      third_cond_base <- c(third_cond_base, merged_knot) #append every body of equations
      
      valid = 1
    }
    
    else #if derivatives are not equal, return as NA and third condition not satisfied.
    {
      valid = 0
      result = list(quadSplineFxns = NA, y = "Third condition was not satisfied.")
      return(result)
    }
  }
  
  head_equations = c(first_cond, second_cond, third_cond) #append all head and body equations to 1 vector
  base_equations = c(first_cond_base, second_cond_base, third_cond_base)
  variables = c()#initialize vectors
  
  final_head_equations = c()
  final_base_equations = c()
  combined_head_base = c()
  
  pattern = "\\b[a-zA-Z][0-9]+\\b" #regular expression for any leters (uppercase or lowercase) with corresponding number and boundary
  for (i in 1:length(head_equations)) 
  {
    match = gregexpr(pattern, head_equations[i]) #find unique values of variables within function("")
    final_vars = regmatches(head_equations[i], match)
    variables = union(variables, unlist(final_vars))
  }
  final_head_eqn = paste("function (", paste(variables, collapse = ", "), ")", sep = "", collapse = NULL) #paste to new function with all collated variables
  final_head_equations = rep(final_head_eqn, length(head_equations))
  
  for(i in 1:length(base_equations)) 
  {
    # call variables to temporary vector
    vars_temp = variables
    
    # split current equation by terms
    terms = strsplit(base_equations[i], " ", fixed = T)
    
    #iterate each term in equation
    for(j in 1:length(terms[[1]])) 
    {
      # Iterate the variables in the temporary vector
      for(m in 1:length(vars_temp)) 
      {
        # Check if unknown unknown variables equals to the coefficient in equation
        if(vars_temp[m] == terms[[1]][j]) #if b == b 
        {
          # Remove the variables not present in each equation
          vars_temp = vars_temp[-m]
          break
        }
      }
    }
    
    # Iterate the remaining unknown variables not present in equation
    for(j in 1:length(vars_temp)) 
    {
      # Add remaining unknown variables for every generated equation with 0 being the coefficient
      base_equations[i] = paste(base_equations[i], "+ 0", sep = " ")
      base_equations[i] = paste(base_equations[i], "*", sep = " ")
      base_equations[i] = paste(base_equations[i], vars_temp[j], sep = " ")
    }
  }
  
  for (i in 1:length(final_head_equations)) 
  { #combine every head and body equation with respect to its index
    combined_eqn = paste(final_head_equations[i], base_equations[i])
    combined_head_base = c(combined_head_base, combined_eqn)
  }
  system = c() #initialize a vector for system of equations
  
  for(i in 1:length(combined_head_base)) #append every complete function to system vectors
    system = c(system, eval(parse(text = combined_head_base[i]))) #transform as expression
  
  gauss_result = Gauss_Jordan_Elimination(system)#plug in to gauss jordan elimination function
  
  if(length(gauss_result$solSet) == 1)#double check for inconsistencies
    return()
  
  gauss_result$solSet = round(gauss_result$solSet, digits = 4)#round to 4 digits
  
  quadspline.fxns = list()#initialize vector
  upper_bound = c()
  lower_bound = c()
  
  #creating f(x1)
  func_str = "function (x)"
  func_str = paste(func_str, gauss_result$solSet[1], sep = " ")
  func_str = paste(func_str, "* x +", sep = " ")
  func_str = paste(func_str, gauss_result$solSet[2], sep = " ")
  quadspline.fxns = list(eval(parse(text = func_str)))
  
  for(i in 1:(n - 1))
  {
    func_str = "function (x)" #function head
    
    func_str = paste(func_str, as.character(gauss_result$solSet[3 * i]), collapse = NULL) #iterate remaining functions
    
    func_str = paste(func_str, "* (x ^ 2) +", sep = " ")
    func_str = paste(func_str, as.character(gauss_result$solSet[(3 * i) + 1]), sep = " ")
    func_str = paste(func_str, "* x +", sep = " ")
    func_str = paste(func_str, as.character(gauss_result$solSet[(3 * i) + 2]), sep = " ")
    
    #continuously append to quadspline
    quadspline.fxns = append(quadspline.fxns, eval(parse(text = func_str)))
  }
  
  f_approx = NA
  for(i in 1:n)
  {#look for interval where x to be estimated lies
    if((unk >= x[i]) && (unk <= x[i + 1])) 
    {
      f_approx = quadspline.fxns[[i]](unk)
      break
    }
  }
  result = list(quadSplineFxns = quadspline.fxns, y = f_approx) #collate to list then return output.
  return(result)
}