##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Gauss Jordan Elimination
# December 04, 2023
##########################################
source("../ALRNOCHE_150PROJECT/NocheACM_project.R") #Import ACM code
options(max.print = 10000)
Gauss_Jordan_Elimination <- function(system)
{
  acm_and_vars = AugCoeffMatrix(system) #import needed data from augmented coefficient matrix
  acm = acm_and_vars$augcoeffmatrix
  vars = acm_and_vars$variables
  
  ith = nrow(acm) #get length of row and col
  jth = ncol(acm)
  solSet = c() #initialize solution set
  
  if(det(acm[,-jth]) == 0) #checking for inconsistencies
    return(list("solSet" = NA, variables = vars, matrix = acm))
  
  for(i in 1:ith)
  {
    if(i < ith)
    {
      for(j in (i + 1):ith)
      {
        if(acm[i,i] < acm[j,i]) #finding largest number in every row
        {
          temp = acm[i,] #swapping of rows for pivot row
          acm[i,] = acm[j,]
          acm[j,] = temp
        }
      }
    }
    pivot_e = acm[i,i] #get pivot element
    
    for(j in i:jth) #normalize pivot row
      acm[i,j] = acm[i,j] / pivot_e
    
    for(j in 1:ith)
    {
      if(i == j) #if pivot row, next iteration.
        next
      
      multiplier = acm[j,i] #get multiplier
      
      temp_vector = multiplier * acm[i,] #get temporary vector
      acm[j,] = acm[j,] - temp_vector #subtract every non-pivot row
    }
  }
  acm = round(acm, digits = 4) #round to 4 digits
  
  for(i in 1:ith)
    solSet = c(solSet, acm[i,jth]) #iterate every RHS values to get solution set
  names(solSet) = vars #rename solution set column and row names
  
  result = list("solSet" = solSet, variables = vars, matrix = acm) #collate data and return list
  return(result)
}

Gauss_Jordan_Elimination_v2 <- function(acm) #gauss-jordan elimination function (for poly regression)
{
  ith = nrow(acm) #get length of row and col
  jth = ncol(acm)
  solSet = c() #initialize solution set vector
  
  if(det(acm[,-jth]) == 0) #check for inconsistencies
    return(list("solSet" = NA, matrix = acm))
  
  for(i in 1:ith)
  {
    if(i < ith)
    {
      for(j in (i + 1):ith)
      {
        if(acm[i,i] < acm[j,i]) #finding largest number in every row
        {
          temp = acm[i,] #swapping rows for pivot row
          acm[i,] = acm[j,]
          acm[j,] = temp
        }
      }
    }
    pivot_e = acm[i,i] #get pivot element
    
    for(j in i:jth)
      acm[i,j] = acm[i,j] / pivot_e #normalize pivot row
    
    for(j in 1:ith)
    {
      if(i == j) #next iteration if pivot row.
        next
      
      multiplier = acm[j,i] #get multiplier
      
      temp_vector = multiplier * acm[i,] #get temporary vector
      acm[j,] = acm[j,] - temp_vector #subtract every non-pivot row
    }
  }
  acm = round(acm, digits = 4) #round off to 4 digits
  
  for(i in 1:ith)
    solSet = c(solSet, acm[i,jth]) #get solution set from RHS values
  colnames(acm) = seq(ncol(acm)) #rename columns
  
  result = list("solSet" = solSet, matrix = acm) #collate data and return list
  return(result)
}

Gauss_Jordan_Elimination_v3 <- function(acm)
{
  # important details for acm
  ith = nrow(acm)
  jth = ncol(acm)
  
  orig_ith = nrow(acm)
  
  # list for solution
  iteration_cnt = 0
  
  initial_soln = NULL
  succeeding_solns = NULL
  all_solutions = list()
  
  basic.solution = NULL
  
  if(iteration_cnt == 0)
  {
    initial_soln = list(
      "ITERATION_NUMBER" = iteration_cnt,
      "ACM" = acm,
      "PIVOT_COLUMN" = NULL,
      "PIVOT_ROW" = NULL,
      "BASIC_SOLUTION" = NULL
    )
    
    all_solutions[[iteration_cnt + 1]] = initial_soln
    iteration_cnt = iteration_cnt + 1
  }
  
  # solve using simplex method
  while(min(acm[orig_ith, ]) < 0)
  {
    PC = NULL
    PC_i = NULL
    
    PR = NULL
    PR_i = NULL
    
    #taking pivot column and its index
    PC_i = which.min(acm[orig_ith, ])
    PC = acm[, PC_i, drop = F]
    
    PR_elems = which(acm[,PC_i] > 0)   # For the pivot rows, only select those that are positive
    if(length(PR_elems) ==  0)
    {
      print("No PR_elems")
      print("No feasible solution")
      return(NULL)
    }
    
    Soln_vector = acm[, ncol(acm), drop = F] # extract solution column
    Soln_vector = Soln_vector[1:(nrow(PC)), , drop = F] # remove last row
    
    TR = as.vector(Soln_vector / PC) # take test ratios
    TR[TR <= 0] = Inf # set every (-Inf, 0] as +Inf
    TR[is.na(TR)] = Inf # set every NA as +Inf
    TR[is.null(TR)] = Inf # set every NA as +Inf
    
    # if every element in the vector is infinite or na or null, break loop and return as infeasible solution
    if (all(is.infinite(TR) | is.na(TR) | is.null(TR)))
    {
      print("INVALID TR")
      print("No feasible solution")
      return(NULL)
    }
    
    else
    {
      #else, find the minimum positive TR index which will serve as the pivot row index
      PR_i = which(TR == min(TR))
      
      # take pivot row and pivot element
      PR = acm[PR_i, , drop = F]
      PE = acm[PR_i, PC_i]
      
      # normalize by PR/PE and replace existing pivot row in original matrix
      nPR = (PR)/(PE)
      acm[PR_i, ] = nPR
      
      # Iteration for Elimination
      for(i in 1:nrow(acm))
      {
        if(PR_i == i)
          next
        
        acm[i,] = acm[i,] - (nPR * acm[i, PC_i])
      }
      
      succeeding_solns = list(
        "ITERATION_NUMBER" = iteration_cnt,
        "ACM" = acm,
        "PIVOT_COLUMN" = PC,
        "PIVOT_ROW" = PR,
        "BASIC_SOLUTION" = NULL
      )
      
      all_solutions[[iteration_cnt + 1]] = succeeding_solns
      iteration_cnt = iteration_cnt + 1
      succeeding_solns = NULL
    }
  }
  
  for(i in 2:length(all_solutions))
  {
    solutions = all_solutions[[i]]
    matrices = solutions$ACM

    soln_temp = matrices[, jth, drop = F]
    bot_row = matrices[ith, -jth, drop = F]

    if(i != length(all_solutions))
    {
      temp_basic = matrix(NA, nrow = nrow(bot_row), ncol = ncol(bot_row))
      colnames(temp_basic) = colnames(bot_row)
      rownames(temp_basic) = seq(1,nrow(bot_row))
      for(j in 1:(jth - 1))
      {
        values = matrices[1:ith, j, drop = F]
        if(all(values %in% c(0, 1)))
        {
          # locate the index of value 1 in every values variable whose sum is 1
          one_index = which(values == 1)
          temp_basic[,j] = soln_temp[one_index]
        }
        else
          temp_basic[,j] = 0
      }
      all_solutions[[i]]$BASIC_SOLUTION = temp_basic
    }

    else
    {
      Z_col = acm[, -jth, drop = F]
      Z_col = Z_col[, ncol(Z_col), drop = F]

      one_index = which(Z_col == 1)
      Z = soln_temp[one_index]

      basic = bot_row
      basic[, ncol(basic)] = Z
      all_solutions[[i]]$BASIC_SOLUTION = basic
    }

    if(i == length(all_solutions))
    {
      last_value = all_solutions[[i]]$BASIC_SOLUTION
      all_solutions[[i]]$OPTIMAL_VALUE = last_value[,ncol(last_value)]
    }
  }
  return(all_solutions)
}