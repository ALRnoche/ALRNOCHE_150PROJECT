##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Simplex Method
# December 04, 2023
##########################################

simplex <- function(isMax, acm)
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