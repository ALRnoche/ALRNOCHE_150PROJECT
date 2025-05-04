##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Augmented Coefficient Matrix
# August 06, 2024
##########################################

#assign empty vectors

# Define your functions
# extracting vars from raw function
extract_variables <- function(system)
{
  variables <- character(0)
  for (func in system) 
  {
    args <- as.list(formals(func))
    if (!is.null(names(args)[1]) && names(args)[1] == ".x")
      args <- args[-1]
    
    args <- names(args)
    variables <- unique(c(variables, args))
  }
  return(variables)
}

# checks if every system contains the same number of var
check_system <- function(system)
{
  variables = extract_variables(system)
  all_present = T
  for (func in system)
  {
    args = as.list(formals(func))
    if (!is.null(names(args)[1]) && names(args)[1] == ".x")
      args = args[-1]
    
    args = names(args)
    if (!all(variables %in% args)) 
    {
      all_present = F
      variables = NA
      break
    }
  }
  result = list(all_present, variables)
  return(result)
}

# takes only body values. Omits function(x1, x2, xn, ...) header
body_values <- function(system)
{
  components = check_system(system)
  all_present = components[[1]]
  
  if (all_present) 
  {
    combined_expr_list = list()
    
    for (i in 1:length(system))
    {
      func_body = body(system[[i]])
      expr_string = deparse(func_body)
      combined_expr_list[[i]] = paste(expr_string, collapse = "")
    }
    return(combined_expr_list)
  }
  
  else
    return(components)
}

# splits into parts by addition symbol
deparsing_values <- function(system)
{
  components = check_system(system)
  all_present = components[[1]]
  
  if (all_present) 
  {
    variables = components[[2]]
    values = body_values(system)
    
    RHS_list = list()
    split_values = list()
    for (i in 1:length(values))
    {
      eq_terms = unlist(strsplit(values[[i]], "\\s+\\+\\s*"))
      constants = eq_terms[!grepl("\\*\\s*[a-zA-Z]\\d+", eq_terms)]
      
      RHS_list[[i]] = constants
      split_values[[i]] = eq_terms
    }
    
    intersect_comp = list()
    for (i in 1:length(split_values))
    {
      complement_terms = split_values[[i]][!split_values[[i]] %in% RHS_list[[i]]]
      intersect_comp[[i]] = complement_terms
    }
    
    LHS_list = list()
    LHS_names = list()
    for (i in 1:length(intersect_comp))
    {
      jth_terms = unlist(strsplit(intersect_comp[[i]], "\\s+\\*\\s*"))
      LHS_terms = jth_terms[!grepl("[a-zA-Z][0-9]", jth_terms)]
      jth_names = jth_terms[grepl("[a-zA-Z][0-9]", jth_terms)]
      
      LHS_names[[i]] = jth_names
      LHS_list[[i]] = LHS_terms
    }
    
    sorted_var_names = unique(unlist(LHS_names))
    sorted_LHS_list = list()
    for (i in 1:length(LHS_list))
    {
      current_names = LHS_names[[i]]
      current_values = LHS_list[[i]]
      
      named_vector = setNames(current_values, current_names)
      
      reordered_values = as.numeric(named_vector[match(sorted_var_names, names(named_vector))])
      sorted_LHS_list[[i]] = reordered_values
    }
    
    result = list(all_present, variables, RHS_list, sorted_LHS_list)
    return(result)
  }
  
  else
    return(components)
}

# combining all components into 1 large matrix
AugCoeffMatrix <- function(system)
{
  components = deparsing_values(system)
  all_present = components[[1]]
  
  if (all_present) 
  {
    variables = components[[2]]
    RHS_list = -(as.numeric(components[[3]]))
    LHS_list = components[[4]]
    
    transformed = as.vector(unlist(LHS_list))
    temp_mat = matrix(transformed, nrow = length(system), ncol = length(variables), byrow = T)
    rownames(temp_mat) = seq(length(system))
    colnames(temp_mat) = c(variables)
    mat_result = cbind(temp_mat, RHS = RHS_list)
    
    result = list("variables" = variables, "Valid" = all_present, "augcoeffmatrix" = mat_result)
    return(result)
  }
  
  else
    return(components)
}

