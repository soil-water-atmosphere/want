# source: http://www.r-bloggers.com/testing-function-agruments-in-gnu-r/
# The allowed argument types are:
# b     logical
# i     integer
# d     double
# n     numeric
# c     complex
# s     character
# f     function
# g     single function, count is then number of formal arguments
# l     list
# a     any type
# and argument lengths are:
# ?     0 or 1
# *     0 or more
# +     1 or more
# n     exactly n
# 
# the function return the number of the first match
# the function stops with an error message if no 

check.counts = function(count,arg)
{
  if (count == "?") {
    if (length(arg) > 1) {
      return(FALSE)
    }
  } else if (count == "*") {
    # nothing to do - always met
    return(TRUE)
  } else if (count == "+") {
    if (length(arg) == 0) {
      return(FALSE)
    }
  } else {
    if (length(arg) != as.integer(count)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

is.valid = function(types, counts, arg, stopmessage) 
{
  
  # first find first type that matches
  for(i in seq_along(types))
  {
    if(types[i]=="b")
    {
      if(is.logical(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }
    }
    if(types[i]=="i")
    {
      if(is.integer(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }     
    }
    if(types[i]=="d")
    {
      if(is.integer(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }           
    }
    if(types[i]=="n")
    {
      if(is.numeric(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }             
    }
    if(types[i]=="m")
    {
      if(is.numeric(arg)&(!is.null(dim(arg))))
      {
        if(length(dim(arg)==2))
        {
          return(i)
        }
      }
    }
    if(types[i]=="c")
    {
      if(is.complex(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }             
    }
    if(types[i]=="s")
    {
      if(is.character(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }             
    }    
    if(types[i]=="f")
    {
      if(is.function(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
      }             
    }      
    if(types[i]=="g")
    {
      if(is.function(arg))
      {
        if(length(formals(arg))==counts[i])
        {
          return(i)
        }
      }             
    }
    if(types[i]=="l")
    {
      if(is.list(arg))
      {
        if(check.counts(counts[i],arg))
        {
          return(i)
        }
#         if(length(arg)==counts[i])
#         {
#           return(i)
#         }
      }                   
    }
  }
  stop(stopmessage,call.=FALSE)
}

# # test
# foo2 = function(x)
# {
#   return(x+10)
# }
# 
# 
# is.valid(c("n","s","g","g"),c(1,"+",1,2),foo2,
# "second argument of add.spatialflux should either be:\ 
#                  - a number,\
#                  - a variable name\
#                  - a function of one argument: space\
#                  - or a function of two arguments: space and state")

# is.valid("n", 2, c(0,10), "first argument of newFLOW1D should be a vector of two numbers")
