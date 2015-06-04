#==================================
# This function takes a Python list
# and converts it to a R vector.
#==================================

convertPythonList = function(l){
  require(stringr)
  m = str_match_all(l, "'(.*?)'")
  m = as.character(as.data.frame(m)$X2)
  return(m)
}