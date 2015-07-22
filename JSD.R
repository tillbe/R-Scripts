
JSD = function(Ps){
  # Ps is a matrix.
  require(entropy)
  Ps = prop.table(Ps, 2) # make it a probability distribution
  pi = 1/dim(Ps)[2] # pi is the number of distributions
  A = entropy((rowSums(Ps * pi)))
  B = sum(apply(Ps, 2, entropy) * pi)
  return(A-B)
} 
