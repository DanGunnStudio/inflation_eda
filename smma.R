# Assuming you have a numeric vector of data called 'data_vector' and a period 'N'
calculate_smma <- function(data_vector, N) {
  # Calculate the initial SMA for the first N points
  smma <- mean(data_vector[1:N])
  
  # Initialize the SMMA vector with the SMA as the first value
  smma_vector <- c(smma)
  
  # Calculate SMMA for each point after the initial period
  for (i in (N+1):length(data_vector)) {
    smma <- (smma * (N - 1) + data_vector[i]) / N
    smma_vector <- c(smma_vector, smma)
  }
  
  # Prepend NA to match the length of the input vector, as the first N-1 values do not have SMMA
  smma_vector <- c(rep(NA, N-1), smma_vector)
  
  return(smma_vector)
}

# Example usage
data_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Replace this with your actual data
N <- 3 # The period over which to smooth the data
smma_values <- calculate_smma(data_vector, N)

print(smma_values)