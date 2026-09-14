# Step 0: Load the data
library(readxl)
data <- read_excel('Data Inflasi Skripsi.xlsx')
data <- data$DATA

# Step 1: Determine the data size
n <- length(data)
n

# Step 2: Choose the wavelet filter
library(waveslim)
filter_type <- "d4"

# Step 3: Choose the scale filter
level <- 3

# Step 4: Calculate the wavelet coefficient
w <- dwt(data, filter_type, level)
write.csv(w$d3,file = "Wavelet Coefficients.csv")

# Step 5: Choose the thresholding function
library(wavethresh)
family <- "DaubLeAsymm" #in this family, applied soft thresholding function
type <- "wavelet"

# Step 6: Choose the thresholding parameters
filter_num <- 10

# Step 7: Calculate the inverse of thresholded DWT
# Apply soft thresholding to the DWT coefficients
w_thresh1 <- wd(w$d1, filter.number = filter_num, family = family, type = type)
w_thresh2 <- wd(w$d2, filter.number = filter_num, family = family, type = type)
w_thresh3 <- wd(w$d3, filter.number = filter_num, family = family, type = type)

# Reconstruct signal/calculate the inverse of thresholded DWT coefficients
data_reconstructed1 <- wr(w_thresh1)
data_reconstructed2 <- wr(w_thresh2)
data_reconstructed3 <- wr(w_thresh3)
write.csv(data_reconstructed1,file = "Inverse of thresholded DWT coefficients Level 1.csv")

# Step 8: Calculate mean squared error between original and reconstructed signals
mse1 <- mean((data - data_reconstructed1)^2)
mse2 <- mean((data - data_reconstructed2)^2)
mse3 <- mean((data - data_reconstructed3)^2)

# Print MSE value
cat("MSE Level 1:", mse1)
cat("MSE Level 2:", mse2)
cat("MSE Level 3:", mse3)

# Step 9: Plotting
plot(data, type = "l", col = "blue", ylim = range(data, data_reconstructed1), xlab = "Time", ylab = "Value")
lines(data_reconstructed1, type = "l", col = "red")
legend("topright", legend = c("Original", "Reconstructed"), col = c("blue", "red"), lty = 1)
