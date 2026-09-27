#===============================================================================
# AEM 6850
# Monte Carlo simulations
# Estimating the circumference of the Earth with naive and block bootstraps
#===============================================================================

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 1). Preliminary -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Clean up workspace and load or install necessary packages if necessary
rm(list=ls())
want <- c("boot")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Working directories
dir <- list()
dir$root <- dirname(getwd())
dir$output_figure <- paste(dir$root,"/output_figure",sep="")
dir$data <- paste(dir$root,"/data",sep="")

# Importing data
file_path <- paste0(dir$data, "/Eratosthenes.csv")
eratosthenes_data <- read.csv(file_path, stringsAsFactors = FALSE)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 2). Main code -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Comuting theta for Ithaca and Kingston
eratosthenes_data$theta_ithaca <- atan(eratosthenes_data$L_ithaca_m / eratosthenes_data$h_ithaca_m)
eratosthenes_data$theta_kingston <- atan(eratosthenes_data$L_kingston_m / eratosthenes_data$h_kingston_m)

# Computing estimate of the circumference based on the formula given and the known parameters
s <- 198.69
eratosthenes_data$delta_theta <- abs(eratosthenes_data$theta_ithaca - eratosthenes_data$theta_kingston)
eratosthenes_data$circ_estimate <- (2 * pi * s) / eratosthenes_data$delta_theta

# Naive bootstrap
mycirc_naive <- function(data, index){
  mean(data$circ_estimate[index])
}
set.seed(123) # Running naive bootstrap
boot_naive <- boot(eratosthenes_data, mycirc_naive, R = 1925)

# Block bootstrap
data_list <- split(eratosthenes_data, eratosthenes_data$team)
mycirc_block <- function(datlist, groupindex) {
  newdata <- datlist[groupindex]
  newdata <- do.call("rbind", newdata)
  mean_c <- mean(newdata$circ_estimate) # Computing the mean Earth circumference for bootstrap
  return(mean_c)
}
set.seed(123) # Running block bootstrap
boot_block <- boot(data_list, mycirc_block, R = 1925)


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 3). Plot -----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Save as png
png(paste0(dir$output_figure, "/bootstrap_distributions.png"), width=1600, height=1000, res = 200)

# Extract bootstrap means
naive_means <- boot_naive$t
block_means <- boot_block$t

# Force x axis limits
xlim_vals <- c(39000, 42750)

# Custom breaks
all_breaks <- seq( min(c(naive_means, block_means)), max(c(naive_means, block_means)), length.out = 40)

# Plot naive bootstrap
hist(naive_means, breaks = all_breaks, xlim = xlim_vals, main = "", ylim = c(0,300), border = NA,
     xlab = "Earth Circumference (km)",
     col = adjustcolor("red", 0.4), freq = TRUE)

# Add block bootstrap
hist(block_means, breaks = all_breaks, xlim = xlim_vals, ylim = c(0,300), border = NA,
     col = adjustcolor("blue", 0.4), add = TRUE, freq = TRUE)

# Add CI/Means/TrueValue
naive_ci <- quantile(naive_means, c(0.025, 0.975))
block_ci <- quantile(block_means, c(0.025, 0.975))
abline(v = naive_ci, col = adjustcolor("red", 0.4), lty = 2)
abline(v = block_ci, col = adjustcolor("blue", 0.4), lty = 2)
abline(v = mean(block_means), lwd = 1.5)
abline(v = 40030, lty = 2, lwd = 2)

box()
title("Bootstrap Distributions", line = 2)
mtext("Block 95% CI: 39733-41921",
      side = 3, line = .9, col = "blue", adj = 0.5, cex = 0.8)
mtext("Naive 95% CI: 40267 -41296",
      side = 3, line = 0, col = "red", adj = 0.5, cex = 0.8)

legend("right", legend = c("Naive", "Grouped", "Mean Estimate", "CI", "True"),
       pch = c(22, 22, NA, NA, NA),
       pt.bg = c(adjustcolor("red", 0.4), adjustcolor("blue", 0.4), NA, NA, NA),
       pt.cex = c(2, 2, NA, NA, NA),
       pt.lwd = 0,
       border = NA, lty = c(NA, NA, 1, 2, 2), lwd = c(NA, NA, 1.5, 1, 2.5),
       bty = "o", inset = c(0.01, 0), cex = 0.75, seg.len = 2)

dev.off()