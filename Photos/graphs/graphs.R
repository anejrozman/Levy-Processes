# Load the ggplot2 package
library(ggplot2)

# Setting random seed
set.seed(1)

################# BROWNIAN MOTION ##############################################

# Number of steps in Brownian motion
n_steps <- 1000

# Parameters for Brownian motion
mu <- 0.1          # Mean
sigma <- 10     # Standard deviation (volatility)

# Function to generate Brownian motion
generate_brownian_motion <- function(n_steps, mu, sigma) {
  increments <- rnorm(n_steps, mean = mu, sd = sigma)
  path <- cumsum(increments)
  return(data.frame(time = 1:n_steps, value = path))
}

# Generate Brownian motion data for multiple instances
num_instances <- 3  # Number of instances to generate
brownian_data <- list()

for (i in 1:num_instances) {
  brownian_data[[i]] <- generate_brownian_motion(n_steps, mu, sigma)
  brownian_data[[i]]$instance <- i
}

brownian_data <- do.call(rbind, brownian_data)

# Plot all instances of Brownian motion using ggplot2
ggplot(brownian_data, aes(x = time, y = value, color = factor(instance))) +
  geom_line() +
  labs(title = "\u03BC=0.1, \u03C3=10",
       x = "Št. korakov",
       y = "Vrednost")+
  theme(legend.position = "none")

# Saving graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diplomski seminar/Levy-Processes/Photos/graphs/BrownGibanje.pdf", plot = last_plot(), device = "pdf")


##################### POISSON PROCESS ##########################################

# Number of steps in Poisson motion
n_steps <- 100

# Parameters for Poisson motion
lambda <- 0.1  # Intensity

# Function to generate Poisson motion
generate_poisson_motion <- function(n_steps, lambda) {
  jumps <- rpois(n_steps, lambda)
  path <- cumsum(jumps)
  return(data.frame(time = 1:n_steps, value = path))
}

# Generate Poisson motion data for multiple instances
num_instances <- 3  # Number of instances to generate
poisson_data <- list()

for (i in 1:num_instances) {
  poisson_data[[i]] <- generate_poisson_motion(n_steps, lambda)
  poisson_data[[i]]$instance <- i
}

poisson_data <- do.call(rbind, poisson_data)

# Plot all instances of Poisson motion using ggplot2
ggplot(poisson_data, aes(x = time, y = value, color = factor(instance))) +
  geom_step() +
  labs(title = "\u03BB=0.1",
       x = "Št. korakov",
       y = "Vrednost")+
  theme(legend.position = "none")

# Saving graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diplomski seminar/Levy-Processes/Photos/graphs/PoissGibanje.pdf", plot = last_plot(), device = "pdf")




