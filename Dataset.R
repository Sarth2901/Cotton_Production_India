library(readr)
df <- read_csv("Data.csv")
ds <- Data.csv
view(df)
dfAvs<-aggregate(df$Year ~ df$Area, FUN=mean)
colnames(dfAvs)<-c("Mode","irrigation")
barplot(dfAvs$irrigation,names=dfAvs$Mode)
barplot(dfAvs$irrigation,names=dfAvs$Mode)
plot(dfAvs$irrigation,dfAvs$Mode)
colnames(dfAvs)<-c("Mode","irrigation")
barplot(dfAvs$irrigation,names=dfAvs$Mode)
table(df$Year,df$Area)



library(ggplot2)
data <- read.csv("Data.csv")
ggplot(data, aes(x = Area)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "Histogram of Area", x = "Area", y = "Frequency") +
  theme_minimal()



ggplot(data, aes(x = Area)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  facet_wrap(~ Year, ncol = 5) +
  labs(title = "Histogram of Area Grouped by Year", x = "Area", y = "Frequency") +
  theme_minimal()

# Create a histogram for Area
hist(data$Area, breaks = 20, col = "blue", border = "black",
     main = "Histogram of Area", xlab = "Area", ylab = "Frequency")

# This will give the statistic and p-value

production_data <- na.omit(data$Production)


# Histogram with density curve
ggplot(data.frame(production_data), aes(x = production_data)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram of Production", x = "Production", y = "Density")


# Q-Q plot
qqnorm(production_data, main = "Q-Q Plot of Production")
qqline(production_data, col = "red")

# Shapiro-Wilk Test
shapiro_test <- shapiro.test(production_data)
print(shapiro_test)

# Interpret results
if (shapiro_test$p.value < 0.05) {
  print("The Production data is NOT normally distributed (reject H0).")
} else {
  print("The Production data is normally distributed (fail to reject H0).")
}


print(production_data)

colnames(data)



shapiro_test <- shapiro.test(production_data)
print(shapiro_test)




shapiro_test <- shapiro.test(production_data)
print(shapiro_test)

# Interpret results
if (shapiro_test$p.value < 0.05) {
  print("The Production data is NOT normally distributed (reject H0).")
} else {
  print("The Production data is normally distributed (fail to reject H0).")
}




# Create the plot
library(ggplot2)

ggplot(data, aes(x = Year, y = Area)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Area Over Time", x = "Year", y = "Area") +
  theme_minimal()

# Save the plot as PNG
ggsave("area_over_time.png", width = 8, height = 6)

# Load necessary library
library(ggplot2)

# Create the histogram of 'Area' to check for normality
ggplot(data, aes(x = Area)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +  # Add density curve to visually inspect normality
  labs(title = "Histogram of Area with Density Curve", x = "Area", y = "Frequency") +
  theme_minimal()



# Save the histogram as PNG
ggsave("area_histogram_normality_check.png", width = 8, height = 6)



# Q-Q plot to check normality visually
qqnorm(data$Area, main = "Q-Q Plot for Area")
qqline(data$Area, col = "red")

# Save Q-Q plot as PNG
dev.copy(png, "qq_plot_area.png")
dev.off()


#Load required libraries
library(ggplot2)

# Histogram with density curve for 'irrigation'
ggplot(Data, aes(x = irrigation)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Histogram with Density Curve for Irrigation",
    x = "Irrigation",
    y = "Density"
  ) +
  theme_minimal()

# Save the plot as PNG
ggsave("irrigation_histogram_with_density.png", width = 8, height = 6)

# Check for non-finite values in 'irrigation' 
non_finite_rows <- data[!is.finite(data$irrigation),] print(non_finite_rows)


# Load necessary libraries
library(ggplot2)

# Load the CSV file (use the correct file path)
data <- read.csv("Data.csv")

# Extract the Production column
production_data <- data$Production

# Plot histogram with Normal curve (density estimate)
ggplot() +
  geom_histogram(aes(x = production_data, y = ..density..), bins = 10, fill = "blue", alpha = 0.7, color = "black") +
  geom_density(aes(x = production_data), color = "red", size = 1) +
  labs(title = "Histogram of Production with Normal Curve",
       x = "Production",
       y = "Density") +
  theme_minimal()



# Load necessary libraries
library(ggplot2)

# Load the CSV file (use the correct file path)
data <- read.csv("Data.csv")

# Extract the Production column
production_data <- data$Production

# Plot histogram with Gaussian peak curve (density estimate)
ggplot() +
  geom_histogram(aes(x = production_data, y = ..density..), 
                 bins = 10, 
                 fill = "blue", 
                 alpha = 0.7, 
                 color = "black") +
  geom_density(aes(x = production_data), 
               color = "red", 
               size = 1) +
  labs(title = "Histogram of Production with Gaussian Peak Curve", 
       x = "Production", 
       y = "Density") +
  theme_minimal()


# Load necessary libraries
library(ggplot2)
library(minpack.lm)  # For non-linear least squares fitting

# Load the CSV file (use the correct file path)
data <- read.csv("Data.csv")

# Extract the Production column
production_data <- data$Production

# Define the Lorentzian function
lorentzian <- function(x, A, x0, gamma) {
  A * (gamma^2 / ((x - x0)^2 + gamma^2))
}

# Get the density estimate
density_data <- density(production_data)

# Initial estimates for the Lorentzian parameters
start_params <- list(
  A = max(density_data$y),  # Peak of the density estimate
  x0 = mean(production_data, na.rm = TRUE),  # Center of the distribution
  gamma = sd(production_data, na.rm = TRUE)  # Width of the distribution
)

# Fit the Lorentzian function to the density estimate using non-linear least squares
fit <- nlsLM(
  y ~ lorentzian(x, A, x0, gamma),
  data = data.frame(x = density_data$x, y = density_data$y),  # Explicitly pass the density data
  start = start_params
)

# Extract fitted parameters
params <- coef(fit)
A <- params["A"]
x0 <- params["x0"]
gamma <- params["gamma"]

# Create Lorentzian curve
x_vals <- seq(min(production_data, na.rm = TRUE), max(production_data, na.rm = TRUE), length.out = 1000)
lorentz_curve <- data.frame(
  x = x_vals,
  y = lorentzian(x_vals, A, x0, gamma)
)

# Plot histogram with Lorentzian peak curve
ggplot() +
  geom_histogram(aes(x = production_data, y = ..density..), 
                 bins = 10, 
                 fill = "blue", 
                 alpha = 0.7, 
                 color = "black") +
  geom_line(data = lorentz_curve, aes(x = x, y = y), 
            color = "red", size = 1) +
  labs(title = "Histogram of Production with Normal Curve", 
       x = "Production", 
       y = "Density") +
  theme_minimal()
