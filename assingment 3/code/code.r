library(readr)
Warning message:
package ‘readr’ was built under R version 4.2.3 
> diabetes <- read_csv("assingment 3/source/diabetes.csv")
Rows: 768 Columns: 9                                                                      
── Column specification ────────────────────────────────────────────────────────────────────
Delimiter: ","
dbl (9): Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedig...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(diabetes)

step1
> set.seed(43)
> s_data <- diabetes[sample(nrow(diabetes), 25),] 
> s_mean_glucose <- mean(s_data$Glucose)
> s_high_glucose <- max(s_data$Glucose)
> population_m_glucose <- mean(diabetes$Glucose)
> population_h_glucose <- max(diabetes$Glucose)

> barplot( c(s_mean_glucose, s_high_glucose, population_m_glucose, population_h_glucose), names.arg = c("s mean", "s high", "population mean", "population high"), col = c("blue", "orange", "green", "red"), ylim = c(0,max(s_high_glucose, population_h_glucose) + 30), ylab = "Glucose Value",  main = "Comparison of Glucose Statistics" )


> legend("topright", legend = c("s mean", "s high", "population mean", "population high"), fill = c("blue", "orange", "green", "red")
+ )


step2

> s_98_percentile <- quantile(s_data$BMI, 0.98)
> population_98_percentile <- quantile(diabetes$BMI, 0.98)

> barplot(c(s_98_percentile, population_98_percentile),
+ names.arg = c("sample 98 percentile", "population 98 percentile"),
+ col = c("blue", "green"),
+ main = "comparsion of BMI 98 percentile",
+ ylab = "BMI 98 percentile",
+ )
> bootstrap_sample <- function(diabetes, N_samples=500, sample_size=150) {}
> bootstrap_sample <- function(diabetes, N_samples=500, sample_size=150) {}
> s_statistics <- matrix(nrow=N_samples, ncol = 3)

step3

> # Set a seed for reproducibility
> set.seed(123)
> population_blood_pressure <- rnorm(1000, mean = 120, sd = 10)
> 
> num_samples <- 500
> sample_size <- 150
> 
> bootstrap_stats <- function(data, indices) {
+     sample <- data[indices]
+     mean_val <- mean(sample)
+     std_val <- sd(sample)
+     percentile_val <- quantile(sample, 0.95)  # Adjust the percentile as needed
+     c(mean_val, std_val, percentile_val)
+ }
> 
> bootstrap_results <- boot(data = population_blood_pressure, statistic = bootstrap_stats, R = num_samples)

> bootstrap_means <- bootstrap_results$t[, 1]
> bootstrap_stds <- bootstrap_results$t[, 2]
> bootstrap_percentiles <- bootstrap_results$t[, 3]

> population_mean <- mean(population_blood_pressure)
> population_std <- sd(population_blood_pressure)
> population_percentile <- quantile(population_blood_pressure, 0.95)  # Adjust the percentile as needed

> cat("Population Mean:", population_mean, "\tBootstrap Mean:", mean(bootstrap_means), "\n")
Population Mean: 120.1613 	Bootstrap Mean: 120.1711 
> cat("Population Standard Deviation:", population_std, "\tBootstrap Standard Deviation:", mean(bootstrap_stds), "\n")
Population Standard Deviation: 9.91695 	Bootstrap Standard Deviation: 9.929369 
> cat("Population 95th Percentile:", population_percentile, "\tBootstrap 95th Percentile:", mean(bootstrap_percentiles), "\n")
Population 95th Percentile: 136.7613 	Bootstrap 95th Percentile: 136.9723 
> 

> par(mfrow=c(1,3))
> hist(population_blood_pressure, main="Population", xlab="Blood Pressure", col="lightblue")
> hist(bootstrap_means, main="Bootstrap Means", xlab="Mean Blood Pressure", col="lightgreen")
> hist(bootstrap_percentiles, main="median", xlab="median blood pressure", col="orange")

