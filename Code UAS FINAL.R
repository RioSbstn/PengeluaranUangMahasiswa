library(readxl)
library(ggplot2)
library(ggpubr)


# Membaca file Excel
data <- read_excel("~/Documents/Campus/probstat rio/Pengeluaran Uang Untuk Makan Siang Mahasiswa UMN Revisi (Jawaban).xlsx",
                   col_types = c("date", "text", "numeric", "numeric", "numeric"))


# Menghitung central tendency untuk pertanyaan 1
mean_pengeluaran1 <- mean(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)
median_pengeluaran1 <- median(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)
modus_pengeluaran1 <- names(table(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`))[which.max(table(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`))]

# Menghitung central tendency untuk pertanyaan 2
mean_pengeluaran2 <- mean(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)
median_pengeluaran2 <- median(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)
modus_pengeluaran2 <- names(table(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`))[which.max(table(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`))]

# Menghitung central tendency untuk pertanyaan 3
mean_uang_jajan <- mean(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)
median_uang_jajan <- median(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)
modus_uang_jajan <- names(table(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`))[which.max(table(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`))]

# Menampilkan hasil
cat("Pertanyaan 1:\n")
cat("Mean: ", mean_pengeluaran1, "\n")
cat("Median: ", median_pengeluaran1, "\n")
cat("Modus: ", modus_pengeluaran1, "\n\n")

cat("Pertanyaan 2:\n")
cat("Mean: ", mean_pengeluaran2, "\n")
cat("Median: ", median_pengeluaran2, "\n")
cat("Modus: ", modus_pengeluaran2, "\n\n")

cat("Pertanyaan 3:\n")
cat("Mean: ", mean_uang_jajan, "\n")
cat("Median: ", median_uang_jajan, "\n")
cat("Modus: ", modus_uang_jajan, "\n")

# Menghitung variabilitas untuk pertanyaan 1
range_pengeluaran1 <- range(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)
var_pengeluaran1 <- var(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)
sd_pengeluaran1 <- sd(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)
quantile_pengeluaran1 <- quantile(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, na.rm = TRUE)

cat("Variabilitas Pertanyaan 1:\n")
cat("Range:", range_pengeluaran1, "\n")
cat("Variance:", var_pengeluaran1, "\n")
cat("Standard Deviation:", sd_pengeluaran1, "\n")
cat("Quantiles:", quantile_pengeluaran1, "\n\n")

# Menghitung variabilitas untuk pertanyaan 2
range_pengeluaran2 <- range(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)
var_pengeluaran2 <- var(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)
sd_pengeluaran2 <- sd(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)
quantile_pengeluaran2 <- quantile(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, na.rm = TRUE)

cat("Variabilitas Pertanyaan 2:\n")
cat("Range:", range_pengeluaran2, "\n")
cat("Variance:", var_pengeluaran2, "\n")
cat("Standard Deviation:", sd_pengeluaran2, "\n")
cat("Quantiles:", quantile_pengeluaran2, "\n\n")

# Menghitung variabilitas untuk pertanyaan 3
range_jajan <- range(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)
var_jajan <- var(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)
sd_jajan <- sd(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)
quantile_jajan <- quantile(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, na.rm = TRUE)

cat("Variabilitas Pertanyaan 3:\n")
cat("Range:", range_jajan, "\n")
cat("Variance:", var_jajan, "\n")
cat("Standard Deviation:", sd_jajan, "\n")
cat("Quantiles:", quantile_jajan, "\n\n")


# Menampilkan histogram
cat("Histogram 1:\n")
ggplot(data, aes(x = `Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 10) +
  labs(x = "Pengeluaran (Rupiah)", y = "Frequency", title = "Histogram Pengeluaran Uang untuk Makan Siang di Kantin UMN") +
  theme_minimal()

cat("Histogram 2:\n")
ggplot(data, aes(x = `Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 10) +
  labs(x = "Pengeluaran (Rupiah)", y = "Frequency", title = "Histogram Pengeluaran Uang untuk Makan Siang di Luar Kantin UMN") +
  theme_minimal()

cat("Histogram 3:\n")
ggplot(data, aes(x = `Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 10) +
  labs(x = "Jumlah Uang Jajan (Rupiah)", y = "Frequency", title = "Histogram Jumlah Uang Jajan per Minggu") +
  theme_minimal()

# Boxplot
cat("Boxplot 1:\n")
ggplot(data, aes(x = "", y = `Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "Pengeluaran (Rupiah)", title = "Boxplot Pengeluaran Uang untuk Makan Siang di Kantin UMN") +
  theme_minimal()

cat("Boxplot 2:\n")
ggplot(data, aes(x = "", y = `Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "Pengeluaran (Rupiah)", title = "Boxplot Pengeluaran Uang untuk Makan Siang di Luar Kantin UMN") +
  theme_minimal()

cat("Boxplot 3:\n")
ggplot(data, aes(x = "", y = `Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "Jumlah Uang Jajan (Rupiah)", title = "Boxplot Jumlah Uang Jajan per Minggu") +
  theme_minimal()

# Menghitung korelasi
cat("Korelasi 1:\n")
correlation1 <- cor(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, use = "complete.obs")
cat("Korelasi antara Pengeluaran di Kantin UMN dan Uang Jajan:", correlation1, "\n")

cat("Korelasi 2:\n")
correlation2 <- cor(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, use = "complete.obs")
cat("Korelasi antara Pengeluaran di Kantin UMN dan Pengeluaran di Luar Kantin UMN:", correlation2, "\n")

cat("Korelasi 3:\n")
correlation3 <- cor(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`, data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, use = "complete.obs")
cat("Korelasi antara Pengeluaran di Luar Kantin UMN dan Uang Jajan:", correlation3, "\n")


# Menghitung korelasi
cat("Association:\n")
correlation <- cor(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, use = "complete.obs")
cat("Correlation between Pengeluaran and Uang Jajan:", correlation, "\n")


# Grafik: Scatter Plot
scatter_plot <- ggplot(data, aes(x = `Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, y = `Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)) +
  geom_point() +
  labs(x = "Pengeluaran di Kantin UMN (Rupiah)", y = "Jumlah Uang Jajan (Rupiah)") +
  ggtitle("Scatter Plot: Pengeluaran di Kantin UMN vs Jumlah Uang Jajan") +
  theme_minimal()
print(scatter_plot)

# Grafik: Histogram
histogram1 <- ggplot(data, aes(x = `Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)) +
  geom_histogram(binwidth = 50000, fill = "lightblue", color = "black") +
  labs(x = "Pengeluaran di Kantin UMN (Rupiah)", y = "Frequency") +
  ggtitle("Histogram: Pengeluaran di Kantin UMN") +
  theme_minimal()
print(histogram1)

histogram2 <- ggplot(data, aes(x = `Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)) +
  geom_histogram(binwidth = 50000, fill = "lightblue", color = "black") +
  labs(x = "Pengeluaran di Luar Kantin UMN (Rupiah)", y = "Frequency") +
  ggtitle("Histogram: Pengeluaran di Luar Kantin UMN") +
  theme_minimal()
print(histogram2)

histogram3 <- ggplot(data, aes(x = `Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)) +
  geom_histogram(binwidth = 50000, fill = "lightblue", color = "black") +
  labs(x = "Jumlah Uang Jajan (Rupiah)", y = "Frequency") +
  ggtitle("Histogram: Jumlah Uang Jajan per Minggu") +
  theme_minimal()
print(histogram3)

# Memeriksa distribusi normal dengan QQ plot
qq_plot <- ggqqplot(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, main = "QQ Plot: Pengeluaran di Kantin UMN")
print(qq_plot)

# Memeriksa distribusi normal dengan Shapiro-Wilk Test
shapiro_test1 <- shapiro.test(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
cat("Shapiro-Wilk Test for Normality (Pengeluaran di Kantin UMN):\n")
cat("p-value:", shapiro_test1$p.value, "\n")
if (shapiro_test1$p.value < 0.05) {
  cat("The data for Pengeluaran di Kantin UMN is not normally distributed.\n")
} else {
  cat("The data for Pengeluaran di Kantin UMN is normally distributed.\n")
}

shapiro_test2 <- shapiro.test(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)
cat("Shapiro-Wilk Test for Normality (Pengeluaran di Luar Kantin UMN):\n")
cat("p-value:", shapiro_test2$p.value, "\n")
if (shapiro_test2$p.value < 0.05) {
  cat("The data for Pengeluaran di Luar Kantin UMN is not normally distributed.\n")
} else {
  cat("The data for Pengeluaran di Luar Kantin UMN is normally distributed.\n")
}

shapiro_test3 <- shapiro.test(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)
cat("Shapiro-Wilk Test for Normality (Jumlah Uang Jajan):\n")
cat("p-value:", shapiro_test3$p.value, "\n")
if (shapiro_test3$p.value < 0.05) {
  cat("The data for Jumlah Uang Jajan is not normally distributed.\n")
} else {
  cat("The data for Jumlah Uang Jajan is normally distributed.\n")
}

# Perform statistical inference test (confidence intervals and hypothesis testing) for one population mean
# One-sample t-test
t.test(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, mu = 50000)

# Perform statistical inference test (confidence intervals and hypothesis testing) for two population mean
# Data pengeluaran di kantin UMN
pengeluaran_kantin <- data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`

# Data pengeluaran di luar kantin UMN
pengeluaran_luar_kantin <- data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`

# Data jumlah uang jajan
uang_jajan <- data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`

# Fungsi untuk menghitung confidence interval
calculate_CI <- function(x, alpha) {
  n <- length(x)
  mean_val <- mean(x)
  std_err <- sd(x) / sqrt(n)
  margin_error <- qt(1 - alpha/2, df = n - 1) * std_err
  lower_bound <- mean_val - margin_error
  upper_bound <- mean_val + margin_error
  return(c(lower_bound, upper_bound))
}

# Menghitung confidence interval untuk pertanyaan 1
ci_pengeluaran_kantin <- calculate_CI(pengeluaran_kantin, 0.05)

# Menghitung confidence interval untuk pertanyaan 2
ci_pengeluaran_luar_kantin <- calculate_CI(pengeluaran_luar_kantin, 0.05)

# Menghitung confidence interval untuk pertanyaan 3
ci_uang_jajan <- calculate_CI(uang_jajan, 0.05)

# Menampilkan hasil confidence interval
cat("Confidence Interval:\n")
cat("1. Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?\n")
cat("   Lower bound:", ci_pengeluaran_kantin[1], "\n")
cat("   Upper bound:", ci_pengeluaran_kantin[2], "\n\n")

cat("2. Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?\n")
cat("   Lower bound:", ci_pengeluaran_luar_kantin[1], "\n")
cat("   Upper bound:", ci_pengeluaran_luar_kantin[2], "\n\n")

cat("3. Berapa jumlah uang jajan Anda yang didapatkan per minggu?\n")
cat("   Lower bound:", ci_uang_jajan[1], "\n")
cat("   Upper bound:", ci_uang_jajan[2], "\n\n")

# Fungsi untuk melakukan hypothesis testing
perform_hypothesis_test <- function(x1, x2, alpha, alternative) {
  n1 <- length(x1)
  n2 <- length(x2)
  mean_diff <- mean(x1) - mean(x2)
  std_error <- sqrt(var(x1)/n1 + var(x2)/n2)
  t_statistic <- mean_diff / std_error
  df <- n1 + n2 - 2
  p_value <- 2 * (1 - pt(abs(t_statistic), df))
  
  if (alternative == "two.sided") {
    if (p_value <= alpha) {
      conclusion <- "Reject null hypothesis"
    } else {
      conclusion <- "Fail to reject null hypothesis"
    }
  } else if (alternative == "less") {
    if (p_value <= alpha) {
      conclusion <- "Reject null hypothesis (mean1 < mean2)"
    } else {
      conclusion <- "Fail to reject null hypothesis"
    }
  } else if (alternative == "greater") {
    if (p_value <= alpha) {
      conclusion <- "Reject null hypothesis (mean1 > mean2)"
    } else {
      conclusion <- "Fail to reject null hypothesis"
    }
  }
  
  result <- list(
    mean_difference = mean_diff,
    t_statistic = t_statistic,
    degrees_of_freedom = df,
    p_value = p_value,
    conclusion = conclusion
  )
  
  return(result)
}

# Melakukan hypothesis testing untuk pertanyaan 1 dan pertanyaan 2
test_result <- perform_hypothesis_test(pengeluaran_kantin, pengeluaran_luar_kantin, 0.05, "two.sided")

# Menampilkan hasil hypothesis testing
cat("Hypothesis Testing:\n")
cat("Null hypothesis: mean(pengeluaran_kantin) = mean(pengeluaran_luar_kantin)\n")
cat("Alternative hypothesis: mean(pengeluaran_kantin) ≠ mean(pengeluaran_luar_kantin)\n")
cat("Mean difference:", test_result$mean_difference, "\n")
cat("T-statistic:", test_result$t_statistic, "\n")
cat("Degrees of Freedom:", test_result$degrees_of_freedom, "\n")
cat("p-value:", test_result$p_value, "\n")
cat("Conclusion:", test_result$conclusion, "\n\n")

# Melakukan hypothesis testing untuk pertanyaan 1 dan pertanyaan 3
test_result2 <- perform_hypothesis_test(pengeluaran_kantin, uang_jajan, 0.05, "two.sided")

# Menampilkan hasil hypothesis testing
cat("Hypothesis Testing:\n")
cat("Null hypothesis: mean(pengeluaran_kantin) = mean(uang_jajan)\n")
cat("Alternative hypothesis: mean(pengeluaran_kantin) ≠ mean(uang_jajan)\n")
cat("Mean difference:", test_result2$mean_difference, "\n")
cat("T-statistic:", test_result2$t_statistic, "\n")
cat("Degrees of Freedom:", test_result2$degrees_of_freedom, "\n")
cat("p-value:", test_result2$p_value, "\n")
cat("Conclusion:", test_result2$conclusion, "\n\n")



# Confidence interval for the population mean
mean <- mean(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
sd <- sd(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
n <- length(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
margin_error <- qt(0.975, df = n-1) * (sd / sqrt(n))
confidence_interval <- c(mean - margin_error, mean + margin_error)
confidence_interval



# Confidence interval for the difference in means
mean1 <- mean(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
mean2 <- mean(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)
mean3 <- mean(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)
sd1 <- sd(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
sd2 <- sd(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)
sd3 <- sd(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)
n1 <- length(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`)
n2 <- length(data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)
n3 <- length(data$`Berapa jumlah uang jajan Anda yang didapatkan per minggu?`)
# Menghitung margin error dan interval kepercayaan
margin_error <- qt(0.975, df = min(n1-1, n2-1, n3-1)) * sqrt((sd1^2 / n1) + (sd2^2 / n2) + (sd3^2 / n3))
confidence_interval <- c(mean1 - mean2 - mean3 - margin_error, mean1 - mean2 - mean3 + margin_error)
confidence_interval



# Perform fitting of a simple linear regression model to your data
# Fitting a simple linear regression model
lm_model <- lm(`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?` ~ `Berapa jumlah uang jajan Anda yang didapatkan per minggu?`, data = data)

# Summary of the linear regression model
summary(lm_model)

# Perform chi-square test for your data
# Chi-square test of independence
chisq.test(data$`Berapa jumlah pengeluaran uang Anda untuk makan siang di kantin UMN dalam per minggu?`, data$`Jika Anda memilih makan di luar kantin kampus UMN, berapa perkiraan jumlah uang dalam rupiah yang Anda keluarkan untuk makan siang dalam per minggu?`)
