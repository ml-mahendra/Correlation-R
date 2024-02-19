
# Importing the visualizations to pdf file
pdf(file = "visualization.pdf")

# Reading Lib
library(readr)

# Loading the Dataset from same folder
heart_df <- read_csv("heart.csv")

# Removing all the rows with cholesterol values 0
filtered_data <- heart_df[heart_df$Cholesterol != 0, ]

# Assigning dependent  and independent  variable columns to the new variables
cholesterol <- filtered_data$Cholesterol
age <- filtered_data$Age

# Check the dependent variable data - is it normal or not
hist_Cholesterol <- hist(cholesterol, 
                         freq = TRUE, 
                         main = "Histogram of patient's cholesterol levels",
                         xlim = c(min(cholesterol), 
                                  max(cholesterol)),
                         xlab = "Cholesterol Levels (mm/dl)",
                         col = "bisque",
                         )

# Curve overlay for the histogram of our dependent variable
hist_curve <- curve(dnorm(x, mean = mean(cholesterol), sd = sd(cholesterol)) * 
                      diff(hist_Cholesterol$breaks)[1] * length(cholesterol),
                    col = "cadetblue", 
                    lwd = 3, 
                    add = TRUE)


# Adding Legend to the Histogram
hist_legend <- legend("topright", legend = c("Cholesterol", "Normal Distribution"), 
                      col = c("bisque", "cadetblue"), 
                      lwd = c(2, 2),
                      pch = c(NA,NA),
                      cex = 0.8)

# Adding margins to the histogram
hist_mar <- par(mar = c(5, 4, 4, 2) + 0.1)


# cor.test(age, cholesterol, method="pearson") # normal so using spearman

# Scatter Plot of dependent  and independent  variables
scatterplot <- plot(age, cholesterol, 
                    xlab = "Age of Patient's (years)",  
                    ylab = "Cholesterol Levels (mm/dl)",
                    main = "Scatterplot of Age Vs Cholesterol",
                    col = c("lightgreen", "cyan"),
                    pch = 16)

# Adding trend line to the scatter plot data points
trend_line <- abline(lm(cholesterol ~ age), col = "red")

# Adding legend to the scatter plot
plot_legend <- legend("topright", legend = c("Cholesterol", "Age", "Correlation Line"), 
                      col = c("cyan", "lightgreen","red"), 
                      pch = c(16,16,NA), 
                      lwd = c(NA,NA,2),
                      cex = 0.6)

# Adding margins to the scatter plot
plot_margins <- par(mar = c(5, 4, 4, 2) + 0.1)

dev.off()  # close the pdf device 

