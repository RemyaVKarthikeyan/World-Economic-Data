# Loading libraries 
install.packages("dplyr")
library(dplyr)
library(e1071)
install.packages("RVAideMemoire") 
library(RVAideMemoire)
library(tseries)
library("forecast")
install.packages("datarium")
install.packages("tidyverse") 
install.packages("corrplot") 
install.packages("rcompanion") 
install.packages("qqplotr") 
install.packages("ggplot2")
install.packages("ppcor")
install.packages("corrplot")
install.packages("TTR") 
install.packages("forecast")
install.packages("car") 
install.packages("caret")
library(ggplot2)
library(reshape2)
library(car) 
library(caret)
library(ppcor)
library(corrplot)
library(qqplotr)
library(datarium)
library(tidyverse) 
library(rcompanion)
library(TTR) 
library(forecast)
library("TTR")

#Loading the WDI_EA dataset
WDI_EA=read.csv("WDI_EA.csv",header=TRUE)
WDI_EA
names(WDI_EA)
head(WDI_EA)
tail(WDI_EA)
str(WDI_EA)
#Whn cehcking services and inflation datatype, its seen chr, that needs to be checked. 
summary(WDI_EA)

# DATA PREPARATION

#Checking missing values
missing_data <- which(apply(WDI_EA, 1:2, function(x) any(x == "..")), arr.ind = TRUE)

#Print the indices
print(missing_data )

#Checking the country where the data is missing
WDI_EA[51,"Country.Name"]
WDI_EA[60,"Country.Name"]

#converting to numeric

WDI_EA[,6]=as.numeric(WDI_EA[,6])
WDI_EA[,8]=as.numeric(WDI_EA[,8]) 

#Assigning zero to the missing value
WDI_EA[51,6]=0
WDI_EA[60,8]=0

#imputing with mean of the available values
ser_impute=sum(WDI_EA[WDI_EA$Country.Name=="Myanmar",6])/9    
inf_impute=sum(WDI_EA[WDI_EA$Country.Name=="Myanmar",8])/9 

#Imputing the missing data
WDI_EA[51,6]=ser_impute
WDI_EA[60,8]=inf_impute

#Checking imputed values
WDI_EA[51,6]
WDI_EA[60,8]

str(WDI_EA)


#outlier detection
boxplot(WDI_EA$GDP_grw, main = "Outlier Detection in GDP growth data", ylab="GDP growth")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$GDP_grw, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")


boxplot(WDI_EA$Industry, main = "Outlier Detection in Annual growth rate for industrial value data", ylab="Annual growth rate for industrial value")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Industry, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")


boxplot(WDI_EA$Services, main = "Outlier Detection in Annual growth rate for services value data", ylab="Annual growth rate for services value")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Services, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Emp_pop, main = "Outlier Detection in Employment to population ratio data", ylab="Employment to population ratio")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Emp_pop, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Inflation, main = "Outlier detection in inflation data", ylab="Inflation")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Inflation, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")


boxplot(WDI_EA$Exports, main = "Outlier detection in Exports of goods and services data", ylab="Exports of goods and services")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Exports, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Imports, main = "Outlier detection in Imports of goods and services data", ylab="Imports of goods and services")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Imports, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Imports, main = "Outlier detection in Imports of goods and services data", ylab="Imports of goods and services")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Imports, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Emp_agri, main = "Outlier detection in Employment in agriculture data", ylab="Employment in agriculture")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Emp_agri, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Emp_ind, main = "Outlier detection in Employment in industry data", ylab="Employment in industry")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Emp_ind, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$Emp_ser, main = "Outlier detection in Employment in services data", ylab="Employment in services")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$Emp_ser, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")

boxplot(WDI_EA$GCF, main = "Outlier detection in Gross capital formation data", ylab="Gross capital formation")

# Identify and print potential outliers
outliers <- boxplot(WDI_EA$GCF, plot = FALSE)$out
cat("Potential Outliers:", outliers, "\n")


#DATA INTEGRATION

#Subsetting Data
SEA=c("Indonesia","Malaysia","Singapore","Thailand","Viet Nam","Myanmar","Philippines","Cambodia")
NEA=c("Hong Kong SAR, China","China","Japan","Mongolia","Korea, Rep.")
WDI_SEA=subset(WDI_EA, Country.Name%in%SEA)
WDI_SEA
WDI_NEA=subset(WDI_EA, Country.Name%in%NEA)
WDI_NEA


#SEA-Country wise sub dataset
Indonesia=subset(WDI_EA, Country.Name=="Indonesia")
Malaysia=subset(WDI_EA, Country.Name=="Malaysia")
Singapore=subset(WDI_EA, Country.Name=="Singapore")
Thailand=subset(WDI_EA, Country.Name=="Thailand")
VietNam=subset(WDI_EA, Country.Name=="Viet Nam")
Myanmar=subset(WDI_EA, Country.Name=="Myanmar")
Philippines=subset(WDI_EA, Country.Name=="Philippines")
Cambodia=subset(WDI_EA, Country.Name=="Cambodia")

#NEA-Country wise sub dataset
HongKong=subset(WDI_EA, Country.Name=="Hong Kong SAR, China")
China=subset(WDI_EA, Country.Name=="China")
Japan=subset(WDI_EA, Country.Name=="Japan")
Mongolia=subset(WDI_EA, Country.Name=="Mongolia")
SouthKorea=subset(WDI_EA, Country.Name=="Korea, Rep.")


#Exploratory Data Analysis

# Load necessary libraries
library(ggplot2)

# Summary statistics
summary(WDI_EA)

mean(WDI_EA$GDP_grw)
median(WDI_EA$GDP_grw)
mode(WDI_EA$GDP_grw)
range(WDI_EA$GDP_grw)
var(WDI_EA$GDP_grw)
sd(WDI_EA$GDP_grw)

#Univariate Analysis
# Convert Time to character
WDI_EA$Time <- as.character(WDI_EA$Time)

# Plot with Time as character
ggplot(WDI_EA, aes(x = Time, y = GDP_grw, group = Country.Name)) +
  geom_line(aes(color = Country.Name)) +
  labs(title = "GDP Growth Over Time", x = "Year", y = "GDP Growth")


# Example data
# Assuming your data frame is named WDI_EA and has columns Country.Name, Time, and GDP_pc

# Load ggplot2 package if not already loaded
# install.packages("ggplot2")
library(ggplot2)

# Convert Time to character
WDI_EA$Time <- as.character(WDI_EA$Time)

# Create a bar plot for GDP per capita
ggplot(WDI_EA, aes(x = Time, y = GDP_pc, fill = Country.Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "GDP per Capita Over Time", x = "Year", y = "GDP per Capita") +
  scale_fill_brewer(palette = "Set3")  # You can choose a different color palette if needed

# Assuming your_data is your dataset
library(ggplot2)

# Create a line chart for the industrial growth rate
ggplot(WDI_EA, aes(x = Time, y = Industry, group = 1)) +
  geom_line() +
  labs(title = "Annual Growth Rate for Industrial Sector Over Time",
       x = "Year",
       y = "Industrial Growth Rate")

mean_industry <- aggregate(Industry ~ Time, data = WDI_EA, mean)


ggplot(mean_industry, aes(x = as.factor(Time), y = Industry)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(title = "Mean Industry Contribution Over Years",
       x = "Year",
       y = "Mean Industry Contribution") +
  theme_minimal()

ggplot(WDI_EA, aes(x = Time, y = Services, color = Country.Name)) +
  geom_point() +
  labs(title = "Scatter Plot of Services Data of East Asia (2011-2020)",
       x = "Year",
       y = "Services",
       color = "Country") +
  theme_minimal()

# Assuming your data frame is named 'your_data'
Emp_pop_2019 <- subset(WDI_EA, Time == 2019, select = c(Country.Name,Emp_pop))


# Filter the dataset for the year 2020 and select specific columns
Emp_pop_2020 <- subset(WDI_EA, Time == 2020, select = c(Country.Name,Emp_pop))

# Assuming Emp_pop_2019 and Emp_pop_2020 are data frames with columns Country.Name and Emp_pop


# Combine the datasets
line_data <- rbind(mutate(Emp_pop_2019, Year = 2019),
                   mutate(Emp_pop_2020, Year = 2020))

# Line chart using ggplot2
ggplot(line_data, aes(x = Country.Name, y = Emp_pop, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Comparison of Employment-Population (2019 vs. 2020)", x = "Country", y = "Employment Population Ratio") +
  theme_minimal()


# Plotting inflation for 13 countries
ggplot(WDI_EA, aes(x = Time, y = Inflation, group = Country.Name, color = Country.Name)) +
  geom_line() +
  labs(title = "Inflation Over Time (2011-2020)", x = "Year", y = "Inflation Rate") +
  theme_minimal()


library(ggplot2)

# Plotting facetted line plot for inflation
ggplot(WDI_EA, aes(x = Time, y = Inflation, group = Country.Name, color = Country.Name)) +
  geom_line() +
  facet_wrap(~Country.Name, scales = "free_y", ncol = 3) +
  labs(title = "Inflation Over Time (2011-2020)", x = "Year", y = "Inflation Rate") +
  theme_minimal()

# Load required libraries
library(ggplot2)

# Check the structure of your dataset
str(WDI_EA)

# Plotting stacked bar chart for exports and imports
ggplot(WDI_EA, aes(x = Time, y = Exports + Imports, fill = Country.Name)) +
  geom_bar(stat = "identity") +
  labs(title = " Trade (Exports and Imports) Over Time (2011-2020)", x = "Year", y = "Trade (Exports and Imports)") +
  scale_fill_manual(values = rainbow(length(unique(WDI_EA$Country.Name)))) +  # Use different colors for each country
  theme_minimal()


WDI_EA$Region <- ifelse(WDI_EA$Country.Name %in% SEA, "SEA", "NEA")

# Plotting FDI over time for SEA and NEA
ggplot(WDI_EA, aes(x = Time, y = FDI, group = Region, color = Region)) +
  geom_line() +
  labs(title = "FDI Over Time (2011-2020)", x = "Year", y = "FDI") +
  scale_color_manual(values = c("SEA" = "blue", "NEA" = "red")) +  # Assign colors for SEA and NEA
  theme_minimal()



# Assuming labor_force_data is your dataset
library(ggplot2)

# Plotting labor force for 13 countries (2011-2020)
ggplot(WDI_EA, aes(x = as.factor(Time), y = Labor.force, fill = Country.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Labor Force Over Time (2011-2020)", x = "Year", y = "Labor Force") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)

# Plotting labor force for each country over time
ggplot(WDI_EA, aes(x = Country.Name, y = as.factor(Time), fill = Labor.force)) +
  geom_tile(color = "white") +
  labs(title = "Labor Force Over Time by Country", x = "Country", y = "Time", fill = "Labor Force") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Assuming labor_force_data is your dataset
library(ggplot2)

# Assuming WDI_EA is your dataset
library(ggplot2)

# Filter data for SEA and NEA
sea_nea_data <- subset(WDI_EA, Region %in% c("SEA", "NEA"))

# Calculate the total Emp_agri for SEA and NEA
total_emp_agri <- aggregate(Emp_agri ~ Region, data = sea_nea_data, sum)

# Plot a pie chart
ggplot(total_emp_agri, aes(x = "", y = Emp_agri, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Contribution of South East Asia and North East Asia \n to Employment in Agriculture (2011-2020)", fill = "Region") +
  theme_minimal()

# Assuming WDI_EA is your dataset
library(ggplot2)

# Filter data for SEA and NEA
sea_nea_data <- subset(WDI_EA, Region %in% c("SEA", "NEA"))

# Calculate the total Emp_agri for SEA and NEA
total_emp_agri <- aggregate(Emp_agri ~ Region, data = sea_nea_data, sum)

# Calculate percentages
total_emp_agri$percentage <- total_emp_agri$Emp_agri / sum(total_emp_agri$Emp_agri) * 100

# Plot a pie chart with percentages
ggplot(total_emp_agri, aes(x = "", y = Emp_agri, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of South East Asia and North East Asia \n to Employment in Agriculture (2011-2020)", fill = "Region") +
  theme_minimal()



total_emp_ind <- aggregate(Emp_ind ~ Region, data = sea_nea_data, sum)

# Calculate percentages
total_emp_ind$percentage <- total_emp_ind$Emp_ind / sum(total_emp_ind$Emp_ind) * 100

# Plot a pie chart with percentages
ggplot(total_emp_ind, aes(x = "", y = Emp_ind, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of South East Asia and North East Asia \n to Employment in Industry (2011-2020)", fill = "Region") +
  theme_minimal()

total_emp_ser <- aggregate(Emp_ser ~ Region, data = sea_nea_data, sum)

# Calculate percentages
total_emp_ser$percentage <- total_emp_ser$Emp_ser / sum(total_emp_ser$Emp_ser) * 100

# Plot a pie chart with percentages
ggplot(total_emp_ser, aes(x = "", y = Emp_ser, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of South East Asia and North East Asia \n to Employment in Services (2011-2020)", fill = "Region") +
  theme_minimal()


par(mfrow = c(1, 3), mar = c(5, 4, 2, 1))
barplot(total_emp_agri$Emp_agri,names.arg=total_emp_agri$Region,horiz= TRUE,xlab = "Total Employment in agriculture", xlim=c(0,4000),col = c("blue", "green"))
barplot(total_emp_ind$Emp_ind,names.arg=total_emp_ind$Region,horiz= TRUE,xlab = "Total Employment in industry",xlim=c(0,4000),col = c("blue", "green"),beside =TRUE)
barplot(total_emp_ser$Emp_ser,names.arg=total_emp_ser$Region,horiz= TRUE,xlab = "Total Employment in services", xlim=c(0,4000),col = c("blue", "green"),beside=TRUE)



# Create a line plot for GCF over the years
ggplot(WDI_EA, aes(x = Time, y = GCF, group = Country.Name, color = Country.Name)) +
  geom_line() +
  labs(title = " Gross Capital Formation (GCF) in East Asia (2011-2020)", x = "Year", y = "GCF") +
  theme_minimal()


#Bivariate anlaysis
# Assuming your dataset is named WDI_EA

# Load necessary libraries
library(ggplot2)

# Scatter plot for GDP growth vs. GDP per capita
ggplot(WDI_EA, aes(x = GDP_grw, y = GDP_pc)) +
  geom_point(aes(color = Country.Name)) +
  labs(title = "Scatter Plot: GDP Growth vs. GDP per Capita", x = "GDP Growth", y = "GDP per Capita") +
  theme_minimal()

# Scatter plot for Industry growth vs. Services growth
ggplot(WDI_EA, aes(x = Industry, 
                   y = Services)) +
  geom_point(aes(color = Country.Name)) +
  labs(title = "Scatter Plot: Industry Growth vs. Services Growth",
       x = "Industry Growth", y = "Services Growth") +
  theme_minimal()

# Scatter plot for Employment to population ratio vs. Labor force participation rate
ggplot(WDI_EA, aes(x = Emp_pop, 
                   y = Labor.force)) +
  geom_point(aes(color = Country.Name)) +
  labs(title = "Scatter Plot: Employment- Population Ratio vs. Labor Force Participation Rate",
       x = "Employment Ratio", y = "Labor Force Participation Rate") +
  theme_minimal()

# You can create similar plots for other bivariate analyses
ggplot(WDI_EA, aes(x = "", y = FDI, fill = Country.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart: FDI by Country", fill = "Country") +
  theme_minimal()

# Assuming your dataset is named WDI_EA
# Assuming your dataset is named WDI_EA

# Load necessary libraries
library(ggplot2)

# Scatter plot with color encoding for GDP growth, GDP per capita, and Employment to population ratio
trivariate_scatter <- ggplot(WDI_EA, aes(x = GDP_grw, y = GDP_pc, color = Emp_pop)) +
  geom_point() +
  labs(title = "GDP Growth Vs GDP Per Capita,\n for variation in Employment to Population Ratio", x = "GDP Growth", y = "GDP per Capita", color = "Employment Ratio")

# Display the plot
print(trivariate_scatter)




#Statistical Analysis 
#Task 1
mean(WDI_EA$GDP_grw)
mean(WDI_SEA$GDP_grw)
mean(WDI_NEA$GDP_grw)

median(WDI_EA$GDP_grw)
median(WDI_SEA$GDP_grw)
median(WDI_NEA$GDP_grw)

mode(WDI_EA$GDP_grw)
mode(WDI_SEA$GDP_grw)
mode(WDI_NEA$GDP_grw)

sd(WDI_EA$GDP_grw)
sd(WDI_SEA$GDP_grw)
sd(WDI_NEA$GDP_grw)

skewness(WDI_EA$GDP_grw)
skewness(WDI_SEA$GDP_grw)
skewness(WDI_NEA$GDP_grw)


kurtosis(WDI_EA$GDP_grw)
kurtosis(WDI_SEA$GDP_grw)
kurtosis(WDI_NEA$GDP_grw)

plot(density(WDI_EA$GDP_grw))
plot(density(WDI_SEA$GDP_grw))
plot(density(WDI_NEA$GDP_grw))



# Create density plots for East Asia, South East Asia, and North East Asia
density_plot <- ggplot() +
  geom_density(data = WDI_EA, aes(x = GDP_grw, color = "East Asia"), fill = "lightblue", alpha = 0.5) +
  geom_density(data = WDI_SEA, aes(x = GDP_grw, color = "South East Asia"), fill = "lightgreen", alpha = 0.5) +
  geom_density(data = WDI_NEA, aes(x = GDP_grw, color = "North East Asia"), fill = "lightcoral", alpha = 0.5) +
  labs(title = "Density Plot of GDP Growth Rate (2011-2020)", x = "GDP Growth Rate") +
  theme_minimal()

# Print the combined density plot
print(density_plot)



# Calculate statistics for East Asia (EA), South East Asia (SEA), and North East Asia (NEA)
stats_EA <- c(mean(WDI_EA$GDP_grw), median(WDI_EA$GDP_grw), sd(WDI_EA$GDP_grw), skewness(WDI_EA$GDP_grw), kurtosis(WDI_EA$GDP_grw))
stats_SEA <- c(mean(WDI_SEA$GDP_grw), median(WDI_SEA$GDP_grw),  sd(WDI_SEA$GDP_grw), skewness(WDI_SEA$GDP_grw), kurtosis(WDI_SEA$GDP_grw))
stats_NEA <- c(mean(WDI_NEA$GDP_grw), median(WDI_NEA$GDP_grw), sd(WDI_NEA$GDP_grw), skewness(WDI_NEA$GDP_grw), kurtosis(WDI_NEA$GDP_grw))

# Create a data frame
summary_stats <- data.frame(
  Metric = c("Mean", "Median", "SD", "Skewness", "Kurtosis"),
  EA = stats_EA,
  SEA = stats_SEA,
  NEA = stats_NEA
)

# Print the data frame
print(summary_stats)




# Melt the data frame for better plotting

summary_melted <- melt(summary_stats, id.vars = "Metric")

# Bar plot
ggplot(summary_melted, aes(x = Metric, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Summary Statistics", x = "Metric", y = "Value") +
  theme_minimal()

#explaning Mode
ggplot(WDI_EA, aes(x = GDP_grw)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of GDP Growth Rate of East Asia", x = "GDP Growth Rate") +
  theme_minimal()

ggplot(WDI_SEA, aes(x = GDP_grw)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution of GDP Growth Rate of South East Asia", x = "GDP Growth Rate") +
  theme_minimal()

ggplot(WDI_NEA, aes(x = GDP_grw)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of GDP Growth Rate of North East Asia", x = "GDP Growth Rate") +
  theme_minimal()





# Line plot for GDP_grw over time (overall distribution)
ggplot(WDI_EA, aes(x = Time, y = GDP_grw)) +
  geom_line(color = "blue") +
  labs(title = "Distribution of GDP Growth Rate (2011-2020)", x = "Year", y = "GDP Growth Rate") +
  theme_minimal()

#analysisng the GDP_grw of East asia
# Box plot for GDP_grw over time
ggplot(WDI_EA, aes(x = factor(Time), y = GDP_grw)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Distribution of GDP Growth Rate (2011-2020) of East Asia", x = "Year", y = "GDP Growth Rate") +
  theme_minimal()

ggplot(WDI_SEA, aes(x = factor(Time), y = GDP_grw)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Distribution of GDP Growth Rate (2011-2020) of South East Asia", x = "Year", y = "GDP Growth Rate") +
  theme_minimal()

ggplot(WDI_NEA, aes(x = factor(Time), y = GDP_grw)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Distribution of GDP Growth Rate (2011-2020) of North East Asia", x = "Year", y = "GDP Growth Rate") +
  theme_minimal()




# Combine box plots for East Asia, South East Asia, and North East Asia
combined_plot <- ggplot(mapping = aes(x = factor(Time), y = GDP_grw, fill = factor(Time))) +
  geom_boxplot(data = WDI_EA, fill = "lightblue", color = "blue") +
  geom_boxplot(data = WDI_SEA, fill = "lightgreen", color = "green") +
  geom_boxplot(data = WDI_NEA, fill = "lightcoral", color = "red") +
  labs(title = "Distribution of GDP Growth Rate (2011-2020) in East Asia", x = "Year", y = "GDP Growth Rate") +
  theme_minimal() +
  facet_wrap(~Country.Name, scales = "free_y")

# Print the combined plot
print(combined_plot)




# Create a summary dataframe
summary_stats <- data.frame(
  Metric = c("Mean", "Standard Deviation", "Median"),
  Value = c(mean(WDI_EA$GDP_grw), sd(WDI_EA$GDP_grw), median(WDI_EA$GDP_grw))
)

# Create a bar plot
ggplot(summary_stats, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Summary Statistics of GDP Growth Rate (2011-2020) - East Asia",
       x = "Statistic",
       y = "GDP growth") +
  theme_minimal()





# Plot histogram
ggplot(WDI_EA, aes(x = GDP_grw)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Histogram of GDP Growth Rate (2011-2020)", x = "GDP Growth Rate") +
  theme_minimal()

# Plot density plot
ggplot(WDI_EA, aes(x = GDP_grw)) +
  geom_density(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Density Plot of GDP Growth Rate (2011-2020)", x = "GDP Growth Rate") +
  theme_minimal()

# Print skewness and kurtosis
cat("Skewness:", skewness(WDI_EA$GDP_grw), "\n")
cat("Kurtosis:", kurtosis(WDI_EA$GDP_grw), "\n")


# Assuming WDI_EA contains the GDP growth rate data for 13 countries over time



# Assuming WDI_EA contains the GDP growth rate data for 13 countries over time
# Create a data frame with the mean GDP growth rate for each country
mean_gdp_growth <- aggregate(GDP_grw ~ Country.Name, data = WDI_EA, mean)

# Create a pie chart with percentage labels
ggplot(mean_gdp_growth, aes(x = "", y = GDP_grw, fill = Country.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round((GDP_grw / sum(GDP_grw)) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of Each Country to GDP Growth Rate (2011-2020)\n in East Asia", fill = "Country") +
  theme_minimal()



mean_gdp_growth_SEA <- aggregate(GDP_grw ~ Country.Name, data = WDI_SEA, mean)

# Create a pie chart with percentage labels
ggplot(mean_gdp_growth_SEA, aes(x = "", y = GDP_grw, fill = Country.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round((GDP_grw / sum(GDP_grw)) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of Each Country to GDP Growth Rate (2011-2020) \n in South East Asia", fill = "Country") +
  theme_minimal()



mean_gdp_growth_NEA <- aggregate(GDP_grw ~ Country.Name, data = WDI_NEA, mean)

# Create a pie chart with percentage labels
ggplot(mean_gdp_growth_NEA, aes(x = "", y = GDP_grw, fill = Country.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round((GDP_grw / sum(GDP_grw)) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Contribution of Each Country to GDP Growth Rate (2011-2020) \n in North East Asia", fill = "Country") +
  theme_minimal()





# Assuming WDI_EA contains the relevant dataset
library(dplyr)

# Filter data for the region SEA and calculate the sum of GDP growth
sea_data <- WDI_EA %>%
  filter(Region == "SEA") %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Calculate the total GDP growth for all regions
total_data <- WDI_EA %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Merge data frames to have both SEA and total GDP growth for each year
merged_data <- merge(sea_data, total_data, by = "Time", suffixes = c("_SEA", "_Total"))

# Calculate the percentage contribution
merged_data$percentage_contribution <- (merged_data$total_gdp_growth_SEA / merged_data$total_gdp_growth_Total) * 100

# Plot the percentage contribution over time
library(ggplot2)

ggplot(merged_data, aes(x = as.factor(Time), y = percentage_contribution)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Percentage Contribution of SEA to GDP Growth (2011-2020)",
       x = "Year",
       y = "Percentage Contribution") +
  theme_minimal()


nea_data <- WDI_EA %>%
  filter(Region == "NEA") %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Calculate the total GDP growth for all regions
total_data <- WDI_EA %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Merge data frames to have both SEA and total GDP growth for each year
merged_data <- merge(nea_data, total_data, by = "Time", suffixes = c("_NEA", "_Total"))

# Calculate the percentage contribution
merged_data$percentage_contribution <- (merged_data$total_gdp_growth_NEA / merged_data$total_gdp_growth_Total) * 100

# Plot the percentage contribution over time
library(ggplot2)

ggplot(merged_data, aes(x = as.factor(Time), y = percentage_contribution)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Percentage Contribution of NEA to GDP Growth (2011-2020)",
       x = "Year",
       y = "Percentage Contribution") +
  theme_minimal()



# Filter data for SEA and NEA, and calculate the sum of GDP growth
sea_data <- WDI_EA %>%
  filter(Region == "SEA") %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

nea_data <- WDI_EA %>%
  filter(Region == "NEA") %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Calculate the total GDP growth for all regions
total_data <- WDI_EA %>%
  group_by(Time) %>%
  summarise(total_gdp_growth = sum(GDP_grw))

# Merge data frames to have both SEA and NEA and total GDP growth for each year
#merged_data <- merge(sea_data, nea_data, by = "Time")
#merged_data <- merge(merged_data, total_data, by = "Time", suffixes = c("_SEA", "_NEA", "_Total"))

# Calculate the percentage contributions
#merged_data$percentage_contribution_SEA <- (merged_data$total_gdp_growth_SEA / merged_data$total_gdp_growth_Total) * 100
#merged_data$percentage_contribution_NEA <- (merged_data$total_gdp_growth_NEA / merged_data$total_gdp_growth_Total) * 100

# Plot the percentage contributions over time
library(ggplot2)

#(ggplot(merged_data, aes(x = as.factor(Time))) +
  #geom_bar(aes(y = merged_data$percentage_contribution_SEA), stat = "identity", fill = "blue", alpha = 0.7) +
  #geom_bar(aes(y = merged_data$percentage_contribution_NEA), stat = "identity", fill = "red", alpha = 0.7) +
  #labs(title = "Percentage Contribution of SEA and NEA to GDP Growth (2011-2020)",
       #x = "Year",
      # y = "Percentage Contribution") +
  #scale_fill_manual(values = c("blue", "red"), name = "Region") +
  #theme_minimal())



barplot(WDI_EA$GDP_grw,names.arg=WDI_EA$Region,col=rainbow(8))
qplot(Time,GDP_grw,data=WDI_EA, color = Region)


qplot(Time,GDP_grw,data=WDI_EA, geom = "boxplot",fill=I("red"))


#barplot(
  height=cbind(WDI_NEA$GDP_grw,WDI_NEA$GDP_pc),
  beside=TRUE,
  names.arg=WDI_NEA$Country.Name,
  col=c("red","blue","green","orange","yellow"),
  main=(" GDP growth in North East Asia"),
  xlab="Countries",
  ylab="GDP Growth Rate",
  legend.text=c("Hong Kong","China","Japan","Mongolia","South Korea"),
  args.legend=list(x="topright",bty="n")
#)
  
#)

library(ggplot2)

# Assuming WDI_EA is your dataset
ggplot(country_means, aes(x = Time, y = GDP_grw, color = Country.Name)) +
  geom_line() +
  labs(title = "GDP Growth Over Time (2011-2020)", x = "Year", y = "GDP Growth") +
  theme_minimal() +
  theme(legend.position = "top")



library(ggplot2)

# Assuming WDI_EA is your dataset
ggplot(WDI_EA, aes(x = Time, y = GDP_grw, fill = Country.Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "GDP Growth Over Time (2011-2020)", x = "Year", y = "GDP Growth") +
  theme_minimal() +
  theme(legend.position = "top")

library(ggplot2)

# Assuming WDI_EA is your dataset
ggplot(WDI_EA, aes(x = Country.Name, y = GDP_grw)) +
  geom_boxplot() +
  labs(title = "Distribution of GDP Growth Rate by Country (2011-2020)", x = "Country", y = "GDP Growth Rate") +
  theme_minimal()
#............................................................................
#Correlation
#Task 2




#Regions
WDI_EA_GDP_grw=mean(WDI_EA$GDP_grw)
WDI_SEA_GDP_grw=mean(WDI_SEA$GDP_grw)
WDI_NEA_GDP_grw=mean(WDI_NEA$GDP_grw)

#SEA Countries
Indonesia_GDP_grw=mean(Indonesia$GDP_grw)
Malaysia_GDP_grw=mean(Malaysia$GDP_grw)
Singapore_GDP_grw=mean(Singapore$GDP_grw)
Thailand_GDP_grw=mean(Thailand$GDP_grw)
VietNam_GDP_grw=mean(VietNam$GDP_grw)
Myanmar_GDP_grw=mean(Myanmar$GDP_grw)
Philippines_GDP_grw=mean(Philippines$GDP_grw)
Cambodia_GDP_grw=mean(Cambodia$GDP_grw)

#NEA Countries
HongKong_GDP_grw=mean(HongKong$GDP_grw)
China_GDP_grw=mean(China$GDP_grw)
Japan_GDP_grw=mean(Japan$GDP_grw)
Mongolia_GDP_grw=mean(Mongolia$GDP_grw)
SouthKorea_GDP_grw=mean(SouthKorea$GDP_grw)


#Mean: GDP_pc
barplot(WDI_EA$GDP_pc,names.arg=WDI_EA$Country.Name,col=rainbow(8),main="10 years GDP per capita by Country")

#Regions
WDI_EA_GDP_pc=mean(WDI_EA$GDP_pc)
WDI_SEA_GDP_pc=mean(WDI_SEA$GDP_pc)
WDI_NEA_GDP_pc=mean(WDI_NEA$GDP_pc)

#SEA Countries
Indonesia_GDP_pc=mean(Indonesia$GDP_pc)
Malaysia_GDP_pc=mean(Malaysia$GDP_pc)
Singapore_GDP_pc=mean(Singapore$GDP_pc)
Thailand_GDP_pc=mean(Thailand$GDP_pc)
VietNam_GDP_pc=mean(VietNam$GDP_pc)
Myanmar_GDP_pc=mean(Myanmar$GDP_pc)
Philippines_GDP_pc=mean(Philippines$GDP_pc)
Cambodia_GDP_pc=mean(Cambodia$GDP_pc)

#NEA Countries
HongKong_GDP_pc=mean(HongKong$GDP_pc)
China_GDP_pc=mean(China$GDP_pc)
Japan_GDP_pc=mean(Japan$GDP_pc)
Mongolia_GDP_pc=mean(Mongolia$GDP_pc)
SouthKorea_GDP_pc=mean(SouthKorea$GDP_pc)




#Mean: Annual % growth in Industry
barplot(WDI_EA$Industry,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Growth Rate in Industry by Country")
#Regions
WDI_EA_Industry=mean(WDI_EA$Industry)
WDI_SEA_Industry=mean(WDI_SEA$Industry)
WDI_NEA_Industry=mean(WDI_NEA$Industry)

#SEA Countries
Indonesia_Industry=mean(Indonesia$Industry)
Malaysia_Industry=mean(Malaysia$Industry)
Singapore_Industry=mean(Singapore$Industry)
Thailand_Industry=mean(Thailand$Industry)
VietNam_Industry=mean(VietNam$Industry)
Myanmar_Industry=mean(Myanmar$Industry)
Philippines_Industry=mean(Philippines$Industry)
Cambodia_Industry=mean(Cambodia$Industry)

#NEA Countries
HongKong_Industry=mean(HongKong$Industry)
China_Industry=mean(China$Industry)
Japan_Industry=mean(Japan$Industry)
Mongolia_Industry=mean(Mongolia$Industry)
SouthKorea_Industry=mean(SouthKorea$Industry)


#Mean: Annual % growth in Services
barplot(WDI_EA$Services,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years growth Rate in Services by Country")
#Regions
WDI_EA_Services=mean(WDI_EA$Services)
WDI_SEA_Services=mean(WDI_SEA$Services)
WDI_NEA_Services=mean(WDI_NEA$Services)

#SEA Countries
Indonesia_Services=mean(Indonesia$Services)
Malaysia_Services=mean(Malaysia$Services)
Singapore_Services=mean(Singapore$Services)
Thailand_Services=mean(Thailand$Services)
VietNam_Services=mean(VietNam$Services)
Myanmar_Services=mean(Myanmar$Services)
Philippines_Services=mean(Philippines$Services)
Cambodia_Services=mean(Cambodia$Services)

#NEA Countries
HongKong_Services=mean(HongKong$Services)
China_Services=mean(China$Services)
Japan_Services=mean(Japan$Services)
Mongolia_Services=mean(Mongolia$Services)
SouthKorea_Services=mean(SouthKorea$Services)
#barplot(country_means$Mean_Ser,names.arg=country_means$Country_Region,col=rainbow(8),main = "Average Growth Rate in Services by Country")

#Mean: Annual % growth in Employment to Population ratio
barplot(WDI_EA$Emp_pop,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Employment to Population ratio by Country")
#Regions
WDI_EA_Emp_pop=mean(WDI_EA$Emp_pop)
WDI_SEA_Emp_pop=mean(WDI_SEA$Emp_pop)
WDI_NEA_Emp_pop=mean(WDI_NEA$Emp_pop)

#SEA Countries
Indonesia_Emp_pop=mean(Indonesia$Emp_pop)
Malaysia_Emp_pop=mean(Malaysia$Emp_pop)
Singapore_Emp_pop=mean(Singapore$Emp_pop)
Thailand_Emp_pop=mean(Thailand$Emp_pop)
VietNam_Emp_pop=mean(VietNam$Emp_pop)
Myanmar_Emp_pop=mean(Myanmar$Emp_pop)
Philippines_Emp_pop=mean(Philippines$Emp_pop)
Cambodia_Emp_pop=mean(Cambodia$Emp_pop)

#NEA Countries
HongKong_Emp_pop=mean(HongKong$Emp_pop)
China_Emp_pop=mean(China$Emp_pop)
Japan_Emp_pop=mean(Japan$Emp_pop)
Mongolia_Emp_pop=mean(Mongolia$Emp_pop)
SouthKorea_Emp_pop=mean(SouthKorea$Emp_pop)


#Mean: Annual % of Inflation
barplot(WDI_EA$Inflation,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Inflation by Country")
#Regions
WDI_EA_Inflation=mean(WDI_EA$Inflation)
WDI_SEA_Inflation=mean(WDI_SEA$Inflation)
WDI_NEA_Inflation=mean(WDI_NEA$Inflation)

#SEA Countries
Indonesia_Inflation=mean(Indonesia$Inflation)
Malaysia_Inflation=mean(Malaysia$Inflation)
Singapore_Inflation=mean(Singapore$Inflation)
Thailand_Inflation=mean(Thailand$Inflation)
VietNam_Inflation=mean(VietNam$Inflation)
Myanmar_Inflation=mean(Myanmar$Inflation)
Philippines_Inflation=mean(Philippines$Inflation)
Cambodia_Inflation=mean(Cambodia$Inflation)

#NEA Countries
HongKong_Inflation=mean(HongKong$Inflation)
China_Inflation=mean(China$Inflation)
Japan_Inflation=mean(Japan$Inflation)
Mongolia_Inflation=mean(Mongolia$Inflation)
SouthKorea_Inflation=mean(SouthKorea$Inflation)

#Mean: Annual % of Exports
barplot(WDI_EA$Exports,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Exports by Country")
#Regions
WDI_EA_Exports=mean(WDI_EA$Exports)
WDI_SEA_Exports=mean(WDI_SEA$Exports)
WDI_NEA_Exports=mean(WDI_NEA$Exports)

#SEA Countries
Indonesia_Exports=mean(Indonesia$Exports)
Malaysia_Exports=mean(Malaysia$Exports)
Singapore_Exports=mean(Singapore$Exports)
Thailand_Exports=mean(Thailand$Exports)
VietNam_Exports=mean(VietNam$Exports)
Myanmar_Exports=mean(Myanmar$Exports)
Philippines_Exports=mean(Philippines$Exports)
Cambodia_Exports=mean(Cambodia$Exports)

#NEA Countries
HongKong_Exports=mean(HongKong$Exports)
China_Exports=mean(China$Exports)
Japan_Exports=mean(Japan$Exports)
Mongolia_Exports=mean(Mongolia$Exports)
SouthKorea_Exports=mean(SouthKorea$Exports)


#Mean: Annual % of Imports
barplot(WDI_EA$Imports,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Imports by Country")
#Regions
WDI_EA_Imports=mean(WDI_EA$Imports)
WDI_SEA_Imports=mean(WDI_SEA$Imports)
WDI_NEA_Imports=mean(WDI_NEA$Imports)

#SEA Countries
Indonesia_Imports=mean(Indonesia$Imports)
Malaysia_Imports=mean(Malaysia$Imports)
Singapore_Imports=mean(Singapore$Imports)
Thailand_Imports=mean(Thailand$Imports)
VietNam_Imports=mean(VietNam$Imports)
Myanmar_Imports=mean(Myanmar$Imports)
Philippines_Imports=mean(Philippines$Imports)
Cambodia_Imports=mean(Cambodia$Imports)

#NEA Countries
HongKong_Imports=mean(HongKong$Imports)
China_Imports=mean(China$Imports)
Japan_Imports=mean(Japan$Imports)
Mongolia_Imports=mean(Mongolia$Imports)
SouthKorea_Imports=mean(SouthKorea$Imports)



#Mean: Annual % of Foreign.DI
barplot(WDI_EA$FDI,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Foreign.DI by Country")
#Regions
WDI_EA_FDI=mean(WDI_EA$FDI)
WDI_SEA_FDI=mean(WDI_SEA$FDI)
WDI_NEA_FDI=mean(WDI_NEA$FDI)

#SEA Countries
Indonesia_FDI=mean(Indonesia$FDI)
Malaysia_FDI=mean(Malaysia$FDI)
Singapore_FDI=mean(Singapore$FDI)
Thailand_FDI=mean(Thailand$FDI)
VietNam_FDI=mean(VietNam$FDI)
Myanmar_FDI=mean(Myanmar$FDI)
Philippines_FDI=mean(Philippines$FDI)
Cambodia_FDI=mean(Cambodia$FDI)

#NEA Countries
HongKong_FDI=mean(HongKong$FDI)
China_FDI=mean(China$FDI)
Japan_FDI=mean(Japan$FDI)
Mongolia_FDI=mean(Mongolia$FDI)
SouthKorea_FDI=mean(SouthKorea$FDI)

#Mean: Annual % of Labor.force
barplot(WDI_EA$Labor.force,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Labor.force by Country")
#Regions
WDI_EA_Labor.force=mean(WDI_EA$Labor.force)
WDI_SEA_Labor.force=mean(WDI_SEA$Labor.force)
WDI_NEA_Labor.force=mean(WDI_NEA$Labor.force)

#SEA Countries
Indonesia_Labor.force=mean(Indonesia$Labor.force)
Malaysia_Labor.force=mean(Malaysia$Labor.force)
Singapore_Labor.force=mean(Singapore$Labor.force)
Thailand_Labor.force=mean(Thailand$Labor.force)
VietNam_Labor.force=mean(VietNam$Labor.force)
Myanmar_Labor.force=mean(Myanmar$Labor.force)
Philippines_Labor.force=mean(Philippines$Labor.force)
Cambodia_Labor.force=mean(Cambodia$Labor.force)

#NEA Countries
HongKong_Labor.force=mean(HongKong$Labor.force)
China_Labor.force=mean(China$Labor.force)
Japan_Labor.force=mean(Japan$Labor.force)
Mongolia_Labor.force=mean(Mongolia$Labor.force)
SouthKorea_Labor.force=mean(SouthKorea$Labor.force)


#Mean: Annual % of Employment in agriculture
barplot(WDI_EA$Emp_agri,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Employment in agriculture by Country")
#Regions
WDI_EA_Emp_agri=mean(WDI_EA$Emp_agri)
WDI_SEA_Emp_agri=mean(WDI_SEA$Emp_agri)
WDI_NEA_Emp_agri=mean(WDI_NEA$Emp_agri)

#SEA Countries
Indonesia_Emp_agri=mean(Indonesia$Emp_agri)
Malaysia_Emp_agri=mean(Malaysia$Emp_agri)
Singapore_Emp_agri=mean(Singapore$Emp_agri)
Thailand_Emp_agri=mean(Thailand$Emp_agri)
VietNam_Emp_agri=mean(VietNam$Emp_agri)
Myanmar_Emp_agri=mean(Myanmar$Emp_agri)
Philippines_Emp_agri=mean(Philippines$Emp_agri)
Cambodia_Emp_agri=mean(Cambodia$Emp_agri)

#NEA Countries
HongKong_Emp_agri=mean(HongKong$Emp_agri)
China_Emp_agri=mean(China$Emp_agri)
Japan_Emp_agri=mean(Japan$Emp_agri)
Mongolia_Emp_agri=mean(Mongolia$Emp_agri)
SouthKorea_Emp_agri=mean(SouthKorea$Emp_agri)


#Mean: Annual % of Employment in industry
barplot(WDI_EA$Emp_ind,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Employment in industry by Country")
#Regions
WDI_EA_Emp_ind=mean(WDI_EA$Emp_ind)
WDI_SEA_Emp_ind=mean(WDI_SEA$Emp_ind)
WDI_NEA_Emp_ind=mean(WDI_NEA$Emp_ind)

#SEA Countries
Indonesia_Emp_ind=mean(Indonesia$Emp_ind)
Malaysia_Emp_ind=mean(Malaysia$Emp_ind)
Singapore_Emp_ind=mean(Singapore$Emp_ind)
Thailand_Emp_ind=mean(Thailand$Emp_ind)
VietNam_Emp_ind=mean(VietNam$Emp_ind)
Myanmar_Emp_ind=mean(Myanmar$Emp_ind)
Philippines_Emp_ind=mean(Philippines$Emp_ind)
Cambodia_Emp_ind=mean(Cambodia$Emp_ind)

#NEA Countries
HongKong_Emp_ind=mean(HongKong$Emp_ind)
China_Emp_ind=mean(China$Emp_ind)
Japan_Emp_ind=mean(Japan$Emp_ind)
Mongolia_Emp_ind=mean(Mongolia$Emp_ind)
SouthKorea_Emp_ind=mean(SouthKorea$Emp_ind)



#Mean: Annual % of Employment in services
barplot(WDI_EA$Emp_ser,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years Employment in services by Country")
#Regions
WDI_EA_Emp_ser=mean(WDI_EA$Emp_ser)
WDI_SEA_Emp_ser=mean(WDI_SEA$Emp_ser)
WDI_NEA_Emp_ser=mean(WDI_NEA$Emp_ser)

#SEA Countries
Indonesia_Emp_ser=mean(Indonesia$Emp_ser)
Malaysia_Emp_ser=mean(Malaysia$Emp_ser)
Singapore_Emp_ser=mean(Singapore$Emp_ser)
Thailand_Emp_ser=mean(Thailand$Emp_ser)
VietNam_Emp_ser=mean(VietNam$Emp_ser)
Myanmar_Emp_ser=mean(Myanmar$Emp_ser)
Philippines_Emp_ser=mean(Philippines$Emp_ser)
Cambodia_Emp_ser=mean(Cambodia$Emp_ser)

#NEA Countries
HongKong_Emp_ser=mean(HongKong$Emp_ser)
China_Emp_ser=mean(China$Emp_ser)
Japan_Emp_ser=mean(Japan$Emp_ser)
Mongolia_Emp_ser=mean(Mongolia$Emp_ser)
SouthKorea_Emp_ser=mean(SouthKorea$Emp_ser)

#Mean: Annual % of GCF
barplot(WDI_EA$GCF,names.arg=WDI_EA$Country.Name,col=rainbow(8),main = "10 years GCF by Country")
#Regions
WDI_EA_GCF=mean(WDI_EA$GCF)
WDI_SEA_GCF=mean(WDI_SEA$GCF)
WDI_NEA_GCF=mean(WDI_NEA$GCF)

#SEA Countries
Indonesia_GCF=mean(Indonesia$GCF)
Malaysia_GCF=mean(Malaysia$GCF)
Singapore_GCF=mean(Singapore$GCF)
Thailand_GCF=mean(Thailand$GCF)
VietNam_GCF=mean(VietNam$GCF)
Myanmar_GCF=mean(Myanmar$GCF)
Philippines_GCF=mean(Philippines$GCF)
Cambodia_GCF=mean(Cambodia$GCF)

#NEA Countries
HongKong_GCF=mean(HongKong$GCF)
China_GCF=mean(China$GCF)
Japan_GCF=mean(Japan$GCF)
Mongolia_GCF=mean(Mongolia$GCF)
SouthKorea_GCF=mean(SouthKorea$GCF)









#................................................................

Country_Region = c("East Asia","South East Asia","North East Asia", "Indonesia", "Malaysia", "Singapore", "Thailand", "VietNam", "Myanmar","Philippines", "Cambodia", "HongKong", "China", "Japan", "Mongolia", "SouthKorea")

Mean_GDP_grw = c(WDI_EA_GDP_grw,WDI_SEA_GDP_grw,WDI_NEA_GDP_grw,Indonesia_GDP_grw, Malaysia_GDP_grw, Singapore_GDP_grw, Thailand_GDP_grw,VietNam_GDP_grw, Myanmar_GDP_grw, Philippines_GDP_grw, Cambodia_GDP_grw,HongKong_GDP_grw, China_GDP_grw, Japan_GDP_grw, Mongolia_GDP_grw, SouthKorea_GDP_grw)
Mean_GDP_pc = c(WDI_EA_GDP_pc,WDI_SEA_GDP_pc,WDI_NEA_GDP_pc,Indonesia_GDP_pc, Malaysia_GDP_pc, Singapore_GDP_pc, Thailand_GDP_pc,VietNam_GDP_pc, Myanmar_GDP_pc, Philippines_GDP_pc, Cambodia_GDP_pc,HongKong_GDP_pc, China_GDP_pc, Japan_GDP_pc, Mongolia_GDP_pc, SouthKorea_GDP_pc)
Mean_Ind = c(WDI_EA_Industry,WDI_SEA_Industry,WDI_NEA_Industry,Indonesia_Industry, Malaysia_Industry, Singapore_Industry, Thailand_Industry,VietNam_Industry, Myanmar_Industry, Philippines_Industry, Cambodia_Industry,HongKong_Industry, China_Industry, Japan_Industry, Mongolia_Industry, SouthKorea_Industry)
Mean_Ser=c(WDI_EA_Services,WDI_SEA_Services,WDI_NEA_Services,Indonesia_Services, Malaysia_Services, Singapore_Services, Thailand_Services,VietNam_Services, Myanmar_Services, Philippines_Services, Cambodia_Services,HongKong_Services, China_Services, Japan_Services, Mongolia_Services, SouthKorea_Services)
Mean_Emp_pop=c(WDI_EA_Emp_pop,WDI_SEA_Emp_pop,WDI_NEA_Emp_pop,Indonesia_Emp_pop, Malaysia_Emp_pop, Singapore_Emp_pop, Thailand_Emp_pop,VietNam_Emp_pop, Myanmar_Emp_pop, Philippines_Emp_pop, Cambodia_Emp_pop,HongKong_Emp_pop, China_Emp_pop, Japan_Emp_pop, Mongolia_Emp_pop, SouthKorea_Emp_pop)
Mean_Inflation=c(WDI_EA_Inflation,WDI_SEA_Inflation,WDI_NEA_Inflation,Indonesia_Inflation, Malaysia_Inflation, Singapore_Inflation, Thailand_Inflation,VietNam_Inflation, Myanmar_Inflation, Philippines_Inflation, Cambodia_Inflation,HongKong_Inflation, China_Inflation, Japan_Inflation, Mongolia_Inflation, SouthKorea_Inflation)
Mean_Exports=c(WDI_EA_Exports,WDI_SEA_Exports,WDI_NEA_Exports,Indonesia_Exports, Malaysia_Exports, Singapore_Exports, Thailand_Exports,VietNam_Exports, Myanmar_Exports, Philippines_Exports, Cambodia_Exports,HongKong_Exports, China_Exports, Japan_Exports, Mongolia_Exports, SouthKorea_Exports)
Mean_Imports=c(WDI_EA_Imports,WDI_SEA_Imports,WDI_NEA_Imports,Indonesia_Imports, Malaysia_Imports, Singapore_Imports, Thailand_Imports,VietNam_Imports, Myanmar_Imports, Philippines_Imports, Cambodia_Imports,HongKong_Imports, China_Imports, Japan_Imports, Mongolia_Imports, SouthKorea_Imports)
Mean_FDI=c(WDI_EA_FDI,WDI_SEA_FDI,WDI_NEA_FDI,Indonesia_FDI, Malaysia_FDI, Singapore_FDI, Thailand_FDI,VietNam_FDI, Myanmar_FDI, Philippines_FDI, Cambodia_FDI,HongKong_FDI, China_FDI, Japan_FDI, Mongolia_FDI, SouthKorea_FDI)
Mean_Labor.force=c(WDI_EA_Labor.force,WDI_SEA_Labor.force,WDI_NEA_Labor.force,Indonesia_Labor.force, Malaysia_Labor.force, Singapore_Labor.force, Thailand_Labor.force,VietNam_Labor.force, Myanmar_Labor.force, Philippines_Labor.force, Cambodia_Labor.force,HongKong_Labor.force, China_Labor.force, Japan_Labor.force, Mongolia_Labor.force, SouthKorea_Labor.force)
Mean_Emp_agri=c(WDI_EA_Emp_agri,WDI_SEA_Emp_agri,WDI_NEA_Emp_agri,Indonesia_Emp_agri, Malaysia_Emp_agri, Singapore_Emp_agri, Thailand_Emp_agri,VietNam_Emp_agri, Myanmar_Emp_agri, Philippines_Emp_agri, Cambodia_Emp_agri,HongKong_Emp_agri, China_Emp_agri, Japan_Emp_agri, Mongolia_Emp_agri, SouthKorea_Emp_agri)
Mean_Emp_ind=c(WDI_EA_Emp_ind,WDI_SEA_Emp_ind,WDI_NEA_Emp_ind,Indonesia_Emp_ind, Malaysia_Emp_ind, Singapore_Emp_ind, Thailand_Emp_ind,VietNam_Emp_ind, Myanmar_Emp_ind, Philippines_Emp_ind, Cambodia_Emp_ind,HongKong_Emp_ind, China_Emp_ind, Japan_Emp_ind, Mongolia_Emp_ind, SouthKorea_Emp_ind)
Mean_Emp_ser=c(WDI_EA_Emp_ser,WDI_SEA_Emp_ser,WDI_NEA_Emp_ser,Indonesia_Emp_ser, Malaysia_Emp_ser, Singapore_Emp_ser, Thailand_Emp_ser,VietNam_Emp_ser, Myanmar_Emp_ser, Philippines_Emp_ser, Cambodia_Emp_ser,HongKong_Emp_ser, China_Emp_ser, Japan_Emp_ser, Mongolia_Emp_ser, SouthKorea_Emp_ser)
Mean_GCF=c(WDI_EA_GCF,WDI_SEA_GCF,WDI_NEA_GCF,Indonesia_GCF, Malaysia_GCF, Singapore_GCF, Thailand_GCF,VietNam_GCF, Myanmar_GCF, Philippines_GCF, Cambodia_GCF,HongKong_GCF, China_GCF, Japan_GCF, Mongolia_GCF, SouthKorea_GCF)

#..........................
# Create a data frame

country_means <- data.frame(Country_Region,Mean_GDP_grw,Mean_GDP_pc,Mean_Ind,Mean_Ser,Mean_Emp_pop,Mean_Inflation,Mean_Exports,Mean_Imports,Mean_Foreign.DI,Mean_Labor.force,Mean_Emp_agri,Mean_Emp_ind,Mean_Emp_ser,Mean_GCF)

Country_Region1 = c( "Indonesia", "Malaysia", "Singapore", "Thailand", "VietNam", "Myanmar","Philippines", "Cambodia", "HongKong", "China", "Japan", "Mongolia", "SouthKorea")

Mean_GDP_grw = c(Indonesia_GDP_grw, Malaysia_GDP_grw, Singapore_GDP_grw, Thailand_GDP_grw,VietNam_GDP_grw, Myanmar_GDP_grw, Philippines_GDP_grw, Cambodia_GDP_grw,HongKong_GDP_grw, China_GDP_grw, Japan_GDP_grw, Mongolia_GDP_grw, SouthKorea_GDP_grw)
Mean_GDP_pc = c(Indonesia_GDP_pc, Malaysia_GDP_pc, Singapore_GDP_pc, Thailand_GDP_pc,VietNam_GDP_pc, Myanmar_GDP_pc, Philippines_GDP_pc, Cambodia_GDP_pc,HongKong_GDP_pc, China_GDP_pc, Japan_GDP_pc, Mongolia_GDP_pc, SouthKorea_GDP_pc)
Mean_Ind = c(Indonesia_Industry, Malaysia_Industry, Singapore_Industry, Thailand_Industry,VietNam_Industry, Myanmar_Industry, Philippines_Industry, Cambodia_Industry,HongKong_Industry, China_Industry, Japan_Industry, Mongolia_Industry, SouthKorea_Industry)
Mean_Ser=c(Indonesia_Services, Malaysia_Services, Singapore_Services, Thailand_Services,VietNam_Services, Myanmar_Services, Philippines_Services, Cambodia_Services,HongKong_Services, China_Services, Japan_Services, Mongolia_Services, SouthKorea_Services)
Mean_Emp_pop=c(Indonesia_Emp_pop, Malaysia_Emp_pop, Singapore_Emp_pop, Thailand_Emp_pop,VietNam_Emp_pop, Myanmar_Emp_pop, Philippines_Emp_pop, Cambodia_Emp_pop,HongKong_Emp_pop, China_Emp_pop, Japan_Emp_pop, Mongolia_Emp_pop, SouthKorea_Emp_pop)
Mean_Inflation=c(Indonesia_Inflation, Malaysia_Inflation, Singapore_Inflation, Thailand_Inflation,VietNam_Inflation, Myanmar_Inflation, Philippines_Inflation, Cambodia_Inflation,HongKong_Inflation, China_Inflation, Japan_Inflation, Mongolia_Inflation, SouthKorea_Inflation)
Mean_Exports=c(Indonesia_Exports, Malaysia_Exports, Singapore_Exports, Thailand_Exports,VietNam_Exports, Myanmar_Exports, Philippines_Exports, Cambodia_Exports,HongKong_Exports, China_Exports, Japan_Exports, Mongolia_Exports, SouthKorea_Exports)
Mean_Imports=c(Indonesia_Imports, Malaysia_Imports, Singapore_Imports, Thailand_Imports,VietNam_Imports, Myanmar_Imports, Philippines_Imports, Cambodia_Imports,HongKong_Imports, China_Imports, Japan_Imports, Mongolia_Imports, SouthKorea_Imports)
Mean_FDI=c(Indonesia_FDI, Malaysia_FDI, Singapore_FDI, Thailand_FDI,VietNam_FDI, Myanmar_FDI, Philippines_FDI, Cambodia_FDI,HongKong_FDI, China_FDI, Japan_FDI, Mongolia_FDI, SouthKorea_FDI)
Mean_Labor.force=c(Indonesia_Labor.force, Malaysia_Labor.force, Singapore_Labor.force, Thailand_Labor.force,VietNam_Labor.force, Myanmar_Labor.force, Philippines_Labor.force, Cambodia_Labor.force,HongKong_Labor.force, China_Labor.force, Japan_Labor.force, Mongolia_Labor.force, SouthKorea_Labor.force)
Mean_Emp_agri=c(Indonesia_Emp_agri, Malaysia_Emp_agri, Singapore_Emp_agri, Thailand_Emp_agri,VietNam_Emp_agri, Myanmar_Emp_agri, Philippines_Emp_agri, Cambodia_Emp_agri,HongKong_Emp_agri, China_Emp_agri, Japan_Emp_agri, Mongolia_Emp_agri, SouthKorea_Emp_agri)
Mean_Emp_ind=c(Indonesia_Emp_ind, Malaysia_Emp_ind, Singapore_Emp_ind, Thailand_Emp_ind,VietNam_Emp_ind, Myanmar_Emp_ind, Philippines_Emp_ind, Cambodia_Emp_ind,HongKong_Emp_ind, China_Emp_ind, Japan_Emp_ind, Mongolia_Emp_ind, SouthKorea_Emp_ind)
Mean_Emp_ser=c(Indonesia_Emp_ser, Malaysia_Emp_ser, Singapore_Emp_ser, Thailand_Emp_ser,VietNam_Emp_ser, Myanmar_Emp_ser, Philippines_Emp_ser, Cambodia_Emp_ser,HongKong_Emp_ser, China_Emp_ser, Japan_Emp_ser, Mongolia_Emp_ser, SouthKorea_Emp_ser)
Mean_GCF=c(Indonesia_GCF, Malaysia_GCF, Singapore_GCF, Thailand_GCF,VietNam_GCF, Myanmar_GCF, Philippines_GCF, Cambodia_GCF,HongKong_GCF, China_GCF, Japan_GCF, Mongolia_GCF, SouthKorea_GCF)

#..........................
# Create a data frame
countries_means <- data.frame(Country_Region1,Mean_GDP_grw,Mean_GDP_pc,Mean_Ind,Mean_Ser,Mean_Emp_pop,Mean_Inflation,Mean_Exports,Mean_Imports,Mean_Foreign.DI,Mean_Labor.force,Mean_Emp_agri,Mean_Emp_ind,Mean_Emp_ser,Mean_GCF)


# Remove the Country_Region1 column from countries_means and create a new data frame

#countries_means_new <- countries_means %>% select(-"Country_Region1")
#round(cor(countries_means_new),digits = 2)

#corrplot(cor(countries_means_new), method = "number", type = "upper")

cor(countries_means$Mean_GDP_grw,countries_means$Mean_Emp_pop)
cor(countries_means$Mean_GDP_grw,countries_means$Mean_Labor.force)

cor(countries_means$Mean_GDP_grw,countries_means$Mean_Exports)
cor(countries_means$Mean_GDP_grw,countries_means$Mean_Imports)


cor(countries_means$Mean_GDP_grw,countries_means$Mean_Emp_agri)
cor(countries_means$Mean_GDP_grw,countries_means$Mean_Emp_ser)

plot(countries_means$Mean_Emp_pop,countries_means$Mean_Labor.force)
cor(countries_means$Mean_Emp_pop,countries_means$Mean_Labor.force)

plot(countries_means$Mean_Exports,countries_means$Mean_Imports)
cor(countries_means$Mean_Exports,countries_means$Mean_Imports)

plot(countries_means$Mean_Emp_agri,countries_means$Mean_Emp_ser)
cor(countries_means$Mean_Emp_agri,countries_means$Mean_Emp_ser)


# Assuming 'countries_means' is your dataframe
# and you have loaded the 'ppcor' package



# Extract the relevant columns
selected_columns <- countries_means[, c("Mean_GDP_grw", "Mean_Emp_pop", "Mean_Labor.force")]

# Compute partial correlation
partial_corr_matrix <- cor(selected_columns)

# Display the partial correlation matrix
print(partial_corr_matrix)


# Extract the relevant columns
selected_columns <- countries_means[, c("Mean_GDP_grw", "Mean_Emp_agri", "Mean_Emp_ser")]

# Compute partial correlation
partial_corr_matrix <- cor(selected_columns)

# Display the partial correlation matrix
print(partial_corr_matrix)


# Extract the relevant columns
selected_columns <- countries_means[, c("Mean_GDP_grw", "Mean_Exports", "Mean_Imports")]

# Compute partial correlation
partial_corr_matrix <- cor(selected_columns)

# Display the partial correlation matrix
print(partial_corr_matrix)

SEA_category=c("Indonesia","Malaysia","Singapore","Thailand","VietNam","Myanmar","Philippines","Cambodia")
NEA_category=c("Hong Kong SAR, China","China","Japan","Mongolia","Korea, Rep.")
countries_means$Region <- ifelse(countries_means$Country_Region1 %in% SEA_category, "SEA", "NEA")
countries_means$categorical_Region=ifelse(countries_means$Region == 'SEA', 1, 0)


cor.test(countries_means$Mean_GDP_grw , countries_means$categorical_Region)

plot(countries_means$categorical_Region , countries_means$Mean_GDP_grw)



head(WDI_EA)
tail(WDI_EA)
dim(WDI_EA)
summary(WDI_EA)


plot(density(WDI_EA$GDP_grw))
mean_GDP_grw_SEA = mean(WDI_EA[WDI_EA$Region=='SEA',3]) 
mean_GDP_grw_NEA = mean(WDI_EA[WDI_EA$Region=='NEA',3]) 
sd_GDP_grw_SEA = sd(WDI_EA[WDI_EA$Region=='SEA',3]) 
sd_GDP_grw_NEA = sd(WDI_EA[WDI_EA$Region=='NEA',3]) 

t_score <- qt(p=(1-0.95)/2, df=128, lower.tail=FALSE)
margin_error = t_score*sqrt((79*sd_GDP_grw_SEA^2 + 49*sd_GDP_grw_NEA^2)/128)


mean_diff = mean_GDP_grw_SEA - mean_GDP_grw_NEA
mean_sum=mean_GDP_grw_SEA + mean_GDP_grw_NEA
sprintf("The 95 per cent confidence interval for the  mean GDP growth (annual) of East Asia  is from %.02f  to %.02f ", mean_diff-margin_error,mean_diff+margin_error)
sprintf("The 95 per cent confidence interval for the  mean GDP growth (annual) of East Asia  is from %.02f  to %.02f ", mean_sum-margin_error,mean_sum+margin_error)


mean(WDI_SEA$GDP_grw)
sd(WDI_SEA$GDP_grw)
dim(WDI_SEA)
t_score <- qt(p=(1-0.95)/2, df=80-1, lower.tail=FALSE)
margin_error <- t_score*sd(WDI_SEA$GDP_grw)/sqrt(80)
sprintf("The 95 per cent confidence interval for the mean percentage GDP growth of East Asia is from %.02f to %.02f ", mean(WDI_SEA$GDP_grw)-margin_error,mean(WDI_SEA$GDP_grw)+margin_error)

mean(WDI_NEA$GDP_grw)
sd(WDI_NEA$GDP_grw)
dim(WDI_NEA)
t_score <- qt(p=(1-0.95)/2, df=50-1, lower.tail=FALSE)
margin_error <- t_score*sd(WDI_NEA$GDP_grw)/sqrt(50)
sprintf("The 95 per cent confidence interval for the mean percentage GDP growth of East Asia from %.02f  to %.02f ", mean(WDI_NEA$GDP_grw)-margin_error,mean(WDI_NEA$GDP_grw)+margin_error)



#Task 3
#Hypothesis testing

ggplot(mapping = aes(sample =WDI_SEA$Labor.force )) + stat_qq_point(size = 2,color = "blue") + stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")

shapiro.test(WDI_SEA$Labor.force)

round(mean(WDI_SEA$Labor.force),2)

#considering indonesia as sample
hist(Indonesia$Labor.force)
mean(Indonesia$Labor.force)
sd(Indonesia$Labor.force)

t.test(Indonesia$Labor.force, mu=67.85, alternative="greater")



boxplot(Inflation ~ Region, data=WDI_EA, names=c("SEA", "NEA"), xlab="Region", ylab="Inflation", main="Annual Percentage Inflation of \nSEA and NEA")
t.test(Inflation ~ Region, WDI_EA)

mean(WDI_SEA$Inflation)
mean(WDI_NEA$Inflation)




Exports_2019 <- WDI_EA[WDI_EA$Time == 2019, c("Country.Name","Exports")]
Exports_2020 <- WDI_EA[WDI_EA$Time == 2020, c("Country.Name","Exports")]


Exports_2019_2020=data.frame(Country=Exports_2019$Country.Name,Exports_2019=Exports_2019$Exports,Exports_2020=Exports_2020$Exports)
boxplot(Exports_2019_2020$Exports_2019, Exports_2019_2020$Exports_2020, names=c("2019", "2020"), xlab="Before & After COVID Outbreak", ylab="Exports (% of GDP)", main="Export Trade Before & After COVID Outbreak")

t.test(Exports_2019_2020$Exports_2019, Exports_2019_2020$Exports_2020, paired=TRUE)


GCF_SEA_NEA=data.frame(GCF=WDI_EA$GCF,Region=WDI_EA$Region)
GCF_SEA_NEA$Region <- as.factor(GCF_SEA_NEA$Region)
summary(GCF_SEA_NEA)


GCF_SEA<-GCF_SEA_NEA$GCF[GCF_SEA_NEA$Region=="SEA"] 
ggplot(mapping = aes(sample = GCF_SEA)) + stat_qq_point(size = 2,color = "blue") + stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")
hist(GCF_SEA)



GCF_NEA<-GCF_SEA_NEA$GCF[GCF_SEA_NEA$Region=="NEA"] 
ggplot(mapping = aes(sample = GCF_NEA)) + stat_qq_point(size = 2,color = "blue") + stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")
hist(GCF_NEA)


wilcox.test(GCF ~ Region, data=GCF_SEA_NEA)
------------------------------------------------------------------
#Task 4
#Regression Analysis
  
WDI_EA_num <- WDI_EA[ ,c('GDP_grw','GDP_pc','Industry','Services','Emp_pop','Inflation','Exports','Imports','FDI','Labor.force','Emp_agri','Emp_ind','Emp_ser','GCF')]
corrplot(cor(WDI_EA_num))

model_1 <-lm(Labor.force ~ Emp_pop, WDI_EA_num) 
summary.lm(model_1)

plot(Labor.force ~ Emp_pop, WDI_EA_num, col = "blue", main = "Regression: Labor force participation \n& Employment-population ratio", xlab = "Emp_pop", ylab = "Labor.force")
abline(model_1, col="red")
plot(model_1, 1)
plot(model_1, 2)
plot(model_1, 3)

model_1 <-lm(GDP_grw ~ Industry, WDI_EA_num) 
summary.lm(model_1)

model_2 <-lm(GDP_grw ~ Industry + Services , WDI_EA_num) 
summary.lm(model_2)


model_3 <-lm(GDP_grw ~ Industry + Services + GCF , WDI_EA_num) 
summary.lm(model_3)

model_4 <-lm(GDP_grw ~ Industry + Services + GCF+ GDP_pc , WDI_EA_num) 
summary.lm(model_4)

data.frame(colnames(WDI_EA_num))
GDP_grw ~ Industry + Services + GCF

pairs(WDI_EA_num[,c(1,3,4,14)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(model_3, 1)
plot(model_3, 2)
residual_3=residuals(model_3)
hist(residual_3)

plot(model_3, 3)
vif(model_3)

WDI_EA_log=WDI_EA
WDI_EA_log$categorical_Region=ifelse(WDI_EA_log$Region == 'SEA', 1, 0)
WDI_EA_log
model_logistic_1 <- glm(categorical_Region ~  GDP_grw + GDP_pc +Industry +Services + Emp_pop + Inflation +Exports + Imports + FDI+ Labor.force+Emp_agri + Emp_ind + Emp_ser + GCF, data = WDI_EA_log, family = "binomial") 
summary(model_logistic_1)

model_logistic_1 <- glm(categorical_Region ~  GDP_grw + Industry +Services + Emp_pop + Inflation +Exports + Imports + Labor.force , data = WDI_EA_log, family = "binomial") 
summary(model_logistic_1)
Imp <- varImp(model_logistic_1, scale = FALSE) 
Imp

model_logistic_2 <- glm(categorical_Region ~  Services + Emp_pop + Inflation + Labor.force , data = WDI_EA_log, family = "binomial") 
summary(model_logistic_2)
Imp <- varImp(model_logistic_2, scale = FALSE) 
Imp

model_logistic_3 <- glm(categorical_Region ~ Emp_pop + Labor.force, data = WDI_EA_log, family = "binomial")
summary(model_logistic_3)

probs <- predict(model_logistic_2, data=WDI_EA_log,type="response") 
WDI_EA_log$probs <- probs

logits <- log(probs/(1-probs)) 
WDI_EA_log$logits <- logits

data.frame(colnames(WDI_EA_log))
pairs(WDI_EA_log[,c(20,6,7,8,12)], lower.panel = NULL, upper.panel = panel.smooth, pch = 19,cex = 0.2)
plot(model_logistic_2, which = 4, id.n = 3)
vif(model_logistic_2)
#...............................

model_logistic_1 <- glm(categorical_Region ~  GDP_grw  + Emp_pop + Inflation + Emp_agri + GCF, data = WDI_EA_log, family = "binomial") 
summary(model_logistic_1)
Imp <- varImp(model_logistic_1, scale = FALSE) 
Imp

model_logistic_2 <- glm(categorical_Region ~  Emp_pop +  Emp_agri + GCF , data = WDI_EA_log, family = "binomial") 
summary(model_logistic_2)
Imp <- varImp(model_logistic_2, scale = FALSE) 
Imp

model_logistic_3 <- glm(categorical_Region ~ Emp_pop + GCF, data = WDI_EA_log, family = "binomial")
summary(model_logistic_3)
Imp <- varImp(model_logistic_3, scale = FALSE) 
Imp

probs <- predict(model_logistic_1, data=WDI_EA_log,type="response") 
WDI_EA_log$probs <- probs

logits <- log(probs/(1-probs)) 
WDI_EA_log$logits <- logits

data.frame(colnames(WDI_EA_log))
pairs(WDI_EA_log[,c(20,7,13,16)], lower.panel = NULL, upper.panel = panel.smooth, pch = 19,cex = 0.2)
plot(model_logistic_1, which = 4, id.n = 3)
vif(model_logistic_1)
#-------------------------------
#Task 5
# Time series
data.frame(colnames(WDI_EA))

# Assuming you have data for these countries in a data frame named WDI_SEA
# Make sure you have the necessary data loaded or replace it with the actual data frame

# Extract data for the specified countries and years
#selected_countries <- c("Indonesia", "Malaysia", "Singapore", "Thailand", "Viet Nam", "Myanmar", "Philippines", "Cambodia")
#selected_years <- 2011:2020
#SEA
# Subset the original data frame based on selected countries and years
#WDI_SEA_Tseries <- subset(WDI_EA$GDP_grw, Country.Name %in% selected_countries & Time %in% selected_years)

# If needed, you can further subset or rearrange columns as necessary
# For example, you might want to select only specific columns of interest

# Print the resulting data frame
#print(WDI_SEA_Tseries)


# Define the years and countries
selected_years <- 2011:2020
selected_countries <- c("Indonesia", "Malaysia", "Singapore", "Thailand", "VietNam", "Myanmar", "Philippines", "Cambodia")

# Create an empty data frame
WDI_SEA_Tseries <- data.frame(matrix(nrow = length(selected_years), ncol = length(selected_countries)))

# Assign column and row names
colnames(WDI_SEA_Tseries) <- selected_countries
rownames(WDI_SEA_Tseries) <- as.character(selected_years)
WDI_SEA_Tseries <- WDI_SEA_Tseries %>%
  rownames_to_column(var = "Year")

# Print the empty data frame
print(WDI_SEA_Tseries)

WDI_SEA_Tseries$Indonesia=Indonesia$GDP_grw
WDI_SEA_Tseries$Malaysia=Malaysia$GDP_grw
WDI_SEA_Tseries$Singapore=Singapore$GDP_grw
WDI_SEA_Tseries$Thailand=Thailand$GDP_grw
WDI_SEA_Tseries$VietNam=VietNam$GDP_grw
WDI_SEA_Tseries$Myanmar=Myanmar$GDP_grw
WDI_SEA_Tseries$Philippines=Philippines$GDP_grw
WDI_SEA_Tseries$Cambodia=Cambodia$GDP_grw


GDP_Tseries=ts(data=WDI_SEA_Tseries[,2:9],start=min(WDI_SEA_Tseries$Year), end=max(WDI_SEA_Tseries$Year))
plot(GDP_Tseries,plot.type="single",col=1:9)
legend("bottomleft",legend=colnames(GDP_Tseries),ncol=2,lty=1,col=1:9,cex=0.9)
plot(GDP_Tseries)

# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the original and SMA data
matplot(GDP_Tseries, type = "l", lty = 1, col = 1:9, xlab = "Year", ylab = "GDP")
matlines(GDP_TseriesSMA3, col = 1:9, lty = 2)
legend("bottomleft", legend = colnames(GDP_Tseries), col = 1:9, lty = c(1, 2), bty = "n")

# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the smoothed curves
matplot(GDP_TseriesSMA3, type = "l", lty = 1:9, col = 1:9, xlab = "Year", ylab = "Smoothed GDP")
legend("bottomleft", legend = colnames(GDP_Tseries), col = 1:9, lty = 1:9, bty = "n")


# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the smoothed curves
matplot(GDP_TseriesSMA3, type = "l", lty = 1:9, col = 1:9, xlab = "Year", ylab = "Smoothed GDP")

# Add x-axis labels
axis(1, at = 1:10, labels = (WDI_SEA_Tseries$Year))

# Add legend
legend("bottomleft", legend = colnames(GDP_Tseries), col = 1:8, lty = 1:8, bty = "n")

#*****************************************************
selected_years <- 2011:2020
selected_countries <- c("HongKong","China","Japan","Mongolia","SouthKorea")
  

# Create an empty data frame
WDI_NEA_Tseries <- data.frame(matrix(nrow = length(selected_years), ncol = length(selected_countries)))

# Assign column and row names
colnames(WDI_NEA_Tseries) <- selected_countries
rownames(WDI_NEA_Tseries) <- as.character(selected_years)
WDI_NEA_Tseries <- WDI_NEA_Tseries %>%
  rownames_to_column(var = "Year")

# Print the empty data frame
print(WDI_NEA_Tseries)

WDI_NEA_Tseries$HongKong=HongKong$GDP_grw
WDI_NEA_Tseries$China=China$GDP_grw
WDI_NEA_Tseries$Japan=Japan$GDP_grw
WDI_NEA_Tseries$Mongolia=Mongolia$GDP_grw
WDI_NEA_Tseries$SouthKorea=SouthKorea$GDP_grw



GDP_Tseries=ts(data=WDI_NEA_Tseries[,2:6],start=min(WDI_NEA_Tseries$Year), end=max(WDI_NEA_Tseries$Year))
plot(GDP_Tseries,plot.type="single",col=1:6)
legend("topright",legend=colnames(GDP_Tseries),ncol=2,lty=1,col=1:6,cex=0.9)
plot(GDP_Tseries)

# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the original and SMA data
matplot(GDP_Tseries, type = "l", lty = 1, col = 1:9, xlab = "Year", ylab = "GDP")
matlines(GDP_TseriesSMA3, col = 1:9, lty = 2)
legend("bottomleft", legend = colnames(GDP_Tseries), col = 1:9, lty = c(1, 2), bty = "n")

# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the smoothed curves
matplot(GDP_TseriesSMA3, type = "l", lty = 1:9, col = 1:9, xlab = "Year", ylab = "Smoothed GDP")
legend("topright", legend = colnames(GDP_Tseries), col = 1:9, lty = 1:9, bty = "n")


# Assuming GDP_Tseries is your data frame
GDP_TseriesSMA3 <- sapply(GDP_Tseries, SMA, n = 1.5)

# Plot the smoothed curves
matplot(GDP_TseriesSMA3, type = "l", lty = 1:9, col = 1:9, xlab = "Year", ylab = "Smoothed GDP")

# Add x-axis labels
axis(1, at = 1:10, labels = (WDI_SEA_Tseries$Year))

# Add legend
legend("topright", legend = colnames(GDP_Tseries), col = 1:8, lty = 1:8, bty = "n")



#****************************************************



















#Task 5 East Asia
mean_GDP_2011 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2011], na.rm = TRUE)
mean_GDP_2012 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2012], na.rm = TRUE)
mean_GDP_2013 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2013], na.rm = TRUE)
mean_GDP_2014 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2014], na.rm = TRUE)
mean_GDP_2015 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2015], na.rm = TRUE)
mean_GDP_2016 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2016], na.rm = TRUE)
mean_GDP_2017 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2017], na.rm = TRUE)
mean_GDP_2018 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2018], na.rm = TRUE)
mean_GDP_2019 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2019], na.rm = TRUE)
mean_GDP_2020 <- mean(WDI_EA$GDP_grw[WDI_EA$Time == 2020], na.rm = TRUE)
GDP_EA=data.frame(GDP= c(mean_GDP_2011,mean_GDP_2012,mean_GDP_2013,mean_GDP_2014,mean_GDP_2015,mean_GDP_2016,mean_GDP_2017,mean_GDP_2018,mean_GDP_2019,mean_GDP_2020))

# Create a vector of years from 2011 to 2020
#years <- 2011:2020

# Initialize an empty data frame
#GDP_EA <- data.frame(Time = years, GDP = numeric(length(years)))

# Calculate the mean GDP for each year and assign to the data frame
#for (year in years) {
  #mean_GDP <- mean(WDI_EA$GDP_grw[WDI_EA$Time == year], na.rm = TRUE)
  #GDP_EA$GDP[GDP_EA$Time == year] <- mean_GDP
#}

# Print the resulting data frame
#print(GDP_EA)


#GDP_EA_ts <- ts(GDP_EA)

#plot.ts(GDP_EA_ts)

GDP_Tseries=ts(data=GDP_EA,start=min(WDI_EA$Time), end=max(WDI_EA$Time))
plot.ts(GDP_Tseries)

#GDP_EA_tsSMA <- SMA(GDP_EA_ts,n=1.5)
GDP_TseriesSMA <- SMA(GDP_Tseries,n=1.5)
#plot.ts(GDP_EA_tsSMA)
plot.ts(GDP_TseriesSMA)


#GDP_EA_tsforecasts <- HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE)
GDP_Tseriesforecasts <- HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE)

#GDP_EA_tsforecasts$fitted
GDP_Tseriesforecasts$fitted

#plot(GDP_EA_tsforecasts)
plot(GDP_Tseriesforecasts)

#GDP_EA_tsforecasts$SSE
GDP_Tseriesforecasts$SSE

#HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE, l.start=6.057355)
HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE, l.start=6.057355)


#GDP_EA_tsforecasts2 <- forecast(GDP_EA_tsforecasts, h=10)
GDP_Tseriesforecasts2 <- forecast(GDP_Tseriesforecasts, h=10)
GDP_Tseriesforecasts2

#plot(GDP_EA_tsforecasts2)
plot(GDP_Tseriesforecasts2)


#acf(GDP_EA_tsforecasts2$residuals, lag.max=10 , na.action = na.pass)
acf(GDP_Tseriesforecasts2$residuals, lag.max=10 , na.action = na.pass)

#Box.test(GDP_EA_tsforecasts2$residuals, lag=8, type="Ljung-Box")
Box.test(GDP_Tseriesforecasts2$residuals, lag=8, type="Ljung-Box")

#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)

#GDP_EA_tsforecasts2$residuals <-GDP_EA_tsforecasts2$residuals[!is.na(GDP_EA_tsforecasts2$residuals)]
GDP_Tseriesforecasts2$residuals <-GDP_Tseriesforecasts2$residuals[!is.na(GDP_Tseriesforecasts2$residuals)]

#plotForecastErrors(GDP_EA_tsforecasts2$residuals)
#plot(density(GDP_EA_tsforecasts2$residuals))
plotForecastErrors(GDP_Tseriesforecasts2$residuals)
plot(GDP_Tseriesforecasts2)
plot(density(GDP_Tseriesforecasts2$residuals))



#GDP_EA_tsdiff1 <- diff(GDP_EA_ts, differences=1)
#plot.ts(GDP_EA_tsdiff1)

GDP_Tseriesdiff1 <- diff(GDP_Tseries, differences=1)
plot.ts(GDP_Tseriesdiff1)


#GDP_EA_tsdiff2 <- diff(GDP_EA_ts, differences=2)
#plot.ts(GDP_EA_tsdiff2)

GDP_Tseriesdiff2 <- diff(GDP_Tseries, differences=2)
plot.ts(GDP_Tseriesdiff2)

#GDP_EA_tsdiff3 <- diff(GDP_EA_ts, differences=3)
#plot.ts(GDP_EA_tsdiff3)

GDP_Tseriesdiff3 <- diff(GDP_Tseries, differences=3)
plot.ts(GDP_Tseriesdiff3)

#GDP_EA_tsdiff4 <- diff(GDP_EA_ts, differences=4)
#plot.ts(GDP_EA_tsdiff4)

GDP_Tseriesdiff4 <- diff(GDP_Tseries, differences=4)
plot.ts(GDP_Tseriesdiff4)

#GDP_EA_tsdiff5 <- diff(GDP_EA_ts, differences=5)
#plot.ts(GDP_EA_tsdiff5)

GDP_Tseriesdiff5 <- diff(GDP_Tseries, differences=5)
plot.ts(GDP_Tseriesdiff5)

#acf(GDP_EA_tsdiff5, lag.max=8)
#acf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE)

acf(GDP_Tseriesdiff5, lag.max=8)
acf(GDP_Tseriesdiff5, lag.max=8, plot=FALSE)


#pacf(GDP_EA_tsdiff5, lag.max=8)
# plot a partial correlogram
#pacf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE) 
# get the partial autocorrelation


pacf(GDP_Tseriesdiff5, lag.max=8)
# plot a partial correlogram
pacf(GDP_Tseriesdiff5, lag.max=8, plot=FALSE) 
# get the partial autocorrelation

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(1,0,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts


auto.arima(GDP_EA)

test=adf.test(GDP_Tseries)
test
# Assuming 'GDP_Tseries' is your time series data
plot(GDP_Tseries, main = "Time Series Plot")


#-------------------

# Differencing the time series
diff_GDP <- diff(GDP_Tseries)

# ADF test on differenced series
adf_result_diff <- adf.test(diff_GDP)
print(adf_result_diff)

# Example: Fit an ARIMA model
arima_model <- arima(GDP_Tseries, order = c(0, 0, 0))  # Replace p, d, q with appropriate values

# Example: Diagnostic plots for the ARIMA model
plot(residuals(arima_model), main = "Residuals Plot")

#-----------------
auto.arima(GDP_Tseries)

auto.arima(GDP_Tseriesdiff5)


auto.arima(GDP_Tseries,ic='bic')

auto.arima(GDP_Tseriesdiff5,ic='bic')

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(0,0,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts




acf(GDP_Tseriesforecasts$residuals, lag.max=8)
acf(GDP_Tseriesforecasts$residuals, lag.max=8,plot=FALSE)
Box.test(GDP_Tseriesforecasts$residuals, lag=8, type="Ljung-Box")

plot.ts(GDP_Tseriesforecasts$residuals)
# time plot forecast error
plotForecastErrors(GDP_Tseriesforecasts$residuals)

mean(GDP_Tseriesforecasts$residuals)

plot(GDP_Tseriesforecasts)
#-----------------------------------------

#Task 5 South East Asia
mean_GDP_SEA_2011 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2011], na.rm = TRUE)
mean_GDP_SEA_2012 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2012], na.rm = TRUE)
mean_GDP_SEA_2013 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2013], na.rm = TRUE)
mean_GDP_SEA_2014 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2014], na.rm = TRUE)
mean_GDP_SEA_2015 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2015], na.rm = TRUE)
mean_GDP_SEA_2016 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2016], na.rm = TRUE)
mean_GDP_SEA_2017 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2017], na.rm = TRUE)
mean_GDP_SEA_2018 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2018], na.rm = TRUE)
mean_GDP_SEA_2019 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2019], na.rm = TRUE)
mean_GDP_SEA_2020 <- mean(WDI_SEA$GDP_grw[WDI_SEA$Time == 2020], na.rm = TRUE)
GDP_SEA=data.frame(GDP= c(mean_GDP_SEA_2011,mean_GDP_SEA_2012,mean_GDP_SEA_2013,mean_GDP_SEA_2014,mean_GDP_SEA_2015,mean_GDP_SEA_2016,mean_GDP_SEA_2017,mean_GDP_SEA_2018,mean_GDP_SEA_2019,mean_GDP_SEA_2020))

# Create a vector of years from 2011 to 2020
#years <- 2011:2020

# Initialize an empty data frame
#GDP_EA <- data.frame(Time = years, GDP = numeric(length(years)))

# Calculate the mean GDP for each year and assign to the data frame
#for (year in years) {
#mean_GDP <- mean(WDI_EA$GDP_grw[WDI_EA$Time == year], na.rm = TRUE)
#GDP_EA$GDP[GDP_EA$Time == year] <- mean_GDP
#}

# Print the resulting data frame
#print(GDP_EA)


#GDP_EA_ts <- ts(GDP_EA)

#plot.ts(GDP_EA_ts)

GDP_Tseries=ts(data=GDP_SEA,start=min(WDI_SEA$Time), end=max(WDI_SEA$Time))
plot.ts(GDP_Tseries)

#GDP_EA_tsSMA <- SMA(GDP_EA_ts,n=1.5)
GDP_TseriesSMA <- SMA(GDP_Tseries,n=1.5)
#plot.ts(GDP_EA_tsSMA)
plot.ts(GDP_TseriesSMA)


#GDP_EA_tsforecasts <- HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE)
GDP_Tseriesforecasts <- HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE)

#GDP_EA_tsforecasts$fitted
GDP_Tseriesforecasts$fitted

#plot(GDP_EA_tsforecasts)
plot(GDP_Tseriesforecasts)

#GDP_EA_tsforecasts$SSE
GDP_Tseriesforecasts$SSE

#HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE, l.start=6.057355)
HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE, l.start=5.422475)


#GDP_EA_tsforecasts2 <- forecast(GDP_EA_tsforecasts, h=10)
GDP_Tseriesforecasts2 <- forecast(GDP_Tseriesforecasts, h=10)


#plot(GDP_EA_tsforecasts2)
plot(GDP_Tseriesforecasts2)


#acf(GDP_EA_tsforecasts2$residuals, lag.max=10 , na.action = na.pass)
acf(GDP_Tseriesforecasts2$residuals, lag.max=10 , na.action = na.pass)

#Box.test(GDP_EA_tsforecasts2$residuals, lag=8, type="Ljung-Box")
Box.test(GDP_Tseriesforecasts2$residuals, lag=8, type="Ljung-Box")

#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)

#GDP_EA_tsforecasts2$residuals <-GDP_EA_tsforecasts2$residuals[!is.na(GDP_EA_tsforecasts2$residuals)]
GDP_Tseriesforecasts2$residuals <-GDP_Tseriesforecasts2$residuals[!is.na(GDP_Tseriesforecasts2$residuals)]

#plotForecastErrors(GDP_EA_tsforecasts2$residuals)
#plot(density(GDP_EA_tsforecasts2$residuals))
plotForecastErrors(GDP_Tseriesforecasts2$residuals)
plot(GDP_Tseriesforecasts2)
plot(density(GDP_Tseriesforecasts2$residuals))



#GDP_EA_tsdiff1 <- diff(GDP_EA_ts, differences=1)
#plot.ts(GDP_EA_tsdiff1)

GDP_Tseriesdiff1 <- diff(GDP_Tseries, differences=1)
plot.ts(GDP_Tseriesdiff1)


#GDP_EA_tsdiff2 <- diff(GDP_EA_ts, differences=2)
#plot.ts(GDP_EA_tsdiff2)

GDP_Tseriesdiff2 <- diff(GDP_Tseries, differences=2)
plot.ts(GDP_Tseriesdiff2)

#GDP_EA_tsdiff3 <- diff(GDP_EA_ts, differences=3)
#plot.ts(GDP_EA_tsdiff3)

GDP_Tseriesdiff3 <- diff(GDP_Tseries, differences=3)
plot.ts(GDP_Tseriesdiff3)

#GDP_EA_tsdiff4 <- diff(GDP_EA_ts, differences=4)
#plot.ts(GDP_EA_tsdiff4)

GDP_Tseriesdiff4 <- diff(GDP_Tseries, differences=4)
plot.ts(GDP_Tseriesdiff4)

#GDP_EA_tsdiff5 <- diff(GDP_EA_ts, differences=5)
#plot.ts(GDP_EA_tsdiff5)

GDP_Tseriesdiff5 <- diff(GDP_Tseries, differences=5)
plot.ts(GDP_Tseriesdiff5)

#acf(GDP_EA_tsdiff5, lag.max=8)
#acf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE)

acf(GDP_Tseriesdiff4, lag.max=5)
acf(GDP_Tseriesdiff4, lag.max=5, plot=FALSE)


#pacf(GDP_EA_tsdiff5, lag.max=8)
# plot a partial correlogram
#pacf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE) 
# get the partial autocorrelation


pacf(GDP_Tseriesdiff4, lag.max=5)
# plot a partial correlogram
pacf(GDP_Tseriesdiff4, lag.max=5, plot=FALSE) 
# get the partial autocorrelation

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(0,4,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts


auto.arima(GDP_SEA)



test=adf.test(GDP_Tseries)
test

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(0,0,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts

acf(GDP_Tseriesforecasts$residuals, lag.max=8)
acf(GDP_Tseriesforecasts$residuals, lag.max=8,plot=FALSE)
Box.test(GDP_Tseriesforecasts$residuals, lag=8, type="Ljung-Box")

plot.ts(GDP_Tseriesforecasts$residuals)
# time plot forecast error
plotForecastErrors(GDP_Tseriesforecasts$residuals)

mean(GDP_Tseriesforecasts$residuals)

plot(GDP_Tseriesforecasts)
#---------------------------------





#Task 5 North East Asia
mean_GDP_NEA_2011 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2011], na.rm = TRUE)
mean_GDP_NEA_2012 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2012], na.rm = TRUE)
mean_GDP_NEA_2013 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2013], na.rm = TRUE)
mean_GDP_NEA_2014 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2014], na.rm = TRUE)
mean_GDP_NEA_2015 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2015], na.rm = TRUE)
mean_GDP_NEA_2016 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2016], na.rm = TRUE)
mean_GDP_NEA_2017 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2017], na.rm = TRUE)
mean_GDP_NEA_2018 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2018], na.rm = TRUE)
mean_GDP_NEA_2019 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2019], na.rm = TRUE)
mean_GDP_NEA_2020 <- mean(WDI_NEA$GDP_grw[WDI_NEA$Time == 2020], na.rm = TRUE)
GDP_NEA=data.frame(GDP= c(mean_GDP_NEA_2011,mean_GDP_NEA_2012,mean_GDP_NEA_2013,mean_GDP_NEA_2014,mean_GDP_NEA_2015,mean_GDP_NEA_2016,mean_GDP_NEA_2017,mean_GDP_NEA_2018,mean_GDP_NEA_2019,mean_GDP_NEA_2020))

# Create a vector of years from 2011 to 2020
#years <- 2011:2020

# Initialize an empty data frame
#GDP_EA <- data.frame(Time = years, GDP = numeric(length(years)))

# Calculate the mean GDP for each year and assign to the data frame
#for (year in years) {
#mean_GDP <- mean(WDI_EA$GDP_grw[WDI_EA$Time == year], na.rm = TRUE)
#GDP_EA$GDP[GDP_EA$Time == year] <- mean_GDP
#}

# Print the resulting data frame
#print(GDP_EA)


#GDP_EA_ts <- ts(GDP_EA)

#plot.ts(GDP_EA_ts)

GDP_Tseries=ts(data=GDP_NEA,start=min(WDI_NEA$Time), end=max(WDI_NEA$Time))
plot.ts(GDP_Tseries)

#GDP_EA_tsSMA <- SMA(GDP_EA_ts,n=1.5)
GDP_TseriesSMA <- SMA(GDP_Tseries,n=1.5)
#plot.ts(GDP_EA_tsSMA)
plot.ts(GDP_TseriesSMA)


#GDP_EA_tsforecasts <- HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE)
GDP_Tseriesforecasts <- HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE)

#GDP_EA_tsforecasts$fitted
GDP_Tseriesforecasts$fitted

#plot(GDP_EA_tsforecasts)
plot(GDP_Tseriesforecasts)

#GDP_EA_tsforecasts$SSE
GDP_Tseriesforecasts$SSE

#HoltWinters(GDP_EA_ts, beta=FALSE, gamma=FALSE, l.start=6.057355)
HoltWinters(GDP_Tseries, beta=FALSE, gamma=FALSE, l.start=7.073164)


#GDP_EA_tsforecasts2 <- forecast(GDP_EA_tsforecasts, h=10)
GDP_Tseriesforecasts2 <- forecast(GDP_Tseriesforecasts, h=10)
GDP_Tseriesforecasts2

#plot(GDP_EA_tsforecasts2)
plot(GDP_Tseriesforecasts2)


#acf(GDP_EA_tsforecasts2$residuals, lag.max=10 , na.action = na.pass)
acf(GDP_Tseriesforecasts2$residuals, lag.max=10 , na.action = na.pass)
acf(GDP_Tseriesforecasts2$residuals, lag.max=10, plot=FALSE)

#Box.test(GDP_EA_tsforecasts2$residuals, lag=8, type="Ljung-Box")
Box.test(GDP_Tseriesforecasts2$residuals, lag=8, type="Ljung-Box")

#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#plot.ts(GDP_EA_tsforecasts2$residuals)
plot.ts(GDP_Tseriesforecasts2$residuals)

#GDP_EA_tsforecasts2$residuals <-GDP_EA_tsforecasts2$residuals[!is.na(GDP_EA_tsforecasts2$residuals)]
GDP_Tseriesforecasts2$residuals <-GDP_Tseriesforecasts2$residuals[!is.na(GDP_Tseriesforecasts2$residuals)]

#plotForecastErrors(GDP_EA_tsforecasts2$residuals)
#plot(density(GDP_EA_tsforecasts2$residuals))
plotForecastErrors(GDP_Tseriesforecasts2$residuals)
plot(GDP_Tseriesforecasts2)
plot(density(GDP_Tseriesforecasts2$residuals))



#GDP_EA_tsdiff1 <- diff(GDP_EA_ts, differences=1)
#plot.ts(GDP_EA_tsdiff1)

GDP_Tseriesdiff1 <- diff(GDP_Tseries, differences=1)
plot.ts(GDP_Tseriesdiff1)


#GDP_EA_tsdiff2 <- diff(GDP_EA_ts, differences=2)
#plot.ts(GDP_EA_tsdiff2)

GDP_Tseriesdiff2 <- diff(GDP_Tseries, differences=2)
plot.ts(GDP_Tseriesdiff2)

#GDP_EA_tsdiff3 <- diff(GDP_EA_ts, differences=3)
#plot.ts(GDP_EA_tsdiff3)

GDP_Tseriesdiff3 <- diff(GDP_Tseries, differences=3)
plot.ts(GDP_Tseriesdiff3)

#GDP_EA_tsdiff4 <- diff(GDP_EA_ts, differences=4)
#plot.ts(GDP_EA_tsdiff4)

GDP_Tseriesdiff4 <- diff(GDP_Tseries, differences=4)
plot.ts(GDP_Tseriesdiff4)

#GDP_EA_tsdiff5 <- diff(GDP_EA_ts, differences=5)
#plot.ts(GDP_EA_tsdiff5)

GDP_Tseriesdiff5 <- diff(GDP_Tseries, differences=5)
plot.ts(GDP_Tseriesdiff5)

GDP_Tseriesdiff6 <- diff(GDP_Tseries, differences=6)
plot.ts(GDP_Tseriesdiff6)


#acf(GDP_EA_tsdiff5, lag.max=8)
#acf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE)

acf(GDP_Tseriesdiff5, lag.max=10)
acf(GDP_Tseriesdiff5, lag.max=10, plot=FALSE)


#pacf(GDP_EA_tsdiff5, lag.max=8)
# plot a partial correlogram
#pacf(GDP_EA_tsdiff5, lag.max=8, plot=FALSE) 
# get the partial autocorrelation


pacf(GDP_Tseriesdiff5, lag.max=10)
# plot a partial correlogram
pacf(GDP_Tseriesdiff5, lag.max=10, plot=FALSE) 
# get the partial autocorrelation

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(0,5,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts


test=adf.test(GDP_Tseries)
test

auto.arima(GDP_NEA)

GDP_Tseriesarima <- arima(GDP_Tseries, order=c(0,1,0))
GDP_Tseriesarima
GDP_Tseriesforecasts <- forecast(GDP_Tseriesarima, h=10)
GDP_Tseriesforecasts

acf(GDP_Tseriesforecasts$residuals, lag.max=10)
acf(GDP_Tseriesforecasts$residuals, lag.max=10,plot=FALSE)
Box.test(GDP_Tseriesforecasts$residuals, lag=10, type="Ljung-Box")

plot.ts(GDP_Tseriesforecasts$residuals)
# time plot forecast error
plotForecastErrors(GDP_Tseriesforecasts$residuals)

mean(GDP_Tseriesforecasts$residuals)

plot(GDP_Tseriesforecasts)

#.....





#Task 5 WDI_EA_direct GDP_grw


# Assuming WDI_EA is your original data frame
subset_WDI_EA <- WDI_EA[, 1:3]

# Print the resulting data frame
print(subset_WDI_EA)

library(tidyr)

# Reshape the data
reshaped_data <- spread(subset_WDI_EA, key = "Country.Name", value = "GDP_grw")

# Print the result
print(reshaped_data)

# Assuming reshaped_data is your data frame
# Extract the Time and Cambodia columns
time_series_Cambodia <- reshaped_data[, c("Time", "Cambodia")]

# Convert the data to a time series object
time_series_cambodia_ts <- ts(time_series_cambodia[, 2], start = time_series_cambodia[1, 1])

# Plot the time series
plot(time_series_cambodia_ts, type = "l", col = "blue", xlab = "Time", ylab = "Cambodia GDP Growth Rate", main = "Time Series Plot for Cambodia")



# Assuming reshaped_data is your data frame
# Extract the Time and Cambodia columns
time_series_China <- reshaped_data[, c("Time", "China")]

# Convert the data to a time series object
time_series_China_ts <- ts(time_series_China[, 2], start = time_series_China[1, 1])

# Plot the time series
plot(time_series_China_ts, type = "l", col = "blue", xlab = "Time", ylab = "China GDP Growth Rate", main = "Time Series Plot for China")

# Assuming reshaped_data is your data frame

# Extract the Time and Cambodia columns
time_series_Cambodia <- reshaped_data[, c("Time", "Cambodia")]
time_series_China <- reshaped_data[, c("Time", "China")]
time_series_Hongkong <- reshaped_data[, c("Time", "Hong Kong SAR, China")]
time_series_Indonesia <- reshaped_data[, c("Time", "Indonesia")]
time_series_Japan <- reshaped_data[, c("Time", "Japan")]
time_series_VietNam <- reshaped_data[, c("Time", "Viet Nam")]
time_series_SouthKorea <- reshaped_data[, c("Time", "Korea, Rep.")]
time_series_Malaysia <- reshaped_data[, c("Time", "Malaysia")]
time_series_Mongolia <- reshaped_data[, c("Time", "Mongolia")]
time_series_Myanmar <- reshaped_data[, c("Time", "Myanmar")]
time_series_Philippines <- reshaped_data[, c("Time", "Philippines")]
time_series_Singapore <- reshaped_data[, c("Time", "Singapore")]
time_series_Thailand <- reshaped_data[, c("Time", "Thailand")]






# Convert the data to time series objects
time_series_Cambodia_ts <- ts(time_series_Cambodia[, 2], start = time_series_Cambodia[1, 1])
time_series_China_ts <- ts(time_series_China[, 2], start = time_series_China[1, 1])
time_series_Hongkong_ts <- ts(time_series_Hongkong[, 2], start = time_series_Cambodia[1, 1])
time_series_Indonesia_ts <- ts(time_series_Indonesia[, 2], start = time_series_China[1, 1])
time_series_Japan_ts <- ts(time_series_Japan[, 2], start = time_series_Cambodia[1, 1])
time_series_VietNam_ts <- ts(time_series_VietNam[, 2], start = time_series_China[1, 1])
time_series_SouthKorea_ts <- ts(time_series_SouthKorea[, 2], start = time_series_Cambodia[1, 1])
time_series_Malaysia_ts <- ts(time_series_Malaysia[, 2], start = time_series_China[1, 1])
time_series_Mongolia_ts <- ts(time_series_Mongolia[, 2], start = time_series_Cambodia[1, 1])
time_series_Myanmar_ts <- ts(time_series_Myanmar[, 2], start = time_series_China[1, 1])
time_series_Philippines_ts <- ts(time_series_Philippines[, 2], start = time_series_Cambodia[1, 1])
time_series_Singapore_ts <- ts(time_series_Singapore[, 2], start = time_series_China[1, 1])
time_series_Thailand_ts <- ts(time_series_Thailand[, 2], start = time_series_China[1, 1])




# Plot the first time series
plot(time_series_Cambodia_ts, type = "l", col = "blue", xlab = "Time", ylab = "GDP Growth Rate", main = "CountrywiseTime Series Plot for East Asia 2011-2020",ylim=c(-15,20))

# Add the second time series to the same plot
lines(time_series_China_ts, col = "red")
lines(time_series_Indonesia_ts, col = "green")
lines(time_series_Hongkong_ts, col = "orange")
lines(time_series_Japan_ts, col = "yellow")
lines(time_series_VietNam_ts, col = "magenta")
lines(time_series_SouthKorea_ts, col = "violet")
lines(time_series_Malaysia_ts, col = "cyan")
lines(time_series_Mongolia_ts, col = "black")
lines(time_series_Myanmar_ts, col = "darkgreen")
lines(time_series_Philippines_ts, col = "darkblue")
lines(time_series_Singapore_ts, col = "maroon")
lines(time_series_Thailand_ts, col = "darkred")

# Add a legend
legend("bottom", legend = c("Cambodia", "China", "Indonesia", "Hongkong", 
                            "Japan", "VietNam", "SouthKorea", "Malaysia", 
                            "Mongolia", "Myanmar", "Philippines",
                            "Singapore", "Thailand"), 
       col = c("blue", "red", "green", "orange", "yellow",
               "magenta", "violet", "cyan", "black", "darkgreen",
               "darkblue", "maroon", "darkred"), lty = 1, ncol = 13,
       bty = "n", horiz = TRUE, inset = 0, x.intersp = 0)

summary(time_series_Cambodia_ts)
library(tseries)
adf.test(time_series_Cambodia_ts)

arima_model <- arima(time_series_Cambodia_ts)
summary(arima_model)
auto.arima(time_series_Cambodia_ts)
auto.arima(WDI_EA$GDP_grw)
auto.arima(WDI_SEA$GDP_grw)
auto.arima(WDI_NEA$GDP_grw)

