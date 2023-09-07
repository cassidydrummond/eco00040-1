load ("Users/cassidydrummond/Downloads/Starbucks.xlsx")
CD_GCAmount <- "DollarAmtprepaidcard"

#1
data <- data.frame(Starbucks)
print(data)

#2
male <-(Starbucks$Male == 1)
female <-(Starbucks$Male == 0)

#3a
means <-colMeans(data)
print(means)

sd <- apply(data, 2, sd)
print(sd)

max <- apply(data, 2, max)
print(max)

min <-apply(data, 2, min)
print(min)

#3b
female_data <- data[data$Male == "0", ]
male_data <- data[data$Male == "1", ]

meanF <- colMeans(female_data)
print(meanF)
meanM <- colMeans(male_data)
print(meanM)

sdF <-apply(female_data, 2, sd)
sdM <-apply(male_data, 2, sd)
print(sdF)
print(sdM)

maxF <- apply(female_data, 2, max)
maxM <- apply(male_data, 2, max)
print(maxF)
print(maxM)

minF <- apply(female_data, 2, min)
minM <- apply(male_data, 2, min)
print(minF)
print(minM)

#3c
gender_table <- table(data$Male)
print(gender_table)

#3d
medianincome <- median(data$Income..1000.)
High_income <- ifelse(data$Income..1000. > medianincome, 1, 0)
print(High_income)

#3e
conf_interval <- t.test(data$Income..1000., conf.level = 0.99)
print(conf_interval)

#3f
male_part <-data[data$Male == "1", ]
female_part <- data[data$Male == "0", ]
colors <- c("lightblue", "pink")
pie(c(male_data, female_data), labels = c("1","0", col = colors)
title("Distribution of Male and Female")
legend("topright", legend = c("1","0"), fill = colors)
#not sure what went wrong

#3g
summary(female_data$Age)
summary(male_data$Age)

#3h
hist(data$Age, 
     main = "Histogram of Age",     
     xlab = "Values",                 
     ylab = "Frequency",                
     col = "lightblue",                
     border = "black",                 
     breaks = 20)                        

#3i
boxplot(male_data$Dollar.Amt.prepaid.card...., female_data$Dollar.Amt.prepaid.card...., names = c("Male", "Female"), main = "Gift Card Amounts by Gender", xlab = "gender", ylab = "gift card amount", col = c("blue", "pink"), border = "black")

#4a

ttest <- t.test(data$Dollar.Amt.prepaid.card...., mu = 80, alternative = "greater")
print(ttest)

#4b
ttestM <- t.test(male_data$Dollar.Amt.prepaid.card...., mu = 80, alternative = "greater")
print(ttestM)
ttestF <- t.test(female_data$Dollar.Amt.prepaid.card...., mu = 80, alternative = "greater")
print(ttestF)

#4c
reg <- lm(data$Dollar.Amt.prepaid.card.... ~ data$Male, data = data)
summary(reg)

#4d
regA <- lm(data$Dollar.Amt.prepaid.card.... ~ data$Age + data$Days.per.Month.at.Starbucks + data$Cups.of.Coffee.per.Day + data$Income..1000., data = data)
summary(regA)

#4f
plot(data$Income..1000., data$Dollar.Amt.prepaid.card...., main = "Scatterplot of Income v. Gift Card Amount", xlab = "income", ylab = "gift card amount", pch = 16, col = "blue")
