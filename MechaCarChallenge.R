
######## (1) MPG REGRESSION ###############################################

# Read data file
data_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Rename column names that have a space
data_table <- rename(data_table,vlength="vehicle length",
               vwt="vehicle weight",spoilerang="spoiler angle",grndc="ground clearance")


## Perfrom MULTIPLE LINEAR REGRESSION

summary(lm(mpg ~ vlength + vwt + spoilerang + grndc,data=data_table)) #generate summary statistics


########## (2) SUSPENSION COIL SUMMARY  ###################################

# Read suspension coil dataset from csv file
data_table2 <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Get PSI summary stats such as mean, median, quartiles
summary(data_table2$PSI)

# Get PSI Variance
var(data_table2$PSI)

# Get PSI Std Dev
sd(data_table2$PSI)

########## (3) SUSPENSION COIL T-Test  ####################################

# Check for narmal distribution of input data
plt <- ggplot(data_table2,aes(x=PSI)) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

# Shapiro-Wilk quantitative test for Normality
shapiro.test(data_table2$PSI)

# One Sample T-Test
t.test(data_table2$PSI,mu=1500) #compare sample versus population means


###########################  END  #########################################
