# Install Packages #
install.packages('ggplot2') # used for plotting 
install.packages('foreign') # used for multinomial inference 
install.packages('nnet') # used for multinomial inference
install.packages('reshape2') # potentially needed for multinomial inference, foreign and nnet depedence

# Load Packages #
library('ggplot2')
library('foreign')
library('nnet')
library('reshape2')

# Load Data #
file <- "/Users/Ryan/Box Sync/CSE Job Test/Test/test.csv" # file path, change as needed
test <- read.csv(file, header=T) # read in .csv file, headers are intact by default but making explicit here

# Change all non-answers to NA #
levels(test$LeaseVsPurchase)[levels(test$LeaseVsPurchase)==''] <- NA
levels(test$AgeGroup)[levels(test$AgeGroup)=='No answer'] <- NA
levels(test$Gender)[levels(test$Gender)==c('No answer', 'Refuse to Answer')] <- NA
levels(test$Education)[levels(test$Education)=='No answer'] <- NA
levels(test$Education)[levels(test$Education)=='Refuse to Answer'] <- NA
levels(test$HousingType)[levels(test$HousingType)== 'Refuse to Answer'] <- NA
levels(test$HousingType)[levels(test$HousingType)== 'No answer'] <- NA
levels(test$RentOrOwn)[levels(test$RentOrOwn)== 'No answer'] <- NA
levels(test$RentOrOwn)[levels(test$RentOrOwn)== 'Refuse to Answer'] <- NA
levels(test$Income)[levels(test$Income)== 'Refuse to Answer'] <- NA
levels(test$Income)[levels(test$Income)== 'No answer'] <- NA

### Plots of Summary Data ###
# County bar plot #
county <- ggplot(test, aes(x=factor(County)))+
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  theme_minimal() +
  labs(y="Number of Respondents",
       x="") +
  scale_x_discrete(limits = rev(levels(test$County)))

# Horizonal #
county +  # flip chart here to accomdate long x-axis character strings, followed suit on the proceding charts as well.
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

# Education # 

levels(test$Education)[levels(test$Education)=="High school grad or GED"] <- "High School Graduate or GED" # rename factor level

ed <- ggplot(test, aes(x=factor(Education)))+
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  theme_minimal() +
  labs(y="Number of Respondents",
       x="") +
  scale_x_discrete(limits = rev(levels(test$Education)))

# Horizontal #
ed + 
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

# Vehicle make # 

vm <- ggplot(test, aes(x=factor(Make)))+
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  theme_minimal() +
  labs(y="Number of Cars",
       x="") +
  theme(text=element_text(size=13)) +
  scale_x_discrete(limits = rev(levels(test$Make))) 

# Horizontal #
vm + 
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

### Model development ###
### Here I took the categorical variables within the original dataset determined reference levels for multinomial regression analysis. I then used multinomial methods, specifically, the 'nnet' and 'foreign' packages to attempt and determine how survey responses would potential steer the purchase of a specific EV 'make'. However, no significant relationship were determined. This may be simply due to the fact that there are no actual relationships between the other survey factors I was interested in using; County, Education and Gender but I feel that more time spent here could reveal some unique relationships. Alternatively, the use of principal components could have been a better first step in 'clustering' important factors together and would likely be a preliminary inference step if I were to investigate further.

test$Make2 <- relevel(test$Make, ref = "Other") # making reference level explicit for this and the proceeding variables
test$County2 <- relevel(test$County, ref = "Alpine")
test$Education2 <- relevel(test$Education, ref="12th grade or less")
test$Gender2 <- relevel(test$Gender, ref="Male")

# Education
m1 <- multinom(Make2 ~ Education2, data = test) # attempted to fit a multinomial model, useful method when using categorical response variables
m1.z <- summary(m1)$coefficients/summary(m1)$standard.errors # computing z-scores
m1.p <- (1 - pnorm(abs(m1.z), 0, 1)) * 2 # computing z-scores

### Sacramento County ###
### Here I decided to instead of continuing to investigate potential multinomial relationships at the state to instead look at summary data from Sacramento county by first subsetting Sacramento data into it's own dataframe. Following this I created a bar chart to describe the distribution of incomes from the survey responses. From here I shifted to using pivot tables in Excel to describe Sacramento specific summary data. 
sac <- subset(test, County=="Sacramento") # subset Sacramento County data into 'sac'

sac$Income <- factor(sac$Income, levels = c("Less than $24,999", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $124,999", "125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", "$200,000 to $249,999", "$250,000 to $299,999", "$300,000 to $349,999", "$350,000 to $399,999", "$400,000 to $499,999", "$500,000 or more")) # relvel income data from lowest to highest

# Sac Income #

sac$Income <- droplevels(sac$Income) # remove NA level for charting purposes

sacin <- ggplot(na.exclude(sac), aes(x=factor(Income))) +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  theme_minimal() +
  labs(y="Number of Respondents",
       x="") +
  scale_x_discrete(limits = rev(levels(test$Income)))

# Horizontal #
sacin + 
  coord_flip()


