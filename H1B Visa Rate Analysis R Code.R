library(ggplot2) 
library(readr) 
library(dplyr)
library(ggmap)
library(ggrepel)
library(lazyeval)
library(data.table)
library(fitdistrplus)
library(scales)


#-----------------------------------------------------------------------------------------------------------------------------------------------#

visa <- fread("C:\\Users\\Shivam\\Downloads\\H1B_Dataset.csv")
visa
str(visa)
head(visa)
colnames(visa)

#----------------------------------------------------------------------------------------------------------------------------------------------#
#Checking the case status of the whole petitioners and their percentage distribution

mycolors <- c("Red","Blue","Green","Orange","Red","Blue","Green","Orange","Red","Blue","Green","Orange","Red","Blue","Green","Orange")
              
case.status <-as.data.frame(visa %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>%
                               summarise(PROPORTION = round(n()*100/nrow(visa),1)))
print(case.status)

ggplot(subset(visa, !is.na(CASE_STATUS)), 
       aes(x = CASE_STATUS, fill = CASE_STATUS)) +
  geom_bar(stat = "count") +
  coord_trans(y = "sqrt") +
  ylab("No. of Applications") +
  theme(legend.position = c(0.9, 0.9), 
        legend.title = element_text("Case Status"), 
        legend.key.size = unit(0.3, "cm"),
        axis.text.x=element_text(angle = -10, hjust = 0, size = rel(1))) +
  ggtitle("Distribution of H1B visa case status")


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#Top employer with highest visa issuance and the case status
mycolors_emp<-c('black','blue','yellow','green','orange','red','pink','purple','gold','brown','grey',"Red","Blue","Green","Orange",'pink','purple','gold','brown')
certified_h1b$EMPLOYER_NAME <- factor(certified_h1b$EMPLOYER_NAME)
top_employer<- as.data.frame(visa %>% group_by(EMPLOYER_NAME) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(15, wt = count))

ggplot(data = top_employer, aes(x = reorder(EMPLOYER_NAME, percent),
                                y = percent, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1, hjust = 1) + 
  labs(x = "EMPLOYER_NAME", y = "Petitions Made(in percentage)") + 
  scale_fill_manual(values = mycolors_emp) + 
  theme(legend.position = "none") +
  coord_flip()+
  labs(x="TOP COMPANIES WITH MAXIMUM VISA APLICATIONS", y="PERCENTAGE OF APPLICATIONS")+
  ggtitle("Recruiting Company and their percentage of applications")


#--------------------------------------------------------------------------------------------------------------#


certified_status <- visa %>%
  filter(CASE_STATUS=="Certified")

top_emp <- function(num_emp) {
  certified_status %>%
    group_by(EMPLOYER_NAME) %>%
    summarise(num_apps=n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num_emp)
}

ggplot(top_emp(15),
       aes(x=reorder(EMPLOYER_NAME,num_apps),y=num_apps))+
  geom_bar(stat="identity", fill ="sky blue", width = 0.7)+
  coord_flip()+
  scale_y_continuous(limits= c(0,2500), breaks  = seq(0,140000,20000))+
  geom_text(aes(label=num_apps), hjust = -0.2, size = 2)+
  ggtitle("top 10 ")+
  theme(plot.title = element_text(size=rel(0.8)))+
  labs(x="TOP COMPANIES WITH MAXIMUM VISA ISSUANCE", y="NUMBER OF SUCCESSFULL APPLICATIONS")+
  ggtitle("Recruiting Company and their successfull applications")



#------------------------------------------------------------------------------------------------------------------------#


#Top employer states and the distribution of case status

certified_h1b$EMPLOYER_STATE <- factor(certified_h1b$EMPLOYER_STATE)
top_employer <- as.data.frame(visa %>% group_by(EMPLOYER_STATE) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(15, wt = count))

ggplot(data = top_employer, aes(x = reorder(EMPLOYER_STATE, percent),
                                y = percent, fill = EMPLOYER_STATE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  labs(x = "Top 15 States ", y = "Percentage of visa issuance") + 
  scale_y_continuous(breaks = seq(0,30,1)) +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "none") +
  labs(x="TOP STATES WITH MAXIMUM VISA APPLICATIONS", y="PERCENTAGEOF APPLICATIONS")+
  ggtitle("Top States and the percentage of applications")


#------------------------------------------------------------------------------------------------------------------------#




#Top Job title and the case status distribution

certified_h1b <- visa %>%
  filter(CASE_STATUS == "CERTIFIED")

certified_h1b$JOB_TITLE <- factor(certified_h1b$SOC_TITLE)
job_titles <- as.data.frame(visa %>% group_by(SOC_TITLE) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(min(count))%>% 
                                top_n(15, wt = count))

ggplot(data = job_titles, aes(x = reorder(SOC_TITLE, percent),
                                y = percent, fill = SOC_TITLE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  labs(x = "JOB TITLE", y = "Petitions Made(in percentage)") + 
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "none") +
  coord_flip()+
  ggtitle("Top job positions and their applications")


##


top_N_SOC <- function(num) {
  certified_status %>%
    filter(!is.na(certified_status$SOC_TITLE)) %>%
    group_by(SOC_TITLE) %>%
    summarise(num_apps = n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num)
}


ggplot(top_N_SOC(15), 
       aes(x = reorder(SOC_TITLE, num_apps), y = num_apps)) +
  geom_bar(stat = "identity", alpha = 0.9, fill = "orange", width = 0.7) +
  coord_flip() +
  scale_y_continuous() +
  geom_text(aes(label = num_apps), hjust = -0.2, size = 2) +
  ggtitle("Top 10 occupations with most H1B petitions") +
  theme(plot.title = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(0.8))) +
  labs(x = "TOP JOB POSITIONS WITH MAXIMUM VISA ISSUANCE", y = "NUMBER OF SUCCESSFULL APPLICATIONS")+
  ggtitle("Top Job Positions and their issuance")



#-------------------------------------------------------------------------------------------------------------------------#



#Salary ranges of petitioners and the number of petitions made


filtered <-dplyr::filter(visa, WAGE_UNIT_OF_PAY =="Year")
sal<-ggplot(data = subset(filtered, filtered$PREVAILING_WAGE < quantile(filtered$PREVAILING_WAGE, 0.99, na.rm = T)),
            aes(x = PREVAILING_WAGE/1000)) +
  geom_histogram(color = "black", fill = mycolors[1], binwidth = 2.5) +
  scale_x_continuous(breaks = seq(0,500,20)) +
  scale_y_continuous(breaks = seq(0,50000,1000)) +
  labs(x = "Salary (in thousand USD)", y = "Number of petitions")+
  ggtitle("Distribution of Salaried of Petitioners applying for H1b Visa")
sal


#
top_employer <- dplyr::filter(visa,CASE_STATUS == 'Certified' & WAGE_UNIT_OF_PAY == 'Year')
head(top_employer) 
ggplot(data = top_employer) + #passing the input dataframe 
  geom_bar(mapping = aes(x = PREVAILING_WAGE), # change the intervals of a histogram using the binwidth parameter 
           color = 'Black', fill = 'White') +
  xlab('Salaries of Successfull Petitioners')+  # X-axis label
  ylab('Number of  of Visas Issued')+
  scale_x_continuous(labels=label_comma())+
  ggtitle("Distributions in of salaries of success applications")


summary(certified_status$PREVAILING_WAGE)


#-------------------------------------------------------------------------------------------------------------------------------#



#Top Employer city with highest visa issuance rates


visa$EMPLOYER_CITY <- factor(visa$EMPLOYER_CITY)
top_employer <- as.data.frame(visa %>% group_by(EMPLOYER_CITY) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(5, wt = count))

ggplot(data = top_employer, aes(x = (EMPLOYER_CITY),
                                y = percent, fill = EMPLOYER_CITY)) +
  labs(x = "Top Cities with maximum issuance rate", y = "Percentage Rates")+
  ggtitle("Top cities with maximum visa applications")+

  geom_point()



#-----------------------------------------------------------------------------------------------------------------------#



# Checking the people with full time and part time jobs with H1b visa

certified_h1b <- visa %>%
  filter(CASE_STATUS == "CERTIFIED")

full_time_pos <- visa %>%
  filter(!is.na(FULL_TIME_POSITION)) %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(n = n()) %>%
  mutate(rel_freq = paste0(round(100 * n/sum(n), 2), "%"),
         pos = cumsum(n) - n/2)


ggplot(full_time_pos, 
       aes(x = factor(1), y = n, fill = factor(FULL_TIME_POSITION, levels = c("Y", "N")))) + 
  geom_bar(width = 1, stat = "identity", color = "black") +
  labs(fill = "Full time position?") +
  coord_polar(theta = "y") +
  geom_text(aes(x = factor(1), y = pos, label = rel_freq), size=5) +
  theme_void() +
  theme(legend.position = "right", legend.text = element_text(size = 14),
        plot.title = element_text(size = rel(1))) +
  ggtitle("Full time positions vs non full time positions")


full <-visa %>%
  dplyr::select(FULL_TIME_POSITION) %>%
  dplyr::filter(FULL_TIME_POSITION == 'Y') %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(number_of_full = n())

full


part<-visa%>%
  dplyr::select(FULL_TIME_POSITION) %>%
  dplyr::filter(FULL_TIME_POSITION=='N')%>%
  group_by(FULL_TIME_POSITION)%>%
  summarise(number_of_half =n ())
part




#-------------------------------------------------------------------------------------------------------------------#


#Salaries of petitioners with all the Case statuses in a box plot


avgsal <- mean(visa[visa$PW_UNIT_OF_PAY == "Year" & visa$CASE_STATUS == "Certified", ]$PREVAILING_WAGE)
avgsal
medsal <- median(visa[visa$PW_UNIT_OF_PAY == "Year" & visa$CASE_STATUS == "Certified", ]$PREVAILING_WAGE)
medsal


z<- boxplot(PREVAILING_WAGE~CASE_STATUS,data=visa, main="Salary Ranges for Petitioners",
            xlab="Case Status of People", ylab="Prevailing Wage of the Petitioners")+
z + scale_y_continuous(labels = label_comma())



#---------------------------------------------------------------------------------------------------------------------#


#Visualizing the case status of petitioners in California, which is the top state issuing the visa


certified_h1b <- visa %>%
  filter(CASE_STATUS == "CERTIFIED")

s<-ggplot(data = visa) + 
  geom_point(aes(x = PREVAILING_WAGE, y = EMPLOYER_STATE=="CA", color =CASE_STATUS)) +
  geom_vline(xintercept = 400) +
  geom_hline(yintercept = 1) 
s+labs(x="Prevaling wage of petioners", y="Status in California")+
  scale_x_continuous(labels = label_comma())+
  ggtitle("No of people who applied in California and their Status")




q<-ggplot(data = certified_status) +
  geom_point(aes(x = PREVAILING_WAGE, y = EMPLOYER_STATE=="CA", color =CASE_STATUS)) + 
  geom_vline(xintercept = 400) + # Creating a vertical reference line at x = 40
  geom_hline(yintercept = 1) # Creating a horizontal reference line at y = 150
q+labs(x="Prevaling wage of petioners", y="Status in California")+
  scale_x_continuous(labels = label_comma())+
  ggtitle("No of successfull applications in California")

#----------------------------------------------------------------------------------------------------------------------------------------------------------


#Visualizing the case status of petitioners in New York City, which is the top city issuing the visa.


r<-ggplot(data = visa) +  
  geom_point(aes(x = PREVAILING_WAGE, y = EMPLOYER_CITY=="New York", color =CASE_STATUS)) + 
  geom_hline(yintercept = 1) # 
r+labs(x="Prevaling wage of petioners", y="Status in New York")+
  scale_x_continuous(labels = label_comma())+
  ggtitle("Number of people who applied in New York and their Status")

t<-ggplot(data = certified_status) + 
  geom_point(aes(x = PREVAILING_WAGE, y = EMPLOYER_CITY=="New York", color =CASE_STATUS)) + 
  geom_hline(yintercept = 1) # 
t+labs(x="Prevaling wage of petioners", y="Status in New York")+
  scale_x_continuous(labels = label_comma())+
  ggtitle("Number of Successfull applications in  New York")


#-----------------------------------------------------------------------------------------------------------------------------------------------------#

#Finding what distribution the salary is been given to the employees of a company

descdist(visa$PREVAILING_WAGE)


# Finding the parameter estimates

# for normal distribution

fit_n <- fitdist(visa$PREVAILING_WAGE, "norm")
summary(fit_n)

# for uniform distribution

fit_ln <- fitdist(visa$PREVAILING_WAGE, "lnorm")
summary(fit_ln)

# for Exponential Distribution

fit_exp <- fitdist(visa$PREVAILING_WAGE/1000, "exp")
summary(fit_exp)

#---------------------------------------------------------------------------------------------------------------------------#

#Finding the GOF tests for each distribution to evaluate the distribution

par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab = 'Prevailing Wage', xlegend = 'topleft')
cdfcomp (list(fit_n), legendtext = plot.legend, xlab = 'Prevailing Wage')
qqcomp  (list(fit_n), legendtext = plot.legend, xlab = 'Prevailing Wage')
ppcomp  (list(fit_n), legendtext = plot.legend, xlab = 'Prevailing Wage')


ggplot(visa, aes(x = PREVAILING_WAGE)) +
  stat_function(
    fun = dnorm,
    args = with(visa, c(mean = mean(PREVAILING_WAGE), sd = sd(PREVAILING_WAGE)))
  ) +
  scale_x_continuous(labels = label_comma(),"Mean of Prevailing Wages of Employees")+
  scale_y_continuous(labels = label_comma(),"Standard Deviation of the wages of Employees")+
  ggtitle("Normal Distibution Curve for the salary of the petitioners")


#---------------------------------------------------------------------------------------------------------------------------------------#



plot.legend <- c("lnormal")
denscomp(list(fit_ln), legendtext = plot.legend, xlab = 'Prevailing Wage', xlegend = 'topleft')
cdfcomp (list(fit_ln), legendtext = plot.legend, xlab = 'Prevailing Wage')
qqcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Prevailing Wage')
ppcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Prevailing Wage')


#-------------------------------------------------------------------------------------------------------------------------------------------#


plot.legend <- c("Exponential")
denscomp(list(fit_exp), legendtext = plot.legend, xlab = 'Prevailing Age', xlegend = 'topright')
cdfcomp (list(fit_exp), legendtext = plot.legend, xlab = 'Prevailing Age')
qqcomp  (list(fit_exp), legendtext = plot.legend, xlab = 'Prevailing Age')
ppcomp  (list(fit_exp), legendtext = plot.legend, xlab = 'Prevailing Age')


#---------------------------------------------------------------------------------------------------------------------------------------------#



cor(visa$WAGE_RATE_OF_PAY_FROM,visa$PREVAILING_WAGE, method = "pearson")

summary(visa$PREVAILING_WAGE)


#Visualizing the correlation


library("ggpubr")

#Pearson Method
my_data <- read.csv("C:\\Users\\Bhupesh\\Downloads\\LCA_Disclosure_Data_FY2021_Q1.csv")
certified_h1b <- visa %>%
  filter(CASE_STATUS == "CERTIFIED")
ggscatter(certified_status,x = "PREVAILING_WAGE",y = "WAGE_RATE_OF_PAY_FROM", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Prevailing Wage of Petitioners", ylab = "Wage rate standards")+
  ggtitle("Correlation Analysis-Pearson Method")+
  scale_x_continuous(labels=label_comma())+
  scale_y_continuous(labels=label_comma())




#---------------------------------------------------------------------------------------------------------------------------------------------#


#Confidence Interval of Salary of petitioners


visaSample <- sample_n(visa,40000, replace = "false")

mean(visaSample$PREVAILING_WAGE)

mean(visa$PREVAILING_WAGE)

sd(visa$PREVAILING_WAGE)
sd(visaSample$PREVAILING_WAGE)

#Data Taken for finding confidence interval
#n=40000, xbar=97916.15, s=41465.59

n <- 40000
xbar <- 97916.15 
s <- 41465.59

CI <- qt(0.95,df=n-1)*s/sqrt(n)

low <- xbar - CI
low
high <- xbar + CI
high

#The 95% confidence interval for the true population mean salary of petitioners is [97509.78,98322.52].


#-------------------------------------------------------------------------------------------------------------------------------------------------------#

#Hypothesis test of percentage of people getting their visa in 2016 and people getting their visa in 2021

# Checking the probability of getting the visa in 2016

getwd()
hy1_df <- read.csv('f2016.csv', header= TRUE, sep=',')
head(hy1_df)
pop <- nrow(hy1_df)
pop
f_df <- dplyr::filter(hy1_df,CASE_STATUS =="CERTIFIED")
samp <- nrow(f_df)
samp
probs_df <- samp/pop
probs_df

# One sample proportion test

#Example 5 - Based on 2016 census data, 87.9% of the total applicants got H-1B visa certified. 
#            Is there a different percentage of people getting the visa in 2021?

#X = R.V. of proportion of applicants getting H-1B visa in 2016

# H0: Pop_prop = 87.9% (There is no significant difference between Proportion of applicants getting H-1b visa in 
#2016 and Proportion of applicants getting H-1B visa in 2021)

# H1: Pop_prop!= 87.9% (There is a significant difference between Proportion of applicants getting H-1b visa in 
#2016 and Proportion of applicants getting H-1B visa in 2021)


# Step 1: Checking the unique values in case-status 

unique(visa$CASE_STATUS)

# Step 2: filtering the data based on Certified people

abc_df <-visa %>% filter( CASE_STATUS %in% c("Certified"))
head(abc_df)

# Step 3: Define x, a vector of counts of successes

vissued <- nrow(abc_df)
vissued

# Step 4: Total Sample size 

Sample_Size <- length(visa$CASE_STATUS)
Sample_Size



# Step 5: Run the one-sample proportion test

prop.test(x= vissued, n=Sample_Size, p=0.879, correct = TRUE, conf.level = 0.95,
          alternative="two.sided")



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#



#Hypothesis of Salaries of People in California and Texas 

# Two sample proportion test

# Test to check whether the proportion of income of people working in California and Texas 
#are equal and conclude what could be the parameter that people in California get higher chances of getting a visa

#X1 = R.V. of proportion of income of international employees working in California 
#X2 = R.V. of proportion of income of international employees working in Texas 


# H0: Pop_prop1-Pop_prop2 = 0 (There is no significant difference between Proportion of income of people 
#working in California and Proportion of income of people working in Texas)

# H1: Pop_prop1-Pop_prop2!= 0 (There is a significant difference between Proportion of income of people 
#working in California and Proportion of income of people working in Texas)

# Step 1-a: Determine the number of people working in CA

n1 <-length(which(visa$EMPLOYER_STATE == "CA"))
n1

# Step 1-b: Filter and calculate the number of rows based on state and income

x1 <- visa %>% filter(EMPLOYER_STATE =="CA" & PREVAILING_WAGE > 60000) %>% nrow()
x1

# Step 1-c: Determine the number of people working in TX

n2 <-length(which(visa$EMPLOYER_STATE == "TX"))
n2

# Step 1-d: Filter and calculate the number of rows based on state and income  

x2 <- visa %>% filter(EMPLOYER_STATE=="TX" & PREVAILING_WAGE > 60000)%>% nrow()
x2


# Step 2: Execute the test
prop.test(x=c(x1, x2),n=c(n1,n2))


#MEAN VALUES
qaz_prob <- dplyr::filter(visa, EMPLOYER_STATE=="TX")
pop_mean <-mean(qaz_prob$PREVAILING_WAGE)
pop_mean

wsx_prob <- dplyr::filter(visa, EMPLOYER_STATE=="CA")
popu_mean <-mean(wsx_prob$PREVAILING_WAGE)
popu_mean


#----------------------------------------------------------------------------------------------------------------------------------------------------#

#CONCLUSION
cc_prob <- dplyr::filter(visa, EMPLOYER_STATE == "CA" & CASE_STATUS =="Certified")
nrow(cc_prob)
ccc_prob <- dplyr::filter(visa, EMPLOYER_STATE == "CA")
nrow(ccc_prob)

p3 <- nrow(cc_prob)/nrow(ccc_prob)
p3

gg_prob <- dplyr::filter(visa, EMPLOYER_STATE == "CA" & CASE_STATUS =="Denied")
nrow(gg_prob)
ggg_prob <- dplyr::filter(visa, EMPLOYER_STATE == "CA")
nrow(ggg_prob)

p7 <- nrow(gg_prob)/nrow(ggg_prob)
p7



dd_prob <- dplyr::filter(visa, EMPLOYER_CITY == "New York" & CASE_STATUS =="Denied")
nrow(dd_prob)
ddd_prob <- dplyr::filter(visa, EMPLOYER_CITY == "New York")
nrow(ddd_prob)

p8 <- nrow(dd_prob)/nrow(ddd_prob)
p8

hh_prob <- dplyr::filter(visa, EMPLOYER_CITY == "New York" & CASE_STATUS =="Certified")
nrow(hh_prob)
hhh_prob <- dplyr::filter(visa, EMPLOYER_CITY == "New York")
nrow(hhh_prob)

p4 <- nrow(hh_prob)/nrow(hhh_prob)
p4



ee_prob <- dplyr::filter(visa, EMPLOYER_NAME == "COGNIZANT TECHNOLOGY SOLUTIONS US CORP" & CASE_STATUS =="Certified")
nrow(ee_prob)
eee_prob <- dplyr::filter(visa, EMPLOYER_NAME == "COGNIZANT TECHNOLOGY SOLUTIONS US CORP")
nrow(eee_prob)

p5 <- nrow(ee_prob)/nrow(eee_prob)
p5

ii_prob <- dplyr::filter(visa, EMPLOYER_NAME == "COGNIZANT TECHNOLOGY SOLUTIONS US CORP" & CASE_STATUS =="Denied")
nrow(ii_prob)
iii_prob <- dplyr::filter(visa, EMPLOYER_NAME == "COGNIZANT TECHNOLOGY SOLUTIONS US CORP")
nrow(iii_prob)

p9 <- nrow(ii_prob)/nrow(iii_prob)
p9




ff_prob <- dplyr::filter(visa, SOC_TITLE == "Software Developers, Applications" & CASE_STATUS =="Certified")
nrow(ff_prob)
fff_prob <- dplyr::filter(visa, SOC_TITLE == "Software Developers, Applications")
nrow(fff_prob)

p6 <- nrow(ff_prob)/nrow(fff_prob)
p6


jj_prob <- dplyr::filter(visa, SOC_TITLE == "Software Developers, Applications" & CASE_STATUS =="Denied")
nrow(jj_prob)
jjj_prob <- dplyr::filter(visa, SOC_TITLE == "Software Developers, Applications")
nrow(jjj_prob)

p10 <- nrow(jj_prob)/nrow(jjj_prob)
p10


xx_prob <- dplyr::filter(visa, FULL_TIME_POSITION=="Y" & CASE_STATUS =="Certified")
nrow(xx_prob)
xxx_prob <- dplyr::filter(visa, FULL_TIME_POSITION=="Y")
nrow(xxx_prob)

p11 <- nrow(xx_prob)/nrow(xxx_prob)
p11


yy_prob <- dplyr::filter(visa, FULL_TIME_POSITION=="Y" & CASE_STATUS =="Denied")
nrow(yy_prob)
yyy_prob <- dplyr::filter(visa, FULL_TIME_POSITION=="Y")
nrow(yyy_prob)

p12 <- nrow(yy_prob)/nrow(yyy_prob)
p12


#-------------------------------------------------------------------------------------------------------------------------------------------------------










