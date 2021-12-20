### Part 1: Analysis by individual U.S. state ###

# Load data (replace "..." by the name of your folder)
UCD_Suicide_1999_2019 <- read.csv("C:/Users/Utilisateur/.../Underlying_Cause_Death_Suicide_Adolescents_1999_2019.txt",header=TRUE,sep='\t')
dim(UCD_Suicide_1999_2019)
UCD_Suicide_1999_2019 <- UCD_Suicide_1999_2019[,2:8]

## Stats ##

# Death counts
for (state in unique(UCD_Suicide_1999_2019$State)){
  UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$State == state,]
  print(state)
  print(dim(UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Deaths == "Suppressed",]))
  UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Deaths != "Suppressed",]
  UCD_Suicide_1999_2019_State$Deaths <- as.numeric(UCD_Suicide_1999_2019_State$Deaths)
  p<-ggplot(data=UCD_Suicide_1999_2019_State, aes(x=Year, y=Deaths)) + geom_point(size=1) + geom_line() + ggtitle(state)
  print(p)
}

# Crude rates
for (state in unique(UCD_Suicide_1999_2019$State)){
  UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$State == state,]
  print(state)
  print(dim(UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Crude.Rate == "Suppressed",]))
  print(dim(UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Crude.Rate == "Unreliable",]))
  UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Crude.Rate != "Suppressed",]
  UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019_State[UCD_Suicide_1999_2019_State$Crude.Rate != "Unreliable",]
  UCD_Suicide_1999_2019_State$Crude.Rate <- as.numeric(UCD_Suicide_1999_2019_State$Crude.Rate)
  p<-ggplot(data=UCD_Suicide_1999_2019_State, aes(x=Year, y=Crude.Rate)) + geom_point(size=1) + geom_line() + ggtitle(state)
  print(p)
}

## Graphs ##

# Graphs based on death counts
UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$Deaths != "Suppressed",]
UCD_Suicide_1999_2019_State$State <- factor(UCD_Suicide_1999_2019_State$State)
UCD_Suicide_1999_2019_State$Deaths <- as.numeric(UCD_Suicide_1999_2019_State$Deaths)

pdf(file="Suicide_Deaths_1999_2019_Adolescents.pdf")
p1 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
    geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
    scale_y_continuous(limits = c(0, 250))+
    scale_x_continuous(breaks = c(2000,2005,2010,2015))+
    xlab("Year") + 
    ylab("Number of suicide deaths among 10-19 individuals") + 
    facet_wrap_paginate(~State,ncol=3,nrow=3,page=1)
print(p1)
p2 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=2)
print(p2)
p3 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=3)
print(p3)
p4 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=4)
print(p4)
p5 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals") +  
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=5)
print(p5)
p6 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = Deaths, x = Year))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals") +  
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=6)
print(p6)
dev.off()

# Graphs based on crude rate
UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$Crude.Rate != "Suppressed",]
UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$Crude.Rate != "Unreliable",]

UCD_Suicide_1999_2019_State$State <- factor(UCD_Suicide_1999_2019_State$State)
UCD_Suicide_1999_2019_State$Crude.Rate <- as.numeric(UCD_Suicide_1999_2019_State$Crude.Rate)
UCD_Suicide_1999_2019_State <- UCD_Suicide_1999_2019_State[!is.na(UCD_Suicide_1999_2019_State$Crude.Rate),]

pdf(file="Suicide_Rates_1999_2019_Adolescents.pdf")
p1 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Crude.Rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = Crude.Rate, x = Year)) +
  scale_y_continuous(limits = c(0, 35))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=1)
print(p1)
p2 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Crude.Rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = Crude.Rate, x = Year)) +
  scale_y_continuous(limits = c(0, 35))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=2)
print(p2)
p3 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Crude.Rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = Crude.Rate, x = Year)) +
  scale_y_continuous(limits = c(0, 35))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=3)
print(p3)
p4 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Crude.Rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = Crude.Rate, x = Year)) +
  scale_y_continuous(limits = c(0, 35))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=4)
print(p4)
p5 <- ggplot(UCD_Suicide_1999_2019_State, aes(y = Crude.Rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = Crude.Rate, x = Year)) +
  scale_y_continuous(limits = c(0, 35))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals") + 
  facet_wrap_paginate(~State,ncol=3,nrow=3,page=5)
print(p5)
dev.off()

### Part 2: Analysis at the cohort level ###

## Stats ##

# Aggregate data for 14-state cohort study
cohort_study <- c("Alaska","Arkansas","California","Colorado","Connecticut","Georgia","Indiana","Montana","Nebraska","New Jersey",
                  "Ohio","Oklahoma","Virginia","Vermont")

# Death counts
UCD_Suicide_1999_2019_Cohort <- UCD_Suicide_1999_2019[UCD_Suicide_1999_2019$State %in% cohort_study,]
UCD_Suicide_1999_2019_Cohort <- UCD_Suicide_1999_2019_Cohort[UCD_Suicide_1999_2019_Cohort$Deaths != "Suppressed",]
UCD_Suicide_1999_2019_Cohort$Deaths <- as.numeric(UCD_Suicide_1999_2019_Cohort$Deaths)
UCD_Suicide_1999_2019_Cohort_Short <- UCD_Suicide_1999_2019_Cohort[,c("Year","Deaths","Population")]
UCD_Suicide_1999_2019_Cohort_Short_df <- UCD_Suicide_1999_2019_Cohort_Short %>% group_by(Year) %>% summarise(n_deaths=sum(Deaths),n_pop=sum(Population))

# Crude rates
UCD_Suicide_1999_2019_Cohort_Short_df$crude_rate <- UCD_Suicide_1999_2019_Cohort_Short_df$n_deaths / UCD_Suicide_1999_2019_Cohort_Short_df$n_pop * 100000

## Graphs ##

# Graph based on death counts
max(UCD_Suicide_1999_2019_Cohort_Short_df$n_deaths)

pdf(file="Cohort_Suicide_Deaths_1999_2019_Adolescents.pdf")
p <- ggplot(UCD_Suicide_1999_2019_Cohort_Short_df, aes(y = n_deaths, x = Year)) +
  geom_line() + geom_smooth(aes(y = n_deaths, x = Year)) +
  scale_y_continuous(limits = c(0, 950))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Number of suicide deaths among 10-19 individuals")
print(p)
dev.off()

# Graph based on suicide rate
max(UCD_Suicide_1999_2019_Cohort_Short_df$crude_rate)

pdf(file="Cohort_Suicide_Rates_1999_2019_Adolescents.pdf")
p <- ggplot(UCD_Suicide_1999_2019_Cohort_Short_df, aes(y = crude_rate, x = Year)) +
  geom_line() + geom_smooth(aes(y = crude_rate, x = Year)) +
  scale_y_continuous(limits = c(0, 8))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Suicide rate among 10-19 individuals")
print(p)
dev.off()

## Predictions for 2020 ##

# Prediction (death count): 950.5463
loess_model_death_count <- loess(n_deaths ~ Year, UCD_Suicide_1999_2019_Cohort_Short_df,
                  control = loess.control(surface = "direct"))
predict(loess_model_death_count, data.frame(Year = 2020), se = TRUE)

# Prediction (crude rate): 6.934962
# If we would like to explicitly account for population change: 950.5463 / estimated adolescent population in 2020 * 100k
loess_model_crude_rate <- loess(crude_rate~ Year, UCD_Suicide_1999_2019_Cohort_Short_df,
                     control = loess.control(surface = "direct"))
predict(loess_model_crude_rate, data.frame(Year = 2020), se = TRUE)


### Part 3: Main outcome of interest, i.e., proportion of suicides among adolescents ###

## Stats

# Load data about all suicide deaths, across age groups (replace "..." by the name of your folder)
UCD_Suicide_1999_2019_All <- read.csv("C:/Users/Utilisateur/.../Underlying_Cause_Death_Suicide_All_1999_2019.txt",header=TRUE,sep='\t')
dim(UCD_Suicide_1999_2019_All)
UCD_Suicide_1999_2019_All <- UCD_Suicide_1999_2019_All[,2:8]
names(UCD_Suicide_1999_2019_All) <- c("State","State.Code","Year","Year.Code","Deaths_All","Population_All","Crude.Rate_All")

UCD_Suicide_1999_2019_Merged <- merge(UCD_Suicide_1999_2019,UCD_Suicide_1999_2019_All,by=c("State","Year"))
dim(UCD_Suicide_1999_2019_Merged)
head(UCD_Suicide_1999_2019_Merged)

UCD_Suicide_1999_2019_Merged <- UCD_Suicide_1999_2019_Merged[UCD_Suicide_1999_2019_Merged$Deaths != "Suppressed",]
UCD_Suicide_1999_2019_Merged <- UCD_Suicide_1999_2019_Merged[UCD_Suicide_1999_2019_Merged$Deaths != "Unreliable",]
UCD_Suicide_1999_2019_Merged <- UCD_Suicide_1999_2019_Merged[UCD_Suicide_1999_2019_Merged$Deaths_All != "Suppressed",]
UCD_Suicide_1999_2019_Merged <- UCD_Suicide_1999_2019_Merged[UCD_Suicide_1999_2019_Merged$Deaths_All != "Unreliable",]

UCD_Suicide_1999_2019_Merged$Deaths <- as.numeric(UCD_Suicide_1999_2019_Merged$Deaths)
UCD_Suicide_1999_2019_Merged$Deaths_All <- as.numeric(UCD_Suicide_1999_2019_Merged$Deaths_All)

UCD_Suicide_1999_2019_Merged$Proportion <- UCD_Suicide_1999_2019_Merged$Deaths/UCD_Suicide_1999_2019_Merged$Deaths_All*100
max(UCD_Suicide_1999_2019_Merged$Proportion)

# Cohort-level stats
UCD_Suicide_1999_2019_Merged_Cohort <- UCD_Suicide_1999_2019_Merged[UCD_Suicide_1999_2019_Merged$State %in% cohort_study,]
UCD_Suicide_1999_2019_Merged_Cohort_Short <- UCD_Suicide_1999_2019_Merged_Cohort[,c("Year","Deaths","Deaths_All","Population", "Population_All")]
UCD_Suicide_1999_2019_Merged_Cohort_Short_df <- UCD_Suicide_1999_2019_Merged_Cohort_Short %>% group_by(Year) %>% summarise(n_deaths=sum(Deaths),n_deaths_all=sum(Deaths_All))

UCD_Suicide_1999_2019_Merged_Cohort_Short_df$Proportion <- UCD_Suicide_1999_2019_Merged_Cohort_Short_df$n_deaths/UCD_Suicide_1999_2019_Merged_Cohort_Short_df$n_deaths_all*100

## Graph ##

# Graph based on proportion of suicides among adolescents
max(UCD_Suicide_1999_2019_Merged_Cohort_Short_df$Proportion)
summary(UCD_Suicide_1999_2019_Merged_Cohort_Short_df$Proportion)

pdf(file="Cohort_Suicide_Death_Ratios_1999_2019_Adolescents.pdf")
p <- ggplot(UCD_Suicide_1999_2019_Merged_Cohort_Short_df, aes(y = Proportion, x = Year)) +
  geom_line() + geom_smooth(aes(y = Proportion, x = Year)) +
  scale_y_continuous(limits = c(0, 10))+
  scale_x_continuous(breaks = c(2000,2005,2010,2015))+
  xlab("Year") + 
  ylab("Proportion of suicide deaths that occurred among 10-19 individuals")
print(p)
dev.off()

## Prediction for 2020 ##

# Prediction proportion: 6.336972
loess_model_proportion <- loess(Proportion ~ Year, UCD_Suicide_1999_2019_Merged_Cohort_Short_df,
                                 control = loess.control(surface = "direct"))
predict(loess_model_proportion, data.frame(Year = 2020), se = TRUE)