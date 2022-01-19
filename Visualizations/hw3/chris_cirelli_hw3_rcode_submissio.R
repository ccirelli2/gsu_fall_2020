'Ref : https://link.medium.com/dM6Y7USpdab
'

# Clear Namespace & Plots
rm(list=ls())
dev.off()

# Load Libraries
library(tidyverse)
library(ggplot2)
library(corrplot)

# Declare Directories
dir.data <- 'C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\hw3\\data'

# Load Data
setwd(dir.data)
df.raw <- read.csv(file ='Pisa mean perfromance scores 2013 - 2015 Data.csv', fileEncoding="UTF-8-BOM", na.strings = '..')
df.raw1 <- read.csv(file ='Pisa mean perfromance scores 2013 - 2015 Data.csv')
df.raw2 <- read.csv(file ='Pisa mean perfromance scores 2013 - 2015 Data.csv',na.strings = '..')

# Inspect Data
head(df.raw)
head(df.raw1)
head(df.raw2)
str(df.raw)
dim(df.raw)


# Data Clearning
df <- df.raw[1:1161, c(1, 4, 7)] %>%
  spread(key=Series.Code, value=X2015..YR2015.) %>% 
  rename(Maths = LO.PISA.MAT,                        
            Maths.F = LO.PISA.MAT.FE,
            Maths.M = LO.PISA.MAT.MA,
            Reading = LO.PISA.REA,
            Reading.F = LO.PISA.REA.FE,
            Reading.M = LO.PISA.REA.MA,
            Science = LO.PISA.SCI,
            Science.F = LO.PISA.SCI.FE,
            Science.M = LO.PISA.SCI.MA) %>%
  drop_na()

view(df)

# Visualize Data
ggplot(data=df,aes(x=reorder(Country.Name,Maths),y=Maths)) + 
  geom_bar(stat ='identity',aes(fill=Maths))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Maths Score Level")+
  labs(title = 'Ranking of Countries by Maths Score',
       y='Score',x='Countries')+ 
  geom_hline(yintercept = mean(df$Maths),size = 1, color = 'blue')

# Box Plot
df2 = df[,c(1,3,4,6,7,9,10)] %>%   # select relevant columns 
  pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
ggplot(data = df2, aes(x=Score,y=value, color=Score)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')


