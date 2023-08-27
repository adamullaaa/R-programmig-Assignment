# Name: ADAM ISLAM SALEH ABDELHAFIZ ISMAIEL
# TP number: TP058354

p_unload(all)  # Easier: clears all add-ons

install.packages("ggplot2")
library("ggplot2")
install.packages("pacman")# install packages, if needed, and then load the packages.
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 
options(scipen = 100, digits = 4)
Placement_data= read.csv(file="/Users/SAMSUNG/Desktop/PFDA/Assignment/Placement_Data_Full_Class.csv",
                         head= TRUE) # importing the file into the Rstudio
View (Placement_data)
head (Placement_data)
Placement_data%>%
  select(starts_with("ssc_")) # showing wanted attributes and similar ones

Placement_data%>%
  select(starts_with("ssc_"), everything) # showing wanted attributes and similar ones

1#does the mother's job affects the students's marks??
mother_job <- table(Placement_data$Mjob)  # Create table
barplot(mother_job)  # data exploration 
Placement_data %>%   # data manipulation 
  group_by(Mjob)%>% count()

par(mfrow = c(4, 2)) # selecting the rows and coloumns

hist(Placement_data$ssc_p [Placement_data$Mjob == "at_home"], # data visualisation 
     xlim = c(0, 100),
     breaks = 5,
     main = "marks when the mothers are in home",
     xlab = "average percentage",
     col = "red")

hist(Placement_data$ssc_p [Placement_data$Mjob == "health"],
     xlim = c(0, 100),
     breaks = 5,
     main = "marks when the mothers are in health industry",
     xlab = "average percentage",
     col = "red")
hist(Placement_data$ssc_p [Placement_data$Mjob == "other"],
     xlim = c(0, 100),
     breaks = 5,
     main = "marks when the mothers are in other jobs",
     xlab = "average percentage",
     col = "red")
hist(Placement_data$ssc_p [Placement_data$Mjob == "services"],
     xlim = c(0, 100),
     breaks = 5,
     main = "marks when the mothers are in services",
     xlab = "average percentage",
     col = "red")
hist(Placement_data$ssc_p [Placement_data$Mjob == "teacher"],
     xlim = c(0, 100),
     breaks = 5,
     main = "marks when the mothers are in teaching industry",
     xlab = "average percentage",
     col = "red")

2# does the location of the students affects either their secondary or high secondary marks??

students <- table(Placement_data$address)  # Create table
barplot(students)  # data exploration 


par(mfrow = c(1, 2))
hist(Placement_data$ssc_p [Placement_data$address == "U"], # data visualization 
     xlim = c(0, 100),
     breaks = 5,
     main = "Urban students' marks",
     xlab = "average percentage",
     col = "red")

hist(Placement_data$ssc_p [Placement_data$address == "R"], # data visualization 
     xlim = c(0, 100),
     breaks = 5,
     main = "Rural students' marks",
     xlab = "average percentage",
     col = "blue")

par(mfrow = c(1, 2))
hist(Placement_data$hsc_p [Placement_data$address == "U"], # data visualization 
     xlim = c(0, 100),
     breaks = 5,
     main = "Urban students' marks",
     xlab = "average percentage",
     col = "red")

hist (Placement_data$hsc_p [Placement_data$address == "R"], # data visualization 
     xlim = c(0, 100),
     breaks = 5,
     main = "Rural students' marks",
     xlab = "average percentage",
     col = "blue")


Placement_data%>%   # data manipulation 
  group_by(hsc_p,ssc_p,address)%>% arrange(desc(hsc_p))%>%arrange(desc(ssc_p)) %>%tibble()

3# which degree type got the most placements??


placed_data =subset(Placement_data, status=="Placed")
View(placed_data)
ggplot(placed_data, aes(x= degree_t))+
  geom_bar(col="black", fill= "blue")+
  labs(title = 'comparison between degree types', x= 'Degree type'
       ,y= ' count')

4# what are the grades that students are chosen to be placed based on?


new_coloumn$avgH_Sa_Dg <- (Placement_data$hsc_p+Placement_data$degree_p)/2
View(new_coloumn)# data Transformation 

new_placed= subset(new_coloumn, status=="Placed")
new_placement= head(new_coloumn, 500)
ggplot(new_placement, aes(x= new_placement$mba_p, y= new_placement$avgH_Sa_Dg))+
  geom_point()+
  geom_smooth(method = lm )
    
5#which board education is better??

ggplot(Placement_data, aes(y=ssc_p, x = ssc_b )) + 
  geom_boxplot()+
  labs(title = 'comparison between the board education systems', x= 'Board education'
       ,y= ' secondary marks')

Placement_data%>%   # data manipulation 
  group_by(ssc_p,ssc_b)%>% filter(ssc_b == "State" & ssc_p<50)%>%count()
Placement_data%>%   # data manipulation 
  group_by(ssc_p,ssc_b)%>% filter(ssc_b == "private" & ssc_p<50)%>%count()
Placement_data%>%   # data manipulation 
  group_by(ssc_p,ssc_b)%>% filter(ssc_b == "central" & ssc_p<50)%>%count()

ggplot(Placement_data, aes(y=hsc_p, x = hsc_b )) + 
  geom_boxplot()+
  labs(title = 'comparison between the board education systems', x= 'Board education'
       ,y= ' high seconadry marks')


6# Does internet access affects marks??

ggplot(Placement_data, aes(y=ssc_p, x = internet )) + 
  geom_point()+
  geom_boxplot()+
  labs(title = 'comparison between the internet accessed students', x= 'interent access'
       ,y= ' secondary marks')

ggplot(Placement_data, aes(y=hsc_p, x = internet )) + 
  geom_point()+
  geom_boxplot()+
  labs(title = 'comparison between the internet accessed students', x= 'interent access'
       ,y= ' high secondary marks')

Placement_data %>%   # data manipulation 
  group_by(internet)%>% count()

Placement_data %>%   # data manipulation 
  group_by(internet)%>%  filter(internet == "yes" & ssc_p<50)%>%count()

Placement_data %>%   # data manipulation 
  group_by(internet)%>%  filter(internet == "no" & ssc_p<50)%>%count()


7# see if distinction students had family support

par(mfrow = c(1, 2)) #data manipulation/ visualization 
hist(Placement_data$hsc_p [Placement_data$famsup=="yes"],
     xlim = c(80, 100),
     breaks = 1,
     main = "Distinction percentages with edu support",
     xlab = "average percentage",
     col = "red")

hist(Placement_data$hsc_p [Placement_data$famsup=="no"],
     xlim = c(80, 100), #data manipulation/ visualization 
     breaks = 1,
     main = "Distinction percentages with no edu support",
     xlab = "average percentage",
     col = "blue")

8# did the extra activities affect the students' academic performance?

ggplot(Placement_data, aes(y=ssc_p, x = activities )) + 
  geom_boxplot()+
  geom_point()+
  labs(title = 'To see if the activities have an effect on secondary marks', x= 'activities'
       ,y= ' percentage mark')

ggplot(Placement_data, aes(y=hsc_p, x = activities )) + 
  geom_boxplot()+
  geom_point()+
  labs(title = 'To see if the activities have an effect on high secondary marks', x= 'activities'
       ,y= ' percentage mark')

9#10# does work experience affect placements or salary 


ggplot(Placement_data, aes(y=sl_no, x = workex, fill= status )) + 
  geom_boxplot()+
  labs(title = 'To see if the work experience has an effect on placement', x= 'work experience'
       ,y= ' students')



ggplot(new_placed, aes(y=salary, x = workex )) + 
  geom_boxplot()+
  geom_point()
  labs(title = 'To see if the work experience has an effect on salary', x= 'work experience'
       ,y= ' salary')

11#do job specialization affect salaries?


ggplot(new_placed, aes(y=salary, x = sl_no, fill= specialisation )) + 
  geom_boxplot()+
  labs(title = 'job majors affect salaries', x= 'students'
       ,y= ' salary')

12#Does the father's and mother's education influence the students' marks?

ggplot(Placement_data, aes(x= Medu, y=ssc_p))+
  geom_point(stat="identity")+
  xlab("mother's education")+
  ylab("percentages marks")+
  ggtitle("see if the mother's education affect their children's marks")+
  theme(plot.background = element_rect(fill="#1b98e0",color = "pink"))

ggplot(Placement_data, aes(x= Fedu, y=ssc_p))+
  geom_point(stat="identity")+
  xlab("father's education")+
  ylab("percentages marks")+
  ggtitle("see if the father's education affect their children's marks")+
  theme(panel.background = element_rect(fill="#1b98e0",color = "pink"))


13#do males have higher placements than girls?

ggplot(Placement_data, aes(x= gender, fill= status))+
  geom_bar()+
  xlab("gender")+
  ylab("count")+
  ggtitle("see if  males have higher placements than girls")

14# do the extra modules affect the students' performance?

ggplot(Placement_data, aes(y=hsc_p, x = paid, fill= gender )) + 
  geom_boxplot()+
  labs(title = 'To see if the extra modules have an effect on high secondary marks', x= 'extra modules'
       ,y= ' percentage mark')


15# does the MBA percentage affect the salary?

ggplot(new_placement, aes(x= mba_p, y= salary))+
  geom_smooth(method = lm )+
  xlab("MBA percentage")+
  ylab("salary")+
  ggtitle("see if MBA percentage affect the salary")

16#Does Employability test percentage affect the placement decision?
new_placed= subset(new_coloumn, status=="Placed")
ggplot(new_placed, aes(x= sl_no, y= etest_p))+
  geom_point()+
  xlab("Students")+
  ylab("Employability test percentage")+
  ggtitle("see if Employability test percentage affect the placement decision?")

# Extra features:
Placement_data%>%
  slice_sample(prop = 0.01) # showing 1% of the data

Placement_data%>%
  slice_sample(prop = 1) # showing shuffled data

Placement_data%>%
  select(starts_with("ssc_")) # showing wanted attributes and similar ones

Placement_data%>%
  select(starts_with("ssc_"), everything) # showing wanted attributes and similar ones


