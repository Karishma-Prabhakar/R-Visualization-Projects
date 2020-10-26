# Install requires packages
packages = c("ggplot2","readr","dplyr","plotly","lubridate","tidyverse","gifski","ffmpeg",
             "tibble","tm","SnowballC","wordcloud","RColorBrewer","corrplot","maditr","stringr")

for (package in packages){
  install.packages(package)
}

# Load packages 
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readr)
library(dplyr)
library(plotly)
library(lubridate)
library(ggpubr)
library(gganimate)
library(tibble)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corrplot)
library(maditr)
library(stringr)

# Load the data
data = read.csv("CrimeCleaned.csv")

# Q) Number of crimes reported over the years?

Crimes_by_Date <- data %>% group_by(OCCURRED_ON_DATE) %>% dplyr::summarise(YEAR = n())
                  ggplot(Crimes_by_Date, aes(OCCURRED_ON_DATE, YEAR, color = OCCURRED_ON_DATE, group =1)) + 
                  geom_line() + ggtitle("Time Series Pattern for Distribution of crime") +
                  theme(plot.title = element_text(color="#D70026", size=14, face="bold.italic", hjust = 0.5, vjust=0.5))
                  

# Q) Where are they taking place? (By District) 
                  
Boston_crime_df <- data[which(as.numeric(data$YEAR) < 2018),] 

K <- Boston_crime_df %>% filter(!is.na(DISTRICT)) %>% group_by(DISTRICT) %>% summarise(count = n(),na.rm = TRUE) %>%
     arrange(desc(count)) %>% ungroup() %>% mutate(DISTRICT = reorder(DISTRICT, count)) %>% 
     ggplot(aes(x = DISTRICT, y = count)) + geom_bar(stat = "identity", color = "white", fill = "cyan3")+
     geom_text(aes(x= DISTRICT, y = 1, label = paste0("",count,"", sep = "")),
            hjust =10, vjust =1.25, size = 3, color = 'black', fontface = 'bold')+
     labs(x = "District", y = "Number of Crimes", title = "Total Crime in each District") + 
     coord_flip() + theme( plot.title = element_text(hjust = 0.5, vjust=0.25))

ggplotly(K)


# Q) Type of crimes commited - Highest and lowest by Month

K1 <- Boston_crime_df %>% filter(!is.na(OFFENSE_CODE_GROUP)) %>% group_by(OFFENSE_CODE_GROUP) %>% summarise(count = n(),na.rm = TRUE) %>%
      arrange(desc(count)) %>% ungroup() %>% mutate(OFFENSE_CODE_GROUP = reorder(OFFENSE_CODE_GROUP, count)) %>% 
      head(10) %>% ggplot(aes(x = OFFENSE_CODE_GROUP, y = count)) +
      geom_bar(stat = "identity", color = "white", fill = "coral3") +
      geom_text(aes(x= OFFENSE_CODE_GROUP, y = 1, label = paste0( "  ",count)),
            hjust =10, vjust =1.25,size = 3, color = 'black', fontface = 'bold')+
      labs(x = "Offense Category", y = "Number of Crimes", title = "Top Offense in District from 2015 to 2018 distibuted")+
      coord_flip() + theme( plot.title = element_text(hjust = 0.5, vjust=0.25))


ggplotly(K1)


# Q) Type of crimes committed - Highest and lowest by Month

top_crimes <- data %>% group_by(OFFENSE_CODE_GROUP) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% slice(1:25)
low_crimes <- data %>% group_by(OFFENSE_CODE_GROUP) %>% count() %>% arrange(n) %>% ungroup() %>% slice(1:25)

data %>% filter(OFFENSE_CODE_GROUP %in% unique(top_crimes$OFFENSE_CODE_GROUP)) %>%
plot_ly(x=~MONTH, y=~OFFENSE_CODE_GROUP) %>% add_histogram2d() %>%
layout(title=list(text="Top 25 Crimes Committed by Month",x=0.55),
       yaxis=list(title="Type of Crime"),
       xaxis=list(title="Month",tickvals=1:12))

data %>% filter(OFFENSE_CODE_GROUP %in% unique(low_crimes$OFFENSE_CODE_GROUP)) %>%
plot_ly(x=~MONTH, y=~OFFENSE_CODE_GROUP) %>%
add_histogram2d()%>%
layout(title=list(text="Lowest 25 Crimes Committed by Month",x=0.60),
       yaxis=list(title="Type of Crime"),
       xaxis=list(title="Month",tickvals=1:12))


# Q) Pattern in the number of crimes for each district

district_data <- data %>% filter(DISTRICT!="") %>% group_by(YEAR,MONTH,DISTRICT) %>% count()%>%
                 ungroup() %>% mutate(Date=paste(YEAR,MONTH,"01",sep="-"),Date=ymd(Date))%>% 
                 filter(Date>"2015-06-01"& Date<"2018-10-01")

total_data <- data %>% group_by(YEAR,MONTH) %>% count()%>%
              ungroup() %>% mutate(DISTRICT="Total") %>% mutate(Date=paste(YEAR,MONTH,"01",sep="-"),Date=ymd(Date))%>%
              filter(Date>"2015-06-01"& Date<"2018-10-01")

combined_data<-bind_rows(district_data,total_data) 

ggplot(district_data,aes(x=Date, y=n,group=DISTRICT))+geom_line(aes(color=DISTRICT))+
  theme_classic()+labs(y="Number of Crimes",title="The Number of Crimes by District Over Time",color="District")+
  theme(plot.title=element_text(hjust=0.5))


# Q) Has the frequency of crime increased or decreased in each district over time?

data2015 <- data %>% filter(DISTRICT!="") %>% group_by(YEAR,DISTRICT) %>% count()%>%
            ungroup()%>%filter(YEAR==2015) %>% mutate(DISTRICT=reorder(DISTRICT,n))

data2018 <- data %>% filter(DISTRICT!="") %>% group_by(YEAR,DISTRICT) %>%
           count() %>% ungroup() %>% filter(YEAR==2018) %>% mutate(DISTRICT=reorder(DISTRICT,n))

graph2015 <- ggplot(data2015,aes(x=DISTRICT,y=n)) + geom_bar(stat="identity",color="black",fill="coral3")+
             coord_flip() + theme_classic() + 
             labs(x="District",y="Number of Crimes in 2015",title="Total Crimes in Each District in 2015")+
             theme(plot.title = element_text(hjust=0.5))

graph2018 <- ggplot(data2018,aes(x=DISTRICT,y=n)) + geom_bar(stat="identity",color="black",fill="cyan3")+
             coord_flip() + theme_classic()+
             labs(x="",y="Number of Crimes in 2018",title="Total Crimes in Each District in 2015 and 2018")+
             theme(plot.title = element_text(hjust=0.5))

subplot(graph2015,graph2018,shareX=FALSE,titleX=TRUE,shareY = FALSE,titleY = TRUE)


# Q) What is UCR(Uniform Crime Reporting) Rate across different time durations

df <- data
df$OCCURRED_ON_DATE2<- mdy_hm(df$OCCURRED_ON_DATE) # Date is in mdy_hms format.
#adding columns day,week of day and month
df <- df %>%
  dplyr::mutate(dom = lubridate::day(df$OCCURRED_ON_DATE2), 
                dow = lubridate::wday(df$OCCURRED_ON_DATE2))
df$M_Name<-month.abb[df$MONTH]

p1 <- layout(plot_ly(df, x = ~YEAR, y = ~UCR_PART,type='histogram2d'),xaxis=list(title=""),yaxis = list(title = "UCR",
             categoryorder = "array",categoryarray = c("Other","Part Three","Part Two","Part One")),
             title="Uniform Crime Reporting (UCR) Across Year and Month")

p2 <- layout(plot_ly(df, x = ~M_Name, y = ~UCR_PART,type='histogram2d'),xaxis = list(title = "",
              categoryorder = "array",categoryarray = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
              yaxis = list(title = "UCR",categoryorder = "array",categoryarray = c("Other","Part Three","Part Two","Part One")))

p3 <- layout(plot_ly(df, x = ~DAY_OF_WEEK, y = ~UCR_PART,type='histogram2d'),xaxis = list(title = "Week Of the Day",
             categoryorder = "array",categoryarray = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
             yaxis = list(title = "UCR",categoryorder = "array",categoryarray = c("Other","Part Three","Part Two","Part One")),
             title="Uniform Crime Reporting (UCR) Across Day of Week and Hour")

p4 <- layout(plot_ly(df, x = ~HOUR, y = ~UCR_PART,type='histogram2d'),xaxis=list(title='Hour of the Day'),yaxis = list(title = "UCR",
             categoryorder = "array",categoryarray = c("Other","Part Three","Part Two","Part One")))

subplot(p1,p2,nrows = 2, titleX=TRUE,shareY=TRUE,margin = 0.10)
subplot(p3,p4,nrows = 2, titleX=TRUE,shareY=TRUE,margin = 0.10)

# Histogram 2D Contour interactive plot showing Crime count across timespans vs subspans

p5 <- layout(plot_ly(df, x = ~M_Name, y = ~dom,type='histogram2dcontour'),xaxis = list(title = "Month",
            categoryorder = "array", categoryarray = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
            yaxis = list(title="Day of Month"))

p6 <- layout(plot_ly(df, x = ~M_Name, y = ~DAY_OF_WEEK,type='histogram2dcontour'),xaxis = list(title = "Month",
             categoryorder = "array",categoryarray = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
             yaxis = list(title = "Day of Week",categoryorder = "array",categoryarray = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))

p7 <- layout(plot_ly(df, x = ~DAY_OF_WEEK, y = ~HOUR,type='histogram2dcontour'),xaxis = list(title = "Day of Week",
             categoryorder = "array",categoryarray = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
             yaxis = list(title = "Hour of Day"),title="Crime Count Across Different Time Spans and Subspans")

subplot(p5,p7,p6,nrows=2,titleY = TRUE,margin=0.07)


# Q) top 15 incidents across different hours of the day

df3 <- df %>% group_by(HOUR,OFFENSE_CODE_GROUP) %>% dplyr::summarise(Total = n())
df3 <- df3 %>% group_by(HOUR) %>% mutate(rank = rank(-Total),
         Total_rel = Total/Total[rank==1],
         Total_lbl = paste0(" ",round(Total))) %>%
        group_by(OFFENSE_CODE_GROUP) %>% filter(rank <=15) %>% ungroup()

staticplot = ggplot(df3, aes(rank, group = OFFENSE_CODE_GROUP,fill = as.factor(OFFENSE_CODE_GROUP), color = as.factor(OFFENSE_CODE_GROUP))) +
  geom_tile(aes(y = Total/2, height = Total,width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = str_wrap(paste(OFFENSE_CODE_GROUP, " "),width=15)),size=6,vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Total,label = Total_lbl, hjust=0,size=5))+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5,vjust=1.5, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, hjust=0.5,color="black"),
        plot.caption =element_text(size=10, hjust=0.9, face="bold", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


anim = staticplot + transition_states(HOUR, transition_length = 1, state_length = 1) +
        view_follow(fixed_x = TRUE)  + labs(title = 'Hour of the day: {closest_state}',subtitle  =  "Top 15 Incidents")

animate(anim, 200, fps = 10,  width = 1200, height = 1000,renderer = gifski_renderer("slide3.gif"))


# Q) Which street is more dangerous

StreetCount <- data %>% group_by(data$STREET) %>% tally()
colnames(StreetCount) <- c("StreetName", "StCount")
StreetCount[(StreetCount=="")] <- "Unknown"
StreetCount = StreetCount[-1,]
pal = brewer.pal(9,"Blues")
wordcloud(StreetCount$StreetName, StreetCount$StCount, min.freq = 500, 
          random.order = F, random.color = F, colors = c("palegreen4", "tan1", "red3"), scale = c(2,.3))


# Q) Correlation between crimes

CrimeCount <- summarise(group_by(data, OFFENSE_CODE_GROUP,MONTH),TotalNumber=length(OFFENSE_CODE_GROUP))
CrimeCount <- CrimeCount[order(CrimeCount$MONTH), ]
CrimeCount <- filter(CrimeCount, TotalNumber > 10 )
crimechart <- dcast(CrimeCount,MONTH ~ OFFENSE_CODE_GROUP, value.var = "TotalNumber" )
crimechart[is.na(crimechart)] <- 0
row.names(crimechart) <- crimechart$MONTH
crimechart = crimechart[,-1]
crimechart <- cor(crimechart)
corrplot(crimechart, type = "upper", order = "hclust", col = brewer.pal(n=8, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45,number.cex=0.55 ,tl.cex = 0.45)