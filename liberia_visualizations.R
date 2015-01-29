getwd()
path <- 'C:/Users/user/Documents/liberia_data' #folder where all the csv files are stored
setwd(path)
df <- read.csv("liberia_clean.csv")
str(df)
summary(df)
head(df)
View(df)
names(df)
lapply(df,is.na)
nacount <- lapply(df,is.na)
nacount <- lapply(nacount,sum)
nacount
#subset the dataframe
sdf <- df[as.Date(df$date) < '2014-10-05',] #Viewed the table and looks like alot of NAs after this period

snacount <- lapply(sdf,is.na)
snacount <- lapply(snacount,sum)
snacount
#nacount is the number of NAs per column

sdf$date <- as.Date(sdf$date)
class(sdf$date) #Changed to Data succesfully

names(sdf)
plot(sdf$date,sdf$Newly.reported.deaths)
library(ggplot2)

# Calculate slope and intercept of line of best fit
coef(lm(Newly.reported.deaths ~ date, data = sdf)) 
summary(lm(Newly.reported.deaths ~ date, data = sdf)) 
#date is not a continuous variable #not a strong fit for a linear model
#still its a positive trend. Better to show lines to show the variability.

p1 <- ggplot(aes(x=date,y=Newly.reported.deaths ), data = sdf) +
  #geom_point() +
  #theme(panel.border = element_rect(linetype = "dashed", colour = "black")) +
  geom_line() +
  stat_smooth(method="lm", se=FALSE) +
  xlab("Date (June 16th to October 
       4th - 2014)") +
  ylab("Newly Reported Deaths") +
  ggtitle("Newly Reported Daily Ebola Deaths in Liberia") 
p1

#Let's convert to a plotly plot

library("plotly")
py <- plotly()
out <- py$ggplotly(p1, kwargs=list(filename="gg-basic-line", fileopt="overwrite"))
plotly_url <- out$response$url
py$ggplotly()
py$ggplotly()


hist(df$Specimens.collected)

sum(is.na(df)) #Total NAs in entire df

sdf$confirmedpercent <- sdf$Total.confirmed.cases/(sdf$Total.probable.cases + sdf$Total.suspected.cases)

#Let's create second plot in Tableau
write.csv(sdf,'tableau_sdf_liberia.csv')


