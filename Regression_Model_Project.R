#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## Final Project                                                                 #
## 06/05/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")

## Get the libraries
library(data.table)


# Setup working directory
setwd('C:/Users/emreo/Documents/DataScience/Projects')

#################################################################################################
#                                       Texas Lead Measurement  Data Set                        #
#################################################################################################
# Reference : Reading a web page into R (http://www.stat.berkeley.edu/~s133/Regexp2a.html)

thepage = readLines('http://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=A&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=LeadandCopperSummaries&begin_date=5%2F2%2F2014&end_date=5%2F2%2F2016&action1=Search+For+Samples')

## check the pattern
grep('Principal County Served',thepage)
thepage[95:100]

## The data that we want is always preceded by the HTML tag "\t\t\t\t\tsize=\"2\">", and followed by "</font></td>".
## Let's grab all the lines that have that pattern:
mypattern = '\t\t\t\t\tsize=\"2\">([^<]*)</font></td>'
datalines = grep(mypattern,thepage[95:length(thepage)],value=TRUE)

## Extract the information without the HTML tags
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)
matches = mapply(getexpr,datalines,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result[1:10]

# Convert it to a data frame and provide the titles
texas_lead = as.data.frame(matrix(result,ncol=7,byrow=TRUE)) 
names(texas_lead) = c('County', 'Water_System_No.','Monitoring_Begin_Date','Monitoring_End_Date', 'Number_of_Samples','Measure','Analyte') 
head(texas_lead)

# Clean up and orginize the dataset
texas_lead$Monitoring_Begin_Date <- as.Date(texas_lead$Monitoring_Begin_Date, "%m-%d-%Y")
texas_lead$Year_Begin <- as.numeric(format(texas_lead$Monitoring_Begin_Date, "%Y"))

texas_lead$Monitoring_End_Date <- as.Date(texas_lead$Monitoring_End_Date, "%m-%d-%Y")
texas_lead$Year_End <- as.numeric(format(texas_lead$Monitoring_End_Date, "%Y"))

# some of the end years are 2016  and 2017 which means that the future measurements will be performed
# therefore initial starting years are gonna be taken as a reference time instead
# clean up the data

texas_lead$Water_System_No.= NULL
texas_lead$Monitoring_Begin_Date = NULL
texas_lead$Monitoring_End_Date = NULL
texas_lead$Number_of_Samples = NULL
texas_lead$Year_End = NULL

# create a data frame and check the data

lead_frame = as.data.frame(texas_lead)
head(lead_frame)

## separate each year lead data per county based on lead measurement only ( remove the copper measurements )
lead_frame_year <- function(year){
        newFrame <-lead_frame[!lead_frame$Analyte ==" Copper " & lead_frame$Year_Begin==year ,]
        return(newFrame)
}
        
lead_frame_2014 <- lead_frame_year(2014)
lead_frame_2015 <- lead_frame_year(2015)
lead_frame_2016 <- lead_frame_year(2016)


## aggregate lead measurement per county for each year
avg_lead_year <- function(lead_frameYear){
        avgLead <-aggregate(Measure ~ County, data = lead_frameYear,function(x) mean(as.numeric(as.character(x))))
        return(avgLead)
}

avg_lead_2014  = avg_lead_year(lead_frame_2014)
avg_lead_2015  = avg_lead_year(lead_frame_2015)
avg_lead_2016  = avg_lead_year(lead_frame_2016)

#################################################################################################
##                                      Texas Counties Data Set                                 #
#################################################################################################

## Google Data set is based on Metro areas. However, lead measurements are based on counties. 
## Based on the texas_counties.csv, we will add an additional column to define the Metro areas for each county.

# get the texas counties vs metro areas data
texas_counties = read.csv('texas_counties.csv', stringsAsFactors = FALSE)

# assign the metro area for each county
lead_metro <- function(avg_lead_data){
        for (i in 1 : nrow(avg_lead_data)){
                for (x in 1 : nrow(texas_counties)){
                        if (avg_lead_data$County[i]==texas_counties$County[x])
                                avg_lead_data$Metro[i]=texas_counties$Metro[x]     
                }
                
        }
        
        return(avg_lead_data)
}
## Apply the function
avg_lead_2014 <-lead_metro(avg_lead_2014)
avg_lead_2015 <-lead_metro(avg_lead_2015)
avg_lead_2016 <-lead_metro(avg_lead_2016)

## Look at the data
head(avg_lead_2015)
head(avg_lead_2014)
head(avg_lead_2016)

## Let's aggregate again but this time based on Metros intread of Counties
## aggregate lead per county for each year
metro_avg_lead_year <- function(lead_frameYear){
        avgLead <-aggregate(Measure ~ Metro, data = lead_frameYear,function(x) mean(as.numeric(as.character(x))))
        return(avgLead)
}

metro_avg_lead_2014  = metro_avg_lead_year(avg_lead_2015)
metro_avg_lead_2015  = metro_avg_lead_year(avg_lead_2015)
metro_avg_lead_2016  = metro_avg_lead_year(avg_lead_2015)

## Look at the data
head(metro_avg_lead_2014)
head(metro_avg_lead_2015)
head(metro_avg_lead_2016)

#################################################################################################
##                                       Google Trends Data Set                                 #
#################################################################################################

## First Define a function to extract the google search 
## Reference : Downloading Data from Google Trends And Analyzing It With R (http://amunategui.github.io/google-trends-walkthrough/)

google_trend <- function(filename, rnumber){
        con  <- file(filename, open = "r")
        linecount <- 0
        stringdata <- ""
        while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
                linecount <- linecount + 1
                
                if (linecount < 3) {
                        filename <- paste0(filename,oneLine)     
                }
                
                # get headers at line 5
                if (linecount == rnumber) {
                        rowheaders = strsplit(oneLine, ",")[[1]]
                }
                # skip firt 5 lines
                if (linecount > rnumber) {
                        # break when there is no more main data
                        if (gsub(pattern=",", x=oneLine, replacement="") == "") break
                        
                        stringdata <- paste0(stringdata,oneLine,"\n")
                }
        }
        close(con)
        
        newData <- read.table(textConnection(stringdata), sep=",", header=FALSE, stringsAsFactors = FALSE)
        rowheaders <- c("metro", "abdominal_pain", "constipation", "joint_pain", "muscle_pain", "headache")
        names(newData) <- rowheaders
        
        return(newData)
}

## Extract the Google Trend Data for each year

# 2014 
filename <- "texas_2014.csv"
Data_2014 <- google_trend(filename, rnumber = 61)

#2015
filename <- "texas_2015.csv"
Data_2015 <- google_trend(filename, rnumber = 61)

#2016
filename <- "texas_2016.csv"
Data_2016 <- google_trend(filename, rnumber = 28)

# Then add an additional column for the total symptoms search
Data_2014$total_symptoms <- rowSums(Data_2014[,2:ncol(Data_2014)] )
Data_2015$total_symptoms <- rowSums(Data_2015[,2:ncol(Data_2015)] )
Data_2016$total_symptoms <- rowSums(Data_2016[,2:ncol(Data_2016)] )

# check the datasets
head(Data_2014)
head(Data_2015)
head(Data_2016)


## Now we can combine two data sets based and add an additional "Measure" column to the Google Trend Data 

google_measure <- function(google_data,year_data){
        google_data$Measure <- NA
        for (i in 1 : nrow(google_data)){
                for (x in 1 : nrow(year_data)){
                        if (google_data$metro[i]==year_data$Metro[x])
                                google_data$Measure[i]=year_data$Measure[x]
                }
                
        }
        
        return(google_data)
}

Data_2014 <-google_measure(Data_2014,metro_avg_lead_2014)
Data_2015 <-google_measure(Data_2015,metro_avg_lead_2015)
Data_2016 <-google_measure(Data_2014,metro_avg_lead_2016)

# Missing data:
# fill in missing measure with the mean:
#2014
mean_measure = mean(Data_2014$Measure, na.rm=TRUE)
Data_2014$Measure[is.na(Data_2014$Measure)] = mean_measure
#2015
mean_measure = mean(Data_2015$Measure, na.rm=TRUE)
Data_2015$Measure[is.na(Data_2015$Measure)] = mean_measure  
#2016
mean_measure = mean(Data_2016$Measure, na.rm=TRUE)
Data_2016$Measure[is.na(Data_2016$Measure)] = mean_measure  

## Look at the data
head(Data_2014)
head(Data_2015)
head(Data_2016)


#################################################################################################
##                                      Linear Model                                            #
#################################################################################################

## Normilize all the columns 
normilize <- function(Data_Year,n){
        for (i in n : ncol(Data_Year)){
                Data_Year[,i] = Data_Year[,i]/max(Data_Year[,i])
        }
        return (Data_Year)
}

Data_2014 <- normilize(Data_2014,2)
Data_2015 <- normilize(Data_2015,2)
Data_2016 <- normilize(Data_2016,2)

## Look at the data
head(Data_2014)
head(Data_2015)
head(Data_2016)

## let's compute a linear model and see how it is related

# Convert to Data Frame
lead_2014_frame = as.data.frame(Data_2014)

##Let's look at the distribution of the data set
hist(lead_2014_frame$Measure, n=10)
hist(lead_2014_frame$Measure^1/4, n=10)
hist(log(lead_2014_frame$Measure), n=10) # Log dirstirbution is slightly better 



##Let's look at our linear model
Lead_2014_Model= lm(log(Measure) ~ . -metro - total_symptoms, data = lead_2014_frame)
summary(Lead_2014_Model)
## The Adjusted R-squared value is very low :  0.08285  and p-value: 0.3031 is very high.
## In addition, none of the individual symptoms search has a significant relationship with the lead measurement 

## The linear model didn't provide a strong relationship between the symptoms and the Google Trends
## Let's look at the linear model between total google symptoms search and lead measurement

## Fist define a function
plot_results <-function(data_frame,graph_year) {
        plot(data_frame$total_symptom,log(data_frame$Measure), pch=16,
             main= c(graph_year, "Lead Measurement vs Google Symptoms Search"),
             xlab="Total Symptoms Search", ylab="log(Lead Measurement)")
        grid()
}

## 2014 Data
plot_results(lead_2014_frame, "2014")
best_line= lm(log(lead_2014_frame$Measure) ~ lead_2014_frame$total_symptom)
abline(best_line, lwd=2, col='red')
conf_bands = predict(best_line,newdata=lead_2014_frame, interval="confidence")
conf_bands = conf_bands[order(conf_bands[,1]),]
lines(sort(lead_2014_frame$total_symptom), conf_bands[,2], col='green', lwd=3)
lines(sort(lead_2014_frame$total_symptom), conf_bands[,3], col='green', lwd=3)
legend(0.70,-2.0,c("best_line", "confidence_interval"),lty=c(1),lwd=c(2.5),col=c("red", "green"))
bestline_2014 = best_line
summary(bestline_2014)



## When we consider total google symptoms search, our linear model has a better fit
## Adjusted R-squared:  0.2366  and p-value: 0.0172, however the values are still low

plot(bestline_2014)

# First plot: Residuals vs. Fitted values
#    - The trend is oscillatory and not very close to zero.
#
# Second plot: Normal Q-Q Plot
#    - The normality is good
#
# Third plot: Scale-Location Plot of Fitted Values
#   - Measurement of total error across fitted values is not flat.
#
# Fourth plot: Residuals vs. Leverage
#   - Measurement of Points impact on fit: Point 11 and 20 has high/low residuals but low leverage.


##Let's repeat the same steps for 2015 and 2016 data sets
##2015
lead_2015_frame = as.data.frame(Data_2015)
Lead_2015_Model= lm(log(Measure) ~ . -metro - total_symptoms, data = lead_2015_frame)
summary(Lead_2015_Model)
plot_results(lead_2015_frame, "2015")
best_line= lm(log(lead_2015_frame$Measure) ~ lead_2015_frame$total_symptom)
abline(best_line, lwd=2, col='red')
conf_bands = predict(best_line,newdata=lead_2015_frame, interval="confidence")
conf_bands = conf_bands[order(conf_bands[,1]),]
lines(sort(lead_2015_frame$total_symptom), conf_bands[,2], col='green', lwd=3)
lines(sort(lead_2015_frame$total_symptom), conf_bands[,3], col='green', lwd=3)
legend(0.70,-2.0,c("best_line", "confidence_interval"),lty=c(1),lwd=c(2.5),col=c("red", "green"))
bestline_2015 = best_line

##2016
lead_2016_frame = as.data.frame(Data_2016)
Lead_2016_Model= lm(log(Measure) ~ . -metro - total_symptoms, data = lead_2016_frame)
summary(Lead_2016_Model)
lead_2016_frame = as.data.frame(Data_2016)
plot_results(lead_2016_frame, "2016")
best_line= lm(log(lead_2016_frame$Measure) ~lead_2016_frame$total_symptom)
abline(best_line, lwd=2, col='red')
conf_bands = predict(best_line,newdata=lead_2016_frame, interval="confidence")
conf_bands = conf_bands[order(conf_bands[,1]),]
lines(sort(lead_2016_frame$total_symptom), conf_bands[,2], col='green', lwd=3)
lines(sort(lead_2016_frame$total_symptom), conf_bands[,3], col='green', lwd=3)
legend(0.70,-2.0,c("best_line", "confidence_interval"),lty=c(1),lwd=c(2.5),col=c("red", "green"))

#################################################################################################
##                                               End                                            #
#################################################################################################
## additonal combined data model

lead_2014_frame<-as.data.frame(Data_2014)
lead_2015_frame<-as.data.frame(Data_2015)
lead_2016_frame<-as.data.frame(Data_2016)


lead_2014_df <-lead_2014_frame[,c("total_symptoms", "Measure")]
lead_2015_df<-lead_2015_frame[,c("total_symptoms", "Measure")]
lead_2016_df<-lead_2016_frame[,c("total_symptoms", "Measure")]

df <- rbind(lead_2014_df,lead_2015_df,lead_2016_df)
dfn <- normilize(df,1)

hist(dfn$Measure, n=25) # this one is better
hist(dfn$Measure^1/4, n=25)
hist(log(dfn$Measure), n=25) 

hist(dfn$total_symptom, n=25) # this one is better
hist(dfn$total_symptom^1/4, n=25)
hist(log(dfn$total_symptom), n=25) 


Model= lm(Measure ~ . - total_symptoms, data = dfn)
summary(Model)
plot(dfn$total_symptom,dfn$Measure, pch=16,
       main= c( "Lead Measurement vs Google Symptoms Search"),
       xlab="Total Symptoms Search", ylab="Lead Measurement")
grid()

best_line= lm(dfn$Measure ~ dfn$total_symptom)
abline(best_line, lwd=2, col='red')
conf_bands = predict(best_line,newdata=dfn, interval="confidence")
conf_bands = conf_bands[order(conf_bands[,1]),]
lines(sort(dfn$total_symptom), conf_bands[,2], col='green', lwd=3)
lines(sort(dfn$total_symptom), conf_bands[,3], col='green', lwd=3)
legend(0.70,-2.0,c("best_line", "confidence_interval"),lty=c(1),lwd=c(2.5),col=c("red", "green"))
summary(best_line)
plot(best_line)


##----Plot the Residuals-----
# Residuals vs. Fitted
plot(best_line$residuals, best_line$fitted.values, pch=16,
     main="Residual Plot", xlab="Residuals", ylab="Fitted Values")
residual_trend_line = lm(best_line$fitted.values ~ best_line$residuals)
abline(residual_trend_line, col='red', lwd=2)
summary(residual_trend_line) # no trend was observed! if there was a trend, liner fitting would fail

# Residuals vs. Y:
plot(best_line$residuals, df$Measure, pch=16,
     main="Residual Plot", xlab="Residuals", ylab="Y Values")
residual_trend_line = lm(df$Measure ~ best_line$residuals)
abline(residual_trend_line, col='red', lwd=2)
summary(residual_trend_line) # always positevely correlated, higher the correlation worse the fit!
# higher values od  y (near the end) tend to have higher residuals

