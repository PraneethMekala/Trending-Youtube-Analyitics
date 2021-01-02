library(tidyverse)
library(ggplot2)

setwd('~/Desktop/Data Science bootcamp/Capstone Project/Youtube Data')

#Load up each country 
df_canada = read.csv('CAvideos.csv')
df_germany = read.csv('DEvideos.csv')
df_france = read.csv('FRvideos.csv')
df_uk = read.csv('GBvideos.csv')
df_india = read.csv('INvideos.csv')
df_japan = read.csv('JPvideos.csv')
df_korea = read.csv('KRvideos.csv')
df_mexico = read.csv('MXvideos.csv')
df_russia = read.csv('RUvideos.csv')
df_us = read.csv('USvideos.csv')
df_us_n = read.csv('USnvideos.csv')

#Remove thumbnail link 
df_canada = subset(df_canada, select = -thumbnail_link)
df_germany = subset(df_germany, select = -thumbnail_link)
df_france = subset(df_france, select = -thumbnail_link)
df_uk = subset(df_uk, select = -thumbnail_link)
df_india = subset(df_india, select = -thumbnail_link)
df_japan = subset(df_japan, select = -thumbnail_link)
df_korea = subset(df_korea, select = -thumbnail_link)
df_mexico = subset(df_mexico, select = -thumbnail_link)
df_russia = subset(df_russia, select = -thumbnail_link)
df_us_n = subset(df_us_n, select = -thumbnail_link)
df_us = subset(df_us, select = -thumbnail_link)
#remove video error videos
df_canada = subset(df_canada, video_error_or_removed != "True")
df_canada = subset(df_canada, select = -video_error_or_removed)
df_germany = subset(df_germany, video_error_or_removed != "True")
df_germany = subset(df_germany, select = -video_error_or_removed)
df_france = subset(df_france, video_error_or_removed != "True")
df_france = subset(df_france, select = -video_error_or_removed)
df_uk = subset(df_uk, video_error_or_removed != "True")
df_uk = subset(df_uk, select = -video_error_or_removed)
df_india = subset(df_india, video_error_or_removed != "True")
df_india = subset(df_india, select = -video_error_or_removed)
df_japan = subset(df_japan, video_error_or_removed != "True")
df_japan = subset(df_japan, select = -video_error_or_removed)
df_korea = subset(df_korea, video_error_or_removed != "True")
df_korea = subset(df_korea, select = -video_error_or_removed)
df_mexico = subset(df_mexico, video_error_or_removed != "True")
df_mexico = subset(df_mexico, select = -video_error_or_removed)
df_russia = subset(df_russia, video_error_or_removed != "True")
df_russia = subset(df_russia, select = -video_error_or_removed)
df_us_n = subset(df_us_n, video_error_or_removed != "True")
df_us_n = subset(df_us_n, select = -video_error_or_removed)
df_us = subset(df_us, video_error_or_removed != "True")
df_us = subset(df_us, select = -video_error_or_removed)


#No missing values at all, go on to compare views for different countries
Countries = c('Canada', 'Germany', 'France', 'UK', 'India', 'Japan', 'South Korea', 'Mexico', 'Russia', 'USA' )
TotalViews = c( sum(df_canada$views), sum(df_germany$views), sum(df_france$views), sum(df_uk$views),
                sum(df_india$views), sum(df_japan$views), sum(df_korea$views), sum(df_mexico$views),
                sum(df_russia$views), sum(df_us$views))
TotalLikes = c( sum(df_canada$likes), sum(df_germany$likes), sum(df_france$likes), sum(df_uk$likes),
                sum(df_india$likes), sum(df_japan$likes), sum(df_korea$likes), sum(df_mexico$likes),
                sum(df_russia$likes), sum(df_us$likes))
TotalDislikes = c( sum(df_canada$dislikes), sum(df_germany$dislikes), sum(df_france$dislikes), sum(df_uk$dislikes),
                sum(df_india$dislikes), sum(df_japan$dislikes), sum(df_korea$dislikes), sum(df_mexico$dislikes),
                sum(df_russia$dislikes), sum(df_us$dislikes))
TotalComments = c( sum(df_canada$comment_count), sum(df_germany$comment_count), sum(df_france$comment_count), sum(df_uk$comment_count),
                   sum(df_india$comment_count), sum(df_japan$comment_count), sum(df_korea$comment_count), sum(df_mexico$comment_count),
                   sum(df_russia$comment_count), sum(df_us$comment_count))

Totalcountry = data.frame(factor(Countries), TotalViews, TotalLikes, TotalDislikes, TotalComments)
str(Totalcountry)
Totalcountry$TotalViews = Totalcountry$TotalViews / 10^9 #scale to billions of views
Totalcountry$TotalLikes = Totalcountry$TotalLikes / 10^9 #scale to billions of likes
Totalcountry$TotalDislikes = Totalcountry$TotalDislikes / 10^9 #scale to billions of Dislikes
Totalcountry$TotalComments = Totalcountry$TotalComments / 10^9 #scale to billions of Comments
#Views by country
ggplot(Totalcountry, aes(x = Countries, y= TotalViews)) + geom_bar(stat="identity") + ylab("Total Views (Billions)") + ggtitle("Total Trending Views Per Country")
#likes by country
ggplot(Totalcountry, aes(x = Countries, y= TotalLikes)) + geom_bar(stat="identity") + ylab("Total Likes (Billions)") + ggtitle("Total Trending Likes Per Country")
#Dislikes by country
ggplot(Totalcountry, aes(x = Countries, y= TotalDislikes)) + geom_bar(stat="identity") + ylab("Total Dislikes (Billions)") + ggtitle("Total Trending Dislikes Per Country")
#Comments by country
ggplot(Totalcountry, aes(x = Countries, y= TotalComments)) + geom_bar(stat="identity") + ylab("Total Comments (Billions)") + ggtitle("Total Comments Per Country")

#Catagories by country
#Canada
table(df_canada$category_id)
df_canada$category_id[df_canada$category_id == 1] = "Film & Animation"
df_canada$category_id[df_canada$category_id == 2] = "Autos & Vehicles"
df_canada$category_id[df_canada$category_id == 10] = "Music"
df_canada$category_id[df_canada$category_id == 15] = "Pets & Animals"
df_canada$category_id[df_canada$category_id == 17] = "Sports"
df_canada$category_id[df_canada$category_id == 19] = "Travel & Events"
df_canada$category_id[df_canada$category_id == 20] = "Gaming"
df_canada$category_id[df_canada$category_id == 22] = "People & Blogs"
df_canada$category_id[df_canada$category_id == 23] = "Comedy"
df_canada$category_id[df_canada$category_id == 24] = "Entertainment"
df_canada$category_id[df_canada$category_id == 25] = "News & Politics"
df_canada$category_id[df_canada$category_id == 26] = "Howto & Style"
df_canada$category_id[df_canada$category_id == 27] = "Education"
df_canada$category_id[df_canada$category_id == 28] = "Science & Technology"
df_canada$category_id[df_canada$category_id == 30] = "Movies"
df_canada$category_id[df_canada$category_id == 43] = "Shows"
#df_canada$category_id = replace("29","Autos & Vehicles",df_canada$category_id) #29 --------CANT FIND so remove
df_canada = subset(df_canada, category_id != "29")
df_canada$category_id = as.factor(df_canada$category_id)
ggplot(df_canada, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for Canada")

#Germany
table(df_germany$category_id)
df_germany$category_id[df_germany$category_id == 1] = "Film & Animation"
df_germany$category_id[df_germany$category_id == 2] = "Autos & Vehicles"
df_germany$category_id[df_germany$category_id == 10] = "Music"
df_germany$category_id[df_germany$category_id == 15] = "Pets & Animals"
df_germany$category_id[df_germany$category_id == 17] = "Sports"
df_germany$category_id[df_germany$category_id == 19] = "Travel & Events"
df_germany$category_id[df_germany$category_id == 20] = "Gaming"
df_germany$category_id[df_germany$category_id == 22] = "People & Blogs"
df_germany$category_id[df_germany$category_id == 23] = "Comedy"
df_germany$category_id[df_germany$category_id == 24] = "Entertainment"
df_germany$category_id[df_germany$category_id == 25] = "News & Politics"
df_germany$category_id[df_germany$category_id == 26] = "Howto & Style"
df_germany$category_id[df_germany$category_id == 27] = "Education"
df_germany$category_id[df_germany$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_germany$category_id[df_germany$category_id == 30] = "Movies"
df_germany$category_id[df_germany$category_id == 43] = "Shows"
df_germany$category_id[df_germany$category_id == 44] = "Trailers"
df_germany = subset(df_germany, category_id != "29")
df_germany$category_id = as.factor(df_germany$category_id)
ggplot(df_germany, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for germany")

#france
table(df_france$category_id)
df_france$category_id[df_france$category_id == 1] = "Film & Animation"
df_france$category_id[df_france$category_id == 2] = "Autos & Vehicles"
df_france$category_id[df_france$category_id == 10] = "Music"
df_france$category_id[df_france$category_id == 15] = "Pets & Animals"
df_france$category_id[df_france$category_id == 17] = "Sports"
df_france$category_id[df_france$category_id == 19] = "Travel & Events"
df_france$category_id[df_france$category_id == 20] = "Gaming"
df_france$category_id[df_france$category_id == 22] = "People & Blogs"
df_france$category_id[df_france$category_id == 23] = "Comedy"
df_france$category_id[df_france$category_id == 24] = "Entertainment"
df_france$category_id[df_france$category_id == 25] = "News & Politics"
df_france$category_id[df_france$category_id == 26] = "Howto & Style"
df_france$category_id[df_france$category_id == 27] = "Education"
df_france$category_id[df_france$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_france$category_id[df_france$category_id == 30] = "Movies"
df_france$category_id[df_france$category_id == 43] = "Shows"
df_france$category_id[df_france$category_id == 44] = "Trailers"
df_france = subset(df_france, category_id != "29")
df_france$category_id = as.factor(df_france$category_id)
ggplot(df_france, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for france")

#UK
table(df_uk$category_id)
df_uk$category_id[df_uk$category_id == 1] = "Film & Animation"
df_uk$category_id[df_uk$category_id == 2] = "Autos & Vehicles"
df_uk$category_id[df_uk$category_id == 10] = "Music"
df_uk$category_id[df_uk$category_id == 15] = "Pets & Animals"
df_uk$category_id[df_uk$category_id == 17] = "Sports"
df_uk$category_id[df_uk$category_id == 19] = "Travel & Events"
df_uk$category_id[df_uk$category_id == 20] = "Gaming"
df_uk$category_id[df_uk$category_id == 22] = "People & Blogs"
df_uk$category_id[df_uk$category_id == 23] = "Comedy"
df_uk$category_id[df_uk$category_id == 24] = "Entertainment"
df_uk$category_id[df_uk$category_id == 25] = "News & Politics"
df_uk$category_id[df_uk$category_id == 26] = "Howto & Style"
df_uk$category_id[df_uk$category_id == 27] = "Education"
df_uk$category_id[df_uk$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_uk$category_id[df_uk$category_id == 30] = "Movies"
df_uk$category_id[df_uk$category_id == 43] = "Shows"
df_uk$category_id[df_uk$category_id == 44] = "Trailers"
df_uk = subset(df_uk, category_id != "29")
df_uk$category_id = as.factor(df_uk$category_id)
ggplot(df_uk, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for uk")

#india
table(df_india$category_id)
df_india$category_id[df_india$category_id == 1] = "Film & Animation"
df_india$category_id[df_india$category_id == 2] = "Autos & Vehicles"
df_india$category_id[df_india$category_id == 10] = "Music"
df_india$category_id[df_india$category_id == 15] = "Pets & Animals"
df_india$category_id[df_india$category_id == 17] = "Sports"
df_india$category_id[df_india$category_id == 19] = "Travel & Events"
df_india$category_id[df_india$category_id == 20] = "Gaming"
df_india$category_id[df_india$category_id == 22] = "People & Blogs"
df_india$category_id[df_india$category_id == 23] = "Comedy"
df_india$category_id[df_india$category_id == 24] = "Entertainment"
df_india$category_id[df_india$category_id == 25] = "News & Politics"
df_india$category_id[df_india$category_id == 26] = "Howto & Style"
df_india$category_id[df_india$category_id == 27] = "Education"
df_india$category_id[df_india$category_id == 28] = "Science & Technology"
df_india$category_id[df_india$category_id == 30] = "Movies"
df_india$category_id[df_india$category_id == 43] = "Shows"
#df_india$category_id = replace("29","Autos & Vehicles",df_india$category_id) #29 --------CANT FIND so remove
df_india = subset(df_india, category_id != "29")
df_india$category_id = as.factor(df_india$category_id)
ggplot(df_india, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for india")

#Japan
table(df_japan$category_id)
df_japan$category_id[df_japan$category_id == 1] = "Film & Animation"
df_japan$category_id[df_japan$category_id == 2] = "Autos & Vehicles"
df_japan$category_id[df_japan$category_id == 10] = "Music"
df_japan$category_id[df_japan$category_id == 15] = "Pets & Animals"
df_japan$category_id[df_japan$category_id == 17] = "Sports"
df_japan$category_id[df_japan$category_id == 19] = "Travel & Events"
df_japan$category_id[df_japan$category_id == 20] = "Gaming"
df_japan$category_id[df_japan$category_id == 22] = "People & Blogs"
df_japan$category_id[df_japan$category_id == 23] = "Comedy"
df_japan$category_id[df_japan$category_id == 24] = "Entertainment"
df_japan$category_id[df_japan$category_id == 25] = "News & Politics"
df_japan$category_id[df_japan$category_id == 26] = "Howto & Style"
df_japan$category_id[df_japan$category_id == 27] = "Education"
df_japan$category_id[df_japan$category_id == 28] = "Science & Technology"
#df_japan$category_id = replace("29","Autos & Vehicles",df_japan$category_id) #29 --------CANT FIND so remove
df_japan = subset(df_japan, category_id != "29")
df_japan$category_id = as.factor(df_japan$category_id)
ggplot(df_japan, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for japan")

#korea
table(df_korea$category_id)
df_korea$category_id[df_korea$category_id == 1] = "Film & Animation"
df_korea$category_id[df_korea$category_id == 2] = "Autos & Vehicles"
df_korea$category_id[df_korea$category_id == 10] = "Music"
df_korea$category_id[df_korea$category_id == 15] = "Pets & Animals"
df_korea$category_id[df_korea$category_id == 17] = "Sports"
df_korea$category_id[df_korea$category_id == 19] = "Travel & Events"
df_korea$category_id[df_korea$category_id == 20] = "Gaming"
df_korea$category_id[df_korea$category_id == 22] = "People & Blogs"
df_korea$category_id[df_korea$category_id == 23] = "Comedy"
df_korea$category_id[df_korea$category_id == 24] = "Entertainment"
df_korea$category_id[df_korea$category_id == 25] = "News & Politics"
df_korea$category_id[df_korea$category_id == 26] = "Howto & Style"
df_korea$category_id[df_korea$category_id == 27] = "Education"
df_korea$category_id[df_korea$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_korea$category_id[df_korea$category_id == 43] = "Shows"
df_korea$category_id[df_korea$category_id == 44] = "Trailers"
df_korea = subset(df_korea, category_id != "29")
df_korea$category_id = as.factor(df_korea$category_id)
ggplot(df_korea, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for korea")

#mexico
table(df_mexico$category_id)
df_mexico$category_id[df_mexico$category_id == 1] = "Film & Animation"
df_mexico$category_id[df_mexico$category_id == 2] = "Autos & Vehicles"
df_mexico$category_id[df_mexico$category_id == 10] = "Music"
df_mexico$category_id[df_mexico$category_id == 15] = "Pets & Animals"
df_mexico$category_id[df_mexico$category_id == 17] = "Sports"
df_mexico$category_id[df_mexico$category_id == 19] = "Travel & Events"
df_mexico$category_id[df_mexico$category_id == 20] = "Gaming"
df_mexico$category_id[df_mexico$category_id == 22] = "People & Blogs"
df_mexico$category_id[df_mexico$category_id == 23] = "Comedy"
df_mexico$category_id[df_mexico$category_id == 24] = "Entertainment"
df_mexico$category_id[df_mexico$category_id == 25] = "News & Politics"
df_mexico$category_id[df_mexico$category_id == 26] = "Howto & Style"
df_mexico$category_id[df_mexico$category_id == 27] = "Education"
df_mexico$category_id[df_mexico$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_mexico$category_id[df_mexico$category_id == 43] = "Shows"
df_mexico = subset(df_mexico, category_id != "29")
df_mexico$category_id = as.factor(df_mexico$category_id)
ggplot(df_mexico, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for mexico")

#russia
table(df_russia$category_id)
df_russia$category_id[df_russia$category_id == 1] = "Film & Animation"
df_russia$category_id[df_russia$category_id == 2] = "Autos & Vehicles"
df_russia$category_id[df_russia$category_id == 10] = "Music"
df_russia$category_id[df_russia$category_id == 15] = "Pets & Animals"
df_russia$category_id[df_russia$category_id == 17] = "Sports"
df_russia$category_id[df_russia$category_id == 19] = "Travel & Events"
df_russia$category_id[df_russia$category_id == 20] = "Gaming"
df_russia$category_id[df_russia$category_id == 22] = "People & Blogs"
df_russia$category_id[df_russia$category_id == 23] = "Comedy"
df_russia$category_id[df_russia$category_id == 24] = "Entertainment"
df_russia$category_id[df_russia$category_id == 25] = "News & Politics"
df_russia$category_id[df_russia$category_id == 26] = "Howto & Style"
df_russia$category_id[df_russia$category_id == 27] = "Education"
df_russia$category_id[df_russia$category_id == 28] = "Science & Technology"
df_russia$category_id[df_russia$category_id == 30] = "Movies"
df_russia$category_id[df_russia$category_id == 43] = "Shows"
#df_russia$category_id = replace("29","Autos & Vehicles",df_russia$category_id) #29 --------CANT FIND so remove
df_russia = subset(df_russia, category_id != "29")
df_russia$category_id = as.factor(df_russia$category_id)
ggplot(df_russia, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for russia")

#us
table(df_us_n$category_id)
df_us_n$category_id[df_us_n$category_id == 1] = "Film & Animation"
df_us_n$category_id[df_us_n$category_id == 2] = "Autos & Vehicles"
df_us_n$category_id[df_us_n$category_id == 10] = "Music"
df_us_n$category_id[df_us_n$category_id == 15] = "Pets & Animals"
df_us_n$category_id[df_us_n$category_id == 17] = "Sports"
df_us_n$category_id[df_us_n$category_id == 19] = "Travel & Events"
df_us_n$category_id[df_us_n$category_id == 20] = "Gaming"
df_us_n$category_id[df_us_n$category_id == 22] = "People & Blogs"
df_us_n$category_id[df_us_n$category_id == 23] = "Comedy"
df_us_n$category_id[df_us_n$category_id == 24] = "Entertainment"
df_us_n$category_id[df_us_n$category_id == 25] = "News & Politics"
df_us_n$category_id[df_us_n$category_id == 26] = "Howto & Style"
df_us_n$category_id[df_us_n$category_id == 27] = "Education"
df_us_n$category_id[df_us_n$category_id == 28] = "Science & Technology"
#29 --------CANT FIND so remove
df_us_n$category_id[df_us_n$category_id == 43] = "Shows"
df_us_n$category_id[df_us_n$category_id == 44] = "Trailers"
df_us_n = subset(df_us_n, category_id != "29")
df_us_n$category_id = as.factor(df_us_n$category_id)
ggplot(df_us_n, aes(x = category_id)) + geom_bar(stat="count") + ylab("Videos in Catagories") + ggtitle("Catagories for USA")


table(df_canada$category_id)
table(df_germany$category_id)
table(df_france$category_id)
table(df_uk$category_id)
table(df_india$category_id)
table(df_japan$category_id)
table(df_korea$category_id)
table(df_mexico$category_id)
table(df_russia$category_id)
table(df_us_n$category_id)

##### USA Regression
#see if there is a correlation with size of title/tags/description, and views
df_us_n$tagcount = nchar(gsub("[^a-z]","",df_us_n$tags))
df_us_n$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_us_n$title))
df_us_n$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_us_n$description))
#set channel title as factor
df_us_n$channel_title = as.factor(df_us_n$channel_title)
df_us_n$comments_disabled = as.factor(df_us_n$comments_disabled)
df_us_n$ratings_disabled = as.factor(df_us_n$ratings_disabled)
str(df_us_n)

#remove some variables we don't need
df_us_n <- subset (df_us_n, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_us_n <- subset (df_us_n, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_us_n, 1, function(row) all(row !=0 ))
df_us_n=df_us_n[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_us_n$views = df_us_n$views + 1
df_us_n$likes = df_us_n$likes + 1
df_us_n$dislikes  = df_us_n$dislikes + 1
df_us_n$comment_count = df_us_n$comment_count + 1
df_us_n$videoDuration = df_us_n$videoDuration + 1
df_us_n$tagcount  = df_us_n$tagcount + 1
df_us_n$titlecount= df_us_n$titlecount + 1
df_us_n$descriptioncount  = df_us_n$descriptioncount + 1
df_us_n$percentlikes  = df_us_n$percentlikes + 1

str(df_us_n)
df_us_n$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_us_n$percentlikes  = df_us_n$percentlikes + 1
#use this for corrplot
df_us_numeric <- subset (df_us_n, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_us_numeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_us_numeric, SplitRatio = 0.8)
train = subset(df_us_numeric, sample == TRUE)
test  = subset(df_us_numeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### CANADA
#see if there is a correlation with size of title/tags/description, and views
df_canada$tagcount = nchar(gsub("[^a-z]","",df_canada$tags))
df_canada$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_canada$title))
df_canada$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_canada$description))
#set channel title as factor
df_canada$channel_title = as.factor(df_canada$channel_title)
df_canada$comments_disabled = as.factor(df_canada$comments_disabled)
df_canada$ratings_disabled = as.factor(df_canada$ratings_disabled)
str(df_canada)

#remove some variables we don't need
df_canada <- subset (df_canada, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_canada <- subset (df_canada, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_canada, 1, function(row) all(row !=0 ))
df_canada=df_canada[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_canada$views = df_canada$views + 1
df_canada$likes = df_canada$likes + 1
df_canada$dislikes  = df_canada$dislikes + 1
df_canada$comment_count = df_canada$comment_count + 1
df_canada$videoDuration = df_canada$videoDuration + 1
df_canada$tagcount  = df_canada$tagcount + 1
df_canada$titlecount= df_canada$titlecount + 1
df_canada$descriptioncount  = df_canada$descriptioncount + 1
df_canada$percentlikes  = df_canada$percentlikes + 1

str(df_canada)
df_canada$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_canada$percentlikes  = df_canada$percentlikes + 1
#use this for corrplot
df_canadaumeric <- subset (df_canada, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_canadaumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_canadaumeric, SplitRatio = 0.8)
train = subset(df_canadaumeric, sample == TRUE)
test  = subset(df_canadaumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### GERMANY
#see if there is a correlation with size of title/tags/description, and views
df_germany$tagcount = nchar(gsub("[^a-z]","",df_germany$tags))
df_germany$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_germany$title))
df_germany$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_germany$description))
#set channel title as factor
df_germany$channel_title = as.factor(df_germany$channel_title)
df_germany$comments_disabled = as.factor(df_germany$comments_disabled)
df_germany$ratings_disabled = as.factor(df_germany$ratings_disabled)
str(df_germany)

#remove some variables we don't need
df_germany <- subset (df_germany, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_germany <- subset (df_germany, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_germany, 1, function(row) all(row !=0 ))
df_germany=df_germany[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_germany$views = df_germany$views + 1
df_germany$likes = df_germany$likes + 1
df_germany$dislikes  = df_germany$dislikes + 1
df_germany$comment_count = df_germany$comment_count + 1
df_germany$videoDuration = df_germany$videoDuration + 1
df_germany$tagcount  = df_germany$tagcount + 1
df_germany$titlecount= df_germany$titlecount + 1
df_germany$descriptioncount  = df_germany$descriptioncount + 1
df_germany$percentlikes  = df_germany$percentlikes + 1

str(df_germany)
df_germany$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_germany$percentlikes  = df_germany$percentlikes + 1
#use this for corrplot
df_germanyumeric <- subset (df_germany, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_germanyumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_germanyumeric, SplitRatio = 0.8)
train = subset(df_germanyumeric, sample == TRUE)
test  = subset(df_germanyumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### FRANCE
#see if there is a correlation with size of title/tags/description, and views
df_france$tagcount = nchar(gsub("[^a-z]","",df_france$tags))
df_france$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_france$title))
df_france$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_france$description))
#set channel title as factor
df_france$channel_title = as.factor(df_france$channel_title)
df_france$comments_disabled = as.factor(df_france$comments_disabled)
df_france$ratings_disabled = as.factor(df_france$ratings_disabled)
str(df_france)

#remove some variables we don't need
df_france <- subset (df_france, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_france <- subset (df_france, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_france, 1, function(row) all(row !=0 ))
df_france=df_france[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_france$views = df_france$views + 1
df_france$likes = df_france$likes + 1
df_france$dislikes  = df_france$dislikes + 1
df_france$comment_count = df_france$comment_count + 1
df_france$videoDuration = df_france$videoDuration + 1
df_france$tagcount  = df_france$tagcount + 1
df_france$titlecount= df_france$titlecount + 1
df_france$descriptioncount  = df_france$descriptioncount + 1
df_france$percentlikes  = df_france$percentlikes + 1

str(df_france)
df_france$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_france$percentlikes  = df_france$percentlikes + 1
#use this for corrplot
df_franceumeric <- subset (df_france, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_franceumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_franceumeric, SplitRatio = 0.8)
train = subset(df_franceumeric, sample == TRUE)
test  = subset(df_franceumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### UK
#see if there is a correlation with size of title/tags/description, and views
df_uk$tagcount = nchar(gsub("[^a-z]","",df_uk$tags))
df_uk$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_uk$title))
df_uk$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_uk$description))
#set channel title as factor
df_uk$channel_title = as.factor(df_uk$channel_title)
df_uk$comments_disabled = as.factor(df_uk$comments_disabled)
df_uk$ratings_disabled = as.factor(df_uk$ratings_disabled)
str(df_uk)

#remove some variables we don't need
df_uk <- subset (df_uk, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_uk <- subset (df_uk, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_uk, 1, function(row) all(row !=0 ))
df_uk=df_uk[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_uk$views = df_uk$views + 1
df_uk$likes = df_uk$likes + 1
df_uk$dislikes  = df_uk$dislikes + 1
df_uk$comment_count = df_uk$comment_count + 1
df_uk$videoDuration = df_uk$videoDuration + 1
df_uk$tagcount  = df_uk$tagcount + 1
df_uk$titlecount= df_uk$titlecount + 1
df_uk$descriptioncount  = df_uk$descriptioncount + 1
df_uk$percentlikes  = df_uk$percentlikes + 1

str(df_uk)
df_uk$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_uk$percentlikes  = df_uk$percentlikes + 1
#use this for corrplot
df_ukumeric <- subset (df_uk, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_ukumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_ukumeric, SplitRatio = 0.8)
train = subset(df_ukumeric, sample == TRUE)
test  = subset(df_ukumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### India
#see if there is a correlation with size of title/tags/description, and views
df_india$tagcount = nchar(gsub("[^a-z]","",df_india$tags))
df_india$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_india$title))
df_india$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_india$description))
#set channel title as factor
df_india$channel_title = as.factor(df_india$channel_title)
df_india$comments_disabled = as.factor(df_india$comments_disabled)
df_india$ratings_disabled = as.factor(df_india$ratings_disabled)
str(df_india)

#remove some variables we don't need
df_india <- subset (df_india, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_india <- subset (df_india, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_india, 1, function(row) all(row !=0 ))
df_india=df_india[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_india$views = df_india$views + 1
df_india$likes = df_india$likes + 1
df_india$dislikes  = df_india$dislikes + 1
df_india$comment_count = df_india$comment_count + 1
df_india$videoDuration = df_india$videoDuration + 1
df_india$tagcount  = df_india$tagcount + 1
df_india$titlecount= df_india$titlecount + 1
df_india$descriptioncount  = df_india$descriptioncount + 1
df_india$percentlikes  = df_india$percentlikes + 1

str(df_india)
df_india$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_india$percentlikes  = df_india$percentlikes + 1
#use this for corrplot
df_indiaumeric <- subset (df_india, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_indiaumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_indiaumeric, SplitRatio = 0.8)
train = subset(df_indiaumeric, sample == TRUE)
test  = subset(df_indiaumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### Japan
#see if there is a correlation with size of title/tags/description, and views
df_japan$tagcount = nchar(gsub("[^a-z]","",df_japan$tags))
df_japan$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_japan$title))
df_japan$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_japan$description))
#set channel title as factor
df_japan$channel_title = as.factor(df_japan$channel_title)
df_japan$comments_disabled = as.factor(df_japan$comments_disabled)
df_japan$ratings_disabled = as.factor(df_japan$ratings_disabled)
str(df_japan)

#remove some variables we don't need
df_japan <- subset (df_japan, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_japan <- subset (df_japan, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_japan, 1, function(row) all(row !=0 ))
df_japan=df_japan[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_japan$views = df_japan$views + 1
df_japan$likes = df_japan$likes + 1
df_japan$dislikes  = df_japan$dislikes + 1
df_japan$comment_count = df_japan$comment_count + 1
df_japan$videoDuration = df_japan$videoDuration + 1
df_japan$tagcount  = df_japan$tagcount + 1
df_japan$titlecount= df_japan$titlecount + 1
df_japan$descriptioncount  = df_japan$descriptioncount + 1
df_japan$percentlikes  = df_japan$percentlikes + 1

str(df_japan)
df_japan$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_japan$percentlikes  = df_japan$percentlikes + 1
#use this for corrplot
df_japanumeric <- subset (df_japan, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_japanumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_japanumeric, SplitRatio = 0.8)
train = subset(df_japanumeric, sample == TRUE)
test  = subset(df_japanumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### Korea
#see if there is a correlation with size of title/tags/description, and views
df_korea$tagcount = nchar(gsub("[^a-z]","",df_korea$tags))
df_korea$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_korea$title))
df_korea$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_korea$description))
#set channel title as factor
df_korea$channel_title = as.factor(df_korea$channel_title)
df_korea$comments_disabled = as.factor(df_korea$comments_disabled)
df_korea$ratings_disabled = as.factor(df_korea$ratings_disabled)
str(df_korea)

#remove some variables we don't need
df_korea <- subset (df_korea, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_korea <- subset (df_korea, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_korea, 1, function(row) all(row !=0 ))
df_korea=df_korea[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_korea$views = df_korea$views + 1
df_korea$likes = df_korea$likes + 1
df_korea$dislikes  = df_korea$dislikes + 1
df_korea$comment_count = df_korea$comment_count + 1
df_korea$videoDuration = df_korea$videoDuration + 1
df_korea$tagcount  = df_korea$tagcount + 1
df_korea$titlecount= df_korea$titlecount + 1
df_korea$descriptioncount  = df_korea$descriptioncount + 1
df_korea$percentlikes  = df_korea$percentlikes + 1

str(df_korea)
df_korea$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_korea$percentlikes  = df_korea$percentlikes + 1
#use this for corrplot
df_koreaumeric <- subset (df_korea, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_koreaumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_koreaumeric, SplitRatio = 0.8)
train = subset(df_koreaumeric, sample == TRUE)
test  = subset(df_koreaumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### Mexico
#see if there is a correlation with size of title/tags/description, and views
df_mexico$tagcount = nchar(gsub("[^a-z]","",df_mexico$tags))
df_mexico$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_mexico$title))
df_mexico$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_mexico$description))
#set channel title as factor
df_mexico$channel_title = as.factor(df_mexico$channel_title)
df_mexico$comments_disabled = as.factor(df_mexico$comments_disabled)
df_mexico$ratings_disabled = as.factor(df_mexico$ratings_disabled)
str(df_mexico)

#remove some variables we don't need
df_mexico <- subset (df_mexico, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_mexico <- subset (df_mexico, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_mexico, 1, function(row) all(row !=0 ))
df_mexico=df_mexico[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_mexico$views = df_mexico$views + 1
df_mexico$likes = df_mexico$likes + 1
df_mexico$dislikes  = df_mexico$dislikes + 1
df_mexico$comment_count = df_mexico$comment_count + 1
df_mexico$videoDuration = df_mexico$videoDuration + 1
df_mexico$tagcount  = df_mexico$tagcount + 1
df_mexico$titlecount= df_mexico$titlecount + 1
df_mexico$descriptioncount  = df_mexico$descriptioncount + 1
df_mexico$percentlikes  = df_mexico$percentlikes + 1

str(df_mexico)
df_mexico$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_mexico$percentlikes  = df_mexico$percentlikes + 1
#use this for corrplot
df_mexicoumeric <- subset (df_mexico, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_mexicoumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_mexicoumeric, SplitRatio = 0.8)
train = subset(df_mexicoumeric, sample == TRUE)
test  = subset(df_mexicoumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

##### Russia
#see if there is a correlation with size of title/tags/description, and views
df_russiao$tagcount = nchar(gsub("[^a-z]","",df_russiao$tags))
df_russiao$titlecount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_russiao$title))
df_russiao$descriptioncount = nchar(gsub("[^a-z]","",ignore.case = TRUE,df_russiao$description))
#set channel title as factor
df_russiao$channel_title = as.factor(df_russiao$channel_title)
df_russiao$comments_disabled = as.factor(df_russiao$comments_disabled)
df_russiao$ratings_disabled = as.factor(df_russiao$ratings_disabled)
str(df_russiao)

#remove some variables we don't need
df_russiao <- subset (df_russiao, select = -c(video_id,publish_time,tags,description,title,trending_date))
df_russiao <- subset (df_russiao, select = -c(channel_title))
#clean up missing values
row_sub = apply(df_russiao, 1, function(row) all(row !=0 ))
df_russiao=df_russiao[row_sub,]
#add one to all numerical rows so we don't deal with 0
df_russiao$views = df_russiao$views + 1
df_russiao$likes = df_russiao$likes + 1
df_russiao$dislikes  = df_russiao$dislikes + 1
df_russiao$comment_count = df_russiao$comment_count + 1
df_russiao$videoDuration = df_russiao$videoDuration + 1
df_russiao$tagcount  = df_russiao$tagcount + 1
df_russiao$titlecount= df_russiao$titlecount + 1
df_russiao$descriptioncount  = df_russiao$descriptioncount + 1
df_russiao$percentlikes  = df_russiao$percentlikes + 1

str(df_russiao)
df_russiao$percentlikes = (df$likes / (df$likes + df$dislikes)) * 100
df_russiao$percentlikes  = df_russiao$percentlikes + 1
#use this for corrplot
df_russiaoumeric <- subset (df_russiao, select = -c(category_id, comments_disabled, ratings_disabled))


library(corrplot)
library(MASS)
library(caTools)
#grab only numerical variables 

corrplot(cor(df_russiaoumeric,use="complete.obs"),method = 'number',type="lower")
#created a test train split for the views3
sample = sample.split(df_russiaoumeric, SplitRatio = 0.8)
train = subset(df_russiaoumeric, sample == TRUE)
test  = subset(df_russiaoumeric, sample == FALSE)


lm01 = lm(views ~., data=train) 
summary(lm01) #r = 0.787

model01 = stepAIC(log01, direction="both")
summary(model01)

test$predictedviews = predict(lm01, test)

plot(lm01$residuals)
mean(lm01$residuals)
#####

#Did not work so stopped here
#test = subset(test, select = -c(views))
library(caret)
library(lattice)
library(ggplot2)
library(corrplot)
library(pROC)

# 10-fold Cross Validation
indices = sample.split(df_us_n$views, SplitRatio = 0.8) #20% is test
train = df_us_n[indices,]
validation = df_us_n[!indices,]

control = trainControl(method="cv", number=10)

str(df_us_n)
summary(df_us_n)

# Random Forest
fit.rf = train(views~., data=train, method="rf", trControl=control) #<---takes much too long!



library(writexl)
write_xlsx(df_us_n, "USvid.xlsx")


