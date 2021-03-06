# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jordan May -> x15515673                               #
# An analysis on suicide rates with Economical Factors  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# I am getting the datasets from "Import Dataset" button within R - Studio

# Getting summary of new datasets and checking for NA's
summary(who_suicide_statistics)
summary(economics_dataset)
summary(gni_dataset)

# Checking structure of the datasets
str(who_suicide_statistics)
str(economics_dataset)
str(gni_dataset)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Now we Start cleaning our FIRST data set  (suicide)                                                         # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Now, for our NA's in the Suicide column, we can't simply just remove them since we need consistent year blocks
replaced_na_in_suicides_column <- who_suicide_statistics

# For every country unique in our country column
for(countries in unique(replaced_na_in_suicides_column$country)){
  
  # Assert, if the country in the dataset is equal to that of 
  column_country = replaced_na_in_suicides_column[replaced_na_in_suicides_column$country == countries,]
  
  # Now, calculate the median of the values excluding na values
  median_of_country = median((column_country$suicides_no), na.rm = TRUE)
  
  # Finally, if the column is an NA and the column is equal to that of countries then replace it with the value of the median 
  replaced_na_in_suicides_column[is.na(replaced_na_in_suicides_column$suicides_no) & replaced_na_in_suicides_column$country == countries, "col"] <- median_of_country
}

# Now, we have a column called col now we should transpose these values into our suicides_no column
for(i in 1:nrow(replaced_na_in_suicides_column)){
  if(is.na(replaced_na_in_suicides_column$suicides_no[i])){
    replaced_na_in_suicides_column$suicides_no[i] = replaced_na_in_suicides_column$col[i]
  }
}

# We still have an extra column names "col", we should remove this
replaced_na_in_suicides_column$col <- NULL

# Make another frame of our dataset
suicide_stats_without_zimbabwe <- replaced_na_in_suicides_column

# After analysing the dataset, I noticed Zimbabwe does not have much data thus it would not be useful. Should delete this.
suicide_stats_without_zimbabwe[suicide_stats_without_zimbabwe$country != "Zimbabwe", ]

# We create a new set of values where the columns country contains the factor Zimbabwe 
removed <- which(suicide_stats_without_zimbabwe$country=="Zimbabwe")

# We then recreate our dataset, removing the columns where country contains the factor Zimbabwe
suicide_stats_without_zimbabwe <- suicide_stats_without_zimbabwe[-removed, ]

# Make another dataset
suicide_stats_without_year <- suicide_stats_without_zimbabwe

# We remove records from 1980 - 1985
removed_years <- which(suicide_stats_without_year$year=="1979" | suicide_stats_without_year$year=="1980" | suicide_stats_without_year$year=="1981" | suicide_stats_without_year$year=="1982" | suicide_stats_without_year$year=="1983" | suicide_stats_without_year$year=="1984")
suicide_stats_without_year <- suicide_stats_without_year[-removed_years, ]

# Keep record of our dataset after removing years
removing_more_countries <- suicide_stats_without_year

# Remove some countries that will not be useful for this project
removed_countries <- which(removing_more_countries$country=="Cayman Islands" | removing_more_countries$country=="British Virgin Islands" | removing_more_countries$country=="Dominica" | removing_more_countries$country=="Dominican Republic" | removing_more_countries$country=="Jordan" | removing_more_countries$country=="Saint Vincent and Grenadines" | removing_more_countries$country=="Saint Pierre and Miquelon" | removing_more_countries$country == "San Marino" | removing_more_countries$country == "Turks and Caicos Islands" | removing_more_countries$country == "Brunei Darussalam" | removing_more_countries$country == "Anguilla" | removing_more_countries$country == "Antigua and Barbuda" | removing_more_countries$country == "Morocco")
removing_more_countries <- removing_more_countries[-removed_countries, ]

# Now we have a cleaned dataset
cleaned_who_suicide <- removing_more_countries

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Now we Start cleaning our SECOND data set  (Economics)                                                      # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Assigning economics to another dataframe so we don't have an effect to the main dataset
cleaning_economics <- economics_dataset

# We can drop the column "Time Code"
cleaning_economics$Time.Code <- NULL

# Rename our columns - Country Name	Country Code	Time	Time Code	GDP (current US$) [NY.GDP.MKTP.CD]	GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]	GDP per capita (current US$) [NY.GDP.PCAP.CD]	GNI (current US$) [NY.GNP.MKTP.CD]	GNI growth (annual %) [NY.GNP.MKTP.KD.ZG]	GNI per capita growth (annual %) [NY.GNP.PCAP.KD.ZG]	Unemployment, total (% of total labor force) (national estimate) [SL.UEM.TOTL.NE.ZS]
colnames(cleaning_economics) = c("Country","Country Code", "Year", "GDP (Current US$)", "GDP Growth (Annual %)", "GDP Per Capita", "GNI (Current US$)", "GNI Growth (Annual %)", "GNI Per Capita (Annual %)", "Unemployment (% of total labour force" )

# copy of our dataset
cleaned_year_dataset <- cleaning_economics

# Remove unnecessary countries 
removing_countries_econ <- which(cleaned_year_dataset$Country=="Cayman Islands" | cleaned_year_dataset$Country=="British Virgin Islands" | cleaned_year_dataset$Country=="Dominica" | cleaned_year_dataset$Country=="Dominican Republic" | cleaned_year_dataset$Country=="Jordan" | cleaned_year_dataset$Country=="Saint Vincent and Grenadines" | cleaned_year_dataset$Country=="Saint Pierre and Miquelon" | cleaned_year_dataset$Country == "San Marino" | cleaned_year_dataset$Country == "Turks and Caicos Islands" | cleaned_year_dataset$Country == "Brunei Darussalam" | cleaned_year_dataset$Country == "Anguilla" | cleaned_year_dataset$Country == "Antigua and Barbuda" | cleaned_year_dataset$Country == "IBRD only" | cleaned_year_dataset$Country == "IDA & IBRD total" | cleaned_year_dataset$Country == "IDA blend" | cleaned_year_dataset$Country == "IDA only" | cleaned_year_dataset$Country == "Late-demographic dividend" | cleaned_year_dataset$Country == "Not classified" | cleaned_year_dataset$Country == "Other small states" | cleaned_year_dataset$Country == "Post-demographic dividend" | cleaned_year_dataset$Country == "Pre-demographic dividend" | cleaned_year_dataset$Country == "Small states" | cleaned_year_dataset == "South Asia (IDA & IBRD)")
removing_countries_economics <- cleaned_year_dataset[-removing_countries_econ, ]

# Cleaned economics dataset
cleaned_economics_dataset <- removing_countries_economics

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Economics dataset is now cleaned now                                                                        # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Now we have our two datasets, now we should combine them into one dataset!                                  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Assign our datasets to new dataframes
new_suicide_dataset <- cleaned_who_suicide
new_economics_dataset <- cleaned_economics_dataset

View(new_suicide_dataset)
View(new_economics_dataset)

# We nneed to make a new columnn for uid
new_suicide_dataset["UID"] <- 0
new_economics_dataset["UID"] <- 0

# Then we paste and combine country and year to make a matching UID in both datasets
new_suicide_dataset$UID <- paste(new_suicide_dataset$country,new_suicide_dataset$year, sep="-")
new_economics_dataset$UID <- paste(new_economics_dataset$Country,new_economics_dataset$Year, sep="-")

# We combine them by UID 
master_dataset = merge(new_suicide_dataset, new_economics_dataset, by='UID')

# We can now delete our UID, Year and Country columns s as these are no longer needed!
master_dataset$UID = NULL
master_dataset$Year = NULL
master_dataset$Country = NULL

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We have another dataset called GNI, lets add this to our master now                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Quickly replace ".." with NA
gni_dataset[gni_dataset == ".."] <- NA

# Make a new UID column in both master and also GNI
master_dataset["UID"] <- 0
gni_dataset["UID"] <- 0

# We paste year and country for uid
master_dataset$UID <- paste(master_dataset$country,master_dataset$year, sep="-")
gni_dataset$UID <- paste(gni_dataset$Country.Name,gni_dataset$Time, sep="-")

# Match
master_dataset = merge(master_dataset, gni_dataset, by='UID')
master_dataset$UID = NULL
master_dataset$Time = NULL
master_dataset$Time.Code = NULL
master_dataset$Country.Name = NULL
master_dataset$Country.Code = NULL


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We now have a master dataset with our needed data, now we export to CSV                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Write to CSV
write.table(master_dataset, file = "master.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Import our libraries for visualisations now                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(reshape2)){
  install.packages("reshape2")
  library("reshape2")
}
if(!require(dplyr)){
  install.packages("dplyr")
  library("dplyr")
}
if(!require(plotly)){
  install.packages("plotly")
  library("plotly")
}
if(!require(plyr)){
  install.packages("plyr")
  library("plyr")
}
if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}
if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}
if(!require(broom)){
  install.packages("broom")
  library(broom)
}
if(!require(factoextra)){
  install.packages("factoextra")
  library(factoextra)
}
if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Now we have our imports                                                                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Apply some algorithims to our dataset                                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

###### K-means Algorithim to Highlight influencing variables
# We make a copy of our master dataset
copy_master_for_kmean <- master_dataset

# We then replace ".." with NA
copy_master_for_kmean[copy_master_for_kmean == ".."] <- NA

# Omit these NA's as we are not concerned with correlated years
copy_master_for_kmean <- na.omit(copy_master_for_kmean)

# we have to ensure both our concering fields are numerical, lets do this now
#gdp_p_capita_europe$`GDP Per Capita` <- as.numeric(as.character(gdp_p_capita_europe$`GDP Per Capita`))
#gdp_p_capita_europe$population <- as.numeric(as.character(gdp_p_capita_europe$population))

# Now, we just group our data so we just have linear countries, also, lets calculate 100k per population. Inner join not working as expected here 
copy_master_for_kmean_filtering <- copy_master_for_kmean %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

# We now just plop our economical factors back in by country match
copy_master_for_kmean_filtering <- cbind(copy_master_for_kmean_filtering, copy_master_for_kmean[match(copy_master_for_kmean_filtering$country, copy_master_for_kmean$country), ])

# delete just normal columnns that are duplicates
copy_master_for_kmean_filtering <- copy_master_for_kmean_filtering[, -c(3:7)]

#copy_master_for_kmean_filtering <- copy_master_for_kmean_filtering[, -c(3:11)]
#copy_master_for_kmean_filtering <- copy_master_for_kmean_filtering[, -c(13:16)]

# now we should make a subset of europe, just incase we might need this full list again (in accordance to -> https://europa.eu/european-union/about-eu/countries_en)
kmeans_european_countries <- copy_master_for_kmean_filtering[
  copy_master_for_kmean_filtering$country == "Austria"|
    copy_master_for_kmean_filtering$country == "Italy" | 
    copy_master_for_kmean_filtering$country == "Belgium" | 
    copy_master_for_kmean_filtering$country == "Latvia" |
    copy_master_for_kmean_filtering$country == "Bulgaria" |
    copy_master_for_kmean_filtering$country == "Lithuania" |
    copy_master_for_kmean_filtering$country == "Luxembourg" | 
    copy_master_for_kmean_filtering$country == "Cyprus" | 
    copy_master_for_kmean_filtering$country == "Malta" | 
    copy_master_for_kmean_filtering$country == "Czechia" | 
    copy_master_for_kmean_filtering$country == "Netherlands" | 
    copy_master_for_kmean_filtering$country == "Denmark" | 
    copy_master_for_kmean_filtering$country == "Poland" | 
    copy_master_for_kmean_filtering$country == "Estonia" | 
    copy_master_for_kmean_filtering$country == "Portugal" | 
    copy_master_for_kmean_filtering$country == "Finland" | 
    copy_master_for_kmean_filtering$country == "Romania" | 
    copy_master_for_kmean_filtering$country == "France" | 
    copy_master_for_kmean_filtering$country == "Slovakia" | 
    copy_master_for_kmean_filtering$country == "Germany" | 
    copy_master_for_kmean_filtering$country == "Slovenia" | 
    copy_master_for_kmean_filtering$country == "Greece" | 
    copy_master_for_kmean_filtering$country == "Spain" | 
    copy_master_for_kmean_filtering$country == "Hungary" | 
    copy_master_for_kmean_filtering$country == "Sweden" | 
    copy_master_for_kmean_filtering$country == "Ireland" | 
    copy_master_for_kmean_filtering$country == "United Kingdom",]

# At this point, we have european data and then the world so we can do a kmeans on both
# World
k_meanns_european <- kmeans_european_countries
# remove all string column, we should only have numerical data
k_meanns_european$country <- NULL
k_meanns_european$`Country Code` <- NULL


#ensure all numerical rows are numerical
k_meanns_european$suicide_per_100k <- as.numeric(as.character(k_meanns_european$suicide_per_100k))
k_meanns_european$`GDP Per Capita` <- as.numeric(as.character(k_meanns_european$`GDP Per Capita`))
k_meanns_european$`GNI (Current US$)` <- as.numeric(as.character(k_meanns_european$`GNI (Current US$)`))
k_meanns_european$`GNI Growth (Annual %)` <- as.numeric(as.character(k_meanns_european$`GNI Growth (Annual %)`))
k_meanns_european$`GNI Per Capita (Annual %)` <- as.numeric(as.character(k_meanns_european$`GNI Per Capita (Annual %)`))
k_meanns_european$`Unemployment (% of total labour force` <- as.numeric(as.character(k_meanns_european$`Unemployment (% of total labour force`))
k_meanns_european$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD. <- as.numeric(as.character(kmeans_european_countries$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.))


# We normalize our dataset columns
normalize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}
k_meanns_european[, 1:6] <- normalize(k[, 1:6])

# set the seed
set.seed(999)

#random generator
rnum <- sample(rep(1:153))
k_meanns_european <- k_meanns_european[rnum, ] 
k_meanns_european <- na.omit(k_meanns_european)

# calculate kmeans for all columns with 3 clusteroids
k_means_clus_euro <- kmeans(k_meanns_european[c(1, 2, 3, 4, 5, 6)], 3)

#plot the graph
plot(k_meanns_european[, ], col = k_means_clus_euro$cluster)

# Explannation : So, it becomes clear that there seems to be some correlation with suicide numbers and GNI
# Furthermore, on level 4, all have a correlation (suicide v gni, pop v gni, gdp vs gni etc). 


####### Linear Regression Model (Help from : https://www.datacamp.com/community/tutorials/linear-regression-R#fit)
# So we should analyse what factors (gdp i.e.) have an impact on the suicide numbers
linear_model_dataset <- master_dataset

# if row contains .. then return NA
linear_model_dataset[linear_model_dataset == ".."] <- NA

linear_model_dataset <- na.omit(linear_model_dataset)

linear_model_dataset$`GDP Per Capita` <- as.numeric(as.character(linear_model_dataset$`GDP Per Capita`))


linear_model_dataset$population <- as.numeric(as.character(linear_model_dataset$population))

#thanks to this link, issue was package dependicies,https://stackoverflow.com/questions/26923862/why-are-my-dplyr-group-by-summarize-not-working-properly-name-collision-with
country_mean_gdp <- linear_model_dataset %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000, 
            gdp_per_capita = mean(linear_model_dataset$`GDP Per Capita`))

# need match with country and GDP
l <- cbind(country_mean_gdp, linear_model_dataset[match(country_mean_gdp$country, linear_model_dataset$country), ])

# filter
# Lets filter to only european countries
l <- l[
  l$country == "Austria"|
    l$country == "Italy" | 
    l$country == "Belgium" | 
    l$country == "Latvia" |
    l$country == "Bulgaria" |
    l$country == "Lithuania" |
    l$country == "Luxembourg" | 
    l$country == "Cyprus" | 
    l$country == "Malta" | 
    l$country == "Czechia" | 
    l$country == "Netherlands" | 
    l$country == "Denmark" | 
    l$country == "Poland" | 
    l$country == "Estonia" | 
    l$country == "Portugal" | 
    l$country == "Finland" | 
    l$country == "Romania" | 
    l$country == "France" | 
    l$country == "Slovakia" | 
    l$country == "Germany" | 
    l$country == "Slovenia" | 
    l$country == "Greece" | 
    l$country == "Spain" | 
    l$country == "Hungary" | 
    l$country == "Sweden" | 
    l$country == "Ireland" | 
    l$country == "United Kingdom",]

# Doing numerics
l$`GDP Per Capita` <- as.numeric(as.character(l$`GDP Per Capita`))
l$`GNI Per Capita (Annual %)` <- as.numeric(as.character(l$`GNI Per Capita (Annual %)`))
l$`Unemployment (% of total labour force` <- as.numeric(as.character(l$`Unemployment (% of total labour force`))
l$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD. <- as.numeric(as.character(l$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.))


# linear model
model1 <- lm(as.numeric(as.character(suicide_per_100k)) ~ 
               as.numeric(as.character(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.)) + 
               as.numeric(as.character(`GNI Per Capita (Annual %)`)) + as.numeric(as.character(`GDP Per Capita`)) + 
               as.numeric(as.character(`GNI Growth (Annual %)`)), data = l)
options(scipen = 999)

summary(model1)
# we are getting improvements on our R square value. We can associate 58.93% of all suicides to contribute to the above factors

set.seed(999)  # setting seed to reproduce results of random sampling
sample_training <- sample(1:nrow(l), 0.8*nrow(l))  # row indices for training data

trainingData <- l[sample_training, ]  # model training data
testData  <- l[-sample_training, ]   # test data


# so we have training and test, lets apply our linear function to training first
linear_model_predict_suicide <- lm(as.numeric(as.character(suicide_per_100k)) ~ as.numeric(as.character(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.)) + 
                                     as.numeric(as.character(`GNI Per Capita (Annual %)`)) + 
                                     as.numeric(as.character(`GDP Per Capita`)) + 
                                     as.numeric(as.character(`GNI Growth (Annual %)`)), data = trainingData)
summary(linear_model_predict_suicide)

# Adjusted R is 0.4651 thus we can say that we can explain 0.4651 or 47% of our dataset being attributed to economical factors
# Now we should put our model to the test 
predicting_dataset <- predict(linear_model_predict_suicide,testData,type = "response")

# ref https://www.datacamp.com/community/tutorials/linear-regression-R
residuals <- testData$suicide_per_100k - predicting_dataset

linreg_pred <- data.frame("Predicted" = predicting_dataset, "Actual" = testData$suicide_per_100k, "Residual" = residuals)

plot(residuals, pch = 16, col = "red")


##### Construct a Heat Map in order to demonstrate what variables correlate to each other
## ref -> http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# AND http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization


# delete non numerical
copy_heat_linear_datset$country <- NULL
copy_heat_linear_datset$year <- NULL
copy_heat_linear_datset$age <- NULL
copy_heat_linear_datset$sex <- NULL
copy_heat_linear_datset$`Country Code` <- NULL
copy_heat_linear_datset$country <- NULL
copy_heat_linear_datset$gdp_per_capita <- NULL

# Convert all columns to numeric enforce
copy_heat_linear_datset[] <- lapply(copy_heat_linear_datset, function(x) as.numeric(as.character(x)))

#Now lets plot our heat map
qplot(x=Var1, y=Var2, data=melt(cor(copy_heat_linear_datset, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", 
                      name="Correlation") +
                      theme_minimal()+ 
                      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
                      coord_fixed()+
                      ggtitle("Correlation Heatmap") +
                      theme(plot.title = element_text(hjust = 0.4))


## Using this formula : Call: lm(formula = suicide_per_100k ~ l$`GDP Per Capita` + l$`GNI Per Capita (Annual %)` +  l$`Unemployment (% of total labour force`, data = l)
## our adjusted R-squared values was -0.01276 lm(formula = suicide_per_100k ~ l$`GDP Per Capita` + l$`GNI Per Capita (Annual %)` +  l$`Unemployment (% of total labour force` + l$population + l$suicides_no, data = l)
## our adjusted R-Squared value increases 0.07413 thus our prediction is getting closer

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Visuals for our insights
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # INSIGHT 1: Lets look at Ireland and analyse GDP and Suicide Rates! ##

# We should make a subset of the data we want to focus on, in this case we just want Ireland
ireland_gdp_suicideRates <- subset(master_dataset, country == "Ireland", select = c("country","year","suicides_no", "GDP (Current US$)", "GDP Per Capita" ))

# Rename our columnns so its easier to read!
colnames(ireland_gdp_suicideRates) = c("Country","Year","su_numbers", "GDP", "GDP-PC" )

# turn our columns into integer values as previously we had factors etc
#ireland_gdp_suicideRates$GDP <- as.numeric(as.character(ireland_gdp_suicideRates$GDP))
#ireland_gdp_suicideRates$GDP <- as.numeric(as.character(ireland_gdp_suicideRates$su_numbers))

# Now we need to aggreagate the suicide number for every year block and sum (2015-200, 2011-300 etc)
dfr <- aggregate(ireland_gdp_suicideRates['su_numbers'], by = ireland_gdp_suicideRates['Year'], sum)

# So we have our suicide numbers, lets add in our GDP as well
dfr['GDP']= unique(ireland_gdp_suicideRates['GDP'])

# we need to turn our values into integers again as GDP is still a factor and graph below won't work
dfr$GDP <- as.numeric(as.character(dfr$GDP))
dfr$su_numbers <- as.numeric(as.character(dfr$su_numbers))

# we create two graphs showing gdp and suicide numbers
suicides_graph <- ggplot(dfr, aes(Year)) + 
  geom_line(aes(y = su_numbers) , colour="green") 

suicides_with_gdp <- ggplot(dfr, aes(Year)) + 
  geom_line(aes(y = GDP) , colour="red") 

# now lets visualize on a combined graph
ggarrange(suicides_graph, suicides_with_gdp, labels = c("Suicide Numbers", "GDP"), ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

# Notes : This is quite interesting, it appears as though as GDP increases so does the suicide rate
# this is interesting as GDP signifies a growing economy. It is common for us to encounter graphs which, upon
# further investigation will show one thing but mean another. Lets plot this!

# So we home in on the years that the recessesion hit Ireland which was : 1999 -> 2014
ireland_recession_timeline <- subset(master_dataset, country == "Ireland" & year > "1999", select = c("country","year","suicides_no", "GDP (Current US$)"))

# Convert factor GDP into just numeric
ireland_recession_timeline$`GDP (Current US$)` <- as.numeric(as.character(ireland_recession_timeline$`GDP (Current US$)`))

# make a copy of recession timeline for later on 
ireland_rec_copy <- ireland_recession_timeline

# Now we should take out duplicates and aggregate the suicide numbers only
ireland_recession_timeline <- aggregate(ireland_recession_timeline['suicides_no'], by = ireland_recession_timeline['year'], sum)

# GDP column is removed through the aggregate function, lets add all this back in
ireland_rec_copy$country <- NULL
ireland_rec_copy$year <- NULL
ireland_rec_copy$suicides_no <- NULL

unique_gdp_ireland <- unique(ireland_rec_copy$`GDP (Current US$)`)

ireland_recession_timeline['gdp'] <- unique_gdp_ireland

# Now lets plot this

graph_suicide_recession <- ggplot(ireland_recession_timeline, aes(year)) + 
  geom_line(aes(y = suicides_no) , colour="green") 

graph_suicide_recession_gdp <- ggplot(ireland_recession_timeline, aes(year)) + 
  geom_line(aes(y = gdp) , colour="red") 


ggarrange(graph_suicide_recession, graph_suicide_recession_gdp, labels = c("Suicide Numbers", "GDP"), ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

# Explanantion : So Ireland experienced a rapid growth in its economy over the 2000's. 
# our graph on the right hand side signifies that GDP rapidly rose over the 2000's. 
# Intriguing enough, suicide numbers dipped a good amount through the "Boom" times but 
# during the conomical downturn where the GDP dipped by a good amount, suicide numbers spiked.
# Through some research, I came across this article : https://www.irishtimes.com/news/ireland/irish-news/suicide-rate-rose-15-during-height-of-recession-1.2800500
# It has to be noted that economics is not the sole reason for suicide however it does play into it 
# especially considering if homeowners lost their homes etc.

# # INSIGHT 2: Lets further examine the GDP per Capita for europe only and see does high suicides correlate to low gdp per capita ##
# Important to note GDP per capita signifies living standard among other things

# So first we make a copy of master
gdp_p_capita_europe <- master_dataset

# Now some of our data has ".." in it which will inaccurate our graph, lets replace with NA 
gdp_p_capita_europe[gdp_p_capita_europe == ".."] <- NA

#linear_model_dataset$`GDP Per Capita` <- as.numeric(as.character(linear_model_dataset$`GDP Per Capita`))
# We don't care about years so just remove all NA's
gdp_p_capita_europe <- na.omit(gdp_p_capita_europe)

# we have to ensure both our concering fields are numerical, lets do this now
gdp_p_capita_europe$`GDP Per Capita` <- as.numeric(as.character(gdp_p_capita_europe$`GDP Per Capita`))
gdp_p_capita_europe$population <- as.numeric(as.character(gdp_p_capita_europe$population))

# Now, group by country and calculate the suicides per 100k of the population
gdp_per_capita_filtered <- gdp_p_capita_europe %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

gdp_per_capita_filtered <- cbind(gdp_per_capita_filtered, gdp_p_capita_europe[match(gdp_per_capita_filtered$country, gdp_p_capita_europe$country), ])

gdp_per_capita_filtered <- gdp_per_capita_filtered[, -c(3:11)]
gdp_per_capita_filtered <- gdp_per_capita_filtered[, -c(13:16)]


# now we should make a subset of europe, just incase we might need this full list again (in accordance to -> https://europa.eu/european-union/about-eu/countries_en)
filtered_countries_only_europe <- gdp_per_capita_filtered[
                                      gdp_per_capita_filtered$country == "Austria"|
                                      gdp_per_capita_filtered$country == "Italy" | 
                                      gdp_per_capita_filtered$country == "Belgium" | 
                                      gdp_per_capita_filtered$country == "Latvia" |
                                      gdp_per_capita_filtered$country == "Bulgaria" |
                                      gdp_per_capita_filtered$country == "Lithuania" |
                                      gdp_per_capita_filtered$country == "Luxembourg" | 
                                      gdp_per_capita_filtered$country == "Cyprus" | 
                                      gdp_per_capita_filtered$country == "Malta" | 
                                      gdp_per_capita_filtered$country == "Czechia" | 
                                      gdp_per_capita_filtered$country == "Netherlands" | 
                                      gdp_per_capita_filtered$country == "Denmark" | 
                                      gdp_per_capita_filtered$country == "Poland" | 
                                      gdp_per_capita_filtered$country == "Estonia" | 
                                      gdp_per_capita_filtered$country == "Portugal" | 
                                      gdp_per_capita_filtered$country == "Finland" | 
                                      gdp_per_capita_filtered$country == "Romania" | 
                                      gdp_per_capita_filtered$country == "France" | 
                                      gdp_per_capita_filtered$country == "Slovakia" | 
                                      gdp_per_capita_filtered$country == "Germany" | 
                                      gdp_per_capita_filtered$country == "Slovenia" | 
                                      gdp_per_capita_filtered$country == "Greece" | 
                                      gdp_per_capita_filtered$country == "Spain" | 
                                      gdp_per_capita_filtered$country == "Hungary" | 
                                      gdp_per_capita_filtered$country == "Sweden" | 
                                      gdp_per_capita_filtered$country == "Ireland" | 
                                      gdp_per_capita_filtered$country == "United Kingdom",]
                                       
ggplot(filtered_countries_only_europe, aes(x = `GDP Per Capita`, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000))

# ignore this linear model, i was playing around with variables
linr <- lm(as.numeric(as.character(suicide_per_100k)) ~ as.numeric(as.character(filtered_countries_only_europe$`GNI (Current US$)`)) + as.numeric(as.character(filtered_countries_only_europe$`GNI Per Capita (Annual %)`)) + as.numeric(as.character(filtered_countries_only_europe$`GDP Per Capita`)) + as.numeric(as.character(filtered_countries_only_europe$`GNI Growth (Annual %)`)) , data = filtered_countries_only_europe)

summary(linr)

ggplot(filtered_countries_only_europe, aes(x = `GDP Per Capita`, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 3)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000))

# Explanation : So analysing the eurozone, we can see as GDP per capita gets lower


## # INSIGHT 3: Lets analyse our clusteroids that we plotted using our Kmeans Algoritihim

# We make a copy of master
copy_dataset_for_kmeans_graphs <- master_dataset
# Replace ".." with NA and remove
copy_dataset_for_kmeans_graphs[copy_dataset_for_kmeans_graphs == ".."] <- NA
copy_dataset_for_kmeans_graphs <- na.omit(copy_dataset_for_kmeans_graphs)

# convert factorer column into numerical
copy_dataset_for_kmeans_graphs$`GDP Per Capita` <- as.numeric(as.character(copy_dataset_for_kmeans_graphs$`GDP Per Capita`))

copy_dataset_for_kmeans_graphs$population <- as.numeric(as.character(copy_dataset_for_kmeans_graphs$population))

# Group
copy_dataset_for_kmeans_graphs_filtered <- copy_dataset_for_kmeans_graphs %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

copy_dataset_for_kmeans_graphs_filtered <- cbind(copy_dataset_for_kmeans_graphs_filtered, copy_dataset_for_kmeans_graphs[match(copy_dataset_for_kmeans_graphs_filtered$country, copy_dataset_for_kmeans_graphs$country), ])

copy_dataset_for_kmeans_graphs_filtered <- copy_dataset_for_kmeans_graphs_filtered[, -c(1)]
copy_dataset_for_kmeans_graphs_filtered$`GNI (Current US$)` <- as.numeric(as.character(copy_dataset_for_kmeans_graphs_filtered$`GNI (Current US$)`))
copy_dataset_for_kmeans_graphs_filtered$`Unemployment (% of total labour force` <- as.numeric(as.character(copy_dataset_for_kmeans_graphs_filtered$`Unemployment (% of total labour force`))

str(copy_dataset_for_kmeans_graphs_filtered)
                                                                        
# Now we can plot our combos that we saw had strong clustering with kmeans!
# Plotting our GNI with our Suicide 
plot_1_suicide_gni <- ggplot(copy_dataset_for_kmeans_graphs_filtered, 
                      aes(x = `GNI (Current US$)`, y = suicide_per_100k)) + 
                      geom_point() + 
                      geom_smooth(method = "lm", aes(group = 3)) + 
                      scale_x_continuous(labels=scales::dollar_format(prefix="$"))

# Plotting GNI and Population
plot_2_population_gni <- ggplot(copy_dataset_for_kmeans_graphs_filtered, 
                      aes(x = `GNI (Current US$)`, y = population)) + 
                      geom_point() + 
                      geom_smooth(method = "lm", aes(group = 3)) + 
                      scale_x_continuous(labels=scales::dollar_format(prefix="$"))

# Lets plot these on same graph
ggarrange(plot_1_suicide_gni, plot_2_population_gni, labels = c("Suicide vs GNI", "Population vs GNI"), ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")



##### Insight 4 : Young people (15-24 people M and F) in first world countries vs third world, suicide numbers top 5

## We should bring in another dataset here that has GNI Per Capita using the Atlas method
## this will give us a list of the richest countries.

# Copy of our dataset
insight_four_economics_and_young_people <- master_dataset

# delete all NA's
insight_four_economics_and_young_people[insight_four_economics_and_young_people == ".."] <- NA

insight_four_economics_and_young_people <- na.omit(insight_four_economics_and_young_people)

# Now we filter to just 2013 (Gives a nice data rannge)
data_filtered <- insight_four_economics_and_young_people %>%
 group_by(country) %>%
 dplyr::filter(year == 2013)

### Highest GNI
# Group Ages by country and gni more than 40000
df_filter_ages <- data_filtered %>%
  dplyr::filter(age == "15-24 years" & as.numeric(as.character(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.)) > 40000) %>%
  dplyr::group_by(country, age)

# Rearrange from highest to lowest
df_filter_high_gni <- df_filter_ages %>%
  dplyr::arrange(desc(as.numeric(as.character(df_filter_ages$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.))))

# Subset the first 10 (We select 20 as we have males and Females!)
top_ten_highest_gni_countries = df_filter_ages[1:10,]

# aggregate each sex
top_ten_highest_gni_countries <- aggregate(top_ten_highest_gni_countries['suicides_no'], by = top_ten_highest_gni_countries['country'], sum)

### Lowest GNI < 20000
# So we have our top ten countries which is great, now we get the lowest GNI countries...
top_ten_lowest_gni_countries <- data_filtered %>%
  dplyr::filter(age == "15-24 years" & as.numeric(as.character(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.)) < 15000) %>%
  dplyr::group_by(country, age)

# Arrange
top_ten_lowest_gni_countries <- top_ten_lowest_gni_countries %>%
  dplyr::arrange(desc(as.numeric(as.character(top_ten_lowest_gni_countries$GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.))))

# Subset the first 10 (We select 20 as we have males and Females!)
top_ten_lowest_gni_countries = top_ten_lowest_gni_countries[1:10,]

# Aggregate each sex

top_ten_lowest_gni_countries <- aggregate(top_ten_lowest_gni_countries['suicides_no'], by = top_ten_lowest_gni_countries['country'], sum)

# Got some help from a kaggle kernal with these visuals, hoever they have being changed as required
# ref -> scroll down to (3.3.2 Age Rates) -> https://www.kaggle.com/lmorgan95/r-suicide-rates-in-depth-stats-insights

ggplot(top_ten_highest_gni_countries, aes(x = country, y = suicides_no, fill = top_ten_highest_gni_countries$country)) + 
 geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "GNI Per Capita > 40,000$", 
       subtitle = "Young people only * (15 - 24 years old)", 
       x = "Country", 
       y = "Suicides", 
       fill = "Countries (Highest GNI Per Capita)")

ggplot(top_ten_lowest_gni_countries, aes(x = country, y = suicides_no, fill = top_ten_lowest_gni_countries$country)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "GNI Per Capita < 15,000$", 
       subtitle = "Young people only * (15 - 24 years old)", 
       x = "Country", 
       y = "Suicides", 
       fill = "Countries (Lowest GNI Per Capita)")

### So just by analysing the graphs, suicide among the top grossing gni are, generally lower, we should look into this a bit more

#### Insight 5 : Europe and GNI, is there a trend?

# Make a copy of master
gni_master_dataset_euro <- master_dataset

# Now lets replace "..." with na and remove :)
gni_master_dataset_euro[gni_master_dataset_euro == ".."] <- NA
gni_master_dataset_euro <- na.omit(gni_master_dataset_euro)

# Now, we filter by europe
gni_master_dataset_euro <- gni_master_dataset_euro %>%
  dplyr::filter(gni_master_dataset_euro$country == "Austria"|
                  gni_master_dataset_euro$country == "Italy" | 
                  gni_master_dataset_euro$country == "Belgium" | 
                  gni_master_dataset_euro$country == "Latvia" |
                  gni_master_dataset_euro$country == "Bulgaria" |
                  gni_master_dataset_euro$country == "Lithuania" |
                  gni_master_dataset_euro$country == "Luxembourg" | 
                  gni_master_dataset_euro$country == "Cyprus" | 
                  gni_master_dataset_euro$country == "Malta" | 
                  gni_master_dataset_euro$country == "Czechia" | 
                  gni_master_dataset_euro$country == "Netherlands" | 
                  gni_master_dataset_euro$country == "Denmark" | 
                  gni_master_dataset_euro$country == "Poland" | 
                  gni_master_dataset_euro$country == "Estonia" | 
                  gni_master_dataset_euro$country == "Portugal" | 
                  gni_master_dataset_euro$country == "Finland" | 
                  gni_master_dataset_euro$country == "Romania" | 
                  gni_master_dataset_euro$country == "France" | 
                  gni_master_dataset_euro$country == "Slovakia" | 
                  gni_master_dataset_euro$country == "Germany" | 
                  gni_master_dataset_euro$country == "Slovenia" | 
                  gni_master_dataset_euro$country == "Greece" | 
                  gni_master_dataset_euro$country == "Spain" | 
                  gni_master_dataset_euro$country == "Hungary" | 
                  gni_master_dataset_euro$country == "Sweden" | 
                  gni_master_dataset_euro$country == "Ireland" | 
                  gni_master_dataset_euro$country == "United Kingdom")

gni_master_dataset_euro <- gni_master_dataset_euro %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD. = first(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.), suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

  
# We have europe now with GNI for each country 
ggplot(gni_master_dataset_euro, aes(x = as.numeric(as.character(GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.)), y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 3)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 40000, 70000))

### Interesting, we can clearly see as gni or how rich a country becomes, the suicide rate dramatically decrease!

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 