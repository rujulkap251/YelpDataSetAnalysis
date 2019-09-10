

library("jsonlite")
library("ggplot2")
library("readr")
library(ggmosaic)

boxplot(df_bus_TO$stars);

dim(df_bus_TO)
df_bus_TO[is.na(a),]$stars 

a <- df_bus_TO[df_bus_TO$attributes$RestaurantsReservations == "TRUE",]$stars

df_Review<-subset(df_bus_TO, is_open==1 & !(df_bus_TO$neighborhood==""), select =c("stars", "neighborhood","is_open","review_count"))
df_review_reserve <- cbind(df_Review, df_bus_TO[df_bus_TO$is_open==1 & !(df_bus_TO$neighborhood==""),]$attributes$RestaurantsReservations)
boxplot(df_review_reserve$stars)
median(df_review_reserve$stars)
df_review_reserve$class <- ifelse(df_review_reserve$stars >= 3.5, "Good", "Bad")
colnames(df_review_reserve)[5] <- "RequireReservation"
df_review_reserve <- df_review_reserve[!is.na(df_review_reserve$RequireReservation),]


#ggplot(data = df_review_reserve) + geom_mosaic(aes(x = product(class), fill=RequireReservation), na.rm=TRUE) +labs(x="Class of Restaurant")
ggplot(df_review_reserve) + geom_boxplot(aes(class, review_count, fill = class)) #+ geom_jitter(aes(class, review_count, shape = df_review_reserve$class))


#Importing the file 
df_bus_TO <- stream_in(file("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/Yelp%20data/Business_Toronto_Restaurant.json"))


#####Understading factors most strongly associated with restaurants being closed?####

df_rest_closed <- df_bus_TO
dim(df_rest_closed)
#Unlisting and adding categories to the dataset
cat_total <- unlist(df_rest_closed$categories)

length(cat_total)

cat_total <- factor(cat_total)

nlevels(cat_total)

cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
#-->Taking the top 60 categories into the consideration
head(cat_names_sort, n = 60)

tail(cat_names_sort, n = 25)

cat_names <- names(cat_names_sort)[2:60] ## 1 is Restaurants - we don't need this

cat_bus_ind_mat <- sapply(df_rest_closed$categories, function(y) as.numeric(cat_names %in% y))
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
df_tidy_rest_cat <- cbind(df_rest_closed, cat_bus_ind_mat)

#Checking the attributes that have more than 50 % of na's
#-->Removing the name,address, business id, categories of the restaurant as it wont have any impact
#-->Removing categories of the restaurant as this variable has already been treated earlier
#-->Diving the hours into opening time and closing time
df_tidy_rest_cat[,"business_id"] = NULL;
df_tidy_rest_cat[,"name"] = NULL;
df_tidy_rest_cat[,"address"] = NULL;
df_tidy_rest_cat[,"categories"] = NULL;

#-->Spliting the week days into opening time and closing time
length(strsplit(as.character(df_tidy_rest_cat$hours$Friday),'-',fixed=TRUE))

df_fri <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Friday),'-',fixed=TRUE)))
df_tidy_rest_cat$Friday_OpeningTime <- df_fri$X1
df_tidy_rest_cat$Friday_ClosingTime <- df_fri$X2

df_mon <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Monday),'-',fixed=TRUE)))
df_tidy_rest_cat$Monday_OpeningTime <- df_mon$X1
df_tidy_rest_cat$Monday_ClosingTime <- df_mon$X2

df_tue <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Tuesday),'-',fixed=TRUE)))
df_tidy_rest_cat$Tuesday_OpeningTime <- df_tue$X1
df_tidy_rest_cat$Tuesday_ClosingTime <- df_tue$X2

df_wed <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Wednesday),'-',fixed=TRUE)))
df_tidy_rest_cat$Wednesday_OpeningTime <- df_wed$X1
df_tidy_rest_cat$Wednesday_ClosingTime <- df_wed$X2

df_thur <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Thursday),'-',fixed=TRUE)))
df_tidy_rest_cat$Thursday_OpeningTime <- df_thur$X1
df_tidy_rest_cat$Thursday_ClosingTime <- df_thur$X2

df_sat <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Saturday),'-',fixed=TRUE)))
df_tidy_rest_cat$Saturday_OpeningTime <- df_sat$X1
df_tidy_rest_cat$Saturday_ClosingTime <- df_sat$X2

df_sun <- data.frame(do.call('rbind', strsplit(as.character(df_tidy_rest_cat$hours$Sunday),'-',fixed=TRUE)))
df_tidy_rest_cat$Sunday_OpeningTime <- df_sun$X1
df_tidy_rest_cat$Sunday_ClosingTime <- df_sun$X2

#-->Deleting the hours attribute from the dataframe
df_tidy_rest_cat[,"hours"] = NULL;

ggplot(data = df_tidy_rest_cat) + geom_mosaic(aes(x = product(is_open), fill=attributes$BusinessAcceptsCreditCards), na.rm=TRUE) +labs(x="Is Restaurant Open?")

dim(df_tidy_rest_cat)
df_attributes <- df_tidy_rest_cat$attributes
df_tidy_rest_cat[,"attributes"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_attributes)


df_BusinessParking <- df_attributes$BusinessParking
df_tidy_rest_cat[,"BusinessParking"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_BusinessParking)

colnames(df_tidy_rest_cat)[116] <- "BusinessParking_Garage"
colnames(df_tidy_rest_cat)[117] <- "BusinessParking_Street"
colnames(df_tidy_rest_cat)[118] <- "BusinessParking_Validated"
colnames(df_tidy_rest_cat)[119] <- "BusinessParking_Lot"
colnames(df_tidy_rest_cat)[120] <- "BusinessParking_Valet"

df_HairSpecializesIn <- df_attributes$HairSpecializesIn
df_tidy_rest_cat[,"HairSpecializesIn"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_HairSpecializesIn)

colnames(df_tidy_rest_cat)[121] <- "HairSpecializesIn_Coloring"
colnames(df_tidy_rest_cat)[122] <- "HairSpecializesIn_Curly"
colnames(df_tidy_rest_cat)[123] <- "HairSpecializesIn_Perms"
colnames(df_tidy_rest_cat)[124] <- "HairSpecializesIn_Kids"
colnames(df_tidy_rest_cat)[125] <- "HairSpecializesIn_Extensions"

df_Music <- df_attributes$Music
df_tidy_rest_cat[,"Music"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_Music)

colnames(df_tidy_rest_cat)[125] <- "Music_Dj"
colnames(df_tidy_rest_cat)[126] <- "Music_Background_music"
colnames(df_tidy_rest_cat)[127] <- "Music_No_music"
colnames(df_tidy_rest_cat)[128] <- "Music_Karaoke"
colnames(df_tidy_rest_cat)[129] <- "Music_Live"
colnames(df_tidy_rest_cat)[130] <- "Music_Video"
colnames(df_tidy_rest_cat)[131] <- "Music_Jukebox"

df_Ambience <- df_attributes$Ambience
df_tidy_rest_cat[,"Ambience"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_Ambience)

colnames(df_tidy_rest_cat)[131] <- "Ambience_Romantic"
colnames(df_tidy_rest_cat)[132] <- "Ambience_Intimate"
colnames(df_tidy_rest_cat)[133] <- "Ambience_Classy"
colnames(df_tidy_rest_cat)[134] <- "Ambience_Hipster"
colnames(df_tidy_rest_cat)[135] <- "Ambience_Touristy"
colnames(df_tidy_rest_cat)[136] <- "Ambience_Trendy"
colnames(df_tidy_rest_cat)[137] <- "Ambience_Upscale"
colnames(df_tidy_rest_cat)[138] <- "Ambience_Casual"

df_BestNights <- df_attributes$BestNights
df_tidy_rest_cat[,"BestNights"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_BestNights)

colnames(df_tidy_rest_cat)[138] <- "BestNights_Monday"
colnames(df_tidy_rest_cat)[139] <- "BestNights_Tuesday"
colnames(df_tidy_rest_cat)[140] <- "BestNights_Friday"
colnames(df_tidy_rest_cat)[141] <- "BestNights_Wednesday"
colnames(df_tidy_rest_cat)[142] <- "BestNights_Thursday"
colnames(df_tidy_rest_cat)[143] <- "BestNights_Sunday"
colnames(df_tidy_rest_cat)[144] <- "BestNights_Saturday"

df_GoodForMeal <- df_attributes$GoodForMeal
df_tidy_rest_cat[,"GoodForMeal"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_GoodForMeal)

colnames(df_tidy_rest_cat)[144] <- "GoodForMeal_Dessert"
colnames(df_tidy_rest_cat)[145] <- "GoodForMeal_Latenight"
colnames(df_tidy_rest_cat)[146] <- "GoodForMeal_Lunch"
colnames(df_tidy_rest_cat)[147] <- "GoodForMeal_Dinner"
colnames(df_tidy_rest_cat)[148] <- "GoodForMeal_Breakfast"
colnames(df_tidy_rest_cat)[149] <- "GoodForMeal_Brunch"

df_DietaryRestrictions <- df_attributes$DietaryRestrictions
df_tidy_rest_cat[,"DietaryRestrictions"] = NULL;
df_tidy_rest_cat <- cbind(df_tidy_rest_cat, df_DietaryRestrictions)

colnames(df_tidy_rest_cat)[149] <- "DietaryRestrictions_Dairy-free"
colnames(df_tidy_rest_cat)[150] <- "DietaryRestrictions_Gluten-free"
colnames(df_tidy_rest_cat)[151] <- "DietaryRestrictions_Vegan"
colnames(df_tidy_rest_cat)[152] <- "DietaryRestrictions_Kosher"
colnames(df_tidy_rest_cat)[153] <- "DietaryRestrictions_Halal"
colnames(df_tidy_rest_cat)[154] <- "DietaryRestrictions_Soy-free"
colnames(df_tidy_rest_cat)[155] <- "DietaryRestrictions_Vegetarian"

dim(df_tidy_rest_cat)

#ifelse(sel_ck$neighborhood == "Scarborough", 1, 2)
#-->Checking for the null values and removing those  greater than 50%
sort(sapply(df_tidy_rest_cat, function(a) sum(is.na(a))),decreasing = TRUE)
dummy<- df_tidy_rest_cat  
cond <- sapply(dummy, function(a) sum(is.na(a))) < 3000
removed_na_df <- dummy[, cond, drop = FALSE]

#Remove states as all the restaurants belong to the state,city Ontario, Toronto
removed_na_df[,"state"] = NULL;
removed_na_df[,"city"] = NULL;
#Removing postal code as it seems to have lesser impact factor(df_bus_TO$postal_code). Since we have around 2500 postal codes w.r,t 7000 datapoints, we decide to remove this attribute
removed_na_df[,"postal_code"] = NULL;
#Removing food. This attribute comes by unlisting catgories and generally does not make sense to  be included as part of the data
removed_na_df[,"Food"] = NULL;

#Removing bars from the data as it has a very high correlation with Nightlife. This seems sensible as the restaurants with  nightlife tend to have bars.
cor.test(removed_na_df$Nightlife, removed_na_df$Bars, method=c("pearson", "kendall", "spearman"))
removed_na_df[,"Bars"] = NULL;

#Keeping only a few categories till Pubs ie. 26
cnames <-names(cat_names_sort[18:60])
removed_na_df[,cnames] = NULL;

names(removed_na_df)
dim(removed_na_df)

#Hypothesis: A restuarant is closed if it has either bad food/service/whatever it is 
ggplot(removed_na_df, aes(y=stars, x=factor(Nightlife), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Canadian (New)`), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Sandwiches), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Breakfast & Brunch`), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Italian), fill=factor(is_open))) +  geom_boxplot()
#----> There is a difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Chinese), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Cafes), fill=factor(is_open))) +  geom_boxplot()
#----> Significant difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Pizza), fill=factor(is_open))) +  geom_boxplot()
#----> Significant difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Coffee & Tea`), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Japanese), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Fast Food`), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Burgers), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Sushi Bars`), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`American (Traditional)`), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Indian), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Middle Eastern`), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$`Asian Fusion`), fill=factor(is_open))) +  geom_boxplot()
#----> Significant difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Thai), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Mediterranean), fill=factor(is_open))) +  geom_boxplot()
#----> Significant difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Mexican), fill=factor(is_open))) +  geom_boxplot()
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Korean), fill=factor(is_open))) +  geom_boxplot()
#----> Significant difference between the means
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Seafood), fill=factor(is_open))) +  geom_boxplot()
#----> There is some difference
ggplot(removed_na_df, aes(y=stars, x=factor(removed_na_df$Pubs), fill=factor(is_open))) +  geom_boxplot()

#Removing the following since no significant difference observed
cat_remov_list <- c("Nightlife", "Canadian (New)", "Sandwiches", "Breakfast & Brunch", "Italian", "Cafes", "Japanese", "Fast Food", "Middle Eastern", "Mediterranean", "Korean")
removed_na_df[,cat_remov_list] = NULL;

#This attribute not able to run seems to be insignificant after visualizing the boxplot and also has only 10% effect on the dataset
sum(removed_na_df$`Coffee & Tea`)
sum(removed_na_df$Pubs)
#Caters has 3053 missing values i.e half of the dataset. Hence, we should remove it.
sum(is.na(removed_na_df$Caters))
removed_na_df[,"categories"] = NULL;
removed_na_df[,9] = NULL;
removed_na_df[,"Pubs"] = NULL;
removed_na_df[,"Caters"] = NULL;

removed_na_df[,cat_remov_list] = NULL;
dim(removed_na_df)
removed_na_df$Sunday_OpeningTime
#Creating a heat map for weekdays and weekends
myweekdayenddata <- removed_na_df[, c("Monday_OpeningTime","Tuesday_OpeningTime","Wednesday_OpeningTime","Thursday_OpeningTime","Friday_OpeningTime","Saturday_OpeningTime","Sunday_OpeningTime")] 
myweekdayenddata$Monday_OpeningTime <- as.numeric(myweekdayenddata$Monday_OpeningTime)
myweekdayenddata$Tuesday_OpeningTime <- as.numeric(myweekdayenddata$Tuesday_OpeningTime)
myweekdayenddata$Wednesday_OpeningTime <- as.numeric(myweekdayenddata$Wednesday_OpeningTime)
myweekdayenddata$Thursday_OpeningTime <- as.numeric(myweekdayenddata$Thursday_OpeningTime)
myweekdayenddata$Friday_OpeningTime <- as.numeric(myweekdayenddata$Friday_OpeningTime)
myweekdayenddata$Saturday_OpeningTime <- as.numeric(myweekdayenddata$Saturday_OpeningTime)
myweekdayenddata$Sunday_OpeningTime <- as.numeric(myweekdayenddata$Sunday_OpeningTime)

corrweekday <- round(cor(myweekdayenddata,use = "complete.obs"),2)


library(reshape2)
melted_corrweekday <- melt(corrweekday)
head(melted_corrweekday)

ggplot(data = melted_corrweekday, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

removed_na_df$Wednesday_ClosingTime
myweekenddata <- removed_na_df[, c("Monday_ClosingTime","Tuesday_ClosingTime","Wednesday_ClosingTime","Thursday_ClosingTime","Friday_ClosingTime","Saturday_ClosingTime","Sunday_ClosingTime")] 
myweekenddata$Monday_ClosingTime <- as.numeric(myweekenddata$Monday_ClosingTime)
myweekenddata$Tuesday_ClosingTime <- as.numeric(myweekenddata$Tuesday_ClosingTime)
myweekenddata$Wednesday_ClosingTime <- as.numeric(myweekenddata$Wednesday_ClosingTime)
myweekenddata$Thursday_ClosingTime <- as.numeric(myweekenddata$Thursday_ClosingTime)
myweekenddata$Friday_ClosingTime <- as.numeric(myweekenddata$Friday_ClosingTime)
myweekenddata$Saturday_ClosingTime <- as.numeric(myweekenddata$Saturday_ClosingTime)
myweekenddata$Sunday_ClosingTime <- as.numeric(myweekenddata$Sunday_ClosingTime)

corrweekend <- round(cor(myweekenddata,use = "complete.obs"),2)


library(reshape2)
melted_corrweekend <- melt(corrweekend)
head(melted_corrweekend)

ggplot(data = melted_corrweekend, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Closing Days - From the heat map and correlation matrix, it can be oberserved that Mon, Tu, Wed, Thur and Sun tend to be have more high correlation . This can be because people tend to have work the next stay and refrain from staying up late unlike the weekends which correspond to the restaursants getting closed early. On the contrary, a high correlation between Fri and Sat showcases the contrary and demonstrated the restaurants tend to be open tilll late hours. Therefore taking Thur,Fri because it has less  na's
#Opening Days - Again from the heat map and correlation matrix, ot can be observed that Mon, Tue, Wed, Thurs, Fri have high correaltion depicting that these days have the same opening hours. While on the other hand Sat and Sun have the same opening hours. This is expected as the restaurants close very late on the weekends(Fri night and Sat night) and hence tend to open a bit late on the next day. Therefore taking Sat, Fri. Therefore taking Sat,Fri because it has less  na's

sum(is.na(removed_na_df$Monday_ClosingTime))
sum(is.na(removed_na_df$Tuesday_ClosingTime))
sum(is.na(removed_na_df$Wednesday_ClosingTime))
sum(is.na(removed_na_df$Thursday_ClosingTime))
sum(is.na(removed_na_df$Friday_ClosingTime))
sum(is.na(removed_na_df$Saturday_ClosingTime))
sum(is.na(removed_na_df$Sunday_ClosingTime))

sum(is.na(removed_na_df$Monday_OpeningTime))
sum(is.na(removed_na_df$Tuesday_OpeningTime))
sum(is.na(removed_na_df$Wednesday_OpeningTime))
sum(is.na(removed_na_df$Thursday_OpeningTime))
sum(is.na(removed_na_df$Friday_OpeningTime))
sum(is.na(removed_na_df$Saturday_OpeningTime))
sum(is.na(removed_na_df$Sunday_OpeningTime))

rm_opentime <- c("Monday_OpeningTime","Tuesday_OpeningTime","Wednesday_OpeningTime","Thursday_OpeningTime","Sunday_OpeningTime")
rm_closetime <- c("Monday_ClosingTime","Tuesday_ClosingTime","Wednesday_ClosingTime","Saturday_ClosingTime","Sunday_ClosingTime")

removed_na_df[,rm_opentime] = NULL;
removed_na_df[,rm_closetime] = NULL;

#Does not make sense to have HairSpecializeIn_Colouring as this is the only attribute related to hair styling. Hence we remove it.
removed_na_df[,"HairSpecializesIn_Coloring"] = NULL;


ggplot(data=subset(removed_na_df, !is.na(BusinessAcceptsCreditCards))) +  geom_boxplot(aes(y=stars, x=BusinessAcceptsCreditCards, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(RestaurantsPriceRange2))) +  geom_boxplot(aes(y=stars, x=RestaurantsPriceRange2, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(GoodForKids))) +  geom_boxplot(aes(y=stars, x=GoodForKids, fill=factor(is_open)), na.rm = TRUE)
#Some difference for beer and wine
ggplot(data=subset(removed_na_df, !is.na(Alcohol))) +  geom_boxplot(aes(y=stars, x=Alcohol, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(HasTV))) +  geom_boxplot(aes(y=stars, x=HasTV, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(NoiseLevel))) +  geom_boxplot(aes(y=stars, x=NoiseLevel, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(RestaurantsAttire))) +  geom_boxplot(aes(y=stars, x=RestaurantsAttire, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(RestaurantsGoodForGroups))) +  geom_boxplot(aes(y=stars, x=RestaurantsGoodForGroups, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(WiFi))) +  geom_boxplot(aes(y=stars, x=WiFi, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(RestaurantsReservations))) +  geom_boxplot(aes(y=stars, x=RestaurantsReservations, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(RestaurantsTakeOut))) +  geom_boxplot(aes(y=stars, x=RestaurantsTakeOut, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(RestaurantsTableService))) +  geom_boxplot(aes(y=stars, x=RestaurantsTableService, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(OutdoorSeating))) +  geom_boxplot(aes(y=stars, x=OutdoorSeating, fill=factor(is_open)), na.rm = TRUE)
#There is significant difference
ggplot(data=subset(removed_na_df, !is.na(RestaurantsDelivery))) +  geom_boxplot(aes(y=stars, x=RestaurantsDelivery, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(BikeParking))) +  geom_boxplot(aes(y=stars, x=BikeParking, fill=factor(is_open)), na.rm = TRUE)
#There is some difference but does not seem important
ggplot(data=subset(removed_na_df, !is.na(BusinessParking_Validated))) +  geom_boxplot(aes(y=stars, x=BusinessParking_Validated, fill=factor(is_open)), na.rm = TRUE)
#There is some difference but does not seem to make an impact
ggplot(data=subset(removed_na_df, !is.na(BusinessParking_Valet))) +  geom_boxplot(aes(y=stars, x=BusinessParking_Lot, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(lot))) +  geom_boxplot(aes(y=stars, x=lot, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(Ambience_Intimate))) +  geom_boxplot(aes(y=stars, x=Ambience_Intimate, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(Ambience_Classy))) +  geom_boxplot(aes(y=stars, x=Ambience_Classy, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(Ambience_Hipster))) +  geom_boxplot(aes(y=stars, x=Ambience_Hipster, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(Ambience_Touristy))) +  geom_boxplot(aes(y=stars, x=Ambience_Touristy, fill=factor(is_open)), na.rm = TRUE)
#There is significant difference
ggplot(data=subset(removed_na_df, !is.na(Ambience_Trendy))) +  geom_boxplot(aes(y=stars, x=Ambience_Trendy, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(Ambience_Upscale))) +  geom_boxplot(aes(y=stars, x=Ambience_Upscale, fill=factor(is_open)), na.rm = TRUE)
#There is significant difference
ggplot(data=subset(removed_na_df, !is.na(Ambience_Casual))) +  geom_boxplot(aes(y=stars, x=Ambience_Casual, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(BestNights_Monday))) +  geom_boxplot(aes(y=stars, x=BestNights_Monday, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(GoodForMeal_Latenight))) +  geom_boxplot(aes(y=stars, x=GoodForMeal_Latenight, fill=factor(is_open)), na.rm = TRUE)
#There is some difference and does not seem important
ggplot(data=subset(removed_na_df, !is.na(GoodForMeal_Lunch))) +  geom_boxplot(aes(y=stars, x=GoodForMeal_Lunch, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(GoodForMeal_Dinner))) +  geom_boxplot(aes(y=stars, x=GoodForMeal_Dinner, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(GoodForMeal_Breakfast))) +  geom_boxplot(aes(y=stars, x=GoodForMeal_Breakfast, fill=factor(is_open)), na.rm = TRUE)
ggplot(data=subset(removed_na_df, !is.na(GoodForMeal_Brunch))) +  geom_boxplot(aes(y=stars, x=GoodForMeal_Brunch, fill=factor(is_open)), na.rm = TRUE)
#There is some difference
ggplot(data=subset(removed_na_df, !is.na(removed_na_df$`DietaryRestrictions_Dairy-free`))) +  geom_boxplot(aes(y=stars, x=`DietaryRestrictions_Dairy-free`, fill=factor(is_open)), na.rm = TRUE)

vic <- c("GoodForMeal_Lunch","BusinessParking_Valet","BusinessParking_Validated","GoodForMeal_Dinner","GoodForMeal_Latenight","BikeParking","BestNights_Monday","GoodForMeal_Brunch","Ambience_Classy","Ambience_Hipster","lot","OutdoorSeating","RestaurantsTableService","RestaurantsReservations","RestaurantsGoodForGroups","NoiseLevel","HasTV","GoodForKids")
removed_na_df[,vic] = NULL;

#GoodForMeal_Breakfast
#RestaurantsPriceRange2
removed_na_df$GoodForMeal_Breakfast <- df_tidy_rest_cat$GoodForMeal_Breakfast
removed_na_df$RestaurantsPriceRange2 <- df_tidy_rest_cat$RestaurantsPriceRange2

sort(sapply(removed_na_df, function(a) sum(is.na(a))),decreasing = TRUE)
removed_na_df_corr <- round(cor(removed_na_df,use = "complete.obs"),2)

removed_na_df[,"DietaryRestrictions_Dairy-free"] = NULL;

removed_na_df$BusinessParking_Lot= df_bus_TO$attributes$BusinessParking$lot
removed_na_df$Ambience_Intimate= df_bus_TO$attributes$Ambience$intimate
removed_na_df$Ambience_Casual= df_bus_TO$attributes$Ambience$casual
removed_na_df$Ambience_Touristy= df_bus_TO$attributes$Ambience$touristy
removed_na_df$Ambience_Trendy= df_bus_TO$attributes$Ambience$trendy
removed_na_df$Ambience_Upscale= df_bus_TO$attributes$Ambience$upscale
removed_na_df$GoodForMeal_Breakfast= df_bus_TO$attributes$GoodForMeal$breakfast

#Visualized the boxplot for Wifi and oberserved that there was no variaiton for "free" and "no" but there has been a difference for "paid" values. On checking the count of these values, we could observe that a limited value of 34 was observed for this category, in contrast to the other categories that had  2889, 2132 values
removed_na_df$WiFi= df_bus_TO$attributes$WiFi
ggplot(data=subset(removed_na_df, !is.na(WiFi))) +  geom_boxplot(aes(y=stars, x=WiFi, fill=factor(is_open)), na.rm = TRUE)
removed_na_df[,"WiFi"] = NULL;

#When oberserving the Ambience_Casual against Ambience_Intimate, Ambience_Touristy, Ambience_Romantic, Ambience_Upscale, Ambience_Trendy it was observed that these attributes had a very lower count of true values compared to the abience_casual
removed_na_df[,"Ambience_Intimate"] = NULL;
removed_na_df[,"Ambience_Touristy"] = NULL;
removed_na_df[,"Ambience_Upscale"] = NULL;
removed_na_df[,"Ambience_Trendy"] = NULL;

ggplot(data=subset(removed_na_df, !is.na(RestaurantsAttire))) +  geom_boxplot(aes(y=stars, x=RestaurantsAttire, fill=factor(is_open)), na.rm = TRUE)
#For the attribute RestaurantAttire, the boxplots showed difference in the distribution of open and closed restaurant with respect to the "dressy" and "formal" categories. However, the count of these value with repect to "casual" category is very small (13, 246 respectively), and hence have a minimal contribution in derterming that a rest is closed. Hence, as seen from the boxplot, since casual does not have an impact, we remove the attribute
removed_na_df[,"RestaurantsAttire"] = NULL;

#Observing the boxplots, shows that there is no significant difference between the distributions w.r.t. open and close rest
ggplot(data=subset(removed_na_df, !is.na(BusinessParking_Lot))) +  geom_boxplot(aes(y=stars, x=BusinessParking_Lot, fill=factor(is_open)), na.rm = TRUE)
removed_na_df[,"GoodForMeal_Breakfast"] = NULL;
removed_na_df[,"BusinessParking_Lot"] = NULL;

#Since Pizza and Burgers belong to the fastfood category, we will remove one of them and keep pizza
#Ran chi-square test for Indian, Seafood, Sushi Bars, Mexican Asian Fusion, and Thai and found that they are independent of the response variable. Hence, dropping these down
removed_na_df[,"Indian"] = NULL;
removed_na_df[,"Burgers"] = NULL;
removed_na_df[,"Thai"] = NULL;
removed_na_df[,"Asian Fusion"] = NULL;

removed_na_df[,"Seafood"] = NULL;
removed_na_df[,"Sushi Bars"] = NULL;
removed_na_df[,"Mexican"] = NULL;

#Again Ran chi-square test for Restaurant Delivery and found that they are independent of the response variable. Hence, dropping these down
removed_na_df[,"RestaurantsDelivery"] = NULL;


tbl = matrix(data=c(3190, 327, 1682, 138), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('Open', 'Close'), Gender=c('Business Accept Credit Cards',
                                                       'not Business Accept Credit Cards'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

tbl = matrix(data=c(4231, 328, 1677, 188), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('Op1', 'C0'), Gender=c('RTO1', 'RTO0'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

tbl = matrix(data=c(1377, 2867, 512, 1155), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('Open', 'Close'), Gender=c('Restaurant Delivery', 'Restaurant Delivery'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)
dim(removed_na_df)

sort(sapply(removed_na_df, function(a) sum(is.na(a))),decreasing = TRUE)


#Imputation for  Friday_Opening using KNN. Potting the barplot shows us the major rest opening timings on fri. We find the majority of the restaurants open at 8 different opening times, hence we consider  k=8 8
library(VIM)
df<- summary(df_tidy_rest_cat$Friday_OpeningTime)[-45]
df<- summary(removed_na_df$Friday_OpeningTime)[-45]

barplot((df))
removed_na_df[,"BikeParking"]<- NULL
sort(sapply(removed_na_df, function(a) sum(is.na(a))),decreasing = TRUE)[16:35]
as.factor(df_tidy_rest_cat$romantic)


impute1 <- kNN(df_tidy_rest_cat, variable="WiFi",k=2)
removed_na_df$WiFi = impute1$WiFi
impute1 <- kNN(df_tidy_rest_cat, variable="BusinessAcceptsCreditCards",k=2)
removed_na_df$BusinessAcceptsCreditCards = impute1$BusinessAcceptsCreditCards
impute1 <- kNN(df_tidy_rest_cat, variable="romantic",k=2)
removed_na_df$romantic = impute1$romantic

c = c("NoiseLevel", "Alcohol", "RestaurantsTableService", "intimate", "classy","hipster", "touristy","trendy","upscale", "casual","HasTV", "garage","street","validated" ,"lot","valet","dessert","latenight" ,"lunch","dinner","breakfast" ,"brunch"    ,  "RestaurantsDelivery",   "OutdoorSeating" ,"RestaurantsAttire",  "RestaurantsReservations",  "GoodForKids" ,"RestaurantsGoodForGroups",       "RestaurantsTakeOut" ,  "RestaurantsPriceRange2" )
as.factor(df_tidy_rest_cat$NoiseLevel)

sum.of.squares <- function(v,a) {
  impute1 <- kNN(df_tidy_rest_cat, variable=v,k=a)
  removed_na_df$v = impute1$v
  
}

as.fac <- function(v) {
  as.numeric(as.factor(v))
  
}
z = c(3,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 ,2, 2,2,3, 2, 2,2,2,4)
las.factor(removed_na_df$RestaurantsPriceRange2)
(as.factor(removed_na_df[,c("WiFi","BusinessAcceptsCreditCards","romantic", "NoiseLevel", "Alcohol", "RestaurantsTableService", "intimate", "classy","hipster", "touristy","trendy","upscale", "casual","HasTV", "garage","street","validated" ,"lot","valet","dessert","latenight" ,"lunch","dinner","breakfast" ,"brunch"    ,  "RestaurantsDelivery",   "OutdoorSeating" ,"RestaurantsAttire",  "RestaurantsReservations",  "GoodForKids" ,"RestaurantsGoodForGroups","RestaurantsTakeOut" ,"RestaurantsPriceRange2" )]))

sum.of.squares(c("WiFi","BusinessAcceptsCreditCards","romantic", "NoiseLevel", "Alcohol", "RestaurantsTableService", "intimate", "classy","hipster", "touristy","trendy","upscale", "casual","HasTV", "garage","street","validated" ,"lot","valet","dessert","latenight" ,"lunch","dinner","breakfast" ,"brunch"    ,  "RestaurantsDelivery",   "OutdoorSeating" ,"RestaurantsAttire",  "RestaurantsReservations",  "GoodForKids" ,"RestaurantsGoodForGroups","RestaurantsTakeOut" ,"RestaurantsPriceRange2" ), c(3,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 ,2, 2,2,3, 2, 2,2,2,4)) 
b =c("WiFi","BusinessAcceptsCreditCards","romantic", "NoiseLevel", "Alcohol", "RestaurantsTableService", "intimate", "classy","hipster", "touristy","trendy","upscale", "casual","HasTV", "garage","street","validated" ,"lot","valet","dessert","latenight" ,"lunch","dinner","breakfast" ,"brunch"    ,  "RestaurantsDelivery",   "OutdoorSeating" ,"RestaurantsAttire",  "RestaurantsReservations",  "GoodForKids" ,"RestaurantsGoodForGroups","RestaurantsTakeOut" ,"RestaurantsPriceRange2" )
for(i in 1:33){
  sum.of.squares(b[i], z[i])
}
sum.of.squares("WiFi", 3)
a = c("WiFi","BusinessAcceptsCreditCards","romantic", "NoiseLevel", "Alcohol", "RestaurantsTableService", "intimate", "classy","hipster", "touristy","trendy","upscale", "casual","HasTV", "garage","street","validated" ,"lot","valet","dessert","latenight" ,"lunch","dinner","breakfast" ,"brunch"    ,  "RestaurantsDelivery",   "OutdoorSeating" ,"RestaurantsAttire",  "RestaurantsReservations",  "GoodForKids" ,"RestaurantsGoodForGroups","RestaurantsTakeOut" ,"RestaurantsPriceRange2" )
impute1 <- kNN(df_tidy_rest_cat, variable="NoiseLevel",k=3)
removed_na_df$NoiseLevel = impute1$NoiseLevel

impute1 <- kNN(df_tidy_rest_cat, variable="Alcohol",k=2)
removed_na_df$BusinessAcceptsCreditCards = impute1$BusinessAcceptsCreditCards
impute1 <- kNN(df_tidy_rest_cat, variable="BusinessAcceptsCreditCards",k=2)
removed_na_df$WiFi = impute1$BusinessAcceptsCreditCards
head(df_tidy_rest_cat$BusinessAcceptsCreditCards, 12)
head(impute1$BusinessAcceptsCreditCards, 12)
barplot(df_tidy_rest_cat$BusinessAcceptsCreditCards)

#install.packages("VIM")
impute1 <- kNN(df_tidy_rest_cat, variable="Friday_OpeningTime",k=8)
removed_na_df$Friday_OpeningTime = impute1$Friday_OpeningTime

#Imputation for  Friday_ClosingTime using KNN. Potting the barplot shows us the major rest opening timings on fri. We find the majority of the restaurants open at 12 different opening times, hence we consider  k=12
library(VIM)
df<- summary(removed_na_df$Friday_ClosingTime)[-51]
barplot((df))

#install.packages("VIM")
impute1 <- kNN(df_tidy_rest_cat, variable="Friday_ClosingTime",k=12)
removed_na_df$Friday_ClosingTime = impute1$Friday_ClosingTime

#Imputation for  Thursday_ClosingTime using KNN. Potting the barplot shows us the major rest opening timings on fri. We find the majority of the restaurants close at 10 different closing times, hence we consider  k=10
library(VIM)
df<- summary(removed_na_df$Thursday_ClosingTime)[-51]
barplot((df))

#install.packages("VIM")
impute1 <- kNN(df_tidy_rest_cat, variable="Thursday_ClosingTime",k=10)
removed_na_df$Thursday_ClosingTime = impute1$Thursday_ClosingTime

#Imputation for  Saturday_OpeningTime using KNN. Potting the barplot shows us the major rest opening timings on fri. We find the majority of the restaurants open at 8 different opening times, hence we consider  k=8
library(VIM)
df<- summary(removed_na_df$Saturday_OpeningTime)[-47]
barplot((df))

#install.packages("VIM")
impute1 <- kNN(df_tidy_rest_cat, variable="Saturday_OpeningTime",k=8)
removed_na_df$Saturday_OpeningTime = impute1$Saturday_OpeningTime
#head(removed_na_df$Saturday_OpeningTime,10)
#head(impute1$Saturday_OpeningTime, 10)

#Imputation for Alcohol using KNN. Taking k=3 for the 3 classes - fullbar, beer and wine, and none(no alcohol)

sort(sapply(removed_na_df, function(a) sum(is.na(a))),decreasing = TRUE)[1]
as.factor(df_tidy_rest_cat$trendy)

impute1 <- kNN(df_tidy_rest_cat, variable="trendy",k=2)
removed_na_df$trendy  = impute1$trendy

as.factor(removed_na_df$Alcohol)


#Imputation for RestaurantsPriceRange2 using KNN. Taking k=4 for the 4 classes - 1,2,3,4

impute1 <- kNN(df_tidy_rest_cat, variable="RestaurantsPriceRange2",k=4)
removed_na_df$RestaurantsPriceRange2 = impute1$RestaurantsPriceRange2

#Imputation for RestaurantsPriceRange2 using KNN. Taking k=4 for the 4 classes - 1,2,3,4

impute1 <- kNN(df_tidy_rest_cat, variable="RestaurantsPriceRange2",k=4)
removed_na_df$RestaurantsPriceRange2 = impute1$RestaurantsPriceRange2

#Imputation for RestaurantsTakeOut using KNN. Taking k=2 for the 2 classes - TRUE,FALSE

impute1 <- kNN(df_tidy_rest_cat, variable="RestaurantsTakeOut",k=2)
removed_na_df$RestaurantsTakeOut = impute1$RestaurantsTakeOut

#Imputation for BusinessAcceptsCreditCards using KNN. Taking k=2 for the 2 classes - TRUE,FALSE

impute1 <- kNN(df_tidy_rest_cat, variable="BusinessAcceptsCreditCards",k=2)
removed_na_df$BusinessAcceptsCreditCards = impute1$BusinessAcceptsCreditCards


sort(sapply(removed_na_df, function(a) sum(is.na(a))),decreasing = TRUE)

#Keeping the null values of Ambience_Casual as missing
removed_na_df$Ambience_Casual = df_tidy_rest_cat$casual
removed_na_df$Ambience_Casual[(is.na(removed_na_df$Ambience_Casual))] = "missing"
