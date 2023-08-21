install.packages("corrplot")
install.packages("readr")
library(readr)
library(stringr)
library(FactoMineR)
library("factoextra")
library(ggplot2)
library(jsonlite)
library(corrplot)
library(dplyr)

#setm working directory and load the data
setwd("D:/Descargas/Universitat/2.2/PMAAD")
datos<-read_csv("D:/Descargas/Universitat/2.2/PMAAD/BasedeDades/RawDatabase.csv",locale = locale(encoding = "UTF-8"))

#Set the seed for the script
set.seed(42277)

#Make the sample of the data
dd <- datos[sample(nrow(datos), 5000), ]
datos_nombres <- dd[,c("host_name")]

#Write a file with only the names to get the gender with the model made by: https://gender-guesser.com
write.csv(datos_nombres,file="names.csv")


###############
# Gender Data #
###############

#To add gender data to our database we thought about extracting the gender from the names

datos_names<-read_csv("D:/Descargas/Universitat/2.2/PMAAD/namsor_gender_names.csv",locale = locale(encoding = "UTF-8"))
datos_names <- datos_names[-1,]
datos_names <- datos_names[, c(1,2,8,11)]
datos_names <- rbind(datos_names, c(5000,"Patricia","female",1))

#We will have to find which names are from companies, we create a table with the uniques to search better.
nombres <- data.frame(unique(datos_names$firstName...2))
#We change the gender to company for those which we filter.
datos_names$likelyGender[str_detect(datos_names$firstName...2,"Hotel") | str_detect(datos_names$firstName...2,"Apart") | str_detect(datos_names$firstName...2,"Catalonia") | str_detect(datos_names$firstName...2,"arcelona") | str_detect(datos_names$firstName...2,"Hostel") | str_detect(datos_names$firstName...2,"Bcn")  | str_detect(datos_names$firstName...2,"Home")  | str_detect(datos_names$firstName...2,"Cosmopolitan") | str_detect(datos_names$firstName...2,"Amra") | str_detect(datos_names$firstName...2,"B&F") | str_detect(datos_names$firstName...2,"flat") | str_detect(datos_names$firstName...2,"Palace")| str_detect(datos_names$firstName...2,"Team")| str_detect(datos_names$firstName...2,"share") | str_detect(datos_names$firstName...2,"Ukio")| str_detect(datos_names$firstName...2,"Sweet") | str_detect(datos_names$firstName...2,"My ")| str_detect(datos_names$firstName...2,"Exceptional")| str_detect(datos_names$firstName...2,"Pla")| str_detect(datos_names$firstName...2,"Casa")| str_detect(datos_names$firstName...2,"Midtown")| str_detect(datos_names$firstName...2,"Lodging")| str_detect(datos_names$firstName...2,"Bed")| str_detect(datos_names$firstName...2,"Rent")| str_detect(datos_names$firstName...2,"Barna")| str_detect(datos_names$firstName...2,"Guest")| str_detect(datos_names$firstName...2,"Key")| str_detect(datos_names$firstName...2,"Suites")| str_detect(datos_names$firstName...2,"Option")| str_detect(datos_names$firstName...2,"port")| str_detect(datos_names$firstName...2,"room")| str_detect(datos_names$firstName...2,"Poble")| str_detect(datos_names$firstName...2,"Ville")| str_detect(datos_names$firstName...2,"Hou")| str_detect(datos_names$firstName...2,"S.L")| str_detect(datos_names$firstName...2,"Book")| str_detect(datos_names$firstName...2,"Hou")| str_detect(datos_names$firstName...2,"Rambla")| str_detect(datos_names$firstName...2,"Diagonal")| str_detect(datos_names$firstName...2,"Alguera")| str_detect(datos_names$firstName...2,"Medite")| str_detect(datos_names$firstName...2,"time")| str_detect(datos_names$firstName...2,"The ")| str_detect(datos_names$firstName...2,"Triomf")| str_detect(datos_names$firstName...2," sl")| str_detect(datos_names$firstName...2,"De ")| str_detect(datos_names$firstName...2,"Urban")| str_detect(datos_names$firstName...2,"Nou")| str_detect(datos_names$firstName...2,"acco")| str_detect(datos_names$firstName...2,"Mr ")| str_detect(datos_names$firstName...2,"Numa")| str_detect(datos_names$firstName...2,"coli")| str_detect(datos_names$firstName...2,"pensi")| str_detect(datos_names$firstName...2,"Croma")| str_detect(datos_names$firstName...2,"B-F")| str_detect(datos_names$firstName...2,"Unique")| str_detect(datos_names$firstName...2,"capi")| str_detect(datos_names$firstName...2,"Close")| str_detect(datos_names$firstName...2,"\\d+")| str_detect(datos_names$firstName...2,"Conjunts")| str_detect(datos_names$firstName...2,"Stay")| str_detect(datos_names$firstName...2,"Prak")| str_detect(datos_names$firstName...2,"Inv")| str_detect(datos_names$firstName...2,"Blue")] <- "company"

nrow(datos_names[datos_names$likelyGender == "company", ])
datos_names$likelyGender[datos_names$probabilityCalibrated <= 0.8 & datos_names$likelyGender != "company"] <- "Unisex"

#Check the results
ggplot(datos_names, aes(x = factor(likelyGender))) + 
  geom_bar(fill = "blue") + 
  xlab("Room Type") +
  ylab("Number of properties") +
  ggtitle("Number of properties by room type")

#Add the gender column to the Database
dd <- cbind(dd,datos_names$likelyGender)
dd <- rename(dd, gender = "datos_names$likelyGender")
############################
# Dimensionality Reduction #
############################

#Manual dimensionality reduction to eliminate those variables that are clearly not important.
first_erase<-c("id","listing_url","scrape_id","last_scraped","name","picture_url","host_id","host_url","host_thumbnail_url","host_picture_url","calendar_updated","calendar_last_scraped","license","host_listings_count","host_total_listings_count","bathrooms","host_name")
dd[,first_erase]<-NULL

#Convert some wrong character variables to int.

dd$host_response_rate <- as.numeric(sub("%", "", dd$host_response_rate))
dd$host_acceptance_rate <- as.numeric(sub("%", "", dd$host_acceptance_rate))
dd$price <- as.numeric(sub("\\$", "", dd$price))
dd$bathrooms_text <- as.numeric(gsub("[^0-9.]+", "", dd$bathrooms_text))

#Treatment of the feature "amenities":
my_list <- lapply(dd$amenities, fromJSON)
dd$amenities_count <- sapply(my_list, length)
dd[,"amenities"]<-NULL

#----------------------------------------#
# Feature Selection: Numerical Variables #
#----------------------------------------#

#Try with correaltion matrix
df_numeric <- dd[sapply(dd, is.numeric)]
corr_matrix <- cor(df_numeric,use="complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.7,addCoef.col = "black",number.cex = 0.5)

#Check if we could do feature selection with PCA too:
pca_data <- na.omit(df_numeric)

pc1 <- prcomp(pca_data, scale=TRUE)

inerProj<- pc1$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner

#Check at the explained variance in the first two Dimensions
par(mfrow = c(1, 2))
barplot(100*cumsum(pc1$sdev[1:dim(pca_data)[2]]^2)/dim(pca_data)[2])
fviz_eig(pc1, addlabels = TRUE, ylim = c(0, 50))
percInerAccum<-100*cumsum(pc1$sdev[1:dim(pca_data)[2]]^2)/dim(pca_data)[2]
pander::pander(percInerAccum)
#As the explained variance is very low, we can say that the feature selection will be better with the correlation matrix.


#After looking at the corr_matrix, we se that some variables are very related more than 0.7

#bed, accommodates and bedrooms. This group is very correlated because as more accommodates you need more beds and bedrooms.
bed_erase <- c("bedrooms","beds")
dd[,bed_erase]<-NULL

#1 minimum nights group: Those variables are very correlated between them (0,83 or more). We erase all except minimum_nights_avg_ntm.

min_erase <- c("minimum_nights","minimum_minimum_nights","maximum_minimum_nights")
dd[,min_erase]<-NULL

#2 maximum nights group: Those variables are very correlated between them (0,83 or more). We erase all except minimum_nights_avg_ntm.
#mirar outliers
boxplot(df_numeric$maximum_nights)
boxplot(df_numeric$minimum_maximum_nights)
boxplot(df_numeric$maximum_maximum_nights)
boxplot(df_numeric$maximum_nights_avg_ntm)

df_numeric$minimum_maximum_nights[df_numeric$minimum_maximum_nights >= 9999] <- NA
df_numeric$maximum_maximum_nights[df_numeric$maximum_maximum_nights >= 9999] <- NA
#as this variable comes from the others, the 9999 outlier modified the avg, making it high but not 9999.
df_numeric$maximum_nights_avg_ntm[df_numeric$maximum_nights_avg_ntm >= 6000] <- NA

boxplot(df_numeric$minimum_maximum_nights)
boxplot(df_numeric$maximum_maximum_nights)
boxplot(df_numeric$maximum_nights_avg_ntm)
#We plot the correaltion matrix of this group of features to see that they are actually correalted
corr_matrix <- cor(df_numeric[, c("maximum_nights","minimum_maximum_nights","maximum_maximum_nights","maximum_nights_avg_ntm")],use="complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.7,addCoef.col = "black",number.cex = 0.8)

max_erase <- c("maximum_nights","minimum_maximum_nights","maximum_maximum_nights")
dd[,max_erase]<-NULL

#3 availability group: This group has a a high correlation too. The ones with less days of difference are more correlated between them. Explain...

ava_erase <- c("availability_60","availability_30","availability_90")
dd[,ava_erase]<-NULL

#4 number_of_reviews group:

num_erase <- c("number_of_reviews","number_of_reviews_ltm","number_of_reviews_l30d")
dd[,num_erase]<-NULL

#5 review_scores group: all of them have high correlation with review_scores_rating.

rev_erase <- c("review_scores_accuracy","review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_location","review_scores_value")
dd[,rev_erase]<-NULL

#6 host_listings_count: High corr with entire_homes. That's because the big majority of properties are of this type. Let's check it:

ggplot(dd, aes(x = factor(room_type))) + 
  geom_bar(fill = "blue") + 
  xlab("Room Type") +
  ylab("Number of properties") +
  ggtitle("Number of properties by room type")
#We see that there are a lot of 
max(dd$calculated_host_listings_count_entire_homes)
max(dd$calculated_host_listings_count_private_rooms)
max(dd$calculated_host_listings_count)
mean(dd$calculated_host_listings_count)
mean(dd$calculated_host_listings_count_entire_homes)
mean(dd$calculated_host_listings_count_private_rooms)
#As we can see the mean of the private_rooms is way lower than entire homes, so it does not have so much impact in the total_calculation.
#We proceed to erase all except the total calculation because it brings us the information of the others.

host_erase <- c("calculated_host_listings_count_entire_homes","calculated_host_listings_count_private_rooms","calculated_host_listings_count_shared_rooms","host_has_profile_pic")
dd[,host_erase]<-NULL

#We plot the correaltion matrix again without those features.
df_numeric <- dd[sapply(dd, is.numeric)]
corr_matrix <- cor(df_numeric,use="complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.9,addCoef.col = "black",number.cex = 0.7)

#----------------------------#
# Feature selection with MCA #
#----------------------------#

#Group host_location modalities
dd$host_location[str_detect(dd$host_location,"Spain") == FALSE] <- "out_sp"
dd$host_location[str_detect(dd$host_location,"Granada") | str_detect(dd$host_location,"Madrid") | str_detect(dd$host_location,"Málaga") | str_detect(dd$host_location,"Balearic Islands") | str_detect(dd$host_location,"Ortigueira") | str_detect(dd$host_location,"Andalusia") | str_detect(dd$host_location,"San Fernando") | str_detect(dd$host_location,"Manuel") | str_detect(dd$host_location,"Valencia") | str_detect(dd$host_location,"Vielha") | str_detect(dd$host_location,"Seville") | str_detect(dd$host_location,"Ibiza") | str_detect(dd$host_location,"Vigo") | str_detect(dd$host_location,"Palma de Mallorca") | str_detect(dd$host_location,"Bilbao") | str_detect(dd$host_location,"Alicante") | str_detect(dd$host_location,"Majadahonda") | str_detect(dd$host_location,"Vitoria-Gasteiz") | str_detect(dd$host_location,"Albacete") | str_detect(dd$host_location,"Canary Isalnds") | str_detect(dd$host_location,"Maspalomas") | str_detect(dd$host_location,"Palma") | str_detect(dd$host_location,"Paiporta") | str_detect(dd$host_location,"Hb004593")] <- "out_cat_in_sp"
dd$host_location[str_detect(dd$host_location,"out_cat_in_sp") == FALSE & str_detect(dd$host_location,"out_sp") == FALSE] <- "in_cat"
unique(dd$host_location)

df_categoric <- dd[sapply(dd, function(x) is.character(x) | is.logical(x))]

#Erase the text variables from the categorical variables
df_categoric[,c("description","host_about","neighborhood_overview")] <- NULL

#Make and visualize the results of the MCA
res.mca <- MCA(df_categoric)

#Look at the frquency of host_location to help with the analysis. 
ggplot(dd, aes(x = factor(host_location))) + 
  geom_bar(fill = "blue") + 
  xlab("Room Type") +
  ylab("Number of properties") +
  ggtitle("Number of properties by room type")

#Erase of the categorical features after the MCA analysis.
categoric_erase <- c("neighbourhood","neighbourhood_cleansed","property_type","has_availability","host_neighbourhood")
dd[,categoric_erase]<-NULL

#####################
# Rename Modalities #
#####################

#neighbourhood_cleaned_text

unique(dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Sant Martí", "St.Marti", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Sants-Montjuïc", "Sants", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Sant Andreu", "St.Andreu", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Sarrià-Sant Gervasi", "Sarria", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Horta-Guinardó", "Horta", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Gràcia", "Gracia", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Ciutat Vella", "Ct.Vella", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Nou Barris", "N.Barris", dd$neighbourhood_group_cleansed)
dd$neighbourhood_group_cleansed <- gsub("Les Corts", "Les_Corts", dd$neighbourhood_group_cleansed)

unique(dd$neighbourhood_group_cleansed)
#Creation of new modalities for gender
unique(dd$gender)

nuevas_categorias <- c("M", "F", "C","U")
dd$gender <- factor(dd$gender, levels = c("male", "female", "company","Unisex"), labels = nuevas_categorias)
#Creation of new modalities for host_response_time
unique(dd$host_response_time)

nuevas_categorias <- c("few_h", "an_h", "a_day","few_days")
dd$host_response_time <- factor(dd$host_response_time, levels = c("within a few hours", "within an hour", "within a day","a few days or more"), labels = nuevas_categorias)

#Creation of new modalities for room_type
unique(dd$room_type)

nuevas_categorias <- c("p_room", "e_home", "h_room","s_room")
dd$room_type <- factor(dd$room_type, levels = c("Private room", "Entire home/apt", "Hotel room","Shared room"), labels = nuevas_categorias)

#Creation of new modalities for source
unique(dd$source)

nuevas_categorias <- c("p_s", "c_s")
dd$source <- factor(dd$source, levels = c("previous scrape","city scrape"), labels = nuevas_categorias)

#Creation of new modalities for host_verifications:
unique(dd$host_verifications)

nuevas_categorias <- c("ph", "mail_ph","mail_ph_wmail","ph_wmail","mail","no_ver")
dd$host_verifications <- factor(dd$host_verifications, levels = c("['phone']","['email', 'phone']","['email', 'phone', 'work_email']","['phone', 'work_email']","['email']","[]"), labels = nuevas_categorias)

#Rename the variables:

dd <- rename(dd,
               n_ovw = neighborhood_overview,
               h_since = host_since,
               h_loc = host_location,
               h_abt = host_about,
               h_res_t = host_response_time,
               h_res_r = host_response_rate,
               h_acc_r = host_acceptance_rate,
               s_host = host_is_superhost,
               h_ver = host_verifications,
               h_id_v = host_identity_verified,
               n_gr_cl = neighbourhood_group_cleansed,
               lt = latitude,
               ln = longitude,
               r_type = room_type,
               accs = accommodates,
               bth_txt = bathrooms_text,
               min_navg = minimum_nights_avg_ntm,
               max_navg = maximum_nights_avg_ntm,
               av_365 = availability_365,
               fst_r = first_review,
               lst_r = last_review,
               r_sco_rt = review_scores_rating,
               inst_bk = instant_bookable,
               h_lst_c = calculated_host_listings_count,
               rws_m = reviews_per_month,
               amts_c = amenities_count
)

write.csv(dd,file="Reduced_Database.csv")
