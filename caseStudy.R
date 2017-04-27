#set enronment /working directory where input files are copied
setwd("C:/DEV/R/IIITB/CaseStudy")

#load libradies
library(dplyr)
library(tidyr)

#Checkpoint 1: Data Cleaning 1
companies <- read.delim("companies.txt", header = T, sep = '\t', stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = T, stringsAsFactors = FALSE)

#cleanup - remove whitespaces
companies$permalink <- trimws(companies$permalink)
rounds2$company_permalink <- trimws(rounds2$company_permalink)

#cleanup - toLower the primary key column
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

##Results Expected: Table 1.1
#How many unique companies are present in rounds2?
nrow(distinct(rounds2,company_permalink))

#How many unique companies are present in companies?
nrow(distinct(companies,permalink))

#Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N: NO
setdiff(rounds2$company_permalink,companies$permalink)


# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
master_frame <- merge(rounds2, companies, by.x=("company_permalink"),by.y=("permalink"))
#find how many observations are present in master_frame? 

#Checkpoint 2: Data Cleaning 2
#count the NA's
sum(is.na(master_frame$raised_amount_usd))

# replace NA's in raised_amount_usd with 0
master_frame[,"raised_amount_usd"][is.na(master_frame[, "raised_amount_usd"])] <- 0

#Checkpoint 3: Funding Type Analysis
#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) 
avg_invst_fundTypesOf4 <- filter(aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN= mean),Group.1 %in% list("venture", "angel", "seed", "private_equity"))

#Results Expected: Table 3.1
# Average funding amount of venture type	
avg_invst_fundTypesOf4[which(avg_invst_fundTypesOf4$Group.1=="venture"),]$x
avg_invst_fundTypesOf4[which(avg_invst_fundTypesOf4$Group.1=="angel"),]$x
avg_invst_fundTypesOf4[which(avg_invst_fundTypesOf4$Group.1=="seed"),]$x
avg_invst_fundTypesOf4[which(avg_invst_fundTypesOf4$Group.1=="private_equity"),]$x

#find suitable fund between 5 to 15 million USD per investment round
suitable_fund <- filter (avg_invst_fundTypesOf4, x >= 5000000 & x <= 15000000)$Group.1

#find the data frame that only suitable fund  for further conuntry analysis
suitable_frame <- master_frame[which(master_frame$funding_round_type==suitable_fund),]

#Checkpoints - Part 2
#Checkpoint 4: Country Analysis
#top 9 countries that has highest total funding in choosen fundng type
top9<- arrange(aggregate(suitable_frame$raised_amount_usd, by=list(suitable_frame$country_code), FUN = sum), desc(x))

#library(countrycode) can used to convert list of Full Name countries to ISO3C format.
#Example: suitable_frame$country_code <- countrycode(suitable_frame$country_code,'iso3c','country.name')

#consider below list contian list of 4 english speaking languages returned from the pdf of contries. 
eng_speaking_cntry_list <- list("IND","GBR","USA","CAN")

#top3 contries list
best_eng_countries <- head(intersect(top9$Group.1,eng_speaking_cntry_list),3) 
#Table 4.1: Analysing the Top 3 English-Speaking Countries
best_eng_countries[1]  # 1. Top English-speaking country	
best_eng_countries[2]  # 2. Second English-speaking country	
best_eng_countries[3]  # 3. Third English-speaking country	

#top3 contries data frame
top3_frame <- filter(suitable_frame, suitable_frame$country_code %in% best_eng_countries)

#Checkpoint 5: Sector Analysis 1

#Apply business rule that the first string before the vertical bar will be considered the primary sector
top3_frame["primary_sector"] <- sub("\\|.*", "", top3_frame$category_list)

#remove obs's that contains primary_sector is blank
top3_frame <- filter(top3_frame,primary_sector!="")

#Read the mapping file into DF
mappings <- read.csv("mapping.csv",stringsAsFactors = FALSE)
#convert wide to long of mapping file
mappings <- filter(gather(data=mappings, key = main_sector, value = value, Automotive...Sports:Social..Finance..Analytics..Advertising),value==1)
mappings$value<- NULL

#replace typo's in mapping file.. ex: "0" with "na"
mappings$category_list<-    gsub("0","na",mappings$category_list)

#Merge the data frame and mapping file with key as primary_sector , this also remove Blanks
merged_frame <- merge(top3_frame,mappings, by.x="primary_sector", by.y="category_list")


#Checkpoint 6- Sector Analysis 2
#D1
D1 <- merged_frame[which(merged_frame$country_code=="USA" & merged_frame$funding_round_type=="venture" & merged_frame$raised_amount_usd >= 5000000 & merged_frame$raised_amount_usd <= 15000000),]
D1_total_count = aggregate(raised_amount_usd~main_sector, D1, length)
colnames(D1_total_count)[2]<-"total_count"
D1_total_amount = aggregate(raised_amount_usd~main_sector, D1, sum)
colnames(D1_total_amount)[2]<-"total_amount"

D1_merge_count_amount <- merge(D1_total_count,D1_total_amount,by="main_sector")

D1<-merge(D1,D1_merge_count_amount,by="main_sector")


#D2
D2 <- merged_frame[which(merged_frame$country_code=="GBR" & merged_frame$funding_round_type=="venture" & merged_frame$raised_amount_usd >= 5000000 & merged_frame$raised_amount_usd <= 15000000),]
D2_total_count = aggregate(raised_amount_usd~main_sector, D2, length)
colnames(D2_total_count)[2]<-"total_count"
D2_total_amount = aggregate(raised_amount_usd~main_sector, D2, sum)
colnames(D2_total_amount)[2]<-"total_amount"

D2_merge_count_amount <- merge(D2_total_count,D2_total_amount,by="main_sector")

D2<-merge(D2,D2_merge_count_amount,by="main_sector")

#D3
D3 <- merged_frame[which(merged_frame$country_code=="IND" & merged_frame$funding_round_type=="venture" & merged_frame$raised_amount_usd >= 5000000 & merged_frame$raised_amount_usd <= 15000000),]
D3_total_count = aggregate(raised_amount_usd~main_sector, D3, length)
colnames(D3_total_count)[2]<-"total_count"
D3_total_amount = aggregate(raised_amount_usd~main_sector, D3, sum)
colnames(D3_total_amount)[2]<-"total_amount"

D3_merge_count_amount <- merge(D3_total_count,D3_total_amount,by="main_sector")

D3<-merge(D3,D3_merge_count_amount,by="main_sector")

#crate temp data sets

D1_temp<- select(D1,main_sector,funding_round_type,raised_amount_usd,name,country_code)
D2_temp<- select(D2,main_sector,funding_round_type,raised_amount_usd,name,country_code)
D3_temp<- select(D3,main_sector,funding_round_type,raised_amount_usd,name,country_code)

#Table 6.1 : Sector-wise Investment Analysis
# Total number of investments 
nrow(D1)
nrow(D2)
nrow(D3)

#Total amount of investment 
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

#Top sector (based on count of investments)
D1_top_sector <- D1[which.max(D1$total_count),]$main_sector
D2_top_sector <- D2[which.max(D2$total_count),]$main_sector
D3_top_sector <- D3[which.max(D3$total_count),]$main_sector
D1_top_sector
D2_top_sector
D3_top_sector

#find uniqueue data sets for each country with main_sector and total_count
D1_unique<-unique(D1[c("main_sector","total_count")]) 
D2_unique<-unique(D2[c("main_sector","total_count")]) 
D3_unique<-unique(D3[c("main_sector","total_count")]) 

#Second-best sector (based on count of investments)

D1_Top2Sector <- D1_unique$main_sector[order(D1_unique$total_count,decreasing=TRUE)[2]]
D2_Top2Sector <- D2_unique$main_sector[order(D2_unique$total_count,decreasing=TRUE)[2]]
D3_Top2Sector <- D3_unique$main_sector[order(D3_unique$total_count,decreasing=TRUE)[2]]
D1_Top2Sector
D2_Top2Sector
D3_Top2Sector

#third-best sector (based on count of investments)
D1_unique$main_sector[order(D1_unique$total_count,decreasing=TRUE)[3]]
D2_unique$main_sector[order(D2_unique$total_count,decreasing=TRUE)[3]]
D3_unique$main_sector[order(D3_unique$total_count,decreasing=TRUE)[3]]

#Number of investments in the top sector 
D1_unique$total_count[order(D1_unique$total_count,decreasing=TRUE)[1]]
D2_unique$total_count[order(D2_unique$total_count,decreasing=TRUE)[1]]
D3_unique$total_count[order(D3_unique$total_count,decreasing=TRUE)[1]]

#Number of investments in the second best sector 
D1_unique$total_count[order(D1_unique$total_count,decreasing=TRUE)[2]]
D2_unique$total_count[order(D2_unique$total_count,decreasing=TRUE)[2]]
D3_unique$total_count[order(D3_unique$total_count,decreasing=TRUE)[2]]

#Number of investments in the third best sector 
D1_unique$total_count[order(D1_unique$total_count,decreasing=TRUE)[3]]
D2_unique$total_count[order(D2_unique$total_count,decreasing=TRUE)[3]]
D3_unique$total_count[order(D3_unique$total_count,decreasing=TRUE)[3]]


#For the top sector count-wise (point 3), which company received the highest investment?
D1_topSector <- filter(D1,main_sector == D1_top_sector)
D1_topSector$name[order(D1_topSector$raised_amount_usd,decreasing=TRUE)[1]]

D2_topSector <- filter(D2,main_sector == D2_top_sector)
D2_topSector$name[order(D2_topSector$raised_amount_usd,decreasing=TRUE)[1]]


D3_topSector <- filter(D3,main_sector == D3_top_sector)
D3_topSector$name[order(D3_topSector$raised_amount_usd,decreasing=TRUE)[1]]

#For the second-best sector count-wise (point 4), which company received the highest investment?
D1_top2Sector <- filter(D1,main_sector == D1_Top2Sector)
D1_top2Sector$name[order(D1_top2Sector$raised_amount_usd,decreasing=TRUE)[1]]

D2_top2Sector <- filter(D1,main_sector == D2_Top2Sector)
D2_top2Sector$name[order(D2_top2Sector$raised_amount_usd,decreasing=TRUE)[1]]

D3_top2Sector <- filter(D3,main_sector == D3_Top2Sector)
D3_top2Sector$name[order(D3_top2Sector$raised_amount_usd,decreasing=TRUE)[1]]



