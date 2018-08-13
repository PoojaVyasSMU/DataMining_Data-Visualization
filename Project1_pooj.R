
#Code
#Obama 2009, 2012
#Bush 2001,2004
dat_raw1 <- readLines("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/obama/Status_Non_DoD_2009_03.txt")
dat_raw2 <- readLines("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/obama/Status_Non_DoD_2009_06.txt")
dat_raw<- rbind(dat_raw1, dat_raw2)
dat_raw3 <- readLines("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/obama/Status_Non_DoD_2009_09.txt")
dat_raw <- rbind(dat_raw,dat_raw3)
dat_raw4 <- readLines("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/obama/Status_Non_DoD_2009_12.txt")
dat_raw <- rbind(dat_raw,dat_raw4)

dat_header <- read.csv("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/bush/headers.csv", header = FALSE)
dat <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
dimnames(dat) <- NULL
dat <- as.data.frame(dat)
colnames(dat) <- dat_header[,1]
head(dat)
summary(dat)
original_len <- length(dat$PseudoID)
print(original_len)

dat$Pay <- as.numeric(as.character(dat$Pay))
#dat$Education <- as.numeric(as.character(dat$Education))
dat$Age <- as.numeric(as.character(dat$Age))
#dat$LOS <- as.numeric(as.character(dat$LOS))
#dat$Occupation <- as.numeric(as.character(dat$Occupation))
summary(dat)

#Sort by full time and seasonal
dat$Fulltime <- FALSE
dat$Fulltime[dat$Schedule == "F" | dat$Schedule == "G"] <- TRUE
dat$Seasonal <- FALSE
dat$Seasonal[dat$Schedule %in% c("G", "J", "Q", "T")] <- TRUE

summary(dat)

#Cleaning
dat$Station <- replace(dat$Station, dat$Station == "#########" | dat$Station == "*********", NA)
dat$Age <- replace(dat$Age, dat$Age == "UNSP", NA)
dat$Education <- replace(dat$Education, dat$Education == "" | dat$Education == "*" | dat$Education == "**", NA)
dat$PayPlan <- replace(dat$PayPlan, dat$PayPlan == "" | dat$PayPlan == "*" | dat$PayPlan == "**", NA)
dat$Grade <- replace(dat$Grade, dat$Grade == "" | dat$Grade == "**", NA )
dat$LOS <- replace(dat$LOS, dat$LOS == "UNSP", NA)
dat$Occupation <- replace(dat$Occupation, dat$Occupation == "" | dat$Occupation == "****", NA)
dat$Category <- replace(dat$Category, dat$Category == "" | dat$Category == "*" | dat$Category == "**", NA)
dat$SupervisoryStatus <- replace(dat$SupervisoryStatus, dat$SupervisoryStatus == "" | dat$SupervisoryStatus == "*" | dat$SupervisoryStatus == "**", NA)
dat$Appointment <- replace(dat$Appointment, dat$Appointment == "" | dat$Appointment == "**", NA)
dat$Schedule <- replace(dat$Schedule, dat$Schedule == "" | dat$Schedule == "*" | dat$Schedule == "**", NA)

# make ordinal fields ordered factors
dat$Age <- factor(dat$Age, ordered = TRUE, levels = levels(dat$Age))
dat$Education <- factor(dat$Education, ordered = TRUE, levels = levels(dat$Education))
levels(dat$LOS)
dat$LOS <- ordered(dat$LOS, levels = c("< 1", "1-2", "3-4", "5-9",
                                       "10-14", "15-19", "20-24", "25-29", "30-34", "35+", "UNSP"))
boxplot(Pay~LOS, data = dat)
typeof(dat$LOS)

# if Age is unspecified, use median age for agency
dat$Age <- with(dat, ave(dat$Age, dat$Agency, FUN = function(x) replace(x, is.na(x), levels(dat$Age)[median(as.integer(x), na.rm = TRUE)])))

# fill NA Education with median Education for employees of the same Age at the Agency
dat$Education <- with(dat, ave(dat$Education, dat$Age, dat$Agency, FUN = function(x) replace(x, is.na(x), levels(dat$Education)[median(as.integer(x), na.rm = TRUE)])))

# fill NA pays with median pay for the Age of the employee at that agency
dat$Pay <- with(dat, ave(dat$Pay, dat$Age, dat$Agency, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))

# drop any rows with NA pay after imputation
na_pay <- is.na(dat$Pay)
dat <- dat[!na_pay,]
summary(dat)


#handle duplicate id's
# 1. select rows with duplicate IDs
dat_dup_ids <- dat[duplicated(dat$PseudoID) | duplicated(dat$PseudoID, fromLast = TRUE),]
# 2. order selection by ID, then Agency, then descending Pay
dat_dup_ids <- dat_dup_ids[order(dat_dup_ids$PseudoID, dat_dup_ids$Agency, -dat_dup_ids$Pay),]
# 3. select rows where the ID and Agency are duplicated (same employee at same agency)
to_remove <- dat_dup_ids[(duplicated(dat_dup_ids[c("PseudoID", "Agency")]) | duplicated(dat_dup_ids[c("PseudoID", "Agency")], fromLast = TRUE)),]
# 4. get row numbers for rows with the lowest pay for each grouping in the above selection
dat <- dat[!(rownames(dat) %in% to_remove),]
summary(dat)
head(dat)
final_len <- length(dat$PseudoID)
print(final_len)
nrows(dat)

#save to a file
save(dat, file = "Employment_2009.rda")


#basic visualizaion related to complete data

hist(dat$Pay, breaks = 50, xlab="Pay for 2009")
barplot(table(dat$Education), las = 2)

barplot(table(dat$Education), las = 2,xlab="Education'09")
boxplot(Pay~Education, data = dat, xlab="Education'09", ylab="Pay'09")

#median pay per agency
med_pay_by_agency <- aggregate(Pay~Agency, data = dat, median)
hist(med_pay_by_agency$Pay)
attach(med_pay_by_agency)
plot(Pay,Agency, main = "Median Pay V/s agency", xlab="Pay'09", ylab="Agency")

#see top
head(med_pay_by_agency)
#order from low to high
head(dat[head(order(dat$StationID, decreasing=TRUE)),])


#high to low
head(med_pay_by_agency[head(order(med_pay_by_agency$Pay,
                                  decreasing = TRUE)),])


#Med pay as per los
med_pay_by_los<-aggregate(Pay~LOS, data=dat, median)
head(med_pay_by_los)
hist(med_pay_by_los$Pay)
attach(dat)
plot(state_id,SupervisoryStatus,main="state V/s supervisor", xlab="state", ylab="Supervisor")

#read agency's
agency_trans <- readLines("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/bush/SCTFILE.TXT")
agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
#Create agency table
agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)
head(agency_trans_table)
m <- match(dat$Agency, agency_trans_table$agency_ID)
dat$AgencyName <-  agency_trans_table$agency_name[m]
head(dat)

#create station table
state_data<-dat$Station
head(state_data)
summary(state_data)
state_id <- sapply(state_data, FUN = function(x) substring(x, 1,2))
state_trans_table<- data.frame(state_id=state_id,Station=dat$Station)
head(state_trans_table)
m <- match(dat$Station, state_trans_table$Station)
dat$StationID <-  state_trans_table$state_id[m]
head(dat)
summary(dat)
boxplot(StationID~Education, data = dat)

#Create frames for all agencies to be analyzed


tsa <- dat[dat$AgencyName=="TRANSPORTATION SECURITY ADMINISTRATION", ]
immi<-dat[dat$AgencyName == "CITIZENSHIP AND IMMIGRATION SERVICES", ]
irs<- dat[dat$AgencyName=="INTERNAL REVENUE SERVICE", ]
epa<- dat[dat$AgencyName=="ENVIRONMENTAL PROTECTION AGENCY", ]
#ssn <-dat[dat$AgencyName=="SOCIAL SECURITY ADMINISTRATION", ]
nsf<- dat[dat$AgencyName=="NATIONAL SCIENCE FOUNDATION", ]
faith<-dat[dat$AgencyName=="CENTR FOR FAITH-BASED & COMM INITIATIVES", ]
#public_affairs<-dat[dat$AgencyName=="OFFICE OF PUBLIC AFFAIRS", ]
#expo_impo_bank<-dat[dat$AgencyName=="EXPORT-IMPORT BANK OF THE UNITED STATES", ]
#wb<-dat[dat$AgencyName=="WOMEN'S BUREAU", ]
#state<-dat[dat$AgencyName=="DEPARTMENT OF STATE", ]
health<-dat[dat$AgencyName=="NATIONAL INSTITUTES OF HEALTH", ]


summary(health)
boxplot(Pay~StationID, data=tsa, xlab="StateID", ylab="Pay")
boxplot(Education~StationID, data=tsa, xlab="State", ylab="Education",las=2)
#Count number of rows
nrow()
typeof(epa$Age)


#Visualizations
hist(tsa$Education,main ="Education_TSA'09")
plot(tsa$Pay,type="o")

boxplot(Pay~Education, data=epa)
hist(epa$Pay)
boxplot(Pay~LOS, data=epa)
levels(health$LOS)
health$LOS <- ordered(health$LOS, levels = c("< 1", "1-2", "3-4", "5-9",
                                       "10-14", "15-19", "20-24", "25-29", "30-34", "35+", "UNSP"))
boxplot(Education~LOS, data=health, xlab='los', ylab='Education', las=2)

barplot(table(dat$Age), las = 2)
boxplot(Pay~Age, data = dat)
typeof(dat$Age)
head(epa)
head(tsa)
summary(tsa)



#See data for citizenship and immi


head(immi)
#see education of these people
barplot(table(immi$Education), las = 2)

boxplot(Pay~Education, data = immi)
hist(immi$Pay)
boxplot(Pay~LOS, data = immi, xlab = 'LOS', ylab='pay')
hist(immi$Age)

#non seasonal and seasonal employee education:
non_seasonal_immi <- immi[immi$Fulltime & !immi$Seasonal, ]
barplot(table(non_seasonal_immi$Education), las = 2)
boxplot(Pay~Education, data = non_seasonal_immi)
hist(non_seasonal_immi$Pay)
#Statitical values
min(dat$Age, na.rm= T)
max(dat$Age, na.rm=T)
mean(dat$Age, na.rm = T)
median(dat$Age,na.rm = T)
typeof(dat$Age)
#mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(dat$Age)
print(result)

#Final visualizations
hist(dat$Education)
barplot(dat$Pay, main="Pay Distribution", horiz=TRUE)
barplot(v)
hist(dat$Occupation, xlab="Occupation'09", main = "Occupation 2009")
barplot(epa$Occupation, xlab="Occupation'09", main = "Occupation 2009")
plot(tsa$Pay,tsa$Age)
tn<-table(epa$Pay, epa$Education)
plot(tn)
barplot(tn, beside = TRUE, legend = TRUE) 
install.packages("ggplot2")
library("ggplot2")
p <- ggplot(epa, aes(Pay, Education)) 
p<-ggplot(dat, aes(Age))
p + geom_point(colour ="Green", size=2)

p<-ggplot(dat, aes(LOS, Agency))
hist(dat$LOS)
p + geom_point()

p + geom_rug(sides = "bl")

p + geom_dotplot(binaxis='y', stackdir = "center")
p + geom_boxplot(colour= "Blue")
p + geom_jitter()
p<- ggplot(irs, aes(Grade,SupervisoryStatus))

summary(dat)
top5states<-read.csv("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/final report/states.csv")
head(top5states)

install.packages("maps")
library(maps)
map<-map_data("state")
head(map)

l<-ggplot(top5states, aes(fill=Count))
l + geom_map(aes(map_id=State),map=map)+ expand_limits(x=map$long, y=map$lat, xlab="lat", ylab="long")
summary(dat$StationID)
#grade
g <- ggplot(dat, aes(Grade))
g + geom_histogram(stat="count", color="Yellow")
#LOS
g <- ggplot(nsf, aes(Education))
g + geom_histogram(stat="count", color="Blue", main="Education levels for Dept of Health 2009")
typeof(dat$Age)
attach(tsa)
plot(Age,main="Age for TSA dept", xlab="age")
age<-dat$Age
head(age)
summary(age)
#Staewise edu
attach(dat)
plot(x=Education,y=StationID,main="Education levels per state 2009", xlab="StateCode")

tsa_matrix <- as.matrix(dat$Pay, tsa$Pay, epa$Pay)
                        
scaled_tsa <- scale(tsa_matrix)

pimage(scaled_tsa,main="Standard deviation Pay")

#corelation
cm1 <- cor(tsa_matrix)
summary(cm1)
pimage(cm1)

install.packages("seriation")
library("seriation")
install.packages("mvtnorm")
