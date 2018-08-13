setwd("~/Fall'17/Data mining/Project 2/clean_proj1")
getwd()

data_files <- c("Status_Non_DoD_2004_03")

for (i in data_files)
{
  ########################################################################################
  # read file
  ########################################################################################
  print(paste("Processing file:", paste(i, ".txt", sep = "")))
  
  #dat_raw <- readLines(paste(i, ".txt", sep = ""))
  dat_raw <-readLines("Status_Non_DoD_2004_03.txt")
  head(dat_raw)
  
  dat_header <- read.csv("C:/Users/POOJA VYAS/Documents/Fall'17/Data mining/Project1/bush/headers.csv", header = TRUE)
  
  dat <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
  
  
  dimnames(dat) <- NULL
  dat <- as.data.frame(dat)
  
  colnames(dat) <- dat_header[,1]
  head(dat)
  summary(dat)
  
  ########################################################################################
  # Fix Pay
  ########################################################################################
  dat$Pay <- as.numeric(as.character(dat$Pay))
  summary(dat)
  nrow(dat)
  
  ########################################################################################
  # replace unknowns with NA
  ########################################################################################
  
  dat$Station <- replace(dat$Station, dat$Station == "#########", NA)
  dat$Age <- replace(dat$Age, dat$Age == "UNSP", NA)
  dat$LOS <- replace(dat$LOS, dat$LOS == "UNSP", NA)
  dat$Education <- replace(dat$Education, dat$Education == "" | dat$Education == "*", NA)
  dat$PayPlan <- replace(dat$PayPlan, dat$PayPlan == "" | dat$PayPlan == "*", NA)
  dat$Category <- replace(dat$Category, dat$Category == "" | dat$Category == "*", NA)
  dat$SupervisoryStatus <- replace(dat$SupervisoryStatus, dat$SupervisoryStatus == "" | dat$SupervisoryStatus == "*", NA)
  dat$Schedule <- replace(dat$Schedule, dat$Schedule == "" | dat$Schedule == "*", NA)
  dat$Appointment <- replace(dat$Appointment, dat$Appointment == "**", NA)
  summary(dat)
  
  ########################################################################################
  # make ordinal fields ordered factors
  ########################################################################################
  dat$Age <- factor(dat$Age, ordered = TRUE, levels = levels(dat$Age))
  dat$Education <- factor(dat$Education, ordered = TRUE, levels = levels(dat$Education))
  
  ########################################################################################
  # Fix order od levels
  ########################################################################################
  levels(dat$LOS)
  dat$LOS <- ordered(dat$LOS, levels = c("< 1", "1-2", "3-4", "5-9",
                                         "10-14", "15-19", "20-24", "25-29", "30-34", "35+", "NA"))
  levels(dat$LOS)
  
  ########################################################################################
  # remove Pay that is NA or 0 (considered outliers)
  # remove Age that is NA
  # remove Education that is NA (*, invalid by translation)
  # remove PayPlan that is NA (*, invalid by translation)
  # remove Schedule that is NA (*, invalid by translation)
  ########################################################################################
  #dat1 <- na.omit(dat)
  dat1 <- subset(dat, Pay != 'NA' & Pay != '0'
                 & Age != 'NA'
                 & Education != 'NA'
                 & PayPlan != 'NA'
                 & Schedule != 'NA')
  summary(dat1)
  ########################################################################################
  # remove duplicated rows
  ########################################################################################
  dat2 <- unique(dat1)
  summary(dat2)
  nrow(dat1)
  nrow(dat2)
  ########################################################################################
  # remove duplicated PseudoID, keeping only 1 with the maximum Pay
  ########################################################################################
  # Reverse sort
  dat3 <- dat2[order(dat2$Pay, decreasing=TRUE),]
  summary(dat3)
  # Keep only the first row for each duplicate of z$id; this row will have the
  # largest value for dat3$PseudoID
  dat3 <- dat3[!duplicated(dat3$PseudoID), ]
  # Sort so it looks nice
  #dat3 <- dat3[order(dat3$PseudoID, dat3$Pay),]
  summary(dat3)
  nrow(dat3)
  ########################################################################################
  # add agency name
  ########################################################################################
  agency_trans <- readLines("SCTFILE.TXT")
  
  agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
  agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
  
  agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)
  head(agency_trans_table)
  
  dat4 <- dat3
  m <- match(dat4$Agency, agency_trans_table$agency_ID)
  dat4$AgencyName <- agency_trans_table$agency_name[m]
  summary(dat4)
  
  ########################################################################################
  # remove NA agencies
  ########################################################################################
  #dat5 <- subset(dat4, is.na(dat4[, 18]))
  #dat5 <- subset(dat4, !is.na(dat4[, 18]))
  #dat5
  #summary(dat5)
  
  ########################################################################################
  # add work schedule
  ########################################################################################
  dat6 <- dat4
  summary(dat6)
  dat6$Fulltime <- FALSE
  dat6$Fulltime[dat6$Schedule == "F" | dat6$Schedule == "G"] <- TRUE
  dat6$Seasonal <- FALSE
  dat6$Seasonal[dat6$Schedule %in% c("G", "J", "Q", "T")] <- TRUE
  
  state_data<-dat6$Station
  state_id <- sapply(state_data, FUN = function(x) substring(x, 1,2))
  state_trans_table<- data.frame(state_id=state_id,Station=dat6$Station)
  m <- match(dat6$Station, state_trans_table$Station)
  dat6$StationID <-  state_trans_table$state_id[m]
  
  small<- c('AGSC',	'CG00' ,	'DLED',	'DLNS',	'GS29',	'ZQ00',	'ZT00',	'GI00',	'PC00',	'ZR00',	'GN00',	'TD20',	'YA00',	'HSDB',	'HSDC',	'BH00',	'HE31',	'DB00',	'DF00',	'GS31',	'AGHL',	'YE00',	'ZU00')
  medium <- c('AGES',	'AN00',	'AP00',	'AW00',	'BK00',	'BW00',	'BZ00',	'CE00',	'CF00',	'CM33',	'CX00',	'DLCA',	'EDEJ',	'EDEX',	'EDEY',	'EQ00',	'EX00',	'FI00',	'FK00',	'GE00',	'GS01',	'GS04',	'GS10',	'GS13',	'GS20',	'GX00',	'GY00',	'HT00',	'HUJJ',	'HUNN',	'HUOO',	'HUUU',	'HUVV',	'HUWW',	'HUYY',	'MA00',	'NK00',	'TS00',	'UJ00',	'UT00',	'VAAC',	'VAAH',	'VABA',	'VABB',	'VABD',	'VADA',	'VADC',	'VAEA',	'VAHA',	'VAHB',	'VAHC',	'VAHD',	'VAJA',	'VAJB',	'VAKA',	'VAKB',	'ZP00',	'ZS00',	'ZW00',	'AGHS', 'GM00',	'GO00',	'GQ00',	'HUKA',	'HUKK',	'HW00',	'IW00',	'YB00',	'ZZ00',	'YD00',	'DQ00',	'DLEB')
  other <- c("NA's")	
 dat6$type <- ifelse(dat6$Agency %in% small, 1 , ifelse(dat6$Agency %in% medium, 2, ifelse(dat6$Agency %in% other, 3, 4)))
  dat6$type<-dat6$Agency
  levels(dat6$type) <- list('small' = small,'medium' = medium, 'other' = other,'large' = 'large')
  
  summary(dat6$Agency=="AGES")
  summary(dat6)
  
  dattry <- summary(dat6$Agency)
  head(dattry)
  type <- cut(dattry,
              breaks = c(0, 100, 999,Inf),
              labels = c("small", "medium", "large"))
  
  summary(type)
  summary(dat6)
  
  ########################################################################################
  # save the data
  ########################################################################################
  save(dat6, file = paste(i, "b.rda", sep = ""))
  
  #write.csv(dat6, file = paste(i, ".csv", sep = ""), row.names = FALSE)
}
