######################################################################################################
## Retrieves GBIF record data for a set of taxa, calculates extent of occurrence for the list
## and compares the calculation to distribution data gathered from literature as distinct groups
## and as a composite dataset.
## 
## Author: V. Shirey, Oct. 18, 2017         Last Update: March 23, 2019
######################################################################################################

######################################################################################################
# Load and clean data and libraries
######################################################################################################

library(red) ## load packages
paper <- read.csv("occurrence.txt", sep="\t", header = TRUE) ## read in paper data
paper <- paper[!is.na(paper$decimalLatitude),] # remove rows with no georef data
names <- as.vector(unique(paper$scientificName)) ## extract unique names from paper data
paper$decimalLatitude <- as.numeric(paper$decimalLatitude)
paper$decimalLongitude <- as.numeric(paper$decimalLongitude)

######################################################################################################
# Iterate through unique names and grab records from GBIF via 'red' library
######################################################################################################
iterations = length(names)
datalist = list()

temp1 <- data.frame() ## temporary data frame for joining occurrence data from GBIF
temp2 <- data.frame()

for(i in 1:iterations){
  taxon <- names[i] ## grab names

  temp1 <- data.frame()
  try(temp1 <- records(taxon))  ## try getting records from GBIF
  try(temp1$V1 <- taxon)
  
  datalist[[i]] <- temp1
  
}

dat = do.call(rbind, datalist) ## merge all occurrence data from GBIF into one data frame
dat <- unique(dat) ## obtain only unique records for each species

######################################################################################################
# Iterate through each species and calculate EOO using 'red' from just the paper data
######################################################################################################
temp2 <- data.frame() ## reset temp variables
temp3 <- data.frame()
datalist = list()

for(i in 1:iterations){
  temp1 <- names[i] ## grab names
  
  try(temp2[i,1] <- temp1)
  try(temp2[i,2] <- eoo(as.matrix(paper[which(paper[,13]==temp1), c(9,8)])))
  
  datalist[[i]] <- temp2
}

paperEOO <- do.call(rbind, datalist)
paperEOO <- unique(paperEOO)

######################################################################################################
# Iterate through each species and calculate EOO using 'red' from just the GBIF data
######################################################################################################
temp2 <- data.frame() ## reset temp variables
temp3 <- data.frame()
datalist = list()

for(i in 1:iterations){
  temp1 <- names[i] ## grab names
  
  try(temp2[i,1] <- temp1)
  try(temp2[i,2] <- eoo(as.matrix(dat[which(dat[,3]==temp1), c(1,2)])))
  
  datalist[[i]] <- temp2
}

GBIFdat <- do.call(rbind, datalist)
GBIFdat <- unique(GBIFdat)

######################################################################################################
# Iterate through each species and calculate EOO using 'red' from both datasets
######################################################################################################
temp1 <- data.frame() ## reset temp variables
temp2 <- data.frame() 
temp3 <- data.frame()
datalist = list()

colnames(dat)[which(names(dat) == "V1")] <- "scientificName"
colnames(dat)[which(names(dat) == "long")] <- "decimalLongitude"
colnames(dat)[which(names(dat) == "lat")] <- "decimalLatitude"

common_cols <- intersect(colnames(paper), colnames(dat)) ## bind the datasets
mdat <- rbind(
  paper[,common_cols],
  dat[,common_cols]
)  

for(i in 1:iterations){
  name <- names[i] ## grab names
  
  try(temp2[i,1] <- name)
  try(temp2[i,2] <- eoo(as.matrix(mdat[which(mdat$scientificName==name), c(2,1)])))
  
  datalist[[i]] <- temp2
}

mergeddat <- do.call(rbind, datalist)
mergeddat <- unique(mergeddat)

######################################################################################################
# Rename columns from calculations and merge into new combined, dataframe
######################################################################################################
colnames(paperEOO)[which(names(paperEOO) == "V2")] <- "paperEOO"
colnames(GBIFdat)[which(names(GBIFdat) == "V2")] <- "gbifEOO"
colnames(mergeddat)[which(names(mergeddat) == "V2")] <- "mergedEOO"

EOO_final <- merge(merge(paperEOO, GBIFdat), mergeddat)


