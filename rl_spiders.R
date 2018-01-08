## Retrieves GBIF record data for a set of taxa, calculates extent of occurrence for the list
## and compares the calculation to distribution data gathered from literature as distinct groups
## and as a composite dataset.
## 
## Author: V. Shirey, Oct. 18, 2017

library(red) ## load packages

names <- read.csv(file.choose(), sep=",", header = TRUE) ## read in names
paper <- read.csv(file.choose(), sep=";", header = TRUE) ## read in paper data

## process namelist to characters
names[] <- lapply(names, as.character)

## process paper data to remove records where coordinate data is invalid or missing
paper$decimalLatitude <- as.numeric(levels(paper$decimalLatitude))[paper$decimalLatitude]
paper$decimalLongitude <- as.numeric(levels(paper$decimalLongitude))[paper$decimalLongitude]
paper <- na.omit(paper) ## omit records with incomplete data, should be 2525 records total

## iteratively retrieve lat/lon record data for each species from GBIF and populate matrix
iterations = nrow(names)
datalist = list()

temp1 <- data.frame() ## temporary data frame for joining occurrence data from GBIF
temp2 <- data.frame()

for(i in 1:iterations){
  gbif <- names[i,] ## grab names

  temp1 <- data.frame()
  try(temp1 <- records(gbif))
  try(temp1$V1 <- gbif)
  
  datalist[[i]] <- temp1
  
} ## iteratively progress through the name list while retrieving records from GBIF

dat = do.call(rbind, datalist) ## merge all occurrence data from GBIF into one data frame
dat <- unique(dat)
colnames(paper) <- colnames(dat)

## iteratively calculate eoo for each record associated with papers only
i = 1 ## reset counter
temp2 <- data.frame() ## reset temp variables
temp3 <- data.frame()
datalist = list()

for(i in 1:iterations){
  temp1 <- names[i,] ## grab names
  
  try(temp2[i,1] <- temp1)
  try(temp2[i,2] <- eoo(as.matrix(paper[which(paper[,3]==temp1), c(1,2)])))
  
  datalist[[i]] <- temp2
}

paperdat <- do.call(rbind, datalist)
paperdat <- unique(paperdat)

## iteratively calculate eoo for the GBIF data only
i = 1 ## reset counter
temp2 <- data.frame() ## reset temp variables
temp3 <- data.frame()
datalist = list()

for(i in 1:iterations){
  temp1 <- names[i,] ## grab names
  
  try(temp2[i,1] <- temp1)
  try(temp2[i,2] <- eoo(as.matrix(dat[which(dat[,3]==temp1), c(1,2)])))
  
  datalist[[i]] <- temp2
}

GBIFdat <- do.call(rbind, datalist)
GBIFdat <- unique(GBIFdat)

## iteratively calculate eoo for the merged data of both GBIF and papers
i = 1 ## reset counter
temp1 <- data.frame() 
temp2 <- data.frame() ## reset temp variables
temp3 <- data.frame()
datalist = list()

mdat <- rbind(paper, dat)

for(i in 1:iterations){
  temp1[i,1] <- names[i,1] ## grab names
  
  try(temp2[i,1] <- names[i,1])
  try(temp2[i,2] <- eoo(as.matrix(mdat[which(mdat$V1==names[i,1]), c(1,2)])))
  try(temp3 <- merge(temp1, temp2, by.x = "V1", by.y = "V1"))
  
  datalist[[i]] <- temp3
}

mergeddat <- do.call(rbind, datalist)
mergeddat <- unique(mergeddat)

## write 3 CVS with data

write.csv(mergeddat, "merged_eoo.csv")
write.csv(GBIFdat, "GBIF_eoo.csv")
write.csv(paperdat, "paper_eoo.csv")
