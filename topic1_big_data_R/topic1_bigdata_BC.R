#Session 1: introduction to R and Big Data
#Author: Bin Chen

# # means comment

#set workspace
setwd("~/Downloads/MSU_TBW/data")

#view the structure of a real R project
#https://github.com/Bin-Chen-Lab/HCC_NEN

#download data
#https://chenlab-data-public.s3-us-west-2.amazonaws.com/OCTAD_cell_line.RData
#https://chenlab-data-public.s3-us-west-2.amazonaws.com/octad_phenoDF.csv

#################
#R command
version #check R version
ls()  # list all objects in the current session
sessionInfo() #list current session
system("ls -al") #call a system command
#system("Rscript *.R") #run a R script

################
#R packages
#install R package
install.packages("ggplot2")
install.packages("data.table")
install.packages("stringr")

#call a library
library(data.table)
#list all library functions
library(help=data.table)
#find help doc
help(data.table)
#get raw code of a given function
getAnywhere("fread")

#install bioconductor R package
install.packages("BiocManager")
BiocManager::install("GEOquery")

#######################
#read/write files

#load data
load("OCTAD_cell_line.RData")
View(octad_cell_line_meta)

#read file
pheno = read.csv("octad_phenoDF.csv", stringsAsFactors = F)
#view  data frame as a table
View(pheno)
#summarize one object
summary(pheno)

#using read.table; suggest to set stringsAsFactors = F and check.names = F
pheno2 = read.delim("octad_phenoDF.csv", sep = ',', stringsAsFactors = F, header= T, check.names=F)
help(read.table)

#load big files, it is much faster than read.csv for big files (>100M)
pheno_big = fread("octad_phenoDF.csv")


#write file
write.csv(pheno, "pheno_new.csv")
write.table(pheno, "pheno_new.txt", sep = "\t", row.names= F, col.names=T, quote=F )

save(pheno, pheno2, file = "pheno.RData") #save objects


###########################################
#data type
#tips: To name a variable: 1) no space, 2) no special character such as #, &, 3) don't start with number, and 4) understandable 
#numeric
age = 60
typeof(age)
class(age)
as.integer()
is.integer()

#character
cancer = "liver cancer"
toupper(cancer)

#logical
is_male = T

#vector
genders = c("male", "female")
ages = c(20, 30, 40)
ages[1]
ages[c(1,2)]
ages[-c(1,2)]
sort(ages, decreasing = T)
rev(ages)

seq(1, 10, by = 1)
rep(1, 10, times = 10)

#factor 
gender = factor(c("male", "female"))

#data.frame
class(pheno)
head(pheno)
head(pheno$sample.id) #calling one column
unique(pheno$cancer) #show unique items

#matrix
class(octad_cell_line_matrix)
octad_cell_line_matrix[1:3, 1:3]

#list
octad_pheno_ls = list(age = ages, gender = genders)
octad_pheno_ls[["age"]][[1]]

#class
library("GEOquery")
gse = getGEO("GSE781",GSEMatrix=FALSE)
class(gse)
head(Meta(gse))
gse@header$contact_address

########################################
#subsetting, and searching
dim(octad_cell_line_matrix)
nrow(octad_cell_line_matrix)
ncol(octad_cell_line_matrix)
octad_cell_line_matrix[1:3, 1:3]

dim(octad_cell_line_features)
dim(octad_cell_line_meta)
octad_cell_line_meta[1:3, 1:5]
colnames(octad_cell_line_meta)
rownames(octad_cell_line_meta)

#show the number of cell lines for all cancers
table(octad_cell_line_meta$disease)
table(octad_cell_line_meta$disease, octad_cell_line_meta$sex)

#list all liver cancer cell lines
octad_cell_line_meta$CCLE.Name[octad_cell_line_meta$disease == "Liver Cancer"]
octad_cell_line_meta[octad_cell_line_meta$disease == "Liver Cancer", ]
#or
subset(octad_cell_line_meta, disease == "Liver Cancer", select = c("CCLE.Name", "lineage"))

#list all breast and liver cancer cell ines
octad_cell_line_meta$CCLE.Name[octad_cell_line_meta$disease %in% c("Liver Cancer", "Breast Cancer")]

#list feature types in octad
head(octad_cell_line_features)
table(octad_cell_line_features$type)

#show cancer cell lines with BRAF mutation
#find BRAF name
colnames(octad_cell_line_matrix)[grep("mutation_BRAF", colnames(octad_cell_line_matrix))]
cell_line_ids = rownames(octad_cell_line_matrix[!is.na(octad_cell_line_matrix[, "mutation_BRAF"]) & octad_cell_line_matrix[, "mutation_BRAF"] == 1,]) #tip NA
octad_cell_line_meta$CCLE.Name[octad_cell_line_meta$DepMap_ID %in% cell_line_ids]

##########################################
#Basic Operators and Calculations
#multiple numbers
age = 20 + 30

#multiple two vectors
ages + ages

ages - ages

ages * 2
ages / 2

#concatenate two characters
gender = paste("fe", "male", sep = "")

#concatenate a vector
paste(genders, collapse = ' ')

#combine two data frames by row
new_pheno = rbind(pheno[1:3, ], pheno[10000:10003, ])
new_pheno = cbind(pheno[, 1:2], pheno[, 4:5])

##############################################
#programming
#condidtions
ages[ages == 20]
ages[ages != 20]
ages[ages > 20]

#if statement
if (age == 40){
  print("age == 40")
}else{
  print("age != 40")
}

#for loop
for (age in ages){
  print(paste("this is age", age))
}

#function
add_ages <- function(ages){
  total_age = 0
  for (age in ages){
    total_age = total_age + age
  }
  return(total_age)
}

add_ages(ages)

age == 20

#basic data summary
mean(ages)
median(ages)
sum(ages)
log(ages)
min(ages)
max(ages)

########################################
#advanced (retrieving data from cbioportal)
library(cgdsr)
mycgds = CGDS("http://www.cbioportal.org/")
test(mycgds)

# Get list of cancer studies at server
a = getCancerStudies(mycgds)
symbols = c("CASP14", "SYDE1", "ILVBL", "NOTCH3", "BRD4", "AKAP8L", "WIZ") 

# Get available case lists (collection of samples) for a given cancer study
mycancerstudies = getCancerStudies(mycgds)[,1]

#brca_metabric
mycancerstudy = "brca_tcga_pub2015" #brca_metabric brca_tcga_pub2015
mycaselist = getCaseLists(mycgds,mycancerstudy)[1,1]
myclinicaldata = getClinicalData(mycgds,mycaselist)
myclinicaldata = myclinicaldata[myclinicaldata$IHC_HER2 == "Negative" & myclinicaldata$ER_STATUS_BY_IHC == "Negative" &  myclinicaldata$PR_STATUS_BY_IHC == "Negative",]

mygeneticprofiles = getGeneticProfiles(mycgds,mycancerstudy)[,1]
mygeneticprofile = mygeneticprofiles[grep("mrna", mygeneticprofiles)][1] 
mrna = getProfileData(mycgds,c(as.character(symbols)) ,mygeneticprofile,mycaselist)
  
mrna_subset = mrna[rownames(mrna) %in% rownames(myclinicaldata),]
summary(mrna_subset)
##############################################
#practice
#find MYC amplified lung cancer cell lines
#summerize AFP expression in liver cancer

#advanced: compare AFP expression in liver cancer lines and liver cancer patient samples

#visualize  MAX expression in MYC amplified lung cancer cell lines vs MYC wild lung cancer cell lines
