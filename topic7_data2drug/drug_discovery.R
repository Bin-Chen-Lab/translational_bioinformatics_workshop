
setwd("~/Downloads/MSU_TBW/data/drug")

#############
#basic cheminformatics
############
#install R packages

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("ChemmineR")

library("ChemmineR")

data(sdfsample) 
sdfset <- sdfsample
sdfset
sdfset[[1]]
plot(sdfset[1:4], print=FALSE)
apset <- sdf2ap(sdfset)
data(apset)

cmp.similarity(apset[1], apset[2])

cmp.search(apset, apset[1], type=3, cutoff = 0.3, quiet=TRUE) # Search apset database with single compound. 

smiles <- sdf2smiles(sdfset) 


#################################
#Example 1. liver hepatocellular carcinoma vs selected reference tissues; 
#################################
#download and install 
#download octad
#https://chenlab-data-public.s3-us-west-2.amazonaws.com/octad/octad_1.0.tar.gz
#see instruction to install octad 
#https://chenlab-data-public.s3-us-west-2.amazonaws.com/octad/octad_tutorial.pdf

library("octad")
library("ChemmineR") # Loads the package

#1. load phenotype data

head(phenoDF)[1:10] #visualize phenotype table
HCC_primary=subset(phenoDF,cancer=='liver hepatocellular carcinoma'&sample.type == 'primary') #select data
case_id=HCC_primary$sample.id #select cases
write.table(case_id,file='case_id.txt',sep='\t',row.names=F,col.names=F,quote=F)


#2. Compute reference tissue. 
#computing reference tissue, by default using small autoencoder, but can use custom expression set,
#by defaulf output=T and outputFolder is empty sending control  corMatrix.csv to working directory
HCC_adjacent=subset(phenoDF,cancer=='liver hepatocellular carcinoma'&sample.type == 'adjacent'&data.source == 'TCGA') #select data
control_id=HCC_adjacent$sample.id #select cases
write.table(control_id,file='control_id.txt',sep='\t',row.names=F,col.names=F,quote=F)


##########OPTIONAL START###############
#visualize relative distance of the computed case and reference ids via precomputed tsne of the OCTAD database
#make a new column called type to state case, control, or others
tsne$type <- "others"
tsne$type[tsne$sample.id %in% case_id] <- "case"
tsne$type[tsne$sample.id %in% control_id] <- "control"

#plot
(p2 <- ggplot(tsne, aes(X, Y, color = type)) + geom_point(alpha = 0.4)+
    labs(title = paste ('TNSE PLOT'), x= 'TSNE Dim1', y='TSNE Dim2', caption="OCTAD")+
    theme_bw())

ggsave("case_control_map.pdf",height=10,width=10)
##########OPTIONAL END###############

#3. Compute DE
#compute differential expression,
#ready expSet='octad.small' or 'octad.whole' using data for LINCS genes or whole octad dataset, IT DOES MATTER!!!, or you can load your own expression data
#by defaulf output=T and outputFolder is empty sending control  corMatrix.csv to working directory
#and you need to source full path to .h5 file containing your TPM expression data. By default files being sourced with package and their names are
# octad.LINCS.h5 for 978 expression signatures and octad.counts.and.tpm.h5
res=diffExp(case_id,control_id,output=T)

#Optional in case of absent 'octad.counts.and.tpm.h5'. To download full .h5 file, please reffer to manual.
#res=diffExp(case_id,control_id,source='octad.small',output=T)

#filter result file
res=subset(res,abs(log2FoldChange)>1&padj<0.001)

##########OPTIONAL START###############
#Compute  GO enrichment for obtained data
GO = geneEnrich(res$Symbol,database_list=c( "KEGG_2019_Human","GO_Biological_Process_2017","GO_Cellular_Component_2017"),output=T)
##########OPTIONAL END###############

#4. Compute sRGES, drug enrichment, cell lines and evaluate results. 
#run
sRGES=runsRGES(res,max_gene_size=500,permutations=10000)

#drug enrichment
#sRGES = read.csv('sRGES.csv',stringsAsFactors = F)
octadDrugEnrichment(sRGES = sRGES)

#compute__cell_line. Return most likely lines for case_id 
cell_line_computed=computeCellLine(case_id=case_id,returnDF=T)
#process them to evaluate correlation between sRGES and selected cell lines
topLineEval(topline = c('HEPG2'),mysRGES = sRGES)

#################################
#End of example 1
#################################




