#
# Purpose: differential gene exrpession analysis between male and female, human liver tissue
#


# load required packages

#https://www.bioconductor.org/
require(DESeq2) # for DE analysis
require(AnnotationDbi) # for id to symbol convertion
require(org.Hs.eg.db)

# https://www.tidyverse.org/
require(plyr)
require(dplyr)

# load the pre-stored data, GTEx liver
load('Liver.RData')

###########################################################################
## STEP1: prepare the read count data and meta information ##

# 1.1 gene filtering to remove lowly expressed gene
# gene filtering based on TPM
Female.sample               <- sample.meta.df$sample.id[sample.meta.df$gender == 'Female'] %>% as.character()
F.log2.tpm.matrix           <- log2.tpm.matrix[,Female.sample]

Male.sample                 <- sample.meta.df$sample.id[sample.meta.df$gender != 'Female'] %>% as.character()
M.log2.tpm.matrix           <- log2.tpm.matrix[,Male.sample]


F.median                    <- apply(F.log2.tpm.matrix,1,median)
F.expressed.gene            <- names(F.median) [F.median > log2(1+1)]

M.median                    <- apply(M.log2.tpm.matrix,1,median)
M.expressed.gene            <- names(M.median) [M.median > log2(1+1)]              

expressed.gene              <- union(F.expressed.gene,M.expressed.gene)

# ALT: gene filtering based on readcount
flag             <- apply(log2.read.count.matrix,1, function(x) sum (x >= log2(5+1) ) )
expressed.gene.2 <- rownames(log2.read.count.matrix)[flag >= 50]


# 1.2 prepare read count matrix , row:gene.id  col:sample.id !!!!
read.count.matrix <- round(2^log2.read.count.matrix - 1)
read.count.matrix <- read.count.matrix[expressed.gene,]

# 1.3 prepare meta data,the sample order MUST be consitent between the read.count.matrix and the sample.meta.df
sum(colnames(read.count.matrix) == sample.meta.df$sample.id)
View(colnames(read.count.matrix))
View(sample.meta.df)

###########################################################################
## STEP2: Let us DE ########
dds           <- DESeqDataSetFromMatrix(countData = round(read.count.matrix),
                                        colData = sample.meta.df,
                                        design = ~ gender )
dds      <- DESeq(dds)

# cooksCutoff = FALSE 
res      <- results(dds,contrast = c('gender','Male','Female'),cooksCutoff = FALSE) %>% as.data.frame  # Male vs Female

# cooksCutoff is used, filtering outlier
res      <- results(dds,contrast = c('gender','Male','Female')) %>% as.data.frame  # Male vs Female
sum(is.na(res$pvalue))
res      <- res[complete.cases(res),]     


res <- res[order(res$pvalue),]

# draw the Volcano plot to visualize DE results
plot(x=res$log2FoldChange, y = -1 * log10(res$padj))
plot(x=res$log2FoldChange, y = -1 * log10(res$padj),ylim =c(0,4), xlab='log2FC', ylab= '-log10(padj)')
abline(v= c(-1,1))
abline(h = -1 * log10(0.05))

###########################################################################
## STEP3: extract DE genes ########
up.gene       <- rownames(res)[res$log2FoldChange > 1  & res$padj < 0.05]
up.gene.DE.df <- res[up.gene,c('log2FoldChange','pvalue','padj')]
dn.gene       <- rownames(res)[res$log2FoldChange < -1 & res$padj < 0.05]
dn.gene.DE.df <- res[dn.gene,c('log2FoldChange','pvalue','padj')]


###########################################################################
## STEP4: convert id to symbol, then enrichR analysis ########

# 4.1 convert gene id to symbol
up.gene.annotation.df  <- AnnotationDbi::select(x = org.Hs.eg.db,keytype = 'ENSEMBL',columns = c('ENSEMBL','SYMBOL'),keys = up.gene) 
dn.gene.annotation.df  <- AnnotationDbi::select(x = org.Hs.eg.db,keytype = 'ENSEMBL',columns = c('ENSEMBL','SYMBOL'),keys = dn.gene) 

# handle redundancy
merge.symbol <- function(x) {
    paste(x$SYMBOL,sep = ":",collapse = ':')  
  
}
up.gene.annotation.df           <- ddply(up.gene.annotation.df,.(ENSEMBL),merge.symbol)
dn.gene.annotation.df           <- ddply(dn.gene.annotation.df,.(ENSEMBL),merge.symbol)
rownames(up.gene.annotation.df) <- up.gene.annotation.df$ENSEMBL
rownames(dn.gene.annotation.df) <- dn.gene.annotation.df$ENSEMBL

# combine DE information and gene symbol
up.gene.DE.df$SYMBOL <- up.gene.annotation.df[rownames(up.gene.DE.df),'V1']
dn.gene.DE.df$SYMBOL <- dn.gene.annotation.df[rownames(dn.gene.DE.df),'V1']

write.csv(x=up.gene.DE.df,file='up.gene.DE.df.csv',quote=FALSE)
write.csv(x=dn.gene.DE.df,file='dn.gene.DE.df.csv',quote=FALSE)

#4.2 enrichR analysis
#Go to https://amp.pharm.mssm.edu/Enrichr/
