setwd("/home/user")

#speficifying the targets 

targets <- read.delim("./Rrnaseq/data/targets.txt", comment.char = "#")
targets

#SystemPipeR will be used along with HISAT2 for the read-mapping aspect

library(systemPipeR)

# create a results directory

dir.create("results") 

# defines location where to write results

results <- "./results" 

#Automating the execution of readmapping using HISAT2

args <- systemArgs(sysma="./Rrnaseq/param/hisat2.param",
      mytargets="./Rrnaseq/data/targets.txt")

# check parameters for running 1st sample using HISAT2

sysargs(args)[1]

#Warnings can be ignored after this command

runCommandline(args=args) 

#generate alignment statistics

read_statsDF <- alignStats(args) 

# display alignment statistics

read_statsDF 


#Now we can visualize the RNA-seq Data

library(GenomicFeatures)

txdb <- makeTxDbFromGFF(file="./Rrnaseq/data/TAIR10_GFF3_trunc.gff",format=
      "gff3",dataSource="TAIR",organism="Arabidopsis thaliana") 

# retrieve exon features grouped by genes

eByg <- exonsBy(txdb, by="gene") 

bfl <- BamFileList(outpaths(args), yieldSize=50000, index=character())

counteByg <- bplapply(bfl, function(x) summarizeOverlaps(eByg, x,
          mode="Union", ignore.strand=TRUE, inter.feature=TRUE, singleEnd=TRUE))

countDFeByg <- sapply(seq(along=counteByg), function(x)
            assays(counteByg[[x]])$counts)

rownames(countDFeByg) <- names(rowRanges(counteByg[[1]]));
                      colnames(countDFeByg) <-names(bfl)

countDFeByg[1:4,]

#Writing the data into a table

write.table(countDFeByg, "results/countDFeByg.xls", col.names=NA,
          quote=FALSE, sep="\t")
#Plotting columns 1 through 4

boxplot(countDFeByg[,1:4])

#Justyfying the data being distrubuted evenly

boxplot(countDFeByg[,1:4],ylim=c(0,2000))
