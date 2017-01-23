##' Finds sam files and convert to bam
##'
##' Finds sam files and convert to bam
##' @title Finds sam files and convert to bam
##' @param samDir Dir where are 'pattern' sam's should be converted
##' @param pattern Pattern to find sam files to be converted
##' @return NULL
##' @author Jochen Kruppa
##' @export
sam2bam <- function(samDir, pattern, keep = TRUE){
  talk("Start converting sam to bam with sorting the bam file...")
  sam2BamCMD <- str_c("find ", samDir, " | grep '", pattern,
                      "' | parallel '",
                      "samtools view -b -S {} | ",
                      "samtools sort - {.}; ",
                      "samtools index {.}.bam' 2> /home/temp/temp.log" ## make index and be quiet!!!
                      )
  runCMD(sam2BamCMD)
  if(!keep){
    talk("Remove *all* sam files from samDir")
    unlink(dir0(samDir, pattern = "*.sam$"))
  }
  talk("Finished")
}

##' Function to quality filter fastq files
##'
##' Function to quality filter fastq files
##' @title Function to quality filter fastq files
##' @param inFile Fastq infiles
##' @param outFile Qc checked output file
##' @param q Minimum quality score to keep (phred)
##' @param p Minimum percent of bases that must have [-q] quality. 
##' @return NULL
##' @author Jochen Kruppa
##' @export
fastq_quality_filter <- function(inFile, outFile, q = 20, p = 50){
  fastq_quality_filter_CMD <- paste("fastq_quality_filter",
                                    "-i", inFile,
                                    "-o", outFile,
                                    "-q", q, ## min quality
                                    "-p", p) ## min percentage of read hit q
  runCMD(fastq_quality_filter_CMD)
}

##' Trimmomatic wrapper
##'
##' Trimmomatic wrapper. Will automatically idenitfy paied or single
##' reads, if they are named 'R1' and 'R2'. Not needed in single end
##' case.
##' @title Trimmomatic wrapper
##' @param inFile Fastq infile
##' @param outFile Fastq outfile
##' @param leading Numeric. Remove leading low quality or N bases
##'   (below quality 10 [default])
##' @param trailing Numeric. Remove trailing low quality or N bases
##'   (below quality 10 [default])
##' @param minlength Min read length to keep. 50 [default]
##' @param illuminaclip Remove the adapter from illumina by TruSeq2
##'   or TruSeq3 protocol
##' @return NULL
##' @author Jochen Kruppa
##' @export
fastq_trimmer <- function(inFile, outFile,
                          leading = 10,
                          trailing = 10,
                          minlength = 50,
                          illuminaclip = "TruSeq3"
                          ){
  if(length(inFile) == 2){
    ## check if both in and out files are named
    is.named(inFile)
    is.named(outFile)
    paired_outFile <- gsub("fastq|fq", "paired.fq", outFile)
    unpaired_outFile <- gsub("fastq|fq", "unpaired.fq", outFile)    
    fastq_trimmer_CMD <- paste("java -jar", trimmomatic,
                               "PE",
                               "-threads", par$nCores,
                               "-phred33",
                               inFile["R1"],
                               inFile["R2"],
                               paired_outFile["R1"],
                               unpaired_outFile["R1"],
                               paired_outFile["R2"],
                               unpaired_outFile["R2"],
                               str_c("ILLUMINACLIP:",
                                     illuminaclip, "-PE.fa:2:30:10"),
                               str_c("LEADING:", leading),
                               str_c("TRAILING:", trailing),
                               str_c("MINLEN:", minlength),
                               "SLIDINGWINDOW:4:15")
  } else{
    fastq_trimmer_CMD <- paste("java -jar", trimmomatic,
                               "SE",
                               "-threads", par$nCores,
                               "-phred33",
                               inFile,
                               outFile,
                               str_c("ILLUMINACLIP:",
                                     illuminaclip, "-PE.fa:2:30:10"),
                               str_c("LEADING:", leading),
                               str_c("TRAILING:", trailing),
                               str_c("MINLEN:", minlength),
                               "SLIDINGWINDOW:4:15")
  }
  runCMD(fastq_trimmer_CMD)
}

##' Main fastq quality function
##'
##' Main fastq quality function. Calls the function
##' fastq_quality_filter and fastq_trimmer. Stores files in a tempDir.
##' @title Main fastq quality function
##' @param inFile Fastq infile
##' @param outFile Fastq outfile
##' @param tmpDir Where to store the temporal files. Files will be
##'   removed automatically
##' @param p Minimum percent of bases that must have [-q] quality.
##' @param q Minimum quality score to keep (phred)
##' @param leading Numeric. Remove leading low quality or N bases
##'   (below quality 10 [default])
##' @param trailing Numeric. Remove trailing low quality or N bases
##'   (below quality 10 [default])
##' @param minlength Min read length to keep. 50 [default]
##' @param illumninaclip Remove the adapter from illumina by TruSeq2
##'   or TruSeq3 protocol
##' @return NULL
##' @author Jochen Kruppa
##' @export
fastq_quality_control <- function(inFile, outFile,
                                  tmpDir = tmpDir,
                                  p = 20,
                                  q = 50,
                                  leading = 10,
                                  trailing = 10,
                                  minlength = 50,
                                  illumninaclip = "TruSeq3"){
  talk("Write all temporal files to /home/temp")
  tmpTrimFq <- file.path(tmpDir,
                         gsub("fastq|fq", "trimmed.fq", basename(inFile)))
  if(length(inFile) == 2){
    names(tmpTrimFq) <- names(inFile)
  } 
  talk("Start trimming")
  fastq_trimmer(inFile, tmpTrimFq,
                leading, trailing,
                minlength, illumninaclip)
  talk("Start quality clipping and read removing")
  if(length(inFile) == 2){
    pairedTrimFq <- gsub("fq", "paired.fq", tmpTrimFq)
    l_ply(seq_along(pairedTrimFq), function(i){
      fastq_quality_filter(inFile = pairedTrimFq[i],
                           outFile = outFile[i], q = q, p = p)   
    }, .parallel = TRUE)
  } else {
    fastq_quality_filter(inFile = tmpTrimFq, outFile = outFile, q = q, p = p)   
  }
  unlink(list.files(tmpDir, pattern = "trimmed", full.names = TRUE))
  talk("Cleaning up and finished")
}
