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
