##' Builds the inference table 
##'
##' This function builds the inference table for beta, upper, and lower 
##' @title Build inference table
##' @param infStat Data.frame of beta, lower, upper, and p. The column names must include BETA, LOWER, UPPER and P)
##' @return data.frame
##' @author Jochen Kruppa
##' @export
buildInference <- function(infStat, pCol = "p"){
    ## build up the inference
    betaCol <- grep("beta", names(infStat))
    lowerCol <- grep("lower", names(infStat))
    upperCol <- grep("upper", names(infStat))
    infStat[betaCol] <- ifelse(infStat[,betaCol] > 1000, Inf, infStat[,betaCol])
    infStat[upperCol] <- ifelse(infStat[,upperCol] > 1000, Inf, infStat[,upperCol])
    inference <- data.frame(OR = paste0(
                                sprintf("%.2f", infStat[,betaCol]), " [",
                                sprintf("%.2f", infStat[,lowerCol]), ", ",
                                sprintf("%.2f", infStat[,upperCol]), "]"),
                            p = infStat[,pCol],
                            row.names = row.names(infStat))
    return(inference)
}
