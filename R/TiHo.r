## from = "7:52"
## lunchStart = "12:17"
## lunchEnd = "12:45"
## end = "17:00"
## workaccount = "16:47"

##' Wrapper for hours minutes
##'
##' Wrapper for hours minutes
##' @title Wrapper for hours minutes
##' @param time 
##' @return Hours:Minutes
##' @author Jochen Kruppa
getHoursMinutes <- function(time){
    hours <- gsub("(\\d+).*", "\\1", format(time))
    if(grepl("\\.", format(time))){
        minutesDigitChr <- paste0("0.", gsub(".*\\.(\\d+).*", "\\1", format(time)))
        minutes <- round(as.numeric(minutesDigitChr) * 60)
        if(minutes < 10)
            minutes <- paste0("0", minutes)
        else 
            minutes <- as.character(minutes)
    } else {
        minutes <- "00"
    }
    return(list(h = hours, m = minutes))
}

##' @title Small function to calculate the working time
##' @param from 
##' @param lunchStart 
##' @param lunchEnd 
##' @param end 
##' @param workaccount 
##' @return data.frame
##' @author Jochen Kruppa
##' @export
timeWorked <- function(from = NULL, lunchStart = NULL, lunchEnd = NULL, end = NULL, 
                       workaccount = NULL){
    times <- as.difftime(c(from, lunchStart, lunchEnd, end), format = "%H:%M")
    shouldWorked <- as.difftime("7:58", format = "%H:%M")
    workaccount <- as.difftime(workaccount, format = "%H:%M")
    if(is.null(end)){
        toWork <- times[1] + (times[3] - times[2]) + shouldWorked
        ## get the hours:minutes
        hmWork <- getHoursMinutes(toWork)
        hmWorkaccount <- getHoursMinutes(workaccount)
        ## actual time
        acTime <- as.difftime(gsub(".*(\\d{2}:\\d{2}):.*", "\\1", Sys.time()), format = "%H:%M")
        actualOvertime <- acTime - (times[1] + (times[3] - times[2])) - shouldWorked
        hmActualWorkaccount <- getHoursMinutes(workaccount + actualOvertime)
        message(paste0("Arbeitsende ist heute um ",  hmWork$h, ":", hmWork$m, " Uhr"))
        message(paste0("Alte Überstunden         ",  hmWorkaccount$h, ":", hmWorkaccount$m, " Stunden"))
        message(paste0("Neue Überstunden (jetzt) ",  hmActualWorkaccount$h, ":",
                       hmActualWorkaccount$m, " Stunden"))
    } else {
        ## worked
        workedTime <- times[4] - (times[3] - times[2]) - times[1]
        hmWorked <- getHoursMinutes(workedTime)
        ## workaccount
        hmWorkaccount <- getHoursMinutes(workaccount)
        ## actual overtime
        overtime <- as.numeric(workedTime - shouldWorked, units = "hours")
        hmOvertime <- getHoursMinutes(overtime)
        hmAcOvertime <- getHoursMinutes(workaccount + overtime) 
        ## return everything
        returnDf <- as.data.frame(rbind(paste(hmWorked$h, "Stunden", hmWorked$m, "Minuten"),
                                        paste(hmOvertime$h, "Stunden", hmOvertime$m, "Minuten"),
                                        paste0(rep("-", time = 20), collapse = ""),
                                        paste(hmWorkaccount$h, "Stunden", hmWorkaccount$m, "Minuten"),
                                        paste(hmAcOvertime$h, "Stunden", hmAcOvertime$m, "Minuten")
                                        ))
        names(returnDf) <- "Zeittabelle"
        row.names(returnDf) <- c("Arbeitszeit:", "(Tages) Saldo:", "",
                                 "Alte Überstunden: ", "Neue Überstunden: ")
        return(returnDf)
    }
}



## timeWorked(from = "7:52", lunchStart = "12:15", lunchEnd = "12:45", end = "14:00",
##            workaccount = "16:47")
