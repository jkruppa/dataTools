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
        message(paste0("Work ends at    ",  hmWork$h, ":", hmWork$m, " o'clock"))
        message(paste0("Old workaccount ",  hmWorkaccount$h, ":", hmWorkaccount$m, " hours"))
        message(paste0("New workaccount ",  hmActualWorkaccount$h, ":",
                       hmActualWorkaccount$m, " hours"))
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
        returnDf <- as.data.frame(rbind(paste(hmWorked$h, "hours", hmWorked$m, "minutes"),
                                        paste(hmOvertime$h, "hours", hmOvertime$m, "minutes"),
                                        paste0(rep("-", time = 20), collapse = ""),
                                        paste(hmWorkaccount$h, "hours", hmWorkaccount$m, "minutes"),
                                        paste(hmAcOvertime$h, "hours", hmAcOvertime$m, "minutes")
                                        ))
        names(returnDf) <- "Time table"
        row.names(returnDf) <- c("Working time:", "(daily) balance:", "",
                                 "Old workaccount: ", "New workaccount: ")
        return(returnDf)
    }
}

##' @title Small function to calculate the working time (improved)
##' @param from 
##' @param lunchStart 
##' @param lunchEnd 
##' @param end 
##' @param workaccount 
##' @return data.frame
##' @author Jochen Kruppa
##' @export
time_worked <- function(start, lunch_start, lunch_end, end, workaccount){
  require(lubridate)
  require(magrittr)
  ## small wrapper to get the seconds
  to_sec <- function(x) {as.numeric(hm(x))}
  sec_to_hhmm <- function(x) {
    if(x > 0){
      t_x <- seconds_to_period(x) 
      return(sprintf('%02d:%02d', t_x@day * 24 + t_x@hour, minute(t_x)))
    } else {
      t_x <- seconds_to_period(abs(x)) 
      time <- sprintf('%02d:%02d', t_x@day * 24 + t_x@hour, minute(t_x))
      return(paste0("-", time))
    }
  }
  ## get the times
  should <- "07:58"
  worked <- to_sec(end) - (to_sec(lunch_end) - to_sec(lunch_start)) - to_sec(start)
  is <- worked - to_sec(should) 
  ending <- to_sec(start) + (to_sec(lunch_end) - to_sec(lunch_start)) + to_sec(should)
  workaccount_new <- to_sec(workaccount) + is
  ## print everything
  cat(paste("Start working       ", start, "\n"))
  cat(paste("End working        ", end, "\n"))
  cat(paste("Should end working ", sec_to_hhmm(ending), "\n"))
  cat(c("-------------------------\n"))
  cat(paste("Time worked        ", sec_to_hhmm(worked), "\n"))
  cat(paste("Daily balance      ", sec_to_hhmm(is), "\n"))
  cat(c("-------------------------\n"))
  cat(paste("Old workaccount    ", sec_to_hhmm(to_sec(workaccount)), "\n"))
  cat(paste("New workaccount    ", sec_to_hhmm(workaccount_new), "\n"))
}
