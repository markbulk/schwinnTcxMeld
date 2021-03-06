################################################################################################################################ 
# This file is intended to convert a Garmin TCX and a Schwinn CSV file into a combined TCX that will actually be useful
# Assumes the following:
# - Schwinn module has been receiving heart rate information from HRM which can be aligned to the TCX file
# - A Garmin TCX file is available of the session
# - The spin bike captured a heart rate signal within the first four minutes of the workout
# 
# Anticipated Process flow:
# - Start Garmin Forerunner Spin Activity, ensure HRM is connected
# - Pedal Schwinn and turn on head unit
# - Insert USB stick to Schwinn Echelon unit
# - SPIN, baby, SPIN!
# - Hit end on Schwinn Echelon unit
# - Stop Garmin activity
# - Pull out USB stick
# - Sync activity with Garmin Connect
# - Download activity TCX file
# - Run below test code, substituting in your file locations
# - Upload new TCX file to GC
# - Delete old GC activity
################################################################################################################################
library(data.table)
library(lubridate)
library(zoo)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
################################################################################################################################
# test code
# csvLocation <- '/Volumes/MacStorage/Dropbox/Fitness/MPOWER01.csv'
# tcxLocation <- '/Users/markbulk/Downloads/activity_2401308812.tcx'
# outputLocation <- '/Volumes/MacStorage/Dropbox/Fitness/test.tcx'
# dt.full <- meldTcx(csvLocation, tcxLocation, timeZone = "America/New_York")
# writeTcxNew(dt.input = dt.full, tcxLocation = tcxLocation, outputLocation = outputLocation)
################################################################################################################################

# Function to write the melded information to an output file, which can then be imported into Garmin Connect
writeTcxNew <- function(dt.input = NULL, timeZone = "America/New_York", tcxLocation = NULL, outputLocation = NULL) {
    stopifnot(!is.null(dt.input))
    chr.block <- '          <Trackpoint>
            <Time>TTTTTTTT</Time>
            <HeartRateBpm>
              <Value>HHHHHHHH</Value>
            </HeartRateBpm>
            <Cadence>CCCCCCCC</Cadence>
            <DistanceMeters>DDDDDDDD</DistanceMeters>
            <Extensions>
              <TPX xmlns="http://www.garmin.com/xmlschemas/ActivityExtension/v2">
                <Watts>WWWWWWWW</Watts>
              </TPX>
            </Extensions>
          </Trackpoint>'
    num.metersInMile <- 1609.344
    dt.input[, zuluTime := as.character(format(as.POSIXct(Time, tz = timeZone), tz = "Europe/London", usetz = TRUE))]
    dt.input[, zuluTime := gsub(" ", "T", gsub(" GMT", "Z", zuluTime))]
    # read in original tcx file
    con <- file(tcxLocation, "r")
    vec.file <- readLines(con, n = -1)
    close(con)
    # use first group of lines as the basis for the beginning our new file
    i = 1
    while(trim(vec.file[i]) != '<Trackpoint>') {
        i = i + 1
    }
    vec.output <- vec.file[1:(i-1)]
    num.dist <- which(vec.output == '        <DistanceMeters>0.0</DistanceMeters>')
    vec.output[num.dist] <- paste0('        <DistanceMeters>', round(max(dt.input$Distance.mile) * num.metersInMile, 3), '</DistanceMeters>')
    # add all of the trackpoints
    for(i in 1:nrow(dt.input)) {
        vec.output <- c(vec.output, gsub("TTTTTTTT", dt.input[i]$zuluTime, 
                                         gsub("HHHHHHHH", dt.input[i]$HeartRateBpm, 
                                              gsub("CCCCCCCC", dt.input[i]$RPM, 
                                                   gsub("DDDDDDDD", round(dt.input[i]$Distance.mile * num.metersInMile, 3), 
                                                        gsub("WWWWWWWW", dt.input[i]$Watts, chr.block))))))
    }
    # use the last group of lines as the basis for the end of our new file
    vec.output <- c(vec.output, vec.file[(max(which(vec.file == '          </Trackpoint>')) + 1):length(vec.file)])
    # write the output to the file
    outputCon <- file(outputLocation)
    writeLines(vec.output, con = outputCon)
    close(outputCon)
}

# Function combines the Schwinn MPower CSV file and the Garmin TCX files to create a more data
# rich rendering of a spin session.  Uses na.approx (linear interpolation) to fill in gaps of the data.
# The Garmin TCX should be sampled almost every second, while the MPower csv will just be 10x per minute.
# The TCX heart rate is used exclusively.  Any time after the MPower file is complete is assumed to have
# no speed or further spin input.
meldTcx <- function(csvLocation = NULL, tcxLocation = NULL, interpolate = TRUE, timeZone = "America/New_York") {
    stopifnot(!is.null(csvLocation) | !is.null(tcxLocation))
    dt.csv <- readSchwinnCsv(csvLocation)
    dt.tcx <- readTCX(tcxLocation, timeZone = timeZone)
    baseTime <- minimizeHeartRateError(dt.csv, dt.tcx)
    dt.csv[, timeJoin := as.character(as.POSIXct(baseTime, tz = timeZone) + seconds(Stage_Workout.min * 60))]
    dt.tcx[, timeJoin := as.character(as.POSIXct(time))]
    dt.join <- merge(dt.csv, dt.tcx, by = 'timeJoin', all = TRUE)[order(timeJoin)]
    # join again to all of the seconds between the start and end to make sure that we have a regular data set
    dt.join <- merge(dt.join, data.table(timeJoin = as.character(seq.POSIXt(from = as.POSIXct(dt.join[1]$timeJoin), to = as.POSIXct(dt.join[nrow(dt.join)]$timeJoin), by = 1))), all = TRUE)
    setkey(dt.join, timeJoin)
    # set the initial conditions, if necessary
    if(is.na(dt.join[1]$RPM)) dt.join[1]$RPM <- 0
    if(is.na(dt.join[1]$Distance.mile)) dt.join[1]$Distance.mile <- 0
    if(is.na(dt.join[1]$Watts)) dt.join[1]$Watts <- 0
    if(is.na(dt.join[1]$Speed.mph)) dt.join[1]$Speed.mph <- 0
    # set the last bits to be zero speed and distance as the maximum
    dt.join[timeJoin > tail(dt.join[!is.na(RPM)], 1)$timeJoin, `:=` (RPM = 0, Speed.mph = 0, Distance.mile = max(dt.join$Distance.mile, na.rm = TRUE), Watts = 0)]
    # fill in the gaps using interpolation
    dt.join[, RPM := round(na.approx(object = RPM, x = as.POSIXct(timeJoin), na.rm = FALSE), 0)]
    dt.join[, Distance.mile := round(na.approx(object = Distance.mile, x = as.POSIXct(timeJoin), na.rm = FALSE), 3)]
    dt.join[, Watts := round(na.approx(object = Watts, x = as.POSIXct(timeJoin), na.rm = FALSE), 0)]
    dt.join[, Speed.mph := round(na.approx(object = Speed.mph, x = as.POSIXct(timeJoin), na.rm = FALSE), 2)]
    dt.join[, tcxHR := round(na.approx(object = tcxHR, x = as.POSIXct(timeJoin), na.rm = FALSE), 0)]
    return(dt.join[, .(Time = timeJoin, HeartRateBpm = tcxHR, Distance.mile, Speed.mph, Watts, RPM)])
}

# Function reads in the file created by a Schwinn MPower Echelon head unit and preps for use in melding
# Updated 2018-01-06 to deal with multiple stages in the information.  Simple fix with eliminating rows with NA's for RPM
readSchwinnCsv <- function(csvLocation = NULL, bln.ignoreZeroHr = TRUE) {
    stopifnot(!is.null(csvLocation))
    vec.requiredColumns <- c('Stage_Workout..min.', 'Distance.mile.', 'Speed..mph.', 'HR', 'RPM')
    dt.csv <- data.table(read.csv2(file = csvLocation, header = TRUE, sep = ',', stringsAsFactors = FALSE))[!is.na(RPM)]
    if(!all(vec.requiredColumns %in% names(dt.csv))) error("Missing expected Schwinn file column headings.")
    if(!all(as.character(sapply(dt.csv, class)) == c("character", "character", "character", "integer", "integer", "integer"))) error("Column types from Schwinn file are not as anticipated.")
    if(bln.ignoreZeroHr) {
        dt.csv <- dt.csv[HR != 0]
    } else {
        while(nrow(dt.csv[HR == 0]) > 0) {
            dt.csv[, lastHR := shift(HR, 1, type="lead")]
            dt.csv[HR == 0, HR := lastHR]
            dt.csv[, lastHR := NULL]
        }
    }
    dt.csv[, Stage_Workout..min. := as.numeric(Stage_Workout..min.)]
    dt.csv[, Distance.mile. := as.numeric(Distance.mile.)]
    dt.csv[, Speed..mph. := as.numeric(Speed..mph.)]
    setnames(dt.csv, old = c('Stage_Workout..min.', 'Distance.mile.', 'Speed..mph.'), new = c('Stage_Workout.min', 'Distance.mile', 'Speed.mph'))
}

# Function reads the trackpoint section of a Garmin indoor biking activity file (using one created by 
# my Forerunner 935, exported from Garmin Connect as an example).  The only columns extracted are heart
# rate and time.
readTCX <- function(tcxLocation = NULL, timeZone = "America/New_York") {
    stopifnot(!is.null(tcxLocation))
    con <- file(tcxLocation, "r")
    vec.file <- readLines(con, n = -1)
    close(con)
    dt.tcx <- rbindlist(lapply(1:length(vec.file), function(x){
                workingLine <- trim(vec.file[x])
                if(workingLine == "<Trackpoint>") {
                    timeOfTrackpoint <- gsub("<Time>", "", gsub("</Time>", "", trim(vec.file[x + 1])))
                    heartRateBPM <- gsub("<Value>", "", gsub("</Value>", "", trim(vec.file[x + 4])))
                    return(data.table(`time` = timeOfTrackpoint, tcxHR = as.numeric(heartRateBPM)))
                } else {
                    return(data.table())
                }
    }))
    dt.tcx[, `time` := gsub("T", " ", `time`)]
    dt.tcx[, zuluTime := as.POSIXct(`time`, tz = "Europe/London")]
    dt.tcx[, localTime := format(zuluTime, tz = timeZone, usetz = TRUE)]
    if(nrow(dt.tcx[is.na(localTime)]) == nrow(dt.tcx)) {
        error("Problem processing the TCX file - not getting valid timestamps for some reason.")
    } else if(nrow(dt.tcx[is.na(localTime)]) > 0) {
        warning(paste0(nrow(dt.tcx[is.na(localTime)]), " rows in the TCX file have invalid timestamps for some reason.  ", nrow(dt.tcx), ' total rows.'))
    }
    return(dt.tcx[, .(time = localTime, tcxHR)])
}

# This function finds when the Schwinn file (dt.csv) should be joined to the Garmin TCX file (dt.tcx)
# to minimize the amount of heart rate error between the two files.  Ideally there would be a time where the
# error was zero, but that is probably just too easy.  If multiple time steps have the same minimum error
# (super unlikely) it will arbitrarily choose the first one.
minimizeHeartRateError <- function(dt.csv = NULL, dt.tcx = NULL, searchMin = 4) {
    lengthOfCsv <- dt.csv[nrow(dt.csv)]$Stage_Workout.min * 60
    lengthOfTcx <- as.numeric(as.duration(interval(dt.tcx[1]$time, dt.tcx[nrow(dt.tcx)]$time)))
    dt.workingCsv <- copy(dt.csv)
    dt.workingTcx <- copy(dt.tcx)
    dt.workingTcx[, time := as.character(as.POSIXct(time))]
    # run throught the first two minutes of the TCX file to find a match
    dt.error <- rbindlist(lapply(dt.workingTcx[1:(searchMin * 60)]$time, function(tm) {
        dt.workingCsv[, time := as.character(as.POSIXct(tm) + seconds(Stage_Workout.min * 60))]
        dt.merged <- merge(dt.workingCsv, dt.workingTcx, by = 'time')
        if(nrow(dt.merged) > 0) {
            dt.merged[, error := HR - tcxHR]
            return(data.table(time = tm, error = sum(dt.merged$error ^ 2)))
        }
    }))
    dt.return <- dt.error[error == min(dt.error$error, na.rm = TRUE)]
    return(dt.return[1]$time)   
}