# closing commands --------------------------------------------------------

# stop the clock
time_taken <- difftime(time1 = Sys.time(),
                       time2 = timer,
                       units = "hours")

# email notification
if (notify_me) {

  msg <- paste0("Model fitting finished in ", round(time_taken, 2), " hours.")
  # something to do with Microsoft365R

}

# if CHANGELOG.md doesn"t exist, create it
if (!file.exists(here("CHANGELOG.md"))) {
  sink(here("CHANGELOG.md"))
  cat("----	----\n")
  sink()
}

# make the changelog
if (change_log) {
  if ("findInFiles" %in% installed.packages()) {

    ## read the old CHANGELOG
    acl <- read.csv(here("CHANGELOG.md"), sep = "\t", header = FALSE)

    ## find new change logs
    cls <- findInFiles::findInFiles(ext = "R*", pattern = "##! CHANGE LOG: ",
                                    output = "tibble")

    ## make it a data.frame
    cls <- data.frame(cls)

    ## fix the table
    cls <- cls[grep("cls <- ", cls$code, invert = TRUE), ]

    ## if there are any new CHANGELOGs, then add them, else do nothing
    if (nrow(cls) > 0) {

      ## prepare messages
      cls$file <- paste0(cls$file, ":", cls$line)
      cls$code <- sapply(1:nrow(cls),
                         function(v)
                           gsub("\\\033\\[31m|\\\033\\[39m",
                                "",
                                toString(cls$code[v])))
      cls <- cls[, -grep("line", colnames(cls))]

      ## make an empty row with line breaks
      cls_ <- data.frame("file" = "----", "code" = "----")

      ## append the old CHANGELOG
      cls <- rbind(cls,
                   cls_,
                   setNames(acl, c("file", "code")))

      ## remove duplication (except line breaks) - ease of reading
      cls <- cls[which(cls$file == "----" | !duplicated(cls)), ]

      ## write it out
      write.table(cls, "CHANGELOG.md",
                  col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t",
                  append = TRUE)

    }

  } else {

    # message
    message("CHANGELOG only works if the \"findInFiles\" package is installed.")

  }

}
