#' To view a data.frame / data.table in LibreOffice Calc
#'
#' Take a data.frame, save it to a temporary file and open it with the
                                        #spreadsheet viewer of the OS.
#' Requires the package 'openxlsx' to work properly.
#' @param data the data.frame (or data.table)
#' @export
view <- function(data) {
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')

    require(openxlsx)
    temp_file <- paste0(tempfile(), '.xlsx')

    data <- as.data.frame(data)
    write.xlsx(data, file = temp_file)

    invisible(system(paste(open_command, temp_file),
                     ignore.stdout = TRUE, ignore.stderr = TRUE))
}
