#' Function to update packages installed from github
#'
#' This function was copied from the link in the references.
#' I only changed the return type.
#' @seealso http://stackoverflow.com/questions/32538052/update-all-packages-from-github
#' @importFrom utils installed.packages
#' @importFrom utils packageDescription
#' @importFrom devtools install_github
#' @export
update_github_pkgs <- function() {

  # check/load necessary packages
  # devtools package
  ## if (!("package:devtools" %in% search())) {
  ##   tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
  ##   on.exit(detach("package:devtools", unload=TRUE))
  ## }

  pkgs <- installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  ## print(github_pkgs)
  res <- lapply(github_pkgs, function(pac) {
      message("\nUpdating ", pac, " from GitHub...")
      
      repo = packageDescription(pac, fields = "GithubRepo")
      username = packageDescription(pac, fields = "GithubUsername")

      install_github(repo = paste0(username, "/", repo))
  })
}
