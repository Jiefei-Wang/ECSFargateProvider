#' @import aws.ecx
#' @import methods
#' @import utils
#' @import DockerParallel
#' @import aws.iam
#' @importFrom jsonlite base64_enc base64_dec
NULL


pkgCache <- new.env(parent = emptyenv())
## We use this to set the region globally
## so we do not have to pass the region value in
## each function
pkgCache$region <- NULL
