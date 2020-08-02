delaunayn <- local({
EnvSupp <- new.env()
function(p, options="", full=FALSE) {

  ## Check directory writable
  tmpdir <- tempdir()
  ## R should guarantee the tmpdir is writable, but check in any case
  if (file.access(tmpdir, 2) == -1) {
    stop(paste("Unable to write to R temporary directory", tmpdir, "\n",
               "This is a known issue in the geometry package\n",
               "See https://r-forge.r-project.org/tracker/index.php?func=detail&aid=5738&group_id=1149&atid=4552"))
  }
  
  ## Input sanitisation
  options <- paste(options, collapse=" ")

  ## Coerce the input to be matrix
  if (is.data.frame(p)) {
    p <- as.matrix(p)
  }

  ## Make sure we have real-valued input
  storage.mode(p) <- "double"
  
  ## We need to check for NAs in the input, as these will crash the C
  ## code.
  if (any(is.na(p))) {
    stop("The first argument should not contain any NAs")
  }
  
  ## It is essential that delaunayn is called with either the QJ or Qt
  ## option. Otherwise it may return a non-triangulated structure, i.e
  ## one with more than dim+1 points per structure, where dim is the
  ## dimension in which the points p reside.
  if (!grepl("Qt", options) & !grepl("QJ", options)) {
    options <- paste(options, "Qt")
  }
  ret <- .Call("delaunayn", p, as.character(options), tmpdir, PACKAGE="scagnostics2")

  if (nrow(ret$tri) == 1) {
    ret$areas <- 1/factorial(ncol(p))*abs(det(cbind(p[ret$tri,], 1)))
    ret$neighbours <- NULL
  } 
  ## Remove degenerate simplicies
  nd <- which(ret$areas != 0)
  ret$tri <- ret$tri[nd,,drop=FALSE]
  ret$areas <- ret$areas[nd]
  ret$neighbours <- ret$neighbours[nd]

  if (!full) {
    return(ret$tri)
  }
  return(ret)
}})
