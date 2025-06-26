.onAttach <- function(libname, pkgname)
{
  f <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"),
                c("Version", "Date"))
  packageStartupMessage('Successfully loaded eyetools version ', f[1,1],
                        '\n If you use this package, we would love to hear from you: t.beesley@lancaster.ac.uk',
                        '\n See NEWS for details of all changes.')
}