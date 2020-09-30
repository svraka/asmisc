tlmgr_install_svraka_pkgs <- function() {
  pkgs <- readLines(system.file(package = "asmisc", "pkgs-svraka.txt"))
  tinytex::tlmgr_install(pkgs)
}
