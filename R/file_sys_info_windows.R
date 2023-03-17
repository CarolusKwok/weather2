#' Get the Size and FreeSpace of Windows machine
#'
#' @return
#' @export
#'
#' @examples file_sysinfo_windows()
file_sysinfo_windows = function(){
  disks = system("wmic logicaldisk get size,freespace,caption", intern = TRUE)

  disks = read.fwf(textConnection(disks[1:(length(disks)-1)]),
                   widths=c(9, 13, 13), strip.white=TRUE, stringsAsFactors=FALSE)

  colnames(disks) = disks[1,]
  disks = disks[-1,]
  rownames(disks) = NULL

  return(disks)
}
