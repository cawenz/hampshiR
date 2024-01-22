termFromNum <- function(x){
  oddeven <- ifelse(x %% 2 == 0,0,1)
  season  <- ifelse(oddeven==0, "S", "F")
  year <- ((x-oddeven)/2)+1970
  t <- paste0(year,season)
  t
}
