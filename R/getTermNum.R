getTermNum <- function(x) {
  addsub = ifelse(grepl("F|R",x),1,
            ifelse(grepl("J", x),-0.5,
             ifelse(grepl("A", x),0.5, 0
             )))

  xnum = as.numeric(substr(x, 1, 4))

  y= (((xnum+addsub)-1970)*2)-addsub
  y
}
