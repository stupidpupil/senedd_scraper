senedd_constituencies <- function(){
  
  sc_path <- system.file("extdata", "senedd_constituencies.csv")
  local_sc_path <- "inst/extdata/senedd_constituencies.csv"

  if(file.exists(local_sc_path)){
    sc_path <- local_sc_path
  }

  read_csv(sc_path, col_types="ccccccc")
}