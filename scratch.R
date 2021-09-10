# Okay to delete anything here

library(psiCGM)
#Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
# Sys.setenv(R_CONFIG_ACTIVE = "local")
# conn_args <- config::get("dataconnection")
# con <- DBI::dbConnect(
#   drv = conn_args$driver,
#   user = conn_args$user,
#   host = conn_args$host,
#   port = conn_args$port,
#   dbname = conn_args$dbname,
#   password = conn_args$password
# )


t <- taster_raw_all %>% transmute(name,productFdcID=as.character(productFdcID),type,barcode,notes)

pids <- t %>% drop_na(productFdcID) %>% distinct(pid =productFdcID) %>% pull(pid)
up <- t %>% filter(productFdcID %in% pids) %>% distinct(name,productFdcID)
up %>% group_by(pid = productFdcID) %>% summarize(n=n(), names = paste0(name)) %>% bind_rows(tibble(pid="a",n=1,names="c"))  %>%
  clipr::write_clip(object_type = c("table"))

name_convert_file <- read_csv(file=file.path(config::get("tastermonial")$datadir, "Tastermonial Name Mapping.csv"), col_types = "cdcc") %>%
  transmute(pid = str_replace_all(pid, "\'",""),names,simpleName)

up %>% group_by(pid = productFdcID) %>% summarize(n=n(), names = paste0(name)) %>% mutate(pid = paste0("\'",pid)) %>%
  clipr::write_clip(object_type = c("table"))
