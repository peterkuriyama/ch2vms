#' Load West Coast Logbook Data

#' Function to load West Coast Logbook data

#' @param Parameter there aren't any

#' @export
#' @examples
#' load_wc_logbook()


#--------------------------------------------------------------------------------
#Load and Format Data
load_wc_logbook <- function(){
  #Load expanded West Coast Data
  load('all_data/wc_data_expanded_tows.Rdata')

  # wc_data_expanded <- wc_data
  wc_data_orig <- wc_data

  ####################################################
  # #Load Port data and rename
  port_codes <- read.csv("all_data/port_codes.csv", stringsAsFactors = FALSE)
  port_codes <- plyr::rename(port_codes, c('Pcid' = 'text_id', 'Agid' = 'state',
    'Agency' = 'number_id', 'Port.Agency.Description' = 'description'))

  # #identify missing ports
  # #Add in ports that I know are missing
  added_ports <- data.frame(text_id = c("", "", "", "", "", "", "", "", "", "", "", ""),
                     state = c("O", "O", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"),
                     number_id = c("02", "46", "WES", "ORE", "BEL", "N.B", "SEA", "BLA",
                                   "P.A", "ILW", "ANA", "CAL"),
                     description = c("ASTORIA", "dont know", "WESTPORT", "dont know ORE", 
                                     "BELLINGHAM", "NORTH BEND", "SEATTLE", "BLAINE", 
                                     "PORT ANGELES", "ILWACO", "ANACORTES", "dont know CAL"))
  port_codes <- rbind(port_codes, added_ports)
  port_codes$state_port <- paste(port_codes$state, port_codes$number_id)

  #Add dport and rport codes
  wc_data$state_dport <- paste(wc_data$agid, wc_data$dport)
  wc_data$state_rport <- paste(wc_data$agid, wc_data$rport)

  ##rport 
  test <- data.frame("state_port" = paste(wc_data$agid, wc_data$rport))
  test$state_port <- as.character(test$state_port)
  thing <- inner_join(x = test, y = port_codes[, c('state_port', 'description')], by = 'state_port')
  wc_data$rport_desc <- thing$description

  ##dport
  test <- data.frame("state_port" = paste(wc_data$agid, wc_data$dport))
  test$state_port <- as.character(test$state_port)
  thing <- inner_join(test, port_codes[, c('state_port', 'description')], by = 'state_port')
  wc_data$dport_desc <- thing$description


  ####################################################
  #Add in ratio of apounds to hpounds
  #ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
  wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

  return(wc_data)  
}
