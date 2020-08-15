library(tidyverse)
library(dplyr)
library(stringr)
library(kableExtra)

get_set_exposure_VEAsRef <- function(court_side,
                                     nb_per_team,
                                     set_length,
                                     safety_factor) {
  #Volleyball England Study Data
  ref_nb_players <- 6
  ref_set_length <- 20
  ref_court_side <- 9
  ref_half_court_area <- ref_court_side^2
  ref_players_density <- ref_nb_players/ref_half_court_area
  ref_measured_exposure <- 1
  ref_exp_weighted <- ref_measured_exposure/ref_players_density/ref_set_length
  
  exposure <- (nb_per_team/court_side^2)*set_length*ref_exp_weighted*safety_factor
  
  return(exposure)
}

get_max_nb_sets_per_player <- function(court_side, 
                                       nb_per_team,
                                       set_length,
                                       safety_factor,
                                       max_allowed_exp) {
  
  max_nb_sets <- max_allowed_exp / get_set_exposure_VEAsRef(court_side, 
                                                            nb_per_team,
                                                            set_length,
                                                            safety_factor)
  
  return(max_nb_sets)
}

get_case_desc <- function(court_side, 
                          nb_per_team,
                          set_length,
                          max_allowed_exp,
                          nb_tot,
                          nb_courts,
                          caseprefix) {
  paste(caseprefix,nb_per_team,"X",nb_per_team," ",set_length,"m ",max_allowed_exp,"exp ",sep="")
}

get_new_play_case_input <- function(court_side, 
                                    nb_per_team,
                                    set_length,
                                    safety_factor,
                                    max_allowed_exp,
                                    nb_tot,
                                    nb_courts,
                                    caseprefix)  {
  case_desc <- get_case_desc(court_side, 
                             nb_per_team,
                             set_length,
                             max_allowed_exp,
                             nb_tot,
                             nb_courts,
                             caseprefix)
  output <- data.frame(case_desc = case_desc,
                       court_side=court_side,
                       nb_per_team=nb_per_team,
                       set_length=set_length,
                       safety_factor=safety_factor,
                       max_allowed_exp=max_allowed_exp,
                       nb_tot=nb_tot,
                       nb_courts=nb_courts)
}


get_pretty_length <- function(minutes) {
  nb_hours <- floor(minutes/60)
  nb_minutes <- round(minutes-60*nb_hours)
  output <- paste(nb_hours,"h",str_pad(nb_minutes, 2, pad = "0"),"m",sep="")
  return(output)
}



get_all_new_play_case_input <- function(court_side_list,
                                nb_players_team_list,
                                set_duration_list,
                                safety_factor_list,
                                max_exposure_list,
                                nb_players_list,
                                nb_courts_list,
                                prefix_list) {
  play_cases <- data.frame() 
  
  for (court_side in court_side_list) {
    for (nb_player_team in nb_players_team_list) {
      for (set_duration in set_duration_list) {
        for (safety_factor in safety_factor_list) {
          for(max_exposure in max_exposure_list) {
            for(nb_players in nb_players_list) {
              for (nb_courts in nb_courts_list) {
                for (prefix in prefix_list) {
                  play_cases <- play_cases  %>%  
                    bind_rows(get_new_play_case_input(court_side,
                                                      nb_player_team,
                                                      set_duration,
                                                      safety_factor,
                                                      max_exposure,
                                                      nb_players,
                                                      nb_courts,
                                                      prefix))
                }
              }
            } 
          }
        }
      }
    }
  }
  return(play_cases)
}




add_calculated_values <- function(play_cases_input) {
  max_max_playtime <- 7*60 #max player time in minutes
  max_session_length <- 7*60 #max session length in minutes
  
  play_cases_output <- play_cases_input %>%
    filter(nb_tot>=2*nb_per_team*nb_courts) %>%
    mutate(set_exp = get_set_exposure_VEAsRef(court_side,
                                              nb_per_team,
                                              set_length,
                                              safety_factor)) %>%
    mutate(max_nb_sets = get_max_nb_sets_per_player(court_side,
                                                    nb_per_team,
                                                    set_length,
                                                    safety_factor,
                                                    max_allowed_exp)) %>%
    mutate(max_playtime_notpretty = max_nb_sets * set_length) %>%
    filter(max_playtime_notpretty<max_max_playtime) %>%
    mutate(max_playtime = get_pretty_length(max_playtime_notpretty)) %>%
    mutate(session_length_notpretty = max_nb_sets * set_length*nb_tot/(2*nb_per_team*nb_courts)) %>%
    filter(session_length_notpretty<max_session_length) %>%
    mutate(session_length= get_pretty_length(session_length_notpretty)) %>%
    arrange(desc(max_playtime_notpretty),session_length_notpretty,nb_courts) %>%
    select(-max_playtime_notpretty) %>%
    select(-session_length_notpretty) #%>% select(-safety_factor)
  
  return(play_cases_output)
}

get_kable_bottom_caption <- function(x,caption) {
  library(knitr)
  library(kableExtra)
  library(xtable)
  
  xtable2kable <- function(x) {
    out <- capture.output(print(x, table.placement = NULL))[-(1:2)]
    out <- paste(out, collapse = "\n")
    structure(out, format="latex", class = "knitr_kable")
  }
  
  xtable(x,caption) %>%
    xtable2kable()
}

get_tableready_stringfromcharlist <- function(charlist) {
  list_marker <- "• "
  Encoding(list_marker) <- "UTF-8"

  if (length(charlist) > 1) {
    charlist <- paste(list_marker,charlist,collapse="\n")
  }
  
  return (charlist)
}

get_new_Hazard <- function(Hazard,
                           Persons_Affected,
                           Likelihood_Before,
                           Severity_Before,
                           Controls,
                           Likelihood_After,
                           Severity_After) {
  
  Hazard <- Hazard %>% get_tableready_stringfromcharlist()
  Persons_Affected <- Persons_Affected %>% get_tableready_stringfromcharlist()
  Controls <- Controls %>% get_tableready_stringfromcharlist()
  
  new_Hazard <- data.frame(Hazard=Hazard,
                           Persons_Affected=Persons_Affected,
                           Likelihood_Before=Likelihood_Before,
                           Severity_Before=Severity_Before,
                           Risk_Before=0,
                           Controls=Controls,
                           Likelihood_After=Likelihood_After,
                           Severity_After=Severity_After,
                           Risk_After=0)
  
  new_Hazard <- new_Hazard %>% 
    mutate(Risk_Before  = Likelihood_Before * Severity_Before,
           Risk_After = Likelihood_After * Severity_After)
  
  new_Hazard <- new_Hazard %>%
    mutate_all(linebreak)

  
  real_names <- names(new_Hazard) #saving just in case
  display_names <- c("Hazard",
                     "Persons Affected",
                     "L",
                     "S",
                     "R",
                     "Controls",
                     "L",
                     "S",
                     "R"
  )
  
  names(new_Hazard) <- display_names
  
  return(new_Hazard)
}

add_new_hazard <- function(HDF,
                           Hazard,
                           Persons_Affected,
                           Likelihood_Before,
                           Severity_Before,
                           Controls,
                           Likelihood_After,
                           Severity_After) {
  
  new_Hazard <- get_new_Hazard(Hazard,
                               Persons_Affected,
                               Likelihood_Before,
                               Severity_Before,
                               Controls,
                               Likelihood_After,
                               Severity_After)
  
  HDF <- HDF %>%
    bind_rows(new_Hazard)
  
  display_names <- c("Hazard",
                     "Persons Affected",
                     "L",
                     "S",
                     "R",
                     "Controls",
                     "L",
                     "S",
                     "R"
  )
  
  names(HDF) <- display_names
  
  return(HDF)
}