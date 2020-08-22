options(digits = 2)   # report 3 significant digits

source("../Functions.R")

shortnames <- c("CaseDesc",
                "LC",
                "NTM",
                "DS",
                "SF",
                "ET",
                "NTT",
                "NC",
                "ES",
                "NSM",
                "DP",
                "DS")

longnames <- c("Cases Description",
               "court length (metres) - Input",
               "Nb players per team - Input",
               "Set duration (minutes) - Input",
               "Safety Factor",
               "Max Session Virus Exposure (minutes) - Input",
               "Nb players total - Input",
               "Nb courts - Input",
               "Set Virus Exposure (minutes) - Calculated",
               "Max Number of Sets - Calculated",
               "Playtime duration per player - Calculated",
               "Session duration (no breaks) - Calculated")  

definitions <- data.frame(Abbrevation = shortnames, Definition = longnames)

max_exposure <- 10
safety_factor <- 1.5


#21 players case
play_cases_21 <- get_all_new_play_case_input(court_side_list= 9,
                                             nb_players_team_list = c(4,5,6),
                                             set_duration_list = 20,
                                             safety_factor_list=safety_factor,
                                             max_exposure_list=max_exposure,
                                             nb_players_list=21,
                                             nb_courts_list=c(1),
                                             prefix_list = "LSKVC ") %>%
  add_calculated_values()
#rename columns
colnames(play_cases_21) <- shortnames

#16 players case
play_cases_16 <- get_all_new_play_case_input(court_side_list= 9,
                                             nb_players_team_list = c(4,5,6),
                                             set_duration_list = 20,
                                             safety_factor_list=safety_factor,
                                             max_exposure_list=max_exposure,
                                             nb_players_list=16,
                                             nb_courts_list=c(1),
                                             prefix_list = "LSKVC ") %>%
  add_calculated_values()
#rename columns
colnames(play_cases_16) <- shortnames

#12 players case
play_cases_12 <- get_all_new_play_case_input(court_side_list= 9,
                                                       nb_players_team_list = c(4,5,6),
                                                       set_duration_list = 20,
                                                       safety_factor_list=safety_factor,
                                                       max_exposure_list=max_exposure,
                                                       nb_players_list=12,
                                                       nb_courts_list=c(1),
                                                       prefix_list = "LSKVC ") %>%
  add_calculated_values()
#rename columns
colnames(play_cases_12) <- shortnames


#all configs
play_cases_all <- get_all_new_play_case_input(court_side_list= 9,
                                                      nb_players_team_list = c(4,5,6),
                                                      set_duration_list = 20,
                                                      safety_factor_list=safety_factor,
                                                      max_exposure_list=max_exposure,
                                                      nb_players_list=c(12,16,21),
                                                      nb_courts_list=c(1),
                                                      prefix_list = "LSKVC ") %>%
  add_calculated_values()
#rename columns
colnames(play_cases_all) <- shortnames
VE_case <- add_calculated_values(get_new_play_case_input(9,6,20,1,5,12,1,"VE STUDY "))
colnames(VE_case) <- shortnames

play_cases_all <- bind_rows(VE_case,play_cases_all)
colnames(play_cases_all) <- shortnames
