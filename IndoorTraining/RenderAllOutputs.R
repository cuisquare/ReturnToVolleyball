#render all outputs
library(rmarkdown)
library(bookdown)

render(input = "./IndoorTraining/LSKVC_IndoorTraining_RiskAssessment.Rmd",output_format = "all" #pdf_document"
       )
render(input = "./IndoorTraining/LSKVC_IndoorTraining_PlayersRules.Rmd",output_format = "all"
       )
render(input = "./IndoorTraining/LSKVC_IndoorTraining_ClubRequirements.Rmd",output_format = "all"
       )

#?render

