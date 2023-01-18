#install.packages("quarto")
library(quarto)

quarto_render(input="syllabus.qmd",output_format = "pdf")
quarto_render(input="syllabus.qmd",output_format = "html")
