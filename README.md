# Custom PPT presentations

This project features a Shiny app that allows you selecting a university and then produce a presentation that contains graphs pertained to entities that have similar characteristics to the chosen university. The algorithm is placed in a parameterized RMarkdown file that produces PowerPoint presentations for the Shiny App. The actual data is fake, however, the algorithm is available in the Rmd file. Shiny app is available here: https://olga-sholderer.shinyapps.io/custom_PPT_presentations/ 

# Decision tree

The algorithm first evaluates whether there are at least six institutions in the dataset that are in the same country where chosen university is, that are also private or publicly funded and that are also accredited or not. If there are less than six universities, the algorithm looks if there are enough universities without the private/public division, and if not, it moves onto a region where the university is located (e.g. South East Asia) and does the same, and if there are not enough universities in that category, then it moves onto macro-regions. 

# Used packages

The project is using Shiny, gt, RMarkdown and tidyverse.