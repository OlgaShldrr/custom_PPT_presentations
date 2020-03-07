library(readr)
library(tidyverse)
setwd(here::here())


#This chunk needs to be replaced by references to SQL tables. 
#Importing data
d1 <- read_csv("XXXdatapath.csv")
d2 <- read_csv("XXXdatapath.csv")
d3 <- read_csv("XXXdatapath.csv")
currency_lookup_18 <- read_csv("XXXdatapath.csv")
e1 <- read_csv("XXXdatapath.csv")
e2 <- read_csv("XXXdatapath.csv")
e3 <- read_csv("XXXdatapath.csv")
b1 <- read_csv("XXXdatapath.csv")
schools <- read_csv("XXXdatapath.csv")


d <- d1 %>% 
  rbind(.,d2) %>% 
  rbind(.,d3) %>% 
  separate(., BSCI01c, into = c("Curr", "Curr_full"), sep = "-")

schools <- schools %>% 
  select(UNITID=unitid, country, class_Control, old_region = class_Region, accred = class_Accreditation, name=unitname, membertype, groupname) %>% 
  mutate(
    country = gsub("United States", "the United States", x=country),
    country = gsub("Czech Republic", "the Czech Republic", x=country),
    country = gsub("Netherlands", "the Netherlands", x=country),
    country = gsub("United Arab Emirates", "the United Arab Emirates", x=country),
    country = gsub("United Kingdom", "the United Kingdom", x=country)
  ) %>%  
  separate(., old_region, into = c("Mega_region", "Sub_region"), sep = ":") %>% 
  na.omit()

schools_short <- schools %>% 
  select(UNITID, country, Mega_region, Sub_region)

data <- e1 %>% 
  rbind(.,e2) %>% 
  rbind(.,e3) %>% 
  inner_join(.,d) %>% 
  left_join(., currency_lookup_18) %>% 
  mutate(BSCU02=BSCU02*con_rate) %>% 
  separate(., class_region, into = c("Mega_region", "Sub_region"), sep = ":") 

b1 <- merge(b1, schools_short, by.x = "unitid", by.y="UNITID")
e1 <- merge(e1, schools_short, by.x = "unitID", by.y="UNITID")
data <- merge(data, schools_short, by.x = "unitID", by.y="UNITID", all.x=TRUE)%>% 
  mutate(Sub_region=Sub_region.x) %>% 
  mutate(Mega_region=Mega_region.x) 

  
#controlled sample vector - schools that participated in past three surveys
controlled <- data %>% 
  group_by(unitID) %>% 
  summarise(n=n()) %>% 
  filter(n>=3) %>% 
  select(unitID) %>% 
  as.matrix()

#tables for trend data - split by countries/regions to identify if <6 schools participated
country_table <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, country) %>% 
  summarise(n=n()) %>% 
  as.data.frame()

sub_table <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, Sub_region) %>% 
  summarise(n=n())%>% 
  as.data.frame()

mega_table <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, Mega_region) %>% 
  summarise(n=n())%>% 
  as.data.frame()

country_table_control <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, country, class_Control) %>% 
  summarise(n=n()) %>% 
  as.data.frame()

sub_table_control <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, Sub_region, class_Control) %>% 
  summarise(n=n())%>% 
  as.data.frame()

mega_table_control <- data %>% 
  filter(unitID %in% controlled) %>% 
  group_by(cycleID, Mega_region, class_Control) %>% 
  summarise(n=n())%>% 
  as.data.frame()

country_table_accredited <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, country) %>% 
  summarise(n=n()) %>% 
  as.data.frame()

sub_table_accredited <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, Sub_region) %>% 
  summarise(n=n())%>% 
  as.data.frame()

mega_table_accredited <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, Mega_region) %>% 
  summarise(n=n())%>% 
  as.data.frame()

country_table_accredited_control <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, country, class_Control) %>% 
  summarise(n=n()) %>% 
  as.data.frame()

sub_table_accredited_control <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, Sub_region, class_Control) %>% 
  summarise(n=n())%>% 
  as.data.frame()

mega_table_accredited_control <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, Mega_region, class_Control) %>% 
  summarise(n=n())%>% 
  as.data.frame()

all_accredited_control <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID, class_Control) %>% 
  summarise(n=n())%>% 
  as.data.frame()

all_accredited <- data %>% 
  filter(unitID %in% controlled & class_Accreditation!="Not Accredited") %>% 
  group_by(cycleID) %>% 
  summarise(n=n())%>% 
  as.data.frame()

theme_corporate <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the plot's title
    plot.title = ggplot2::element_text(family=font,
                                       size=20,
                                       face="bold",
                                       color="#006E62",
                                       hjust = 0),
    #This sets the font, size, type and color of text for the plots's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=16,
                                          color = "#006E62",
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_text(family=font,
                                         size=10,
                                         color = "#55565A",
                                         hjust = 0),
    plot.margin = unit(c(1, 0, 1, 1), "lines"),
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#55565A"),
    
    #Axis format
    #This sets the text font, size and color for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the plot.
    axis.title.x = ggplot2::element_blank(),
    #axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=12,
                                      color="#55565A"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. 
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#DBD9D6"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    # Panel Border
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background color and sets the title size of the facet-wrap title to font size 16)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 16,  hjust = 0)
  ) 
}

corporate_colors <- c("#800080",
                  "#800080",
                  "#800080",
                  "#800080",
                  "#800080",
                  "#800080",
                  "#800080")