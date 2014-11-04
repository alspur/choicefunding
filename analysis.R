library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(RColorBrewer)

# download CT grant data at: 
# https://www.csde.state.ct.us/public/dgm/grantreports1/HPayMain.aspx
# select the following before submitting query:
# section a: all years
# section b: all grants
# section c: all
# section d: all grantees
# section 3: all drgs
# submit and download as .csv

# load grant data
grants <- read.csv("data/histpay.csv")

# remove empty column
grants$X <- NULL

# gather year columns into single variable
grants <- grants %>%
    gather(year, amount, pay1990:pay2014)

# clean year column
grants$year <- as.numeric(str_replace_all(grants$year,"pay",""))

# summarise magnet and charter grants by year
# note: magnet funding began in 1995
choice.grants <- grants %>%
    filter(grant_name == "Magnet School" | grant_name == "Charter Schools") %>%
    group_by(grant_name, year) %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    filter(year >1994)

# plot data
ggplot(choice.grants, aes(x=year, y=total, fill = grant_name))+
    geom_bar(stat = "identity", position = position_dodge())+
    scale_fill_manual(values = c("steelblue1", "steelblue4"))+
    facet_grid(.~grant_name)+
    scale_y_continuous(labels = dollar, breaks = seq(0,300*10^6, 50*10^6))+
    labs(x = "Year", y = "Grant Total") +
    ggtitle("Connecticut Grants for Charter and Magnet Schools, 1995-2014")+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,hjust =1, vjust = 1))

#save image
ggsave(filename = "choicegrants.png", width = 7, height = 5, units = "in")
