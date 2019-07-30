# The scope of the project is to use the data 
# (and perhaps any other relevant and useful data) 
# to study whether there is an increasing trend for Greece 
# or better whether the behavior of Greece
# is different than the rest of the Europe.

##
setwd("~/projects/Visualization")
Sys.getlocale()

## -- Libraries -- 
library(dplyr)
library(readxl)
library(reshape)
library(plotly)
library(ggthemes)
library(lattice)
library(ggalt)
library(tidyr)
library(dplyr)
library(extrafont)
library(CGPfunctions)
library(RColorBrewer)
library(leaflet)
library(googleVis)
library(tidyr)
library(highcharter)
library(grid)
library(ggsubplot)
library(gridExtra)
library(eurostat)
## -- Importing datasets --

## *  OECD dataset *
tbl <- read.table("suicide2.csv",fileEncoding="UCS-2LE",
                    sep = ",",
                    quote = NULL,
                    header = TRUE,
                    skipNul = TRUE,
                    stringsAsFactors = TRUE,
                    as.is = TRUE
                    )
OECD <- as.data.frame(apply(tbl, c(1,2), function(x) gsub("\"", "", x)))
glimpse(OECD)
names(OECD) = c("LOCATION","Indicator","Subject","Measure","Frequency","TIME","Value","Flag_Codes")
OECD$Value = as.numeric(as.character(OECD$Value))
OECD = OECD[,c("LOCATION","TIME","Value")]; head(OECD)

## * Eurostat Dataset * 
eurostat <- read_excel("suicide1.xls",skip = 2)
names(eurostat) = c("Country","2011","2012","2013","2014","2015")
eurostat = as.data.frame(eurostat); head(eurostat); head(eurostat)
melt_eurostat = melt(eurostat, id = ("Country"))
names(melt_eurostat) = c("Country","Year","Value")
melt_eurostat = melt_eurostat[!(melt_eurostat$Country == "European Union (current composition)"),]; head(melt_eurostat)

# print the total number of states:
# - Iceland, Switzerland, Turkey, Norway, Serbia 
unique(melt_eurostat$Country)

#----------------------------------------------------------
## --> 1. PLOT INTERACTIVE MAP FOR LAST YEAR (2015) 
#         Absolute Suicide Number 
#----------------------------------------------------------

df1_2014 = melt_eurostat %>% 
  filter((Year %in% c("2015")),
         !(Country == "European Union (current composition)")) 

df1_2014 = df1_2014[order(-df1_2014$Value),]

data_geomap <- gvisGeoChart(df1_2014,"Country", "Value",options=list( region = "150",width=800, height=700))
suicide_gdp_country_clean2 <- df1_2014 %>% select(Country, Value) # Select the variable
data_table <- gvisTable(df1_2014, options=list(width=400, height=700)) # Show the data table
geomap_table <- gvisMerge(data_geomap, data_table, horizontal=TRUE) # Merged two data set
plot(geomap_table) # Plot
df1_2014$Country

#----------------------------------------------------------
## --> 2. PLOT MAP FOR EVERY YEAR (2011-2012-2013-2014-2015) 
#         Absolute Suicide Number 
#----------------------------------------------------------

# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "0", year = 2013)

unique(geodata$CNTR_CODE) # Not present Ireland, LIECHTENSTEIN
vec = c("AT","BE","BG","CH","CY","CZ","DE","DK","EE",
        "EL","ES","FI","FR","HR","HU" ,"IS","IT",
        "LT","LU","LV","MT","NL","NO","PL","PT","RO","SI","SK","SE","TR","UK")
length(vec)

vec2 =  c("Austria","Belgium","Bulgaria","Switzerland","Cyprus",
           "Czech Republic","Germany","Denmark","Estonia","Greece","Spain",
           "Finland","France","Croatia","Hungary","Iceland","Italy",
           "Lithuania","Luxembourg","Lithuania","Malta",
            "Netherlands","Norway","Poland","Portugual","Romania","Slovenia",
            "Slovakia", "Sweden", "Turkey","United Kingdom")
length(vec2)

geodata = subset(geodata, CNTR_CODE %in% vec)
geodata = geodata[order(geodata$CNTR_CODE),]

geodata$CNTR_CODE <- mapvalues(geodata$CNTR_CODE, 
                               from=vec, 
                               to=vec2)

names(geodata) = c("id","Country","NUTS_NAME","LEVL_CODE","FID","NUTS_ID","geometry","geo") 


map_data <- inner_join(geodata, melt_eurostat, by = "Country")

map_data$bins = as.factor(as.numeric(cut(map_data$Value, 9)))

library(mltools)
map_data$interval <- (bin_data(map_data$Value, bins=9, binType = "explicit"))
levels(map_data$interval)
map_data$interval = revalue(map_data$interval , c("[22, 1194.44444444444)" = "[22, 1194)",
                              "[1194.44444444444, 2366.88888888889)" = "[1194, 2366)",
                              "[2366.88888888889, 3539.33333333333)" = "[2366,3539)",
                              "[3539.33333333333, 4711.77777777778)" = "[3539,4711)",
                              "[4711.77777777778, 5884.22222222222)" = "[4711, 5884)",
                              "[5884.22222222222, 7056.66666666667)" = "[5884, 7056)",
                              "[7056.66666666667, 8229.11111111111)" = "[7056, 8229)",
                              "[8229.11111111111, 9401.55555555555)" = "[8229, 9401)",
                              "[9401.55555555555, 10574]" = "[9401, 10574]" ))

# The code works. Sometime it need to re-run the ggplot to obtain the graphs (usually 2-3 time)
ggplot(data=map_data) + geom_sf(aes(fill=interval),color="dim grey", size=.1) + 
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +
   guides(fill = guide_legend(reverse=T, title = "Legend", direction = "vertical", ncol = 1, keyheight = 1.2 )) +
   labs(title="Total Suicide in Europe - Absolute Number",
       subtitle="Trend from 2011 to 2015",
       caption="(C) EuroGeographics for the administrative boundaries 
                Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
  theme_light() + theme(legend.position=c(0.9,.2)) +
  facet_wrap(~Year)+
  coord_sf(xlim=c(-12,44), ylim=c(35,70))

#----------------------------------------------------------
## 3 . TREND FOR GREECE 
#----------------------------------------------------------
greeceOECD = OECD %>% 
  filter(LOCATION %in% c("GRC")) 

main = ggplot(greeceOECD, aes(x = TIME, y = Value, group = 1, col = "2"))+
       geom_line(size = 1)+
       geom_point(size=3, color = "darkred")+
       theme(legend.position="none",axis.text.x=element_text(angle=90))+
       labs(x = 'Year', y = 'Ratio', 
            title = 'Trend of Suicide in Greece',
            subtitle = "Ratio over 100.000 Person")+
       theme(plot.title = element_text(lineheight=.8, face="italic",hjust = 0.5, size = 23))+
       theme(plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="black"))+
       theme_grey(base_size = 15)
  
main


## Plot of absolute values 

# greece_eurostat = melt_eurostat %>% 
#                     filter(Country %in% c("Greece")) 

# subplot = ggplot(greece_eurostat, aes(x = Year, y = Value, group = 1, col = "2"))+
#        geom_line(size = 1)+
#        geom_point(size=2)+
#        theme(legend.position="none",axis.text.x=element_text(angle=90))+
#        labs(x = 'Year', y = 'Ratio', 
#             title = 'Trend of Suicide in Greece',
#             subtitle = "Ratio over 100.000 Person")+
#        theme(plot.title = element_text(lineheight=.8, face="italic",hjust = 0.5, size = 23))+
#        theme(plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="black"))
# 
# grid.arrange(main, subplot, ncol=2)

#----------------------------------------------------------
## 4. LET'S COMPARE GREECE WITH OTHER COUNTRIES!
#----------------------------------------------------------

  ggplot(OECD, aes(x=TIME, y = Value, group = 1)) +
  geom_line(color="grey") +
  geom_point(color="blue", size = 1) +
  geom_point(data=subset(OECD, LOCATION == "GRC"), colour="orange", size=1.5)+
  facet_wrap(~LOCATION) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  theme(strip.text.x = element_text(size = 13, colour = "black"))+
  labs(x = 'Year', y = 'Ratio', 
            title = 'Trend of Suicide in OECD states',
            subtitle = "Ratio over 100.000 Person")+
       theme(plot.title = element_text(lineheight=.8, face="italic",hjust = 0.5, size = 23))+
       theme(plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="black"))+
       theme(axis.text.y = element_text(size = 10))+
       theme(axis.text.x = element_text(size = 8))


##- Using Trellis
#  dotplot(Value~Year|Country,OECD,
#   	ylab="Cylinders", xlab="Miles per Gallon", 
#    main="Mileage by Cylinders and Gears")

#----------------------------------------------------------
# 5. Cool Graph about trends
#----------------------------------------------------------
newggslopegraph(OECD, TIME,Value, LOCATION, LineThickness = 0.3) +
  labs(title="Suicide per 100.000 person", 
       subtitle="OECD states", 
       caption="source: DataVisualization Project")


#----------------------------------------------------------
# 6. AGAIN TREND in a different way 
#----------------------------------------------------------

### https://www.kaggle.com/gsdeepakkumar/suicide-s-2001-2012
ggplot(data=OECD, aes(x=TIME,y=LOCATION)) + 
  geom_tile(aes(fill =Value ),colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(10,'PiYG'))) + 
  theme(legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.text=element_text(size=13),
        legend.position="right", 
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 20))+ 
        ggtitle("Trend of Suicide Cases by State 2004-2014")



#----------------------------------------------------------
## 7. COMPARING GREECE Vs AVERAGE EUROPE
#----------------------------------------------------------

OECD_spread = spread(OECD,LOCATION,Value)
OECD_spread$TIME = as.character(OECD_spread$TIME)

#Inpute Missing Values with the mean 
for(i in 2:ncol(OECD_spread)){
  OECD_spread[is.na(OECD_spread[,i]), i] <- mean(OECD_spread[,i], na.rm = TRUE)
}
OECD_spread

# Take the avg for each country 
average = rowMeans(OECD_spread[,2:ncol(OECD_spread)]); average

gr = OECD %>% 
     filter(LOCATION == "GRC")

df = as.data.frame(cbind(OECD_spread[,"TIME"],average, gr$Value))
names(df) = c("TIME","Avg","Greece"); df
df$Avg = as.numeric(as.character(df$Avg))
df$Greece = as.numeric(as.character(df$Greece))

h <- highchart() %>% 
   hc_xAxis(categories = df$TIME) %>% 
   hc_add_series(name = "Avg", 
                 data = df$Avg) %>%
   hc_add_series(name = "Greece", 
                data = df$Greece) %>% 
   hc_title(text = "Greece vs Average suicide rate for  EU countries",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "Period: 2011 to 2015",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Data Visualization Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

h


#----------------------------------------------------------
## 8. AVERAGE RATE FOR ALL THE COUNTRIES 
#----------------------------------------------------------

OECD$LOCATION = as.character(OECD$LOCATION)
OECD$TIME  = as.numeric(as.character(OECD$TIME))

country <- OECD %>%
  group_by(LOCATION) %>%
  summarize(suicide_per_100k = mean(Value)) %>%
  arrange(desc(suicide_per_100k))

country$LOCATION <- factor(country$LOCATION, 
                           ordered = T, 
                           levels = rev(country$LOCATION))

europe_average = mean(country$suicide_per_100k); europe_average

ggplot(country, aes(x = LOCATION, y = suicide_per_100k)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = europe_average, linetype = 2, color = "orange", size = 1.5) +
  theme(axis.text.y = element_text(size=14), axis.text.x = element_text(size = 12),
        plot.title = element_text(size =20))+
  labs(title = "European suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") +
       coord_flip() +
       scale_y_continuous(breaks = seq(0, 45, 2)) + 
       theme(legend.position = "bottom")


#----------------------------------------------------------
## 9. LINEAR TRENDS 
#----------------------------------------------------------

library(broom)
# https://www.kaggle.com/lmorgan95/r-suicide-rates-in-depth-stats-insights

country_year1 <- OECD %>%
  group_by(LOCATION, TIME) %>%
  summarize(suicide_per_100k = Value)

country_year_trends1 <- country_year1 %>%
  ungroup() %>%
  nest(-LOCATION) %>% # format: country, rest of data (in list column)
  mutate(model = map(data, ~ lm(suicide_per_100k ~ TIME, data = .)), # for each item in 'data', fit a linear model
         tidied = map(model, tidy)) %>% # tidy each of these into dataframe format - call this list 'tidied'
  unnest(tidied)


country_year_sig_trends1 <- country_year_trends1 %>%
  filter(term == "TIME") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  filter(p.adjusted < .05) %>%
  arrange(estimate)

country_year_sig_trends1$country <- factor(country_year_sig_trends1$LOCATION, 
                                          ordered = T, 
                                          levels = country_year_sig_trends1$LOCATION)

# plot
ggplot(country_year_sig_trends1, aes(x=country, y=estimate, col = estimate)) + 
  geom_point(stat='identity', size = 4) +
  geom_hline(yintercept = 0, col = "grey", size = 1) +
  scale_color_gradient(low = "green", high = "red") +
  geom_segment(aes(y = 0, 
                   x = country, 
                   yend = estimate, 
                   xend = country), size = 1) +
   theme(axis.text.y = element_text(size=14), axis.text.x = element_text(size = 12),
        plot.title = element_text(size =20))+
  labs(title="Change per year (Suicides per 100k)", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Country", y = "Change Per Year (Suicides per 100k)") +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), limits = c(-1.5, 1.5)) +
  theme(legend.position = "none") +
  coord_flip()


#----------------------------------------------------------
## 10. Curve for Countries which registred significant trend
#----------------------------------------------------------

top8 <- tail(country_year_sig_trends1$LOCATION, 8)

country_year1 %>%
  filter(LOCATION %in% top8 )%>%
  ggplot(aes(x = TIME, y = suicide_per_100k, col = LOCATION)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ LOCATION) + 
  theme(legend.position = "none", plot.title = element_text(size =20)) + 
  labs(title="8 Steepest  Trends", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Year", 
       y = "Suicides per 100k")



#----------------------------------------------------------
## 11. Total Suicide Neighb country
#----------------------------------------------------------
### https://rkabacoff.github.io/datavis/Interactive.html#leaflet

eurostat_spread = spread(melt_eurostat,Country, Value)
# generate graph
h <- highchart() %>% 
  hc_xAxis(categories = eurostat_spread$Year) %>% 
  hc_add_series(name = "Turkey", 
                data = eurostat_spread$Turkey) %>%
  hc_add_series(name = "Bulgaria", 
                data = eurostat_spread$Bulgaria) %>%
  hc_add_series(name = "Serbia", 
                data = eurostat_spread$Serbia) %>%
  hc_add_series(name = "Romania", 
                data = eurostat_spread$Romania) %>% 
  hc_add_series(name = "Greece", 
                data = eurostat_spread$Greece)  %>% 
  hc_title(text = "Total Suicide in Neighboord Countries",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "2011 to 2015",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Data Visualization Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)
 
h


#----------------------------------------------------------
## 12. DUMBELL Plot 
#----------------------------------------------------------

# Inpute media
OECD_spread2 = spread(OECD,TIME,Value)
ind = which(is.na(OECD_spread2), arr.ind=TRUE)
OECD_spread2[ind] = rowMeans(OECD_spread2[,2:ncol(OECD_spread2)],  na.rm = TRUE)[ind[,1]]

dumbell = OECD_spread2[,c("LOCATION","2004","2014")]
names(dumbell) = c("Country","y2004","y2014"); dumbell

diff= round(as.numeric(sprintf("%G", as.numeric((dumbell$y2014-dumbell$y2004)))),2)
dumbell = cbind(dumbell, diff)
dumbell = dumbell[order(-dumbell$diff),]

gg <- ggplot()

gg <- gg + geom_segment(data=dumbell, aes(y=reorder(Country,diff), yend=Country, x=0, xend=40), color="#b2b2b2", size=0.15)

gg <- gg + geom_dumbbell(data=dumbell, aes(y=reorder(Country,diff), x=y2004, xend=y2014),
                         size = 1.2,
                         size_x = 3, 
                         size_xend = 3,
                         colour = "grey", 
                         colour_x = "darkgreen", 
                         colour_xend = "violet")
# difference column
gg <- gg + geom_rect(data=dumbell, aes(xmin=37, xmax=40, ymin=-Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=dumbell, aes(label=diff, y=Country, x=38.5), fontface="bold", size=4, family="Helvetica")
gg <- gg + geom_text(data=dumbell, aes(x=38.5, y=22.9, label="DIFF"),
                     color="black", size=3.7, vjust=-2, fontface="bold", family="Helvetica")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, 40))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="Change in the ratio of suicide per 100.000 population",
                subtitle="From 2004 to 2014",
                caption="Source: OECD Suicide dataset ")
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(plot.title=element_text(face="italic"))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg <- gg + theme(axis.text.y = element_text(size = 15),
                 axis.text.x = element_text(size = 8),
                 plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 13))

gg


#----------------------------------------------------------
# 13 FINAL MAP (DONE NOT DISPLAYED) + FACET WRAP (Not  Done due to temporal limitation) 
#https://rpubs.com/beancounter/430586
#---------------------------------------------------------
# df2_2014 = OECD %>% 
#   filter(TIME %in% c("2014"))%>% 
#   select(c("LOCATION","Value"))
# 
# as.vector(unique(df2_2014$LOCATION))
# df2_2014$LOCATION = as.character(df2_2014$LOCATION)
# as.vector(unique(df2_2014$LOCATION))
# df2_2014$LOCATION[df2_2014$LOCATION ==  c("AUT","BEL","CZE","DNK", 
#                             "FIN","DEU","GRC","HUN" ,
#                             "LUX","NLD","POL","PRT" ,
#                             "SVK","ESP","SWE","EST", 
#                             "SVN","LVA","LTU")]  =  c("Austria","Belgium",
#                                                        "Czech Republic","Denmark",
#                                                        "Finland","Germany", "Greece",
#                                                         "Hungary","Luxembourg",
#                                                         "Netherlands","Poland",
#                                                         "Portugual","Slovakia","Spain",
#                                                          "Sweden","Estonia","Switzerland",
#                                                           "Latvia","Lithuania")
# 
# # data_geomap <- gvisGeoChart(df2_2014,"LOCATION", "Value",options=list(region = "150", width=800, height=700))
# # suicide_gdp_country_clean2 <- df2_2014 %>% select(LOCATION, Value) # Select the variable
# # data_table <- gvisTable(df2_2014, options=list(width=400, height=400)) # Show the data table
# # geomap_table <- gvisMerge(data_geomap, data_table, horizontal=TRUE) # Merged two data set
# # plot(geomap_table) # Plot
  

## --- ADDING OTHER DATASET ---

# Unemployment rate
unemp = read.csv("Unemployment.csv")
head(unemp)
unemp = unemp[,c("LOCATION","TIME","Value")]
nrow(unemp)
unique(unemp$LOCATION)
# Tax 
tax = read.csv("tax.csv")
head(tax)
tax = tax[,c("LOCATION","TIME","Value")]
nrow(tax)


one = unique(tax$LOCATION);
due = unique(unemp$LOCATION) ; 
tre = unique(OECD$Country)

as.vector(one)
as.vector(tre)

# Filtering 
unemp = unemp %>% 
        filter((TIME != "2015") & (LOCATION != "OECD"))
tax = tax %>% 
        filter((TIME != "2015" & LOCATION != "OAVG"))

df_merge1 = df_merge1[order(df_merge1$Country),]

m = merge(OECD, tax, by = c("LOCATION", "TIME"), all = FALSE)
df_merge =  merge(m, unemp, by = c("LOCATION", "TIME"), all = FALSE)

names(df_merge) = c("LOCATION","TIME","SUICIDE","TAX","UNEMP")

##  CORRELATION 

all = aggregate(df_merge[, 3:5], list(df_merge$TIME), mean)

# generate graph
h <- highchart() %>% 
  hc_xAxis(categories = all$Group.1) %>% 
  hc_add_series(name = "Suicide", 
                data = all$SUICIDE) %>%
  hc_add_series(name = "Taxation", 
                data = all$TAX) %>%
  hc_add_series(name = "Unemployment", 
                data = all$UNEMP) %>% 
  hc_title(text = "Average Trend for Suicide,Taxation,Unemployment for EU countries",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "2004 to 2014",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Data Visualization Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)
h

# Greece situation 
greece = df_merge %>% 
         filter(LOCATION == "GRC")

# generate graph
h <- highchart() %>% 
  hc_xAxis(categories = greece$TIME) %>% 
  hc_add_series(name = "Suicide", 
                data = greece$SUICIDE) %>%
  hc_add_series(name = "Taxation", 
                data = greece$TAX) %>%
  hc_add_series(name = "Unemployment", 
                data = greece$UNEMP) %>% 
  hc_title(text = "Trend for Suicide,Taxation,Unemployment for Greece",
            margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "2004 to 2014",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Data Visualization Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)
h



## TREEMAP
library(treemap)

tax_2014 <- tax %>%
        filter(TIME == 2014)

tm <- treemap(tax_2014, 
              index = c('LOCATION'), 
              vSize = 'Value', 
              border.col = 'darkgrey',
              border.lwds = 1,
              title = "GDP in 2013",
              draw = FALSE)

highchart() %>%
    hc_add_series_treemap(tm, layoutAlgorithm = 'squarified') %>%
    hc_title(text = 'Europe tax in 2014')

unemp_2014 <- unemp %>%
        filter(TIME == 2014)

un <- treemap(unemp_2014, 
              index = c('LOCATION'), 
              vSize = 'Value', 
              border.col = 'darkgrey',
              border.lwds = 1,
              title = "GDP in 2013",
              draw = FALSE)

highchart() %>%
    hc_add_series_treemap(un, layoutAlgorithm = 'squarified') %>%
    hc_title(text = 'Europe unemployment in 2014')

suicide_2014 <- OECD %>%
        filter(TIME == 2014)

sui <- treemap(suicide_2014, 
              index = c('LOCATION'), 
              vSize = 'Value', 
              border.col = 'darkgrey',
              border.lwds = 1,
              title = "GDP in 2013",
              draw = FALSE)

highchart() %>%
    hc_add_series_treemap(sui, layoutAlgorithm = 'squarified') %>%
    hc_title(text = 'Europe Suicide rate in 2014')


###
library(FactoMineR)
library(factoextra)

df_merge$TIME = as.numeric(as.character(df_merge$TIME))

#2004
pca_data <- df_merge %>% group_by(LOCATION, TIME)


pca_2004 <- pca_data %>% 
            filter(TIME == 2004) %>% 
            select(LOCATION,SUICIDE,TAX,UNEMP) %>% 
            data.frame(row.names = "LOCATION") 
pca_2004 = pca_2004[,2:4]

res_2004 <- PCA(pca_2004, scale.unit = T)

library(factoextra)
fviz_pca_biplot(res_2004, labelsize = 4, repel = T,
               title = "PCA - Biplot 2004") + theme_economist()

#2014
pca_data <- df_merge %>% group_by(LOCATION, TIME)


pca_2014 <- pca_data %>% 
            filter(TIME == 2014) %>% 
            select(LOCATION,SUICIDE,TAX,UNEMP) %>% 
            data.frame(row.names = "LOCATION") 
pca_2014 = pca_2014[,2:4]

res_2014 <- PCA(pca_2004, scale.unit = T)

library(factoextra)
fviz_pca_biplot(res_2014, labelsize = 4, repel = T,
               title = "PCA - Biplot 2014") + theme_economist()



plt_unemp = df_merge %>%
              ggplot(aes(x = SUICIDE, 
                   y = UNEMP,
                   size = SUICIDE,
                   frame = TIME,
                   group = LOCATION)) +
        geom_point(aes(alpha = 1), colour='#1050DC') +
        scale_x_log10() + 
        scale_alpha_continuous(range = c(0.4, 0.8)) + 
        labs(title= "Suicide rates grouped by country wrt Unemployment from 2004 to 2014") +
        geom_hline(aes(yintercept = mean(UNEMP)), color="gray") +
        theme_minimal()

pltly_unemp = ggplotly(plt_unemp);pltly_unemp

plt_tax = df_merge %>%
              ggplot(aes(x = SUICIDE, 
                   y = TAX,
                   size = SUICIDE,
                   frame = TIME,
                   group = LOCATION)) +
        geom_point(aes(alpha = 1), colour='#1050DC') +
        scale_x_log10() + 
        scale_alpha_continuous(range = c(0.4, 0.8)) + 
        labs(title= "Suicide rates grouped by country from 1985 to 2016") +
        geom_hline(aes(yintercept = mean(TAX)), color="gray") +
        theme_minimal()

pltly_tax = ggplotly(plt_tax);pltly_tax





























