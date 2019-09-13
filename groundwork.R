library(openair)
library(tidyverse)
#base <- importMeta()
list1 <- base$code #All the sites
sites <- list1[seq(from=1,to = 219, by = 20)]#grabbing 11 sites
print(sites)

year_05 <- importAURN(site = sites, year = 2005)
year_17 <- importAURN(site = list1, year = 2017)
year_05_1 <- merge(year_05,base)
year_17_1 <- merge(year_17,base)
year_17 %>% transmute(hours = hour(date), code, no2) %>%  group_by(hours,code) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
geom_line(aes(color = code))


year_17 %>% transmute(hours = month(date), site, no2) %>%  group_by(hours,site) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
  geom_line(aes(color = site))

year_05 %>% transmute(hours = hour(date), code, no2) %>%  group_by(hours,code) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
  geom_line(aes(color = code))


year_05 %>% transmute(hours = month(date), code, no2) %>%  group_by(hours,code) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
  geom_line(aes(color = code))

year_05 %>% transmute(Weeks = week(date), code, no2) %>%  group_by(Weeks,code) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = Weeks, y = no2)) +
  geom_line(aes(color = code))

year_17 %>% group_by(date) %>% ggplot(aes(x = date, y = no2)) +
  geom_point()

year_05_1 %>% transmute(hours = hour(date), site.type, no2) %>%  group_by(hours,site.type) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
  geom_line(aes(color = site.type))

year_17_1 %>% transmute(hours = hour(date), site.type, no2) %>%  group_by(hours,site.type) %>% summarise(no2 = mean(no2, na.rm = TRUE)) %>% ggplot(aes(x = hours, y = no2)) +
  geom_line(aes(color = site.type))
UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = base, aes(x = longitude, y = latitude)) + 
  geom_polygon() +
  coord_map()
UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon() +
  coord_map()

am_data2 <- get_eurostat(id = "hlth_cd_apr") %>% filter(time == max(time), unit == "RT") %>% mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) ## stratify data into quintiles

mapdata <- merge_eurostat_geodata(am_data2, resolution = "20") ## add to mapping function

## amenable mortality European map
ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cat), colour = "grey", size = .1) +
  labs(title = "Amenable mortality by NUTS-3 region, 2014",
       subtitle = "Mortality rate per 100,000", 
       fill = "Amenable mortality \nrate", 
       caption = "Source: EUROSTAT") +
  theme_void() +
  scale_fill_brewer(palette = "RdYlBu") + 
  coord_map(xlim = c(-12, 44), ylim = c(35,67))

NLD <- readRDS("gadm36_GBR_3_sp.rds")
intitial <- ggplot()+
  geom_polygon(data = NLD, 
               aes(x = long, y = lat, group = group)) +
  coord_fixed() +  geom_point(data = year_05_1, aes(longitude,latitude),col = site) +theme_bw()
