library(tidyverse)
library(eurostat)
library(animation)
theme_set(theme_bw())
EUlist <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI",
             "FR","DE","EL","HU",
             "IE","IT","LV","LT",
             "LU","MT","NL","PL",
             "PT","RO","SK","SI",
             "ES","SE","UK")

dat <- get_eurostat("migr_acq") 

## static version of plot
dat %>%
  filter(citizen == "UK") %>%
  filter(sex == "T" & age == "TOTAL" & agedef == "REACH" & geo %in% EUlist) %>%
  filter(geo != "UK") %>%
  filter(time > "2007-01-01") %>% 
  mutate(year = str_sub(time, 1, 4)) %>%
  group_by(time, year) %>% 
  summarise(values = sum(values)) %>%
  ggplot() +
  geom_col(aes(year, values)) +
  geom_text(aes(year, values, label = values), vjust = -0.3) +
  geom_vline(aes(xintercept = 9)) +
  geom_text(aes(x = 8, y = 10000, label = "Brexit vote"), hjust = 0.2) +
  labs(x = "", y = "Number of citizenship acquisitions",
       title = "UK nationals acquiring citizenship in other EU countries, 2008-2017",
       subtitle = "",
       caption = "Source: data from Eurostat, table 'migr_acq', extracted on 9 March 2019")


## store only data that are needed in a separate dataframe
dat2 <- dat1 %>%
  filter(citizen == "UK") %>%
  filter(geo != "EU28") %>%
  filter(geo != "UK") %>%
  filter(time > "2007-01-01") %>%
  mutate(year = str_sub(time, 1, 4)) %>%
  group_by(time, year) %>% 
  summarise(values = sum(values),
            n = n())


## animated version (there is probably a more elegant way using gganimate, but I still need to look into it)
yrs <- c(rep(2017, 3), 2007:2017) # starting with the end year so that the final plot appears first for a while

saveGIF({for (i in yrs) {
  dat2s <- filter(dat2, year <= i)
  print(ggplot(data = dat2) + 
          geom_col(aes(year, values), alpha = 0.2) +
          geom_col(data = dat2s, aes(year, values)) +
          geom_text(data = dat2s, aes(year, values, label = values), vjust = 1.3, colour = "white") +
          # scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
          geom_vline(aes(xintercept = 9)) +
          geom_text(aes(x = 8, y = 10000, label = "Brexit vote"), hjust = 0.2) +
          labs(x = "", y = "Number of citizenship acquisitions",
               title = "UK nationals acquiring citizenship in other EU countries, 2008-2017",
               caption = "Source: data from Eurostat, table 'migr_acq', extracted on 9 March 2019\nOwn calculations, data missing for RO in 2017."))
  }
  }, movie.name = "Citizenship_acquisitions_UK_EU.gif", 
  interval = 0.6, ani.width = 510, ani.height = 315)


