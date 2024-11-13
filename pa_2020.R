library(tidyverse)

pa_2024 <- read_csv("Data//2024 Election Results//pennsylvania.csv") %>%
  select(`County Name`, `Party Name`, `Votes`) %>%
  pivot_wider(., names_from = "Party Name", values_from = "Votes") %>%
  rename(County = "County Name",
         DemVotes_24 = "Democratic",
         RepVotes_24 = "Republican",
         LibVotes_24 = "Libertarian",
         GreenVotes_24 = "Green Party") %>%
  mutate(County = str_to_upper(County)) %>%
  merge(.,
        read_csv("Data//2024 Election Results//pennsylvania_reg_2024.csv") %>%
          mutate(CountyName = str_to_upper(CountyName),
                 CountyName = sub("LACKWANNA", "LACKAWANNA", CountyName)),
        by.x = "County", by.y = "CountyName", all = TRUE) %>%
  rename(DemReg_24 = "Dem",
         RepReg_24 = "Rep",
         OtherReg_24 = "Other",
         AllReg_24 = "Total Count of All Voters") %>%
  filter(County != "TOTAL") %>%
  drop_na(County) %>%
  mutate("RepDiff_24" = RepVotes_24 / RepReg_24,
         "DemDiff_24" = DemVotes_24 / DemReg_24,
         "AllVotes_24" = DemVotes_24 + RepVotes_24 + LibVotes_24 + GreenVotes_24,
         "Winner_24" = ifelse(RepVotes_24 > DemVotes_24, "Republican", "Democrat"),
         "DemPct_24" = DemVotes_24 / AllVotes_24 * 100,
         "RepPct_24" = RepVotes_24 / AllVotes_24 * 100,
         "Margin_24" = DemPct_24 - RepPct_24) %>%
  select(-`...8`, -CountyID)

pa_2024 %>%
  mutate("DemDiff" = DemReg - DemVotes) %>%
  filter(diff < 30000) %>%
  ggplot(aes(x = diff)) +
  geom_boxplot()

pa_2024 %>%
  mutate("RepDiff" = RepReg - RepVotes) %>%
  #filter(diff < 30000) %>%
  ggplot(aes(x = diff)) +
  geom_boxplot()

pa_2024 %>%
  mutate("RepDiff" = RepVotes / RepReg,
         "DemDiff" = DemVotes / DemReg,
         "AllVotes" = DemVotes + RepVotes + LibVotes + GreenVotes,
         "Winner" = ifelse(RepVotes > DemVotes, "Republican", "Democrat")) %>%
  ggplot(., aes(x = DemDiff, y = RepDiff, color = Winner, size = AllVotes)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1)
  
pa_2020 <- read_csv("Data//2020 Election Results//pennsylvania.csv") %>%
  select(`County Name`, `Party Name`, `Votes`) %>%
  pivot_wider(., names_from = "Party Name", values_from = "Votes") %>%
  rename(County = "County Name",
         DemVotes_20 = "Democratic",
         RepVotes_20 = "Republican",
         LibVotes_20 = "Libertarian") %>%
  mutate(County = str_to_upper(County)) %>%
  merge(.,
        read_csv("Data//2020 Election Results//pennsylvania_reg_2020.csv") %>%
          mutate(County = str_to_upper(County),
                 County = sub("LACKWANNA", "LACKAWANNA", County)),
        by = "County", all = TRUE) %>%
  rename(DemReg_20 = "Democratic",
         RepReg_20 = "Republican",
         OtherReg_20 = "OtherParties",
         AllReg_20 = "AllParties") %>%
  filter(County != "TOTAL") %>%
  mutate("RepDiff_20" = RepVotes_20 / RepReg_20,
         "DemDiff_20" = DemVotes_20 / DemReg_20,
         "AllVotes_20" = DemVotes_20 + RepVotes_20 + LibVotes_20,
         "Winner_20" = ifelse(RepVotes_20 > DemVotes_20, "Republican", "Democrat"),
         "DemPct_20" = DemVotes_20 / AllVotes_20 * 100,
         "RepPct_20" = RepVotes_20 / AllVotes_20 * 100,
         "Margin_20" = DemPct_20 - RepPct_20)

merge(pa_2020, pa_2024, by = "County") %>%
  ggplot(., aes(x = DemDiff_20, DemDiff_24, color = Winner_24, size = AllVotes_24)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_abline(slope = 1)

merge(pa_2020, pa_2024, by = "County") %>%
  ggplot(., aes(x = RepDiff_24, DemDiff_24, color = Winner_24, size = AllVotes_24)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1)

merge(pa_2020, pa_2024, by = "County") %>%
  mutate("MarginDiff" = Margin_24 - Margin_20) %>%
  ggplot(., aes(y = Margin_24, x = Margin_20, size = AllVotes_24, color = Winner_24)) +
  geom_point() +
  scale_size(range = c(0.5, 5)) +
  geom_abline(slope = 1)
  
