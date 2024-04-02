library(tidyverse)
library(readxl)

fomo0 <- read_excel("Exams/2024/Lab/Anth 260 Spring 2024_April 2, 2024_08.01.xlsx")

key <- tibble(
  Q = names(fomo0),
  Text = as.character(fomo0[1,])
)

fomo0 <- read_excel("Exams/2024/Lab/Anth 260 Spring 2024_April 2, 2024_08.01.xlsx", skip = 1)
names(fomo0) <- str_trim(key$Q)

fomo <-
  fomo0 |>
  mutate(
    FOMO = rowMeans(across(FOMO_1:FOMO_10)),
    Sex = case_when(
      Sex == 1 ~ "Male",
      Sex == 2 ~ "Female",
      Sex == 3 ~ "Non binary",
      Sex == 4 ~ "Not stated"
    ),
    Race = case_when(
      Race == 1 ~ "White",
      Race == 2 ~ "Black",
      Race == 3 ~ "Hispanic",
      Race == 4 ~ "American Indian",
      Race == 5 ~ "Asian",
      Race == 6 ~ "Pacific Islander",
      Race == 7 ~ "Multiracial",
      Race == 8 ~ "Not stated"
    ),
    Relationship_qual = ifelse(Relationship_status == 1, NA, Relationship_qual),
    Relationship_status = case_when(
      Relationship_status == 1 ~ 'Single',
      Relationship_status == 2 ~ 'Dating',
      Relationship_status == 3 ~ 'Partnered'
    ),
    ruralurban = case_when(
      ruralurban == 1 ~ "Rural",
      ruralurban == 2 ~ "Suburb",
      ruralurban == 3 ~ "Urban"
    ),
    Remote = case_when(
      Remote == 1 ~ 'Remote',
      Remote == 2 ~ 'In person',
      Remote == 3 ~ 'Do not work'
    ),
    socialinfo = case_when(
      socialinfo == 1 ~ "Friends",
      socialinfo == 2 ~ "Family",
      socialinfo == 3 ~ "Work",
      socialinfo == 4 ~ "Local news",
      socialinfo == 5 ~ "National news",
      socialinfo == 6 ~ "Global news",
      socialinfo == 7 ~ "Celebrities",
      socialinfo == 8 ~ "Sports",
      socialinfo == 9 ~ "Hobbies",
      socialinfo == 10 ~ "Nothing",
    )
  ) |>
  dplyr::select(
    -Status, -IPAddress, -Progress, -Finished, -ResponseId, -RecipientLastName, -RecipientFirstName, -RecipientEmail,
    -ExternalReference, -DistributionChannel, -UserLanguage, -consent, -RecordedDate
  ) |>
  rename(
    Duration = `Duration (in seconds)`,
    Latitude = LocationLatitude,
    Longitude = LocationLongitude
  ) |>
  rename_with(
    \(nm) str_remove(nm, '_1'),
    !matches("FOMO_*|valued_*")
  )

write_csv(fomo, "fomo.csv")
