library(tidyverse)
library(gt)


Name <- c(rep(x = "Alex", times = 4), rep(x = "Barbara", times = 4), rep(x = "Chris", times = 4), rep(x = "David", times = 4), rep(x = "Elena", times = 4))
Donut <- rep(x = c("1st", "2nd", "3rd", "4th"), times = 5)
Willingness_To_Pay <- c(8, 5, 3, 0, 10, 9, 7, 4, 6, 5, 4, 2, 9, 7, 4, 1, 4, 2, 0, -2)

benefits <- data.frame(Name, Donut, Willingness_To_Pay)

benefits |>
  pivot_wider(names_from = Name, values_from = Willingness_To_Pay) |>
  gt() |>
  tab_spanner(
    label = "Willingness to Pay",
    columns = Alex:Elena
  )


Price <- 10:0
Demand <- tibble(Price) |>
  mutate(
    Quantity_Demanded_Alex = 
      if_else(subset(benefits,subset = Name == "Alex" & Donut == "1st")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Alex" & Donut == "2nd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Alex" & Donut == "3rd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Alex" & Donut == "4th")$Willingness_To_Pay >= Price, 1, 0),
    Quantity_Demanded_Barbara = 
      if_else(subset(benefits,subset = Name == "Barbara" & Donut == "1st")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Barbara" & Donut == "2nd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Barbara" & Donut == "3rd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Barbara" & Donut == "4th")$Willingness_To_Pay >= Price, 1, 0),
    Quantity_Demanded_Chris = 
      if_else(subset(benefits,subset = Name == "Chris" & Donut == "1st")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Chris" & Donut == "2nd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Chris" & Donut == "3rd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Chris" & Donut == "4th")$Willingness_To_Pay >= Price, 1, 0),
    Quantity_Demanded_David = 
      if_else(subset(benefits,subset = Name == "David" & Donut == "1st")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "David" & Donut == "2nd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "David" & Donut == "3rd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "David" & Donut == "4th")$Willingness_To_Pay >= Price, 1, 0),
    Quantity_Demanded_Elena = 
      if_else(subset(benefits,subset = Name == "Elena" & Donut == "1st")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Elena" & Donut == "2nd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Elena" & Donut == "3rd")$Willingness_To_Pay >= Price, 1, 0) +
      if_else(subset(benefits,subset = Name == "Elena" & Donut == "4th")$Willingness_To_Pay >= Price, 1, 0),
    Quantity_Demanded_Aggregate = Quantity_Demanded_Alex + Quantity_Demanded_Barbara + Quantity_Demanded_Chris + Quantity_Demanded_David + Quantity_Demanded_Elena
    ) |>
  pivot_longer(Quantity_Demanded_Alex:Quantity_Demanded_Aggregate, names_to = "Name", 
               values_to = "Quantity_Demanded", names_prefix = "Quantity_Demanded_")

demand_table <- Demand |>
  pivot_wider(names_from = Name, values_from = Quantity_Demanded) |>
  gt() |>
  tab_spanner(
    label = "Quantity Demanded (Donuts)",
    columns = Alex:Aggregate
  )

demand_table


# Demand <- tibble(Price) |>
#   mutate(Quantity_Demanded = 
#            if_else(benefits_Alex$Willingness_To_Pay[1] >= Price, 1, 0) +
#            if_else(benefits_Alex$Willingness_To_Pay[2] >= Price, 1, 0) +
#            if_else(benefits_Alex$Willingness_To_Pay[3] >= Price, 1, 0) +
#            if_else(benefits_Alex$Willingness_To_Pay[4] >= Price, 1, 0) )

demand_curve_alex <- Demand |>
  filter(Name == "Alex") |>
  ggplot(mapping = aes(x = Quantity_Demanded, y = Price)) +
  geom_point() +
  geom_step(direction = "vh") +
  theme_minimal() +
  scale_y_continuous(name = "Price $", breaks = seq(0, 10, 2)) +
  scale_x_continuous(name = "Quantity Demanded") +
  labs(title = "Demand: Alex", subtitle = "From Willingness to Pay")

demand_curve_alex

demand_curves <- Demand |>
  ggplot(mapping = aes(x = Quantity_Demanded, y = Price, color = Name)) +
  geom_point() +
  geom_step(direction = "vh") +
  theme_minimal() +
  scale_y_continuous(name = "Price $", breaks = seq(0, 10, 2)) +
  scale_x_continuous(name = "Quantity Demanded") +
  labs(title = "Demand", subtitle = "From Willingness to Pay")

demand_curves

