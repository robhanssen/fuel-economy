library(tidyverse)

log <- read_csv("export/gasrecordsample.csv") %>%
    mutate(Cost = as.numeric(Cost))

car <- read_csv("cars/2011quest.csv")

load("Rdata/fuel.Rdata")

f <- fuel %>%
    filter(str_detect(car_name, "2008")) %>%
    # mutate(cost = gallons * price) %>%
    select(Date = date, Odometer = odometer, FuelConsumed = gallons, Cost = cost) %>%
    mutate(
        Date = format(Date, format = "%m/%d/%Y"),
        Cost = round(Cost,2),
        FuelEconomy = "",
        IsFillToFull = TRUE,
        MissedFuelUp = FALSE,
        Notes = "Test Note",
        Tags = "test1 test2"
    )
    
full_join(log, f) %>% slice(-1) %>% write_csv("export/export_2008.csv")
