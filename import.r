fuel <-
    map_df(
        list.files(path = "./cars", pattern = "*.csv$", full.names = TRUE),
        read_csv,
        comment = "#", col_types = "ccDdddd"
    ) %>%
    arrange(date) %>%
    mutate(
        car_name = factor(
            car_name,
            ordered = TRUE,
            levels = rev(c("2008 Nissan Altima", "2013 Nissan Altima 2.5SV", "2011 Nissan Quest 3.5SL"))
        ),
        mpg = miles / gallons,
        cost = price * gallons,
        month = month(date, label = TRUE, abbr = TRUE),
        quarter = quarter(date),
        year = year(date),
        dayofweek = wday(date, label = TRUE),
        dayofmonth = day(date),
        dayofyear = yday(date)
    ) %>%
    filter(year >= 2013)


save(fuel, file = "Rdata/fuel.Rdata")
