load("Rdata/fuel.Rdata")

# exch <- get_index()

fuel_summ <- function(dat, over = "year") {
    dat %>%
        summarize(
            price_mean = mean(price),
            price_max = max(price),
            price_min = min(price),
            .by = all_of(over)
        )
}


fuelprice <-
    fuel %>%
    select(
        date, price, year,
    ) %>%
    arrange(date) %>%
    mutate(month = month(date))

fuel_annual <-
    fuelprice %>%
    fuel_summ() %>%
    mutate(date = ymd(glue::glue("{year}-07-01")))

fuel_month <-
    fuelprice %>%
    fuel_summ(over = c("year", "month")) %>%
    mutate(date = ymd(glue::glue("{year}-{month}-15")))

ggplot(
    fuel_annual,
    aes(x = date, y = price_mean)
) +
    geom_line(
        data = fuel_month,
        aes(y = price_mean, color = factor(year), group = factor(year))
    ) +
    geom_errorbar(
        data = fuel_annual,
        aes(ymin = price_min, ymax = price_max, color = factor(year)),
        width = 100
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        label = scales::label_dollar(accuracy = 0.01),
        sec.axis = sec_axis(~ . / 3.78, labels = scales::label_dollar(), name = "Fuel price (in $/L)")
    ) +
    labs(
        x = "", y = "Fuel price (in $/gallon)",
        caption = "Average fuel price over a month. Errorbars indicate annual high and low prices"
    ) +
    scale_color_brewer(palette = "Paired")
