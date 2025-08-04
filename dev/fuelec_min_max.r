library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)


load("Rdata/fuel.Rdata")


summarize_fuel <- function(dat, var) {
    dat %>% 
        summarize(
            min = min({{var}}),
            low5 = quantile({{var}}, 0.05),
            average = mean({{var}}, na.rm = TRUE),
            median = quantile({{var}}, 0.50),
            high95 = quantile({{var}}, 0.95),
            max = max({{var}}), 
            .by = car_name
        )
}


summarize_fuel(fuel %>% filter(gallons < 20), miles)

fuel %>% 
    summarize(
        low5 = quantile(mpg, 0.05),
        median = quantile(mpg, 0.50),
        high95 = quantile(mpg, 0.95),
        max_mpg = max(mpg),
        .by = car_name
    )

fuel %>%
    ggplot(aes(y = car_name, x = gallons)) + 
    ggridges::geom_density_ridges() +
    scale_x_continuous(
        # breaks = seq(10, 40, 2)
        )


fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    ggplot(aes(x = date, y = mpg, color = car_name)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(vars(car_name))

x <- ecdf(fuel$mpg[fuel$car_name == "2013 Nissan Altima 2.5SV"])


mpg <- 
    fuel %>%
    filter(str_detect(car_name, "2011")) %>%
    pull(mpg)

mpg_e_cfg <- ecdf(log(mpg+1))
length(mpg)

 1/ mpg_e_cfg(min(mpg))

mpg_t <- tibble(x = log(seq(10, 30, 1)), t = mpg_e_cfg(x))

ggplot(mpg_t, aes(x = x, y = t)) + geom_point()


j <- nls(t ~ pnorm(x, mean = mean0, sd = sd0), start = list(mean0 = log(20), sd0 = .1), data = mpg_t) %>%
broom::augment(newdata = tibble(x = log(mpg[length(mpg)]+1))) %>%
mutate(x = exp(x)) %>% pull(.fitted)
1/j

fuel %>%
    nest(data = !car_name) %>%
    mutate(
        cdf_form = map(data, \(d) {
            # e_cdf = ecdf(d$mpg)
            # mpg_t <- tibble(mpg = seq(10, 50, .2), cdf = e_cdf(mpg))
            # mod <- nls(cdf ~ pnorm(mpg, mean = mean0, sd = sd0), start = list(mean0 = 20, sd0 = 4), data = mpg_t)

            e_cdf = ecdf(log(d$mpg+1))
            mpg_t <- tibble(mpg = log(seq(10, 50, .2)), cdf = e_cdf(mpg))
            mod <- nls(cdf ~ pnorm(mpg, mean = mean0, sd = sd0), start = list(mean0 = 3, sd0 = .05), data = mpg_t)
            broom::augment(mod)
        }
        )
    ) %>%
    unnest(cdf_form) %>%
    filter(cdf < 0.999, cdf > 0.001) %>%
    ggplot(
        # aes(mpg, cdf)
        aes(exp(mpg), cdf)
    ) + geom_point() + 
    geom_line(aes(y = .fitted)) +
    facet_wrap(vars(car_name))



fuel %>%
    nest(data = !car_name) %>%
    mutate(
        cdf_form = map(data, \(d) {
            e_cdf = ecdf(d$mpg)
            mpg_t <- tibble(mpg = seq(10, 50, .2), cdf = e_cdf(mpg))
            mod <- nls(cdf ~ pnorm(mpg, mean = mean0, sd = sd0), start = list(mean0 = 20, sd0 = 1), data = mpg_t)
            broom::tidy(mod)
        }
        )
    ) %>% unnest(cdf_form) %>%
    select(car_name, term, estimate, std.error) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error))


p <- 3

lbs <- seq(0, 20, 5)
brk <- lbs^p

fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    mutate(fuel_year = 1) %>% #as.character(year(date)%/%10)) %>%
    ggplot(aes(x = gallons^p, color = car_name)) + 
    scale_x_continuous(
        breaks = brk, labels = lbs
    ) +
    geom_histogram() + 
    facet_grid(fuel_year~car_name, scale = "free_y")



x <- fuel %>%
    filter(!str_detect(car_name, "Altima")) %>%
    pull(gallons)

gal_cdf <- ecdf(x^2)

tibble(
    g = seq(1, 400, 1),
    cdf = gal_cdf(g)
) %>%
    ggplot(aes(g, cdf)) + 
    geom_line()


fuel %>% 
    filter(gallons < 21) %>%
    summarize(
        min_mpg = min(gallons),
        low5 = quantile(gallons, 0.025),
        average = mean(gallons, na.rm = TRUE),
        median = quantile(gallons, 0.50),
        high95 = quantile(gallons, 0.975),
        max_mpg = max(gallons),
        .by = car_name
    )


fuel %>% 
    filter(gallons < 21) %>%
    summarize(
        min_mpg = min(miles),
        low5 = quantile(miles, 0.025),
        average = mean(miles, na.rm = TRUE),
        median = quantile(miles, 0.50),
        high95 = quantile(miles, 0.975),
        max_mpg = max(miles),
        .by = car_name
    )

stat_script <- function(dat, var) {
    dat %>% 
    filter(gallons < 21) %>%
    summarize(
        min = min({{var}}),
        low5 = quantile({{var}}, 0.025),
        average = mean({{var}}, na.rm = TRUE),
        median = quantile({{var}}, 0.50),
        high95 = quantile({{var}}, 0.975),
        max = max({{var}})
    )

}


fuel %>% 
    filter(gallons < 21, !str_detect(car_name, "2008")) %>%
    nest(data = !car_name) %>%
    mutate(sum = map(data, stat_script, cost)) %>%
    unnest(sum)

fuel %>%
    ggplot(aes(x = cost, y = car_name)) +
    ggridges::geom_density_ridges()


fuel %>%
    ggplot(aes(x = miles, y = cost, color = car_name)) +
    geom_point()