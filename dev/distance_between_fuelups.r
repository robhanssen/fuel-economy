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


altima_out <- boxplot(fuel$miles[str_detect(fuel$car_name, "2013")], plot = FALSE)$out
quest_out <- boxplot(fuel$miles[str_detect(fuel$car_name, "2011")], plot = FALSE)$out

altima_all <- fuel %>%
    filter(str_detect(car_name, "2013")) %>%
    mutate(outlier = miles %in% altima_out) %>% 
    filter(!outlier)

quest_all <- fuel %>%
    filter(str_detect(car_name, "2011")) %>%
    mutate(outlier = miles %in% quest_out) %>% 
    filter(!outlier)

fuel_alt <- bind_rows(altima_all, quest_all) 

fuel_alt2 <-
    fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    nest(data = -car_name) %>%
    mutate(
        outliers = map(data, ~ boxplot(.x$miles, plot = FALSE)$out),
        filtered_data = map2(data, outliers, ~ filter(.x, !miles %in% .y))
    ) %>%
    select(-data, -outliers) %>%
    # do a normality test on filtered_data
        mutate(
            normality_test = map(filtered_data, ~ shapiro.test(.x$miles)),
            p_value = map_dbl(normality_test, "p.value")
        ) %>%
    unnest(filtered_data)

dist_g <-
    fuel_alt2 %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        av_dist_0 = quantile(miles, pnorm(0)),
        av_dist_1 = quantile(miles, pnorm(1)),
        .by = c(car_name, year)
    ) %>%
    ggplot(aes(x = year, y = av_dist_1, fill = car_name, color = car_name)) +
    geom_line() +
    geom_line(aes(y = av_dist_0), linetype = "dashed") +
    geom_point(shape = 21, size = 3) +
    geom_point(aes(y = av_dist_0), shape = 21, size = 3, fill = "white") +
    # facet_wrap(~car_name) +
    scale_fill_manual(values = car_colors) +
    scale_color_manual(values = car_colors) +
    scale_x_continuous(breaks = seq(2010, 2030, 5)) +
    annotate(
        geom = "text", x = c(2017, 2023), y = c(310, 460),
        label = levels(fuel$car_name)[1:2], color = car_colors
    ) +
    labs(
        x = NULL, y = "Distance (in miles)",
        title = "Distance between fuel-ups at the 84th and 50th percentiles"
    )

ggsave("graphs/av_distance.png", width = 8, height = 6, plot = dist_g)



fuel_alt2 %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        av_dist_0 = quantile(miles, pnorm(0)),
        av_dist_1 = quantile(miles, pnorm(1)),
        .by = c(car_name, year)
    ) %>%
    filter(str_detect(car_name, "2011")) %>%
    lm(av_dist_1 ~ year, data = .) %>%
    summary()