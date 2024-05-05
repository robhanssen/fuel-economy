load("Rdata/fuel.Rdata")

right_graph_theme <-
    theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

miles_g <-
    (annual_use_graph(fuel, miles) +
        expand_limits(y = 0) +
        labs(x = "", y = "Distance (in miles)") +
        scale_y_continuous(labels = scales::number_format())
    ) +
    (use_over_year_graph(fuel, miles) +
        labs(x = "") +
        expand_limits(y = 0) +
        right_graph_theme
    )

gallons_g <-
    (annual_use_graph(fuel, gallons) +
        expand_limits(y = 0) +
        labs(x = "", y = "Fuel use (in gallons)") +
        scale_y_continuous(labels = scales::number_format())
    ) +
    (use_over_year_graph(fuel, gallons) +
        labs(x = "") +
        expand_limits(y = 0) +
        right_graph_theme
    )

cost_g <-
    (annual_use_graph(fuel, cost) +
        expand_limits(y = 0) +
        labs(x = "", y = "Total spend (in USD)") +
        scale_y_continuous(labels = scales::dollar_format())
    ) +
    (use_over_year_graph(fuel, cost) +
        labs(x = "") +
        expand_limits(y = 0) +
        right_graph_theme
    )

full_g <-
    miles_g / gallons_g / cost_g +
    plot_annotation(
        title = "Fuel use and cost over all years and across all years"
    )

ggsave("graphs/usage_over_year.png",
    width = 10, height = 10,
    plot = full_g
)
