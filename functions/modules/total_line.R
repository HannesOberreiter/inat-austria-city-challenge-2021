fTotalLine <- function(df, y_lab) {
    df %>%
        ggplot(aes(x = year, y = value)) +
        geom_line() +
        geom_area(aes(fill = name), show.legend = FALSE) +
        geom_point() +
        theme_classic() +
        scale_fill_manual(values = c("#009E73", "#E69F00")) +
        scale_y_continuous(
            breaks = scales::breaks_pretty(),
            labels = scales::label_number_si()
        ) +
        scale_x_continuous(
            breaks = scales::breaks_pretty()
        ) +
        labs(
            x = "Jahr",
            y = y_lab
        ) +
        facet_wrap(~label, scales = "free_y") +
        theme(
            panel.grid.major.y = element_line()
        )
}
