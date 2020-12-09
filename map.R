library(magrittr)
library(ggplot2)

webinar_metrics <- 
  readxl::read_excel("Webinar Metrics.xlsx") %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::filter(Date >= "2020-04-01") %>%
  dplyr::select(Date,
                Name,
                Registered,
                `Zoom Unique Viewers`,
                `Youtube Views`,
                `Facebook Views`) %>%
  tidyr::pivot_longer(cols = `Zoom Unique Viewers`:`Facebook Views`,
                      names_to = "Platform",
                      values_to = "Views") %>%
  dplyr::mutate(Platform = factor(Platform, 
                                  levels = c("Facebook Views",
                                             "Youtube Views",
                                             "Zoom Unique Viewers"),
                                  labels = c("Facebook",
                                             "Youtube",
                                             "Zoom"),
                                  ordered = TRUE),
                Views = tidyr::replace_na(Views, 0))


mycols <- c("#18787C", 
            "#CA411D", 
            "#6C7A7B", 
            "#F7C308")

plotly::ggplotly(
  ggplot(webinar_metrics,
         aes(x = Date,
             y = Views,
             fill = Platform)) +
    geom_area(aes(text = paste0(Name, "\n",
                                Date, "\n",
                                Platform, " Views: ", Views),
                  group = 1)) + 
    geom_line(aes(x = Date,
                  y = Registered,
                  text = paste0(Name, "\n",
                                Date, "\n",
                                "Registrants: ", Registered),
                  group = 1)) +
    scale_fill_manual(name = NULL,
                      values = mycols[c(1,2,4)]) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b") +
    xlab("Date") +
    ylab("Participants") +
    theme_minimal() +
    theme(legend.justification = c(1,1),
          legend.position = c(1,1),
          legend.background=element_blank(),
          plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)
    ),
  tooltip = "text"
) %>%
  htmlwidgets::saveWidget("participants.html",
                          title = "2020 Webinar Participants")

webinar_donations <- 
  readxl::read_excel("Webinar Donations - December 2020.xlsx") %>%
  dplyr::select(Date = `Event Date`,
                Donations = `Donation Amount`) %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(Donations = sum(Donations, na.rm = TRUE)) %>%
  dplyr::left_join(webinar_metrics %>%
                     dplyr::select(Date, Name) %>%
                     dplyr::distinct(),
                   .) %>%
  dplyr::arrange(Date)

g <-
  ggplot(webinar_donations) +
  geom_line(aes(x = Date,
                y = Donations,
                text = paste0(Name, "\n",
                              Date, "\n",
                              "Amount: ", Donations),
                group = 1),
            size = 2) +
  scale_x_date(name = "",
               date_breaks = "1 month",
               date_labels = "%b",
               expand = expansion(0,0)) +
  scale_y_continuous(name = "Donations",
                     limits = c(0,3000),
                     expand = expansion(0,0),
                     labels = scales::dollar) +
  theme_minimal() +
  theme(legend.justification = c(1,1),
        legend.position = c(1,1),
        legend.background=element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16)
  )

ggsave("./donations.pdf",
       plot = g,
       height = 5.5,
       width = 11)

plotly::ggplotly(
  g,
  tooltip = "text"
) %>%
  htmlwidgets::saveWidget("donations.html",
                          title = "2020 Webinar Donations")

webinar_donation_dist <- 
  readxl::read_excel("Webinar Donations - December 2020.xlsx") %>%
  dplyr::select(Date = `Event Date`,
                Donations = `Donation Amount`) %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(min = min(Donations, na.rm = TRUE),
                   median = median(Donations, na.rm = TRUE),
                   max = max(Donations, na.rm = TRUE)) %>%
  dplyr::left_join(webinar_metrics %>%
                     dplyr::select(Date, Name) %>%
                     dplyr::distinct(),
                   .) %>%
  dplyr::arrange(Date)

g <-
  ggplot(webinar_donation_dist,
         aes(text = paste0(Name, "\n",
                           Date, "\n",
                           "Median: ", median,"\n",
                           "Min: ", min,"\n",
                           "Max: ", max))) +
  geom_ribbon(aes(x = Date,
                  ymin = min,
                  ymax = max,
                  group = 1),
              fill = mycols[[4]]
  ) +
  geom_line(aes(x = Date,
                y = median,
                group = 1),
            color = mycols[[2]],
            size = 2) +
  scale_x_date(name = "",
               date_breaks = "1 month",
               date_labels = "%b",
               expand = expansion(0,0)) +
  scale_y_continuous(name = "Donations",
                     limits = c(0,1050),
                     expand = expansion(0,0),
                     labels = scales::dollar) +
  theme_minimal() +
  theme(legend.justification = c(1,1),
        legend.position = c(1,1),
        legend.background=element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16)
  )

ggsave("./donations_dist.pdf",
       plot = g,
       height = 5.5,
       width = 11)

plotly::ggplotly(
  g,
  tooltip = "text"
) %>%
  htmlwidgets::saveWidget("donations_dist.html",
                          title = "2020 Webinar Donation Levels")


g <-
  readxl::read_excel("webinar_counties.xlsx") %>%
  dplyr::left_join(tigris::fips_codes,
                   by = c("County" = "county",
                          "State" = "state_name")) %>%
  dplyr::left_join(tigris::counties(cb = TRUE, resolution = "20m") %>%
                     sf::st_transform(4326),
                   .,
                   by = c("STATEFP" = "state_code",
                          "COUNTYFP" = "county_code")) %>%
  dplyr::left_join(tigris::fips_codes,
                   by = c("STATEFP" = "state_code",
                          "COUNTYFP" = "county_code")) %>%
  dplyr::select(County = county,
                State = state_name,
                n) %>%
  dplyr::filter(!(State %in% c("Hawaii", "Alaska", "Puerto Rico")))  %>%
  sf::st_transform(5070) %>%
  # dplyr::mutate(n = tidyr::replace_na(n, 0)) %>%
  ggplot(aes(fill = n,
             text = paste0(County, ", ", State, "\n",
                           "Households: ", n))) +
  geom_sf(color = "white", 
          size=0.1) +
  # coord_sf(crs = 5070, 
  #          datum = NA) +
  scale_fill_viridis_c("Number of Households",
                       limits = c(1, 200),
                       breaks = c(1,50,100,150, 200),
                       na.value = "gray75") +
  labs(title = "2020 Webinar Households") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_colorbar(title.position="top", 
                               title.hjust = 0.5,
                               barwidth = 15, 
                               barheight = 0.5))

ggsave("webinar_map.pdf",
       plot = g,
       height = 6,
       width = 6)

plotly::ggplotly(
  g,
  tooltip = "text"
) %>%
  plotly::hide_guides() %>%
  htmlwidgets::saveWidget("webinar_map.html",
                          title = "2020 Webinar Households")
