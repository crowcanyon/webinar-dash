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
  htmlwidgets::saveWidget("participants.html")

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

plotly::ggplotly(
  ggplot(webinar_donations) +
    geom_line(aes(x = Date,
                  y = Donations,
                  text = paste0(Name, "\n",
                                Date, "\n",
                                "Amount: ", Donations),
                  group = 1)) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b") +
    xlab("Date") +
    ylab("Donations") +
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
  htmlwidgets::saveWidget("donations.html")
