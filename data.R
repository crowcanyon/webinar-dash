library(magrittr)
library(httr)
library(furrr)
library(purrr)

zips <-
  tigris::zctas() %>%
  dplyr::select(ZCTA5CE10) %>%
  dplyr::left_join(tigris::counties() %>%
                     dplyr::select(NAMELSAD)) %>%
  dplyr::left_join()

webinar_data <- 
  httr::GET("https://altrurig05bo3.blackbaudhosting.com/4454PP_f5a15944-0384-4163-8287-0c23fbc3ba1a/ODataQuery.ashx",
            query = list(databasename = Sys.getenv("webinar_databasename"),
                         AdHocQueryID = Sys.getenv("webinar_AdHocQueryID")),
            httr::authenticate(user = Sys.getenv("altru_user"),
                               password = Sys.getenv("altru_pw"))) %>%
  httr::content(as = "text")



all_data <-
  webinar_data %>%
  jsonlite::fromJSON() %$%
  value %>%
  tibble::as_tibble() %>%
  dplyr::select(event = RegistrantEventEventname,
                event_date = RegistrantEventEventstartdate,
                participants = RegistrantNumberofguests,
                street = AddressPrimaryAddressline1,
                city = AddressPrimaryCity, 
                state = AddressPrimaryState, 
                zip = AddressPrimaryZIP,
                country = AddressPrimaryCountry) %>%
  dplyr::mutate(event_date = lubridate::as_date(event_date),
                participants = ifelse(participants < 1, 1, participants),
                street = 
                  street %>%
                  stringr::str_trim() %>%
                  dplyr::na_if(""),
                city = 
                  city %>%
                  stringr::str_trim() %>%
                  dplyr::na_if(""),
                state = 
                  state %>%
                  stringr::str_trim() %>%
                  dplyr::na_if(""),
                zip = 
                  zip %>%
                  stringr::str_trim() %>%
                  dplyr::na_if(""),
                country = 
                  country %>%
                  stringr::str_trim() %>%
                  dplyr::na_if("")
  )

international <- 
  all_data %>%
  dplyr::filter(country != "United States")

united_states <- 
  all_data %>%
  dplyr::filter(country == "United States",
                !(is.na(city) & is.na(zip) & !is.na(state)),
                !(is.na(city) & is.na(state) & is.na(zip))) %>%
  dplyr::mutate(street = ifelse(!is.na(zip), NA, street),
                city = ifelse(!is.na(zip), NA, city),
                state = ifelse(!is.na(zip), NA, state),
                zip = stringr::str_sub(zip, end = 5)) %>%
  dplyr::group_by(city, state, zip) %>%
  tidyr::nest(events = c(event, event_date, participants)) %>%
  dplyr::arrange(zip)



unlist(places$location, recursive = FALSE) %>% sf::st_sfc(places$location)

do.call(c, places$location)


test <-
  households %>%
  dplyr::rowwise() %>%
  magrittr::extract(1:10,) %>%
  dplyr::mutate(location = httr::GET("https://nominatim.openstreetmap.org/search",
                                     
                                     query = list(#street = street,
                                       city = city,
                                       state = state,
                                       postalcode = zip,
                                       limit = 1,
                                       format = "geojson")) %>%
                  httr::content(as = "text") %>%
                  sf::read_sf() %>%
                  list())

httr::GET("https://nominatim.openstreetmap.org/search",
          query = list(
            city = NA,
            state = NA,
            postalcode = 94306,
            limit = 1,
            format = "geojson") %>%
            purrr::map(na.omit)) %>%
  httr::content(as = "text") %>%
  sf::read_sf()

%>%
  dplyr::mutate(geom =   paste(street, city, state, zip, sep = ", ") %>%
                  stringr::str_remove("\\#") %>%
                  ggmap::geocode() %$%
                  points_to_datum(lon, lat) %>%
                  sf::st_as_sfc(crs = 4326)
  ) %>%
  sf::st_as_sf()
