library(magrittr)
library(httr)
library(furrr)
library(purrr)

zips <-
  tigris::zctas() %>%
  dplyr::select(ZCTA5CE10)

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
                  dplyr::na_if(""))

international <- 
  all_data %>%
  dplyr::filter(country != "United States")

united_states <-
  all_data %>%
  dplyr::filter(country == "United States",
                !(is.na(city) & is.na(zip) & !is.na(state)),
                !(is.na(city) & is.na(state) & is.na(zip))) %>%
  dplyr::mutate(zip = stringr::str_sub(zip, end = 5)) %>%
  dplyr::select(-street:-state, -country) %>%
  dplyr::group_by(zip) %>%
  tidyr::nest(events = c(event, event_date, participants)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(zip) %>%
  dplyr::left_join(readr::read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt",
                                   col_types = readr::cols(.default = readr::col_character())) %>%
                     dplyr::select(ZCTA5, STATE, COUNTY),
                   by = c("zip" = "ZCTA5")) %>%
  dplyr::left_join(tigris::fips_codes,
                   by = c("STATE" = "state_code",
                          "COUNTY" = "county_code")) %>%
  dplyr::mutate(id = paste0(STATE,COUNTY)) %>%
  dplyr::select(id,
                County = county,
                State = state,
                events) %>%
  tidyr::unnest(events) %>%
  dplyr::group_by(id, County, State, event, event_date) %>%
  dplyr::summarise(Households = dplyr::n(),
                   Participants = as.integer(sum(participants))) %>%
  dplyr::ungroup() %>%
  dplyr::select(id, 
                County, 
                State, 
                Event = event,
                Date = event_date,
                Households,
                Participants) %>%
  dplyr::arrange(County, State, Date) %T>%
  readr::write_csv("webinars.csv")
