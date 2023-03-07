source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

# c("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=109026141581994&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=258516741270048&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=1306892789407030&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=784500348361030&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=7975984239&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=107922728029680&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=108148711054082&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=102360361583666&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=126927444629367&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=669307523469574&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=102341921819487&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=188789377812793&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=428340480554355&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=457989087617669&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=113104761165055&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=1598476943723854&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=103745598776990&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=183040208776602&search_type=page&media_type=all", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=KG&view_all_page_id=421154791612167&search_type=page&media_type=all") %>%
#     walk(browseURL)

library(httr)
# install.packages("tidyverse")
# install.packages("gt")
#
# install.packages("gtExtras")
# install.packages("highcharter")
# install.packages("rmdformats")
# install.packages("tidytext")


library(tidyverse)

tstamp <- Sys.time()

source("utils.R")



# rawadvertisers <- read_csv("data/advertisers - advertisers.csv")  %>%
#   mutate(party_lab = case_when(
#     str_detect(advertiser_name, "VVD") ~ "VVD",
#     str_detect(advertiser_name, "\\bCDA\\b") ~ "CDA",
#     str_detect(advertiser_name, "PvdA|Jonge Socialisten") ~ "PvdA",
#     str_detect(advertiser_name, "D66|Jonge Democraten") ~ "D66",
#     str_detect(advertiser_name, "GroenLinks") ~ "GroenLinks",
#     str_detect(advertiser_name, "ChristenUnie") ~ "ChristenUnie",
#     str_detect(advertiser_name, "\\bSP\\b") ~ "SP",
#     str_detect(advertiser_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
#     str_detect(advertiser_name, "50PLUS") ~ "50PLUS",
#     str_detect(advertiser_name, "\\bSGP\\b") ~ "SGP",
#     str_detect(advertiser_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
#     str_detect(advertiser_name, "PVV") ~ "PVV",
#     str_detect(advertiser_name, "DENK") ~ "DENK",
#     str_detect(advertiser_name, "Volt") ~ "Volt Nederland",
#     str_detect(advertiser_name, "BIJ1") ~ "BIJ1",
#     str_detect(advertiser_name, "BVNL") ~ "BVNL",
#     str_detect(advertiser_name, "Ja21") ~ "Ja21",
#     T ~ ""
#   ))



# internal_page_ids <- read_csv("data/nl_advertisers.csv") %>%
#   mutate(page_id = as.character(page_id))

# internal_page_ids %>%
#     count(party, sort = T) %>% View

all_dat <- read_csv("data/wtm-advertisers-ee-2023-02-28.csv") %>% #names
    select(page_id = advertisers_platforms.advertiser_platform_ref,
           page_name = name, party = entities.short_name)  %>%
    mutate(page_id = as.character(page_id))

# all_dat <- internal_page_ids %>%
#     bind_rows(wtm_data) %>%
#     distinct(page_id, .keep_all = T)

# janitor::clean_names() %>%
# arrange(desc(amount_spent_usd)) %>%
# mutate(spend_upper = amount_spent_usd %>% as.numeric()) %>%
# arrange(-spend_upper) %>%
# mutate_all(as.character)C

# internal_page_ids %>% count(party, sort =T) %>% slice(11:17)
#
# internal_page_ids %>%
#   filter(party == "Politiek Op Maat")
#
# rawadvertisers %>%
#   # filter(category == "Political Organization") %>% View
#   # filter(str_detect(category, "Party|Politician|Candidade")) %>%
#   rename(page_id = advertiser_id) %>%
#   select(page_id, page_name = advertiser_name, party = party_lab)
#   left_join(internal_page_ids) %>%
#   # drop_na(party) %>%
#   filter(!is.na(party) | party_lab != "") %>%
#   # filter(party == "PvdA" & party_lab == "")
#   count(party, party_lab, sort = T)  %>% View
#
#
#
#   internal_page_ids %>%
#     bind_rows(
#       rawadvertisers %>%
#         rename(page_id = advertiser_id) %>%
#         select(page_id, page_name = advertiser_name, party = party_lab) %>%
#         filter(party != "") %>%
#         filter(str_starts(page_id, "AR", negate = T)) %>%
#         mutate(source = "yo")
#     ) %>%
#     distinct(page_id, .keep_all = T) %>%
#     write_csv("data/nl_advertisers.csv")


# georgia_wtm <- readr::read_csv("data/wtm-advertisers-us-2022-11-28T14_22_01.338Z.csv") %>%
#   select(page_name = name,
#          page_id = advertisers_platforms.advertiser_platform_ref) %>%
#   mutate(page_id = as.character(page_id))

# options(scipen = 999999)

# georgia_wtm

# internal_page_ids <- georgia_wtm %>%
#   mutate_all(as.character) %>%
#   bind_rows(last90days)  %>%
#   distinct(page_id, .keep_all = T)

# get_targeting(internal_page_ids$page_id[1], timeframe = "LAST_30_DAYS")
# debugonce(get_targeting)
# get_targeting("121264564551002", timeframe = "LAST_30_DAYS")

scraper <- function(.x, time = "7") {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  yo <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)

  if(nrow(yo)!=0){
    path <- paste0(glue::glue("data/{time}/"),.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {

    # saveRDS(yo, file = path)
    # }
  }

  # print(nrow(yo))
  # })

  return(yo)

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("provincies/7", full.names
# }

### save seperately
yo7 <- all_dat %>% #count(cntry, sort  =T) %>%
  # filter(!(page_id %in% already_there)) %>%
  # filter(cntry == "GB") %>%
  # slice(1:10) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(scraper, 7)

yo30 <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper, 30)

# saveRDS(yo, file = )
library(tidyverse)
da30  <- yo30  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)
# da30 %>% count(party)
da7  <- yo7 %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")


rmarkdown::render("index.Rmd", "html_document")

#
# da7 %>%
#   distinct(internal_id, .keep_all = T) %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids) %>%
#   group_by(party) %>%
#   summarize(total_spend = sum(total_spend))
#
#
# amgna <- da7 %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids)
#
#
# amgna %>%
#   filter(type == "gender") %>%
#   filter(value == "Women") %>%
#   # mutate(total_spend = total_spend*total_spend_pct) %>%
#   ggplot(aes(party, total_spend_pct)) +
#   geom_boxplot() #+
#   # scale_y_log10()
#
#
#
# amgna %>%
#   filter(type == "detailed")
