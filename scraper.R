library(tidyverse)
library(jsonlite)
library(beepr)

#-------------------------
# Presidential

## Presidential election results by state
pres2024 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/pages/data/2024-11-05/results-president.json")

pres_2024_results <- data.frame(
  nyt_id = character(),
  url = character(),
  update_time = character(),
  eevp = numeric(),
  votes_harris_k = integer(),
  votes_trump_d = integer(),
  total_votes = integer(),
  votes = integer(),
  pct_dem = numeric(),
  pct_rep = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(pres2024$races$reporting_unit)) {
  nyt_id <- pres2024$races$nyt_id[i]
  url <- pres2024$races$url[i]
  update_time <- pres2024$races$updated_at[i]
  
  reporting_unit <- pres2024$races$reporting_unit[[i]]
  candidates_data <- reporting_unit$candidates[[1]]
  
  harris_k_votes <- candidates_data$votes$total[candidates_data$nyt_id == "harris-k"]
  trump_d_votes <- candidates_data$votes$total[candidates_data$nyt_id == "trump-d"]
  total_votes <- sum(candidates_data$votes$total)
  
  pct_dem <- reporting_unit$historical_2020_pres$pct_dem
  pct_rep <- reporting_unit$historical_2020_pres$pct_rep
  votes <- reporting_unit$historical_2020_pres$votes
  eevp <- reporting_unit$eevp

  pres_2024_results <- rbind(pres_2024_results, data.frame(
    nyt_id = nyt_id,
    url = url,
    update_time = update_time,
    eevp = ifelse(length(eevp) > 0, eevp, NA),
    votes_harris_k = ifelse(length(harris_k_votes) > 0, harris_k_votes, NA),
    votes_trump_d = ifelse(length(trump_d_votes) > 0, trump_d_votes, NA),
    total_votes_2024 = ifelse(length(total_votes) > 0, total_votes, NA),
    total_votes_2020 = ifelse(length(votes) > 0, votes, NA), 
    pct_dem_2020 = ifelse(length(pct_dem) > 0, pct_dem, NA),
    pct_rep_2020 = ifelse(length(pct_rep) > 0, pct_rep, NA) 
  ))
}

pres_2024_results <- pres_2024_results %>% 
  rename(state = nyt_id) %>% 
  mutate(pct_dem_2024 = ifelse(total_votes_2024 == 0, NA, votes_harris_k*100 / total_votes_2024),
         pct_rep_2024 = ifelse(total_votes_2024 == 0, NA, votes_trump_d*100 / total_votes_2024)) %>% 
  relocate(total_votes_2024, .after = votes_trump_d) %>%
  relocate(pct_dem_2024, .after = total_votes_2024) %>%
  relocate(pct_rep_2024, .after = pct_dem_2024)

write_csv(pres_2024_results, "pres_2024_results_by_state.csv")

## Presidential election results by county

states <- state.name
failed_states <- c()
all_states_results <- list()

base_url <- "https://static01.nyt.com/elections-assets/pages/data/2024-11-05/results-%s-president.json"

for (state in states) {
  state_url <- sprintf(base_url, tolower(gsub(" ", "-", state)))
  
  tryCatch({
    pres_state_data <- fromJSON(state_url)
    
    state_result_df <- data.frame(
      name = character(),
      level = character(),
      eevp = numeric(),
      votes_harris_k_total = integer(),
      votes_harris_k_advance = integer(),
      votes_harris_k_provisional = integer(),
      votes_harris_k_absentee_mail = integer(),
      votes_harris_k_absentee_in_person = integer(),
      votes_harris_k_election_day_in_person = integer(),
      votes_trump_d_total = integer(),
      votes_trump_d_advance = integer(),
      votes_trump_d_provisional = integer(),
      votes_trump_d_absentee_mail = integer(),
      votes_trump_d_absentee_in_person = integer(),
      votes_trump_d_election_day_in_person = integer(),
      total_votes_2024 = integer(),
      total_votes_2020 = integer(),
      pct_dem_2020 = numeric(),
      pct_rep_2020 = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(pres_state_data$races$reporting_units[[1]]$name)) {
      reporting_unit <- pres_state_data$races$reporting_units[[1]]
      
      name <- reporting_unit$name[i]
      eevp <- reporting_unit$eevp[i]
      level <- reporting_unit$level[i]
      total_votes_2024 <- reporting_unit$total_votes[i]
      
      total_votes_2020 <- reporting_unit$historical_2020_pres$votes[i]
      pct_dem_2020 <- reporting_unit$historical_2020_pres$pct_dem[i]
      pct_rep_2020 <- reporting_unit$historical_2020_pres$pct_rep[i]
      
      candidates_data <- reporting_unit$candidates[[i]]
      
      harris_k_index <- which(candidates_data$nyt_id == "harris-k")
      harris_k_votes_total <- candidates_data$votes$total[harris_k_index]
      harris_k_votes_advance <- if ("advance" %in% names(candidates_data$votes)) candidates_data$votes$advance[harris_k_index] else NA
      harris_k_votes_provisional <- if ("provisional" %in% names(candidates_data$votes)) candidates_data$votes$provisional[harris_k_index] else NA
      harris_k_votes_absentee_mail <- if ("absentee_mail" %in% names(candidates_data$votes)) candidates_data$votes$absentee_mail[harris_k_index] else NA
      harris_k_votes_absentee_in_person <- if ("absentee_in_person" %in% names(candidates_data$votes)) candidates_data$votes$absentee_in_person[harris_k_index] else NA
      harris_k_votes_election_day_in_person <- if ("election_day_in_person" %in% names(candidates_data$votes)) candidates_data$votes$election_day_in_person[harris_k_index] else NA
      
      trump_d_index <- which(candidates_data$nyt_id == "trump-d")
      trump_d_votes_total <- candidates_data$votes$total[trump_d_index]
      trump_d_votes_advance <- if ("advance" %in% names(candidates_data$votes)) candidates_data$votes$advance[trump_d_index] else NA
      trump_d_votes_provisional <- if ("provisional" %in% names(candidates_data$votes)) candidates_data$votes$provisional[trump_d_index] else NA
      trump_d_votes_absentee_mail <- if ("absentee_mail" %in% names(candidates_data$votes)) candidates_data$votes$absentee_mail[trump_d_index] else NA
      trump_d_votes_absentee_in_person <- if ("absentee_in_person" %in% names(candidates_data$votes)) candidates_data$votes$absentee_in_person[trump_d_index] else NA
      trump_d_votes_election_day_in_person <- if ("election_day_in_person" %in% names(candidates_data$votes)) candidates_data$votes$election_day_in_person[trump_d_index] else NA
      
      state_result_df <- rbind(state_result_df, data.frame(
        name = name,
        level = level,
        eevp = eevp,
        votes_harris_k_total = harris_k_votes_total,
        votes_harris_k_advance = harris_k_votes_advance,
        votes_harris_k_provisional = harris_k_votes_provisional,
        votes_harris_k_absentee_mail = harris_k_votes_absentee_mail,
        votes_harris_k_absentee_in_person = harris_k_votes_absentee_in_person,
        votes_harris_k_election_day_in_person = harris_k_votes_election_day_in_person,
        votes_trump_d_total = trump_d_votes_total,
        votes_trump_d_advance = trump_d_votes_advance,
        votes_trump_d_provisional = trump_d_votes_provisional,
        votes_trump_d_absentee_mail = trump_d_votes_absentee_mail,
        votes_trump_d_absentee_in_person = trump_d_votes_absentee_in_person,
        votes_trump_d_election_day_in_person = trump_d_votes_election_day_in_person,
        total_votes_2024 = total_votes_2024,
        total_votes_2020 = total_votes_2020,
        pct_dem_2020 = pct_dem_2020,
        pct_rep_2020 = pct_rep_2020
      ))
    }
    all_states_results[[state]] <- state_result_df
  }, error = function(e) {
    failed_states <<- c(failed_states, state)
  })
}

all_results_df <- bind_rows(all_states_results, .id = "state")

pres_2024_results_by_county <- all_results_df %>%
  rename(county = name) %>%
  mutate(
    pct_dem_2024 = ifelse(total_votes_2024 == 0, NA, votes_harris_k_total * 100 / total_votes_2024),
    pct_rep_2024 = ifelse(total_votes_2024 == 0, NA, votes_trump_d_total * 100 / total_votes_2024)
  ) %>%
  relocate(state, .before = county) %>%
  relocate(pct_dem_2024, .after = total_votes_2024) %>%
  relocate(pct_rep_2024, .after = pct_dem_2024)

# display failed states if any
if (length(failed_states) > 0) {
  message("Failed to load data for the following states: ", paste(failed_states, collapse = ", "))
}

write_csv(pres_2024_results_by_county, "pres_2024_results_by_county.csv")

#-------------------------
# Senate
states <- state.name
failed_states <- c()

base_url <- "https://static01.nyt.com/elections-assets/pages/data/2024-11-05/results-%s-us-senate.json"
all_states_senate_results <- list()

for (state in states) {
  state_url <- sprintf(base_url, tolower(gsub(" ", "-", state)))

  tryCatch({
    senate_data <- fromJSON(state_url)
    
    office <- senate_data$races$office
    updated_at <- senate_data$races$updated_at
    
    state_senate_results <- data.frame(
      office = character(),
      updated_at = character(),
      eevp = numeric(),
      name = character(),
      level = character(),
      candidate_id = character(),
      party = character(),
      votes_total = integer(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(senate_data$races$reporting_units[[1]]$name)) {
      reporting_unit <- senate_data$races$reporting_units[[1]]
      
      eevp <- reporting_unit$eevp[i]
      name <- reporting_unit$name[i]
      level <- reporting_unit$level[i]
      
      candidates_data <- reporting_unit$candidates[[i]]
      
      for (j in seq_len(nrow(candidates_data))) {
        candidate_id <- candidates_data$nyt_id[j]
        votes_total <- candidates_data$votes$total[j]
        
        party <- senate_data$races$candidate_metadata[[candidate_id]]$party$name
        
        state_senate_results <- rbind(state_senate_results, data.frame(
          office = office,
          updated_at = updated_at,
          eevp = eevp,
          place = name,
          level = level,
          candidate_id = candidate_id,
          party = ifelse(!is.null(party), party, NA),
          votes_total = votes_total
        ))
      }
    }
    
    all_states_senate_results[[state]] <- state_senate_results
    
  }, error = function(e) {
    failed_states <<- c(failed_states, state)
  })
}

all_senate_results_df <- bind_rows(all_states_senate_results, .id = "state")

# check states without a senator race
if (length(failed_states) > 0) {
  message("Failed to load data for the following states: ", paste(failed_states, collapse = ", "))
}

write_csv(all_senate_results_df, "senate_2024_results_by_county.csv")

#-------------------------
# House

## House election results by district
states <- state.name

base_url <- "https://static01.nyt.com/elections-assets/pages/data/2024-11-05/results-%s-us-house-%s.json"
all_district_results <- list()

for (state in states) {
  formatted_state <- tolower(gsub(" ", "-", state))
  
  for (district in c(1:52, "at-large")) {
    district_url <- sprintf(base_url, formatted_state, district)
    
    tryCatch({
      house_data <- fromJSON(district_url)
      
      seat <- house_data$races$seat
      nyt_id <- house_data$races$nyt_id
      office <- house_data$races$office
      updated_at <- house_data$races$updated_at
      seat_description <- house_data$races$seat_description
      
      eevp <- house_data$races$top_reporting_unit$eevp
      reporting_unit_name <- house_data$races$top_reporting_unit$name
      
      candidates_data <- house_data$races$top_reporting_unit$candidates[[1]]
      
      for (i in seq_len(nrow(candidates_data))) {
        candidate_id <- candidates_data$nyt_id[i]
        total_votes <- candidates_data$votes$total[i]
        
        all_district_results <- append(all_district_results, list(data.frame(
          state = state,
          district = as.character(district),
          seat = seat,
          nyt_id = nyt_id,
          office = office,
          updated_at = updated_at,
          seat_description = seat_description,
          eevp = eevp,
          reporting_unit_name = reporting_unit_name,
          candidate_id = candidate_id,
          total_votes = total_votes,
          stringsAsFactors = FALSE
        )))
      }
    }, error = function(e) {
      NULL
    })
  }
}

all_house_results_df <- bind_rows(all_district_results)
beep()

all_house_results_df <- all_house_results_df %>% 
  mutate(unit = paste0(state, "_", seat))

write_csv(all_senate_results_df, "house_2024_results_by_district.csv")
