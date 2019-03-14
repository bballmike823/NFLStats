#' download and process an NFL game from the internet
#'
#' @param game_id the unique game identifier
#' @param check_url check whether the URL for the game is valid
#' @param return_all_data return a list containing the table as well as the preprocessing structures
#'
#' @return play by play table
#'
#' @include stat_mappings.R
#' @include game_environment.R
#' @include game_play_by_play_modifiers.R
#'
#' @export
download_and_parse_game <-
  function(game_id,
             check_url = TRUE, return_all_data = FALSE) {
    game_id <- as.integer(game_id)
    raw_data <- download_game_raw_data(game_id, check_url)
    parse_raw_game_data(raw_data, return_all_data)
  }

#' convert raw game data into table
#'
#' @param raw_data raw game data list created by RJSONIO from the NFL JSON
#' @param return_all_data return auxililary data structures in addition to the table
#'
#' @return play by play table
#' @export
parse_raw_game_data <- function(raw_data, return_all_data = FALSE) {
  game_environment <- generate_game_environment_from_raw_data(raw_data)

  game_pbp <- create_pbp_table(game_environment, game_environment$team_columns, play_list = game_environment$play_list, play_id_matrix = game_environment$play_id_matrix)
  game_pbp <- add_basic_info(game_pbp)
  game_pbp <- yard_line_info(game_pbp)

  game_pbp <- add_date_columns(game_pbp, game_environment$game_id)

  game_pbp <-extract_timing_info(game_pbp)
  #game_pbp$game_seconds_remaining <- game_pbp$quarter_seconds_remaining + (900 * (4 - min(game_pbp$qtr, 4)))
  game_pbp <- replay_challenge_info(game_pbp)
  game_pbp <- two_point_conversion_info(game_pbp)
  game_pbp <- extract_penalty_type(game_pbp)
  game_pbp <-convert_zero_down_to_NA(game_pbp)
  game_pbp <- add_goal_to_go(game_pbp)
  game_pbp <-add_field_goal_result(game_pbp)
  game_pbp <- add_extra_point_result(game_pbp)
  game_pbp <-add_pass_descriptors(game_pbp)
  game_pbp <- add_play_descriptors(game_pbp)
  game_pbp <-add_run_descriptors(game_pbp)
  game_pbp <- add_half(game_pbp)
  game_pbp <- add_timeout_info(game_pbp)
  game_pbp <- add_scoring_info(game_pbp)

  game_pbp <- add_nflscrapR_variables(game_pbp)
  game_pbp <- select_columns_nflscrapR_final_order(game_pbp)

  if (return_all_data) {
    return(list(play_by_play = game_pbp, game_environment = game_environment))
  }
  return(game_pbp)
}

#' Create the url to fetch the game json data
#'
#' @param game_id the game_id for which the url will be created
#' @param check_url confirm that the url can be reached
#'
#' @return the url for the game JSON
#' @export
get_json_url <- function(game_id, check_url = TRUE) {
  json_url<-paste0(
    "http://www.nfl.com/liveupdate/game-center/",
    game_id,
    "/",
    game_id,
    "_gtd.json"
  )
  assert_that((!check_url)||RCurl::url.exists(json_url),
              msg = paste("Unable to connect to", game_url, "please confirm that", as.character(game_id), "is a valid game id")
  )
  return(json_url)
}

#' Fetch the game file structure from the NFL website
#'
#' @param game_id the game_id whose data is to be fetched
#' @param check_url confirm that the url can be reached
#'
#' @return the JSON data from the NFL converted into an R readable format
#' @export
download_game_raw_data <- function(game_id, check_url = TRUE) {
  game_url <- get_json_url(game_id, check_url)
  RJSONIO::fromJSON(RCurl::getURL(game_url))
}


create_pbp_table <-
  function(game_environment, team_columns, play_list, play_id_matrix) {
    basic_play_info <- bind_cols(create_base_play_info_df(play_list), as.data.frame(play_id_matrix))
    # Apply the get_drive_play_data function to each drive in the game,
    # and row bind all the plays
    game_pbp <-
      generate_game_play_by_play_data(game_environment)


    # Add to the basic_play_info:
    game_pbp <- bind_cols(basic_play_info, game_pbp)
    game_pbp <- bind_cols(game_pbp, team_columns)
    return(game_pbp)
  }

create_base_play_info_df <-
  function(play_list) {
    play_by_play_table <- data.frame(do.call(rbind, play_list))[, c(1:7, 9, 10)]
    # note and yrdln are the only variables where I have encountered nulls
    # this should be handled better, but works for now
    play_by_play_table$note <-
      lapply(play_by_play_table$note, function(x)
        if_else(is.null(x), NA_character_, x))
    play_by_play_table$yrdln <-
      lapply(play_by_play_table$yrdln, function(x)
        if_else(is.null(x), NA_character_, x))
    mutate_all(play_by_play_table, unlist)
  }

#' Extract variables from every play and merge them into a data frame
#'
#' @param game_environment
#'
#' @keywords internal
#'
#' @return data frame containing the variable that are processed from every play
generate_game_play_by_play_data <-
  function(game_environment) {
    return(suppressWarnings(purrr::map_dfr(
      1L:game_environment$play_count,
      function(play_number) {
        generate_play_data(play_number, game_environment$play_list[[play_number]], game_environment)
      }
    )))
  }

#' Converts integer columns to doubles so that nflscrapR ep and wp functions work properly
#'
#' @param game_pbp play by play table
#'
#' @keywords internal
#'
#' @return the play by play table with the columns having the correct class for the ep and wp formulas
convert_types_for_nflscrapR <- function(game_pbp) {
  temp <- head(game_pbp, 1)
  integer_cols <- logical(length(temp))
  for (index in 1:length(temp))
  {
    integer_cols[index] <- is.integer(temp[[index]])
  }
  name_list <- names(game_pbp)
  name_list <- name_list[integer_cols]
  game_pbp <- mutate_at(game_pbp, vars(name_list), as.double)
  return(mutate(game_pbp,game_id = as.integer(game_id), drive = as.integer(drive), quarter_end = as.double(quarter_end)))
}

#' calculate ep and wp using the functions from nflscrapR
#'
#' @param game_pbp play by play table
#'
#' @keywords internal
#'
#' @return play by play table with columns for ep and wp stats
add_nflscrapR_variables <- function(game_pbp) {
  game_pbp<-select_and_order_for_nflscrapR_ep_wp(convert_types_for_nflscrapR(game_pbp))
  nflscrapR::add_air_yac_wp_variables(nflscrapR::add_wp_variables(
    nflscrapR::add_air_yac_ep_variables(nflscrapR::add_ep_variables(game_pbp))
  ))
}

#' Put the columns in the order necessary for the ep and wp functions
#'
#' @param game_pbp the play by play table
#'
#' @keywords internal
#'
#' @return the play by play table with the columns in the correct positions for the ep and wp functions
select_and_order_for_nflscrapR_ep_wp <- function(game_pbp) {
  game_pbp <- mutate(
    game_pbp,
    posteam = case_when(
      quarter_end == 1 ~ NA_character_,
      desc == "Two-Minute Warning" ~ NA_character_,
      TRUE ~ posteam
    ),
    defteam = case_when(
      quarter_end == 1 ~ NA_character_,
      desc == "Two-Minute Warning" ~ NA_character_,
      TRUE ~ defteam
    ),
    posteam_type = case_when(
      quarter_end == 1 ~ NA_character_,
      desc == "Two-Minute Warning" ~ NA_character_,
      TRUE ~ posteam_type
    ),
    goal_to_go = if_else(
      quarter_end == 1 | desc == "Two-Minute Warning",
      NA_real_,
      goal_to_go
    ),
    qb_dropback = if_else(
      quarter_end == 1 | desc == "Two-Minute Warning",
      NA_real_,
      qb_dropback
    ),
    posteam_score = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      posteam_score
    ),
    defteam_score = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      defteam_score
    ),
    posteam_score_post = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      posteam_score_post
    ),
    defteam_score_post = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      defteam_score_post
    ),
    score_differential = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      score_differential
    ),
    score_differential_post = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_real_,
      score_differential_post
    ),
    play_type = if_else(
      quarter_end == 1 |
        desc == "Two-Minute Warning",
      NA_character_,
      play_type
    ),
    yardline_100 = case_when(
      quarter_end == 1 ~ NA_real_,
      desc == "Two-Minute Warning" ~ NA_real_,
      TRUE ~ yardline_100
    ),
    posteam_timeouts_remaining = case_when(
      quarter_end == 1 ~ NA_real_,
      desc == "Two-Minute Warning" ~ NA_real_,
      TRUE ~ posteam_timeouts_remaining
    ),
    defteam_timeouts_remaining = case_when(
      quarter_end == 1 ~ NA_real_,
      desc == "Two-Minute Warning" ~ NA_real_,
      TRUE ~ defteam_timeouts_remaining
    ),
    play_id = as.character(play_id) # ,
    # quarter_end = as.logical(quarter_end)
  )

  return(select(
    game_pbp,
    drive,
    sp,
    qtr,
    down,
    time,
    yrdln,
    ydstogo,
    ydsnet,
    posteam,
    desc,
    punt_blocked,
    first_down_rush,
    first_down_pass,
    first_down_penalty,
    third_down_converted,
    third_down_failed,
    fourth_down_converted,
    fourth_down_failed,
    incomplete_pass,
    interception,
    punt_inside_twenty,
    punt_in_endzone,
    punt_out_of_bounds,
    punt_downed,
    punt_fair_catch,
    kickoff_inside_twenty,
    kickoff_in_endzone,
    kickoff_out_of_bounds,
    kickoff_fair_catch,
    fumble_forced,
    fumble_not_forced,
    fumble_out_of_bounds,
    timeout,
    field_goal_missed,
    field_goal_made,
    field_goal_blocked,
    extra_point_good,
    extra_point_failed,
    extra_point_blocked,
    two_point_rush_good,
    two_point_rush_failed,
    two_point_pass_good,
    two_point_pass_failed,
    solo_tackle,
    safety,
    penalty,
    tackled_for_loss,
    extra_point_safety,
    two_point_rush_safety,
    two_point_pass_safety,
    kickoff_downed,
    two_point_pass_reception_good,
    two_point_pass_reception_failed,
    fumble_lost,
    own_kickoff_recovery,
    own_kickoff_recovery_td,
    qb_hit,
    extra_point_aborted,
    two_point_return,
    rush_attempt,
    pass_attempt,
    sack,
    touchdown,
    pass_touchdown,
    rush_touchdown,
    return_touchdown,
    extra_point_attempt,
    two_point_attempt,
    field_goal_attempt,
    kickoff_attempt,
    punt_attempt,
    fumble,
    complete_pass,
    assist_tackle,
    lateral_reception,
    lateral_rush,
    lateral_return,
    lateral_recovery,
    passer_player_id,
    passer_player_name,
    receiver_player_id,
    receiver_player_name,
    rusher_player_id,
    rusher_player_name,
    lateral_receiver_player_id,
    lateral_receiver_player_name,
    lateral_rusher_player_id,
    lateral_rusher_player_name,
    lateral_sack_player_id,
    lateral_sack_player_name,
    interception_player_id,
    interception_player_name,
    lateral_interception_player_id,
    lateral_interception_player_name,
    punt_returner_player_id,
    punt_returner_player_name,
    lateral_punt_returner_player_id,
    lateral_punt_returner_player_name,
    kickoff_returner_player_name,
    kickoff_returner_player_id,
    lateral_kickoff_returner_player_id,
    lateral_kickoff_returner_player_name,
    punter_player_id,
    punter_player_name,
    kicker_player_name,
    kicker_player_id,
    own_kickoff_recovery_player_id,
    own_kickoff_recovery_player_name,
    blocked_player_id,
    blocked_player_name,
    tackle_for_loss_1_player_id,
    tackle_for_loss_1_player_name,
    tackle_for_loss_2_player_id,
    tackle_for_loss_2_player_name,
    qb_hit_1_player_id,
    qb_hit_1_player_name,
    qb_hit_2_player_id,
    qb_hit_2_player_name,
    forced_fumble_player_1_team,
    forced_fumble_player_1_player_id,
    forced_fumble_player_1_player_name,
    forced_fumble_player_2_team,
    forced_fumble_player_2_player_id,
    forced_fumble_player_2_player_name,
    solo_tackle_1_team,
    solo_tackle_2_team,
    solo_tackle_1_player_id,
    solo_tackle_2_player_id,
    solo_tackle_1_player_name,
    solo_tackle_2_player_name,
    assist_tackle_1_player_id,
    assist_tackle_1_player_name,
    assist_tackle_1_team,
    assist_tackle_2_player_id,
    assist_tackle_2_player_name,
    assist_tackle_2_team,
    assist_tackle_3_player_id,
    assist_tackle_3_player_name,
    assist_tackle_3_team,
    assist_tackle_4_player_id,
    assist_tackle_4_player_name,
    assist_tackle_4_team,
    pass_defense_1_player_id,
    pass_defense_1_player_name,
    pass_defense_2_player_id,
    pass_defense_2_player_name,
    fumbled_1_team,
    fumbled_1_player_id,
    fumbled_1_player_name,
    fumbled_2_player_id,
    fumbled_2_player_name,
    fumbled_2_team,
    fumble_recovery_1_team,
    fumble_recovery_1_yards,
    fumble_recovery_1_player_id,
    fumble_recovery_1_player_name,
    fumble_recovery_2_team,
    fumble_recovery_2_yards,
    fumble_recovery_2_player_id,
    fumble_recovery_2_player_name,
    td_team,
    return_team,
    timeout_team,
    yards_gained,
    return_yards,
    air_yards,
    yards_after_catch,
    penalty_team,
    penalty_player_id,
    penalty_player_name,
    penalty_yards,
    kick_distance,
    defensive_two_point_attempt,
    defensive_two_point_conv,
    defensive_extra_point_attempt,
    defensive_extra_point_conv,
    penalty_fix,
    return_penalty_fix,
    play_id,
    game_id,
    home_team,
    away_team,
    quarter_end,
    posteam_type,
    defteam,
    side_of_field,
    yardline_100,
    game_year,
    game_month,
    game_date,
    quarter_seconds_remaining,
    half_seconds_remaining,
    game_seconds_remaining,
    replay_or_challenge,
    replay_or_challenge_result,
    two_point_conv_result,
    penalty_type,
    goal_to_go,
    field_goal_result,
    extra_point_result,
    pass_length,
    pass_location,
    qb_kneel,
    qb_spike,
    qb_scramble,
    shotgun,
    no_huddle,
    play_type,
    qb_dropback,
    run_location,
    run_gap,
    game_half,
    home_timeouts_remaining,
    away_timeouts_remaining,
    home_timeout_used,
    away_timeout_used,
    total_home_timeouts_used,
    total_away_timeouts_used,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    home_points_scored,
    away_points_scored,
    total_home_score,
    total_away_score,
    posteam_score,
    defteam_score,
    score_differential,
    abs_score_differential,
    posteam_score_post,
    defteam_score_post,
    score_differential_post,
    abs_score_differential_post,
    touchback
  ))
}
