#' detect wheter or not the game is at the end of a quarter
#'
#' @param game_description description of the play from play by play data
#'
#' @keywords internal
#'
#' @return TRUE or FALSE if the game is at the end of a quarter
quarter_end_status <- function(game_description) {
  str_detect(
    game_description,
    "(END QUARTER)|(END GAME)"
  ) |
    str_detect(game_description, "End of game")
}

#' add the date information to the game play by play table
#'
#' @param game_id the id of the game from which to draw date information
#' @param game_play_by_play the game play by play table
#'
#' @keywords internal
#'
#' @return the game play by play table with date and time info added
add_date_columns <- function(game_play_by_play, game_id) {
  date_parse <- as.character(game_id)
  date_year <- str_sub(date_parse, 1, 4)
  date_month <- str_sub(date_parse, 5, 6)
  date_day <- str_sub(date_parse, 7, 8)
  mutate(game_play_by_play,
    game_year = date_year,
    game_month = date_month,
    game_date = as.Date(paste(date_month,
      date_day,
      date_year,
      sep = "/"
    ),
    format = "%m/%d/%Y"
    )
  )
}

extract_timing_info <- function(game_play_by_play) {
  # Need to create columns for properly formatting the time columns:
  # Create a column with the time in seconds remaining for the quarter:
  mutate(game_play_by_play,
    quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
    # Create a column with the time in seconds remaining for each half:
    half_seconds_remaining = quarter_seconds_remaining + 900 * as.numeric(qtr %in% c(1, 3)),
    # Create a column with the time in seconds remaining for the game:
    game_seconds_remaining = quarter_seconds_remaining + (900 * (4 - pmin(qtr, 4)))
  )
}
# DO NOT USE THIS WHEN A CHALLENGE OR REPLAY DID NOT OCCUR
parse_challenge_result <- function(description) {
  result<-str_extract(
    tolower(description),
    "( upheld)|( reversed)|( confirmed)"
  )
  ifelse(is.na(result),"denied",str_trim(result))
}

# Create a play type column: either pass, run, field_goal, extra_point,
# kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
# penalties):
add_play_type <- function(game_play_by_play) {
  mutate(
    game_play_by_play,
    play_type = case_when(
      quarter_end == 1 ~ "quarter_end",
      desc == "Two-Minute Warning" ~ "two_minute_warning",
      is.na(penalty) ~ "no_play",
      penalty == 1 & (!penalty_override) ~ "no_play",
      qb_spike == 1 ~ "qb_spike",
      qb_kneel == 1 ~ "qb_kneel",
      pass_attempt == 1 ~ "pass",
      incomplete_pass == 1 ~ "pass",
      two_point_pass_good == 1 ~ "pass",
      two_point_pass_failed == 1 ~ "pass",
      two_point_pass_safety == 1 ~ "pass",
      two_point_pass_reception_good == 1 ~ "pass",
      two_point_pass_reception_failed == 1 ~ "pass",
      pass_touchdown == 1 ~ "pass",
      complete_pass == 1 ~ "pass",
      two_point_rush_good == 1 ~ "run",
      two_point_rush_failed == 1 ~ "run",
      two_point_rush_safety == 1 ~ "run",
      rush_attempt == 1 ~ "run",
      rush_touchdown == 1 ~ "run",
      kickoff_attempt == 1 ~ "kickoff",
      punt_attempt == 1 ~ "punt",
      field_goal_attempt == 1 ~ "field_goal",
      extra_point_attempt == 1 ~ "extra_point",
      TRUE ~ "no_play"
    )
  )
}

add_timeout_info <- function(game_play_by_play) {
  game_play_by_play <-
    mutate(game_play_by_play,
      home_timeouts_remaining = as.numeric(qtr %in% c(1, 2, 3, 4)) + 2,
      away_timeouts_remaining = as.numeric(qtr %in% c(1, 2, 3, 4)) + 2,
      home_timeout_used = as.numeric(timeout == 1 &
        timeout_team == home_team),
      away_timeout_used = as.numeric(timeout == 1 &
        timeout_team == away_team),
      home_timeout_used = if_else(is.na(home_timeout_used),
        0, home_timeout_used
      ),
      away_timeout_used = if_else(is.na(away_timeout_used),
        0, away_timeout_used
      )
    )
  # Group by the game_half to then create cumulative timeouts used for both
  # the home and away teams:
  game_play_by_play <- ungroup(mutate(group_by(game_play_by_play, game_half),
    total_home_timeouts_used = cumsum(home_timeout_used),
    total_away_timeouts_used = cumsum(away_timeout_used)
  ))
  # Now just take the difference between the timeouts remaining
  # columns and the total timeouts used, and create the columns for both
  # the pos and def team timeouts remaining:
  mutate(game_play_by_play,
    home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
    away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
    posteam_timeouts_remaining = if_else(
      posteam == home_team,
      home_timeouts_remaining,
      away_timeouts_remaining
    ),
    defteam_timeouts_remaining = if_else(
      defteam == home_team,
      home_timeouts_remaining,
      away_timeouts_remaining
    )
  )
}
# TODO this should be done elsewhere
add_scoring_info <- function(game_play_by_play) {
  mutate(game_play_by_play,
    home_points_scored = case_when(
      touchdown == 1 &
        td_team == home_team ~ 6,
      posteam == home_team &
        field_goal_made == 1 ~ 3,
      posteam == home_team &
        (
          extra_point_good == 1 |
            extra_point_safety == 1 |
            two_point_rush_safety == 1 |
            two_point_pass_safety == 1
        ) ~ 1,
      posteam == home_team &
        (
          two_point_rush_good == 1 |
            two_point_pass_good == 1 |
            two_point_pass_reception_good == 1
        ) ~ 2,
      defteam == home_team &
        (safety == 1 |
          two_point_return == 1) ~ 2,
      TRUE ~ 0
    ),
    away_points_scored = case_when(
      touchdown == 1 &
        td_team == away_team ~ 6,
      posteam == away_team &
        field_goal_made == 1 ~ 3,
      posteam == away_team &
        (
          extra_point_good == 1 |
            extra_point_safety == 1 |
            two_point_rush_safety == 1 |
            two_point_pass_safety == 1
        ) ~ 1,
      posteam == away_team &
        (
          two_point_rush_good == 1 |
            two_point_pass_good == 1 |
            two_point_pass_reception_good == 1
        ) ~ 2,
      defteam == away_team &
        (safety == 1 |
          two_point_return == 1) ~ 2,
      TRUE ~ 0
    ),
    # Now create cumulative totals:
    total_home_score = cumsum(home_points_scored),
    total_away_score = cumsum(away_points_scored),
    posteam_score = if_else(
      posteam == home_team,
      lag(total_home_score),
      lag(total_away_score)
    ),
    defteam_score = if_else(
      defteam == home_team,
      lag(total_home_score),
      lag(total_away_score)
    ),
    score_differential = posteam_score - defteam_score,
    abs_score_differential = abs(score_differential),
    # Make post score differential columns to be used for the final
    # game indicators in the win probability calculations:
    posteam_score_post = if_else(posteam == home_team,
      total_home_score,
      total_away_score
    ),
    defteam_score_post = if_else(defteam == home_team,
      total_home_score,
      total_away_score
    ),
    score_differential_post = posteam_score_post - defteam_score_post,
    abs_score_differential_post = abs(posteam_score_post - defteam_score_post)
  )
}

replay_challenge_info <- function(game_play_by_play) {
  mutate(game_play_by_play,
    # Add column for replay or challenge:
    replay_or_challenge = as.integer(str_detect(
      desc,
      "(Replay Official reviewed)|( challenge(d)? )"
    )),
    # Result of replay or challenge:
    replay_or_challenge_result = if_else(
      replay_or_challenge == 1,
      parse_challenge_result(desc),
      NA_character_
    )
  )
}
# TODO case_when to improve readability
two_point_conversion_info <- function(game_play_by_play) {
  mutate(game_play_by_play,
    two_point_conv_result = case_when(
      (
        two_point_rush_good == 1 |
          two_point_pass_good == 1 |
          two_point_pass_reception_good == 1
      ) &
        two_point_attempt == 1 ~
      "success",
      (two_point_rush_failed == 1 |
        two_point_pass_failed == 1 |
        two_point_pass_reception_failed == 1
      ) &
        two_point_attempt == 1 ~
      "failure",
      (two_point_rush_safety == 1 |
        two_point_pass_safety == 1) &
        two_point_attempt == 1 ~ "safety",
      two_point_return == 1 &
        two_point_attempt == 1 ~ "return",
      TRUE ~ NA_character_
    ),
    # If the result was a success, make the yards_gained to be 2:
    # Debug forcing yards gained to integer
    yards_gained = if_else(
      !is.na(two_point_conv_result) &
        two_point_conv_result == "success",
      2L,
      as.integer(yards_gained)
    )
  )
}

yard_line_info <- function(game_play_by_play) {
  temp <- mutate(
    game_play_by_play,
    yrdln = case_when(
      yrdln == "50" ~ "MID 50",
      nchar(yrdln) == 0 | is.null(yrdln) | yrdln == "NULL" ~ lag(yrdln),
      TRUE ~ yrdln
    ),
    # Create two columns: one for side of field and the other as the numeric
    # distance from the opponents end zone:
    # TODO get rid of purrr
    side_of_field = purrr::map_chr(
      str_split(yrdln, " "),
      function(x)
        x[1]
    ),
    yardline = as.integer(purrr::map_chr(
      str_split(yrdln, " "),
      function(x)
        x[2]
    ))
  )
  select(mutate(
    temp,
    yardline_100 = case_when(
      is.na(posteam) ~ NA_integer_,
      is.na(side_of_field) ~ NA_integer_,
      (side_of_field == posteam) ~ 100L - yardline,
      (yardline == 50) ~ 50L,
      TRUE ~ yardline
    )
  ), -yardline)
}

extract_penalty_type <- function(game_play_by_play) {
  mutate(
    game_play_by_play,
    penalty_type = if_else(
      penalty == 1,
      extract_penalty_from_description(desc),
      NA_character_
    )
  )
}
extract_penalty_from_description <- function(play_desc) {
  penalty_desc <-
    str_extract(play_desc, "PENALTY on (.){2,35},.+, [0-9]{1,2} yard(s)?,")
  penalty_desc <- str_extract(penalty_desc, "(, (([:alpha:])+([:space:])?)+([(][0-9]{1,2} Yard(s)?[)])?,)")
  penalty_desc <- str_remove_all(penalty_desc, ",")
  str_trim(penalty_desc)
}

add_goal_to_go <- function(game_play_by_play) {
  mutate(game_play_by_play, goal_to_go = as.numeric(side_of_field != posteam &
    ((ydstogo == yardline_100) |
      (ydstogo <= 1 &
        yardline_100 == 1))))
}

add_field_goal_result <- function(game_play_by_play) {
  mutate(game_play_by_play,
    # Using the field goal indicators make a column with the field goal result:
    field_goal_result = case_when(
      field_goal_attempt == 1 &
        field_goal_made == 1 ~ "made",
      field_goal_attempt == 1 &
        field_goal_missed == 1 ~
      "missed",
      field_goal_attempt == 1 &
        field_goal_blocked == 1 ~
      "blocked",
      TRUE ~ NA_character_
    )
  )
}

add_extra_point_result <- function(game_play_by_play) {
  mutate(game_play_by_play,
    kick_distance = if_else(
      extra_point_attempt == 1,
      yardline_100 + 18L,
      as.integer(kick_distance)
    ),
    # Using the indicators make a column with the extra point result:
    extra_point_result = case_when(
      extra_point_attempt == 0 ~ NA_character_,
      extra_point_good == 1 ~ "good",
      extra_point_failed == 1 ~ "failed",
      extra_point_blocked == 1 ~ "blocked",
      extra_point_safety == 1 ~ "safety",
      extra_point_aborted == 1 ~ "aborted",
      TRUE ~ NA_character_
    )
  )
}

add_pass_descriptors <- function(game_play_by_play) {
  mutate(game_play_by_play,
    # Create the column denoting the categorical description of the pass length:
    pass_length = if_else(
      two_point_attempt == 0 &
        sack == 0 &
        pass_attempt == 1,
      str_extract(str_extract(desc, "pass (incomplete )?(short|deep)"), "short|deep"),
      NA_character_
    ),
    # Create the column denoting the categorical location of the pass:
    pass_location = if_else(
      two_point_attempt == 0 &
        sack == 0 &
        pass_attempt == 1,
      str_extract(str_extract(desc, "(short|deep) (left|middle|right)"), "left|middle|right"),
      NA_character_
    )
  )
}


add_play_descriptors <- function(game_play_by_play) {
  game_play_by_play <- mutate(game_play_by_play,
    # Indicator columns for both QB kneels, spikes, scrambles,
    # no huddle, shotgun plays:
    qb_kneel = as.integer(str_detect(desc, " kneels ")),
    qb_spike = as.integer(str_detect(desc, " spiked ")),
    qb_scramble = as.integer(str_detect(desc, " scrambles ")),
    shotgun = as.integer(str_detect(desc, "Shotgun")),
    no_huddle = as.integer(str_detect(desc, "No Huddle"))
  )
  mutate(add_play_type(game_play_by_play),
    # Indicator for QB dropbacks (exclude spikes and kneels):
    qb_dropback = as.numeric(play_type == "pass" |
      (play_type == "run" &
        qb_scramble == 1)),
    touchback = as.numeric(str_detect(tolower(desc), "touchback"))
  )
}

add_run_descriptors <- function(game_play_by_play) {
  mutate(game_play_by_play,
    # Columns denoting the run location and gap:
    run_location = if_else(
      two_point_attempt == 0 &
        rush_attempt == 1,
      str_trim(str_extract(desc, " (left|middle|right) ")),
      NA_character_
    ),
    run_gap = if_else(
      two_point_attempt == 0 &
        rush_attempt == 1,
      str_trim(str_extract(desc, " (guard|tackle|end) ")),
      NA_character_
    )
  )
}
# Make plays marked with down == 0 as NA:
convert_zero_down_to_NA <- function(game_play_by_play) {
  mutate(game_play_by_play, down = if_else(down == 0,
    NA_integer_, as.integer(down)
  ))
}

add_basic_info <- function(game_play_by_play) {
  mutate(game_play_by_play,
    quarter_end = quarter_end_status(desc),
    two_minute_warning = desc == "Two-Minute Warning",
    time = if_else(quarter_end, "00:00", time)
  )
}


add_half <- function(game_play_by_play) {
  mutate(game_play_by_play,
    game_half = case_when(
      qtr %in% c(1L, 2L) ~ "Half1",
      qtr %in% c(3L, 4L) ~ "Half2",
      qtr >= 5L ~ "Overtime",
      TRUE ~ "error"
    )
  )
}
#' arrange the play by play table to match output from nflscrapR
#'
#' @param game_play_by_play play by play table
#'
#' @keywords internal
#'
#' @return the play by play table with only the columns returned by nflscrapR in the order returned by nflscrapR
select_columns_nflscrapR_final_order <- function(game_play_by_play) {
  select(
    game_play_by_play,
    play_id,
    game_id,
    home_team,
    away_team,
    posteam,
    posteam_type,
    defteam,
    side_of_field,
    yardline_100,
    game_date,
    quarter_seconds_remaining,
    half_seconds_remaining,
    game_seconds_remaining,
    game_half,
    quarter_end,
    drive,
    sp,
    qtr,
    down,
    goal_to_go,
    time,
    yrdln,
    ydstogo,
    ydsnet,
    desc,
    play_type,
    yards_gained,
    shotgun,
    no_huddle,
    qb_dropback,
    qb_kneel,
    qb_spike,
    qb_scramble,
    pass_length,
    pass_location,
    air_yards,
    yards_after_catch,
    run_location,
    run_gap,
    field_goal_result,
    kick_distance,
    extra_point_result,
    two_point_conv_result,
    home_timeouts_remaining,
    away_timeouts_remaining,
    timeout,
    timeout_team,
    td_team,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    total_home_score,
    total_away_score,
    posteam_score,
    defteam_score,
    score_differential,
    posteam_score_post,
    defteam_score_post,
    score_differential_post,
    no_score_prob,
    opp_fg_prob,
    opp_safety_prob,
    opp_td_prob,
    fg_prob,
    safety_prob,
    td_prob,
    extra_point_prob,
    two_point_conversion_prob,
    ep,
    epa,
    total_home_epa,
    total_away_epa,
    total_home_rush_epa,
    total_away_rush_epa,
    total_home_pass_epa,
    total_away_pass_epa,
    air_epa,
    yac_epa,
    comp_air_epa,
    comp_yac_epa,
    total_home_comp_air_epa,
    total_away_comp_air_epa,
    total_home_comp_yac_epa,
    total_away_comp_yac_epa,
    total_home_raw_air_epa,
    total_away_raw_air_epa,
    total_home_raw_yac_epa,
    total_away_raw_yac_epa,
    wp,
    def_wp,
    home_wp,
    away_wp,
    wpa,
    home_wp_post,
    away_wp_post,
    total_home_rush_wpa,
    total_away_rush_wpa,
    total_home_pass_wpa,
    total_away_pass_wpa,
    air_wpa,
    yac_wpa,
    comp_air_wpa,
    comp_yac_wpa,
    total_home_comp_air_wpa,
    total_away_comp_air_wpa,
    total_home_comp_yac_wpa,
    total_away_comp_yac_wpa,
    total_home_raw_air_wpa,
    total_away_raw_air_wpa,
    total_home_raw_yac_wpa,
    total_away_raw_yac_wpa,
    punt_blocked,
    first_down_rush,
    first_down_pass,
    first_down_penalty,
    third_down_converted,
    third_down_failed,
    fourth_down_converted,
    fourth_down_failed,
    incomplete_pass,
    touchback,
    interception,
    punt_inside_twenty,
    punt_in_endzone,
    punt_out_of_bounds,
    punt_downed,
    punt_fair_catch,
    kickoff_inside_twenty,
    kickoff_in_endzone,
    kickoff_out_of_bounds,
    kickoff_downed,
    kickoff_fair_catch,
    fumble_forced,
    fumble_not_forced,
    fumble_out_of_bounds,
    solo_tackle,
    safety,
    penalty,
    tackled_for_loss,
    fumble_lost,
    own_kickoff_recovery,
    own_kickoff_recovery_td,
    qb_hit,
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
    return_team,
    return_yards,
    penalty_team,
    penalty_player_id,
    penalty_player_name,
    penalty_yards,
    replay_or_challenge,
    replay_or_challenge_result,
    penalty_type,
    defensive_two_point_attempt,
    defensive_two_point_conv,
    defensive_extra_point_attempt,
    defensive_extra_point_conv
  )
}
