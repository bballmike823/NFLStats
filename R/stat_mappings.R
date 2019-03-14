# This file contains a mapping of known statId's from the nfl to names that describe the event indicated by the statId
# Documentation from the NFL regarding statId's can be found here http://www.nflgsis.com/gsis/documentation/Partners/StatIDs.html
# The names are the same as those used by nflscrapR https://github.com/maksimhorowitz/nflscrapR
# It also contains groups of stats that are used to define various indicators
stat_map <- c(
  2L:16L,
  19L:64L,
  68L:80L,
  82L:89L,
  91L,
  93L,
  95L:96L,
  99L,
  100L,
  102L:108L,
  110L:113L,
  115L,
  120L,
  301L,
  402L:406L,
  410L,
  420L
)
names(stat_map) <- c(
  "punt_blocked",
  "first_down_rush",
  "first_down_pass",
  "first_down_penalty",
  "third_down_converted",
  "third_down_failed",
  "fourth_down_converted",
  "fourth_down_failed",
  "rushing_yards",
  "rushing_yards_td",
  "lateral_rushing_yards",
  "lateral_rushing_yards_td",
  "incomplete_pass",
  "passing_yards",
  "passing_yards_td",
  "interception",
  "sack_yards",
  "receiving_yards",
  "receiving_yards_td",
  "lateral_receiving_yards",
  "lateral_receiving_yards_td",
  "interception_return_yards",
  "interception_return_yards_td",
  "lateral_interception_return_yards",
  "lateral_interception_return_yards_td",
  "punting_yards",
  "punt_inside_twenty",
  "punt_in_endzone",
  "punt_touchback_kicking",
  "punt_return_yards",
  "punt_return_yards_td",
  "lateral_punt_return_yards",
  "lateral_punt_return_yards_td",
  "punt_out_of_bounds",
  "punt_downed",
  "punt_fair_catch",
  "punt_touchback_receiving",
  "kickoff_yards",
  "kickoff_inside_twenty",
  "kickoff_in_endzone",
  "kickoff_touchback_kicking",
  "kickoff_return_yards",
  "kickoff_return_yards_td",
  "lateral_kickoff_return_yards",
  "lateral_kickoff_return_yards_td",
  "kickoff_out_of_bounds",
  "kickoff_fair_catch",
  "kickoff_touchback_receiving",
  "fumble_forced",
  "fumble_not_forced",
  "fumble_out_of_bounds",
  "own_fumble_recovery_yards",
  "own_fumble_recovery_yards_td",
  "lateral_own_fumble_recovery_yards",
  "lateral_own_fumble_recovery_yards_td",
  "opp_fumble_recovery_yards",
  "opp_fumble_recovery_yards_td",
  "lateral_opp_fumble_recovery_yards",
  "lateral_opp_fumble_recovery_yards_td",
  "miscellaneous_yards",
  "miscellaneous_yards_td",
  "timeout",
  "field_goal_yards_missed",
  "field_goal_yards_made",
  "field_goal_yards_blocked",
  "extra_point_good",
  "extra_point_failed",
  "extra_point_blocked",
  "two_point_rush_good",
  "two_point_rush_failed",
  "two_point_pass_good",
  "two_point_pass_failed",
  "solo_tackle",
  "assisted_tackle",
  "tackle_assist",
  "solo_sack_yards",
  "assist_sack_yards",
  "pass_defense_player",
  "punt_blocked_player",
  "extra_point_blocked_player",
  "field_goal_blocked_player",
  "safety_tackle",
  "forced_fumble_player",
  "penalty_yards",
  "tackled_for_loss",
  "extra_point_safety",
  "two_point_rush_safety",
  "two_point_pass_safety",
  "kickoff_downed",
  "lateral_sack_yards",
  "two_point_pass_reception_good",
  "two_point_pass_reception_failed",
  "fumble_lost",
  "own_kickoff_recovery",
  "own_kickoff_recovery_td",
  "qb_hit",
  "air_yards_complete",
  "air_yards_incomplete",
  "yards_after_catch",
  "targeted_receiver",
  "tackle_for_loss_player",
  "extra_point_aborted",
  "tackle_for_loss_yards",
  "defensive_two_point_attempt",
  "defensive_two_point_conv",
  "defensive_extra_point_attempt",
  "defensive_extra_point_conv",
  "kickoff_yard_length",
  "two_point_return"
)


#' return all stats that contain a certain term in their name
#'
#' @param part_of_name string to search stats for
#'
#' @return named integer vector with stats that contain part_of_name
find_stats_with_name <- function(part_of_name) {
  return(stat_map[str_detect(names(stat_map), part_of_name)])
}

generate_one_to_one_indicators<-function(){
one_to_one_indicator_stats_temp <- stat_map[c(
  "punt_blocked",
  "first_down_rush",
  "first_down_pass",
  "first_down_penalty",
  "third_down_converted",
  "third_down_failed",
  "fourth_down_converted",
  "fourth_down_failed",
  "incomplete_pass",
  "interception",
  "punt_inside_twenty",
  "punt_in_endzone",
  "punt_out_of_bounds",
  "punt_downed",
  "punt_fair_catch",
  "kickoff_inside_twenty",
  "kickoff_in_endzone",
  "kickoff_out_of_bounds",
  "kickoff_fair_catch",
  "fumble_forced",
  "fumble_not_forced",
  "fumble_out_of_bounds",
  "timeout",
  "field_goal_yards_missed",
  "field_goal_yards_made",
  "field_goal_yards_blocked",
  "extra_point_good",
  "extra_point_failed",
  "extra_point_blocked",
  "two_point_rush_good",
  "two_point_rush_failed",
  "two_point_pass_good",
  "two_point_pass_failed",
  "solo_tackle",
  "safety_tackle",
  "penalty_yards",
  "tackled_for_loss",
  "extra_point_safety",
  "two_point_rush_safety",
  "two_point_pass_safety",
  "kickoff_downed",
  "two_point_pass_reception_good",
  "two_point_pass_reception_failed",
  "fumble_lost",
  "own_kickoff_recovery",
  "own_kickoff_recovery_td",
  "qb_hit",
  "extra_point_aborted",
  "defensive_two_point_attempt",
  "defensive_two_point_conv",
  "defensive_extra_point_attempt",
  "defensive_extra_point_conv",
  "two_point_return"
)]
# Some one to one indicators have overly specific names which we rename here
names(one_to_one_indicator_stats_temp)[match(c("safety_tackle", "penalty_yards", "field_goal_yards_missed", "field_goal_yards_made", "field_goal_yards_blocked"), names(one_to_one_indicator_stats_temp))] <- c("safety", "penalty", "field_goal_missed", "field_goal_made", "field_goal_blocked")
return(one_to_one_indicator_stats_temp)
}
one_to_one_indicator_stats<-generate_one_to_one_indicators()
passing_indicator_stats <- stat_map[c(
  "passing_yards",
  "passing_yards_td",
  "incomplete_pass",
  "interception",
  "sack_yards",
  "air_yards_complete",
  "air_yards_incomplete",
  "two_point_pass_good",
  "two_point_pass_failed",
  "two_point_pass_safety"
)]

passer_indicator_stats <- stat_map[c(
  "passing_yards",
  "passing_yards_td",
  "incomplete_pass",
  "interception",
  "sack_yards",
  "air_yards_complete",
  "air_yards_incomplete",
  "two_point_pass_good",
  "two_point_pass_failed",
  "two_point_pass_safety"
)]

rushing_indicator_stats <- stat_map[c(
  "rushing_yards",
  "rushing_yards_td",
  "lateral_rushing_yards",
  "lateral_rushing_yards_td",
  "two_point_rush_good",
  "two_point_rush_failed",
  "two_point_rush_safety"
)]

pass_attempt_indicator_stats <- stat_map[c(
  "passing_yards",
  "passing_yards_td",
  "incomplete_pass",
  "interception",
  "sack_yards",
  "receiving_yards",
  "receiving_yards_td",
  "lateral_receiving_yards",
  "lateral_receiving_yards_td",
  "interception_return_yards",
  "interception_return_yards_td",
  "lateral_interception_return_yards",
  "lateral_interception_return_yards_td",
  "air_yards_complete",
  "air_yards_incomplete",
  "yards_after_catch",
  "targeted_receiver",
  "two_point_pass_good",
  "two_point_pass_failed",
  "two_point_pass_safety",
  "two_point_pass_reception_good",
  "two_point_pass_reception_failed"
)]

touchdown_indicator_stats <- find_stats_with_name("_td")

return_indicator_stats <- stat_map[c(
  find_stats_with_name("_return"),
  "punt_touchback_receiving",
  "punt_downed",
  "punt_fair_catch",
  "kickoff_fair_catch",
  "kickoff_touchback_receiving"
)]

sack_indicator_stats <- stat_map[c(
  "sack_yards", "solo_sack_yards",
  "assist_sack_yards"
)]

pass_touchdown_indicator_stats <- stat_map[c(
  "passing_yards_td",
  "receiving_yards_td",
  "lateral_receiving_yards_td"
)]


rush_touchdown_indicator_stats <- stat_map[c(
  "rushing_yards_td",
  "lateral_rushing_yards_td"
)]

return_touchdown_indicator_stats <- stat_map[c(
  "interception_return_yards_td",
  "lateral_interception_return_yards_td",
  "kickoff_return_yards_td",
  "lateral_kickoff_return_yards_td",
  "punt_return_yards_td",
  "lateral_punt_return_yards_td"
)]

extra_point_attempt_indicator_stats <- stat_map[c(
  "extra_point_good",
  "extra_point_failed",
  "extra_point_blocked",
  "extra_point_safety",
  "extra_point_aborted"
)]

two_point_attempt_indicator_stats <- stat_map[c(
  "two_point_rush_good",
  "two_point_rush_failed",
  "two_point_pass_good",
  "two_point_pass_failed",
  "two_point_rush_safety",
  "two_point_pass_safety",
  "two_point_pass_reception_good",
  "two_point_pass_reception_failed",
  "two_point_return"
)]


field_goal_attempt_indicator_stats <- find_stats_with_name("field_goal")

kickoff_attempt_indicator_stats <- find_stats_with_name("kickoff")

punt_attempt_indicator_stats <- find_stats_with_name("punt")

fumble_indicator_stats <- find_stats_with_name("fumble")

complete_pass_indicator_stats <- stat_map[c(
  "passing_yards",
  "passing_yards_td",
  "receiving_yards",
  "receiving_yards_td",
  "lateral_receiving_yards",
  "lateral_receiving_yards_td",
  "air_yards_complete",
  "yards_after_catch"
)]

assist_tackle_indicator_stats <- stat_map[c(
  "assisted_tackle",
  "tackle_assist",
  "assist_sack_yards"
)]

lateral_indicator_stats <- find_stats_with_name("lateral")


lateral_reception_indicator_stats <- stat_map[c(
  "lateral_receiving_yards",
  "lateral_receiving_yards_td"
)]

lateral_rush_indicator_stats <- stat_map[c(
  "lateral_rushing_yards",
  "lateral_rushing_yards_td"
)]

lateral_return_indicator_stats <- stat_map[c(
  "lateral_interception_return_yards",
  "lateral_interception_return_yards_td",
  "lateral_punt_return_yards",
  "lateral_punt_return_yards_td",
  "lateral_kickoff_return_yards",
  "lateral_kickoff_return_yards_td"
)]

lateral_recovery_indicator_stats <- stat_map[c(
  "lateral_own_fumble_recovery_yards",
  "lateral_own_fumble_recovery_yards_td",
  "lateral_opp_fumble_recovery_yards",
  "lateral_opp_fumble_recovery_yards_td"
)]

#character vector containing the names of stats at the index of thier id.
#fastest way of converting an id to a name
generate_stat_names_by_index <- function() {
  stat_names_by_index <- rep(NA, 420)
  for (i in 1L:length(stat_map)) stat_names_by_index[stat_map[i]] <- names(stat_map[i])
  return(stat_names_by_index)
}
stat_names_by_index <- generate_stat_names_by_index()
