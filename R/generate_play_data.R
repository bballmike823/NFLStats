#' parse the data for a given play
#'
#' @param play the play for which to generate stats
#' @param game_environment the game
#' @param play_number the numeric index of the play
#'
#' @keywords internal
#'
#' @return a row of the game play by play dataframe representing the play.
#' it includes indicators for whether  or not certain events occurred
#'
generate_play_data <-
  function(play_number, play, game_environment) {
    # Check the number of players in the play, if there is at least one then
    # go through the mapping process of stat ids, otherwise return a row full
    # of NA values:
    if (length(play$players) == 0) {
      return(initialize_NA_play_data())
    }

    # Generate a data frame containing the player level information:
    play_data <-
      create_binary_indicators(play_number, game_environment)
    # (2)
    # Next thing is to get the players involved in the play in various ways.
    # One thing to keep in mind is that on turnovers you can have players on
    # offense then make the tackle. This will happen on fumbles when a tackle
    # is made, forcing a fumble, then the team that fumbled will have tackle(s)
    # as well. To address this issue, there will just be numbered solo tackle,
    # assist tackle, forced fumble players along with columns with their
    # respective teams. This is the simplest solution for analysis.

    play_stat_matrix <- game_environment$play_stat_matrix_list[[play_number]]
    player_matrix <- game_environment$player_matrix

    stat_yards_vector <- play_stat_matrix[, 3L]
    stat_id_vector <- play_stat_matrix[, 2L]
    player_row_id_vector <- play_stat_matrix[, 1L]

    passing_player <-
      stat_id_vector %in%
      passing_indicator_stats
    if (any(passing_player)) {
      passing_player_location <- player_row_id_vector[passing_player]
      play_data$passer_player_id <- player_matrix[passing_player_location[1L], 1L]
      play_data$passer_player_name <- player_matrix[passing_player_location[1L], 2L]
    }
    # Next get the receiver information:
    receiver <- stat_in(
      stat_id_vector,
      c(
        "receiving_yards",
        "receiving_yards_td",
        "yards_after_catch",
        "targeted_receiver",
        "two_point_pass_reception_good",
        "two_point_pass_reception_failed"
      )
    )
    if (any(receiver)) {
      receiving_player <- player_row_id_vector[receiver]
      play_data$receiver_player_id <- player_matrix[receiving_player[1L], 1L]
      play_data$receiver_player_name <- player_matrix[receiving_player[1L], 2L]
    }

    # Next the rusher information:
    rusher <- stat_in(
      stat_id_vector,
      c(
        "rushing_yards",
        "rushing_yards_td",
        "two_point_rush_good",
        "two_point_rush_failed",
        "two_point_rush_safety"
      )
    )
    if (any(rusher)) {
      rushing_player <- player_row_id_vector[rusher]
      play_data$rusher_player_id <- player_matrix[rushing_player[1L], 1L]
      play_data$rusher_player_name <- player_matrix[rushing_player[1L], 2L]
    }



    # Next the lateral receiver:

    lateral_receiver <-
      stat_in(
        stat_id_vector,
        c(
          "lateral_receiving_yards",
          "lateral_receiving_yards_td"
        )
      )
    if (any(lateral_receiver)) {
      lateral_receiving_player <- player_row_id_vector[lateral_receiver]
      play_data$lateral_receiver_player_id <- player_matrix[lateral_receiving_player[1L], 1L]
      play_data$lateral_receiver_player_name <- player_matrix[lateral_receiving_player[1L], 2L]
    }

    # Next the lateral rusher information:
    lateral_rusher <-
      stat_in(
        stat_id_vector,
        c("lateral_rushing_yards", "lateral_rushing_yards_td")
      )
    if (any(lateral_rusher)) {
      lateral_rushing_player <- player_row_id_vector[lateral_rusher]
      play_data$lateral_rusher_player_id <- player_matrix[lateral_rushing_player[1L], 1L]
      play_data$lateral_rusher_player_name <- player_matrix[lateral_rushing_player[1L], 2L]
    }

    # Lateral sack player information:
    lateral_sack <-
      stat_in(
        stat_id_vector,
        c("lateral_sack_yards")
      )
    if (any(lateral_sack)) {
      lateral_sack_player <- player_row_id_vector[lateral_sack]
      play_data$lateral_sack_player_id <- player_matrix[lateral_sack_player[1L], 1L]
      play_data$lateral_sack_player_name <- player_matrix[lateral_sack_player[1L], 2L]
    }

    # Interception player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "interception_return_yards",
        "interception_return_yards_td"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$interception_player_id <- player_matrix[statistic[1L], 1L]
      play_data$interception_player_name <- player_matrix[statistic[1L], 2L]
    }

    statistic <- stat_in(
      stat_id_vector,
      c(
        "lateral_interception_return_yards",
        "lateral_interception_return_yards_td"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$lateral_interception_player_id <- player_matrix[statistic[1L], 1L]
      play_data$lateral_interception_player_name <- player_matrix[statistic[1L], 2L]
    }

    # Punt returner player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "punt_return_yards",
        "punt_return_yards_td",
        "punt_fair_catch"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$punt_returner_player_id <- player_matrix[statistic[1L], 1L]
      play_data$punt_returner_player_name <- player_matrix[statistic[1L], 2L]
    }

    # Lateral punt returner player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "lateral_punt_return_yards",
        "lateral_punt_return_yards_td"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$lateral_punt_returner_player_id <- player_matrix[statistic[1L], 1L]
      play_data$lateral_punt_returner_player_name <- player_matrix[statistic[1L], 2L]
    }

    # Kickoff returner player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "kickoff_return_yards",
        "kickoff_return_yards_td",
        "kickoff_fair_catch"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$kickoff_returner_player_id <- player_matrix[statistic[1L], 1L]
      play_data$kickoff_returner_player_name <- player_matrix[statistic[1L], 2L]
    }

    # Lateral kickoff returner player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "lateral_kickoff_return_yards",
        "lateral_kickoff_return_yards_td"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$lateral_kickoff_returner_player_id <- player_matrix[statistic[1L], 1L]
      play_data$lateral_kickoff_returner_player_name <- player_matrix[statistic[1L], 2L]
    }


    statistic <- stat_in(
      stat_id_vector,
      c(
        "kickoff_yards",
        "kickoff_inside_twenty",
        "kickoff_in_endzone",
        "kickoff_touchback_kicking",
        "kickoff_out_of_bounds",
        "field_goal_yards_missed",
        "field_goal_yards_made",
        "field_goal_yards_blocked",
        "extra_point_good",
        "extra_point_failed",
        "extra_point_blocked",
        "kickoff_yard_length"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$kicker_player_id <- player_matrix[statistic[1L], 1L]
      play_data$kicker_player_name <- player_matrix[statistic[1L], 2L]
    }
    # Punter information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "punting_yards",
        "punt_inside_twenty",
        "punt_in_endzone",
        "punt_touchback_kicking",
        "punt_out_of_bounds"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$punter_player_id <- player_matrix[statistic[1L], 1L]
      play_data$punter_player_name <- player_matrix[statistic[1L], 2L]
    }

    statistic <- stat_in(
      stat_id_vector,
      c(
        "punting_yards",
        "kickoff_yards",
        "field_goal_yards_missed",
        "field_goal_yards_made",
        "field_goal_yards_blocked"
      )
    )
    if (any(statistic)) {
      yards <- stat_yards_vector[statistic]
      play_data$kick_distance <- yards[1L]
    }

    # Kicker information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "own_kickoff_recovery",
        "own_kickoff_recovery_td"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$own_kickoff_recovery_player_id <- player_matrix[statistic[1L], 1L]
      play_data$own_kickoff_recovery_player_name <- player_matrix[statistic[1L], 2L]
    }



    # Blocking player information:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "punt_blocked_player",
        "extra_point_blocked_player",
        "field_goal_blocked_player"
      )
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$blocked_player_id <- player_matrix[statistic[1L], 1L]
      play_data$blocked_player_name <- player_matrix[statistic[1L], 2L]
    }

    # Tackle for loss information (this is a tricky one):
    statistic <- stat_in(
      stat_id_vector,
      c("tackle_for_loss_player")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$tackle_for_loss_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$tackle_for_loss_1_player_name <- player_matrix[statistic[1L], 2L]

      if (length(statistic > 1L)) {
        play_data$tackle_for_loss_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$tackle_for_loss_2_player_name <- player_matrix[statistic[2L], 2L]
      }
    }

    # Same thing for QB hit players:
    statistic <- stat_in(
      stat_id_vector,
      c("qb_hit")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$qb_hit_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$qb_hit_1_player_name <- player_matrix[statistic[1L], 2L]

      if (length(statistic > 1L)) {
        play_data$qb_hit_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$qb_hit_2_player_name <- player_matrix[statistic[2L], 2L]
      }
    }

    statistic <- stat_in(
      stat_id_vector,
      c("forced_fumble_player")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$forced_fumble_player_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$forced_fumble_player_1_player_name <- player_matrix[statistic[1L], 2L]
      play_data$forced_fumble_player_1_team <- player_matrix[statistic[1L], 3L]

      if (length(statistic > 1L)) {
        play_data$forced_fumble_player_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$forced_fumble_player_2_player_name <- player_matrix[statistic[2L], 2L]
        play_data$forced_fumble_player_2_team <- player_matrix[statistic[2L], 3L]
      }
    }


    # Solo tackler information (this is a tricky one):
    statistic <- stat_in(
      stat_id_vector,
      c("solo_tackle")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$solo_tackle_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$solo_tackle_1_player_name <- player_matrix[statistic[1L], 2L]
      play_data$solo_tackle_1_team <- player_matrix[statistic[1L], 3L]
      if (length(statistic > 1L)) {
        play_data$solo_tackle_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$solo_tackle_2_player_name <- player_matrix[statistic[2L], 2L]
        play_data$solo_tackle_2_team <- player_matrix[statistic[2L], 3L]
      }
    }

    # Next do the same thing for assist tackles except this will go up to four:
    statistic <- stat_in(
      stat_id_vector,
      c("tackle_assist", "assisted_tackle")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$assist_tackle_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$assist_tackle_1_player_name <- player_matrix[statistic[1L], 2L]
      play_data$assist_tackle_1_team <- player_matrix[statistic[1L], 3L]

      if (length(statistic > 1L)) {
        play_data$assist_tackle_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$assist_tackle_2_player_name <- player_matrix[statistic[2L], 2L]
        play_data$assist_tackle_2_team <- player_matrix[statistic[2L], 3L]

        if (length(statistic > 2L)) {
          play_data$assist_tackle_3_player_id <- player_matrix[statistic[3L], 1L]
          play_data$assist_tackle_3_player_name <- player_matrix[statistic[3L], 2L]
          play_data$assist_tackle_3_team <- player_matrix[statistic[3L], 3L]

          if (length(statistic > 3L)) {
            play_data$assist_tackle_4_player_id <- player_matrix[statistic[4L], 1L]
            play_data$assist_tackle_4_player_name <- player_matrix[statistic[4L], 2L]
            play_data$assist_tackle_4_team <- player_matrix[statistic[4L], 3L]
          }
        }
      }
    }

    # Same style for pass defense player except do not need team since this
    # will only be defense:
    statistic <- stat_in(
      stat_id_vector,
      c("pass_defense_player")
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$pass_defense_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$pass_defense_1_player_name <- player_matrix[statistic[1L], 2L]
      if (length(statistic > 1L)) {
        play_data$pass_defense_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$pass_defense_2_player_name <- player_matrix[statistic[2L], 2L]
      }
    }

    # Similar for players that fumbled since there can be multiple fumbles
    # on a play by both teams, will limit to two here since three is really rare
    statistic <- stat_in(
      stat_id_vector,
      c(
        "fumble_forced",
        "fumble_not_forced",
        "fumble_out_of_bounds",
        "fumble_lost"
      )
    )
    if (any(statistic)) {
      statistic <- unique(player_row_id_vector[statistic])
      play_data$fumbled_1_player_id <- player_matrix[statistic[1L], 1L]
      play_data$fumbled_1_player_name <- player_matrix[statistic[1L], 2L]
      play_data$fumbled_1_team <- player_matrix[statistic[1L], 3L]

      if (length(statistic > 1L)) {
        play_data$fumbled_2_player_id <- player_matrix[statistic[2L], 1L]
        play_data$fumbled_2_player_name <- player_matrix[statistic[2L], 2L]
        play_data$fumbled_2_team <- player_matrix[statistic[2L], 3L]
      }
    }

    # Again for fumble recovery players:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "own_fumble_recovery_yards",
        "own_fumble_recovery_yards_td",
        "opp_fumble_recovery_yards",
        "opp_fumble_recovery_yards_td"
      )
    )
    if (any(statistic)) {
      yards <- (unique(stat_yards_vector[statistic]))[1L]
      player1 <- (player_row_id_vector[statistic])[1L]
      play_data$fumble_recovery_1_player_id <- player_matrix[player1, 1L]
      play_data$fumble_recovery_1_player_name <- player_matrix[player1, 2L]
      play_data$fumble_recovery_1_team <- player_matrix[player1, 3L]
      play_data$fumble_recovery_1_yards <- yards

      if (length(statistic > 1L)) {
        player2 <- unique(player_row_id_vector[statistic])[2L]
        yards <-
          (stat_yards_vector[player2 == player_row_id_vector])[1L]
        play_data$fumble_recovery_2_player_id <- player_matrix[player2, 1L]
        play_data$fumble_recovery_2_player_name <- player_matrix[player2, 2L]
        play_data$fumble_recovery_2_team <- player_matrix[player2, 3L]
        play_data$fumble_recovery_2_yards <- yards
      }
    }

    # (3) Team level data (beyond the player team identifiers that are
    # already gathered above)


    assert_that(all(stat_id_vector %in% stat_map), msg = paste0("unrecognized stat: ", as.character(stat_id_vector[!stat_id_vector %in% stat_nums]), " on play number ", as.character(play_number)))


    statistic <-
      stat_id_vector %in% touchdown_indicator_stats
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$td_team <- player_matrix[statistic[1L], 3L]
    }
    # Return team:
    statistic <-
      stat_id_vector %in% return_indicator_stats
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$return_team <- player_matrix[statistic[1L], 3L]
    }

    # Timeout team:
    statistic <- stat_in(
      stat_id_vector,
      c("timeout")
    )
    if (any(statistic)) {
      statistic <- player_row_id_vector[statistic]
      play_data$timeout_team <- player_matrix[statistic[1L], 3L]
    }

    # Penalty information (team, player, and yards):
    statistic <- stat_in(
      stat_id_vector,
      c("penalty_yards")
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      statistic <- player_row_id_vector[statistic]
      play_data$penalty_player_id <- player_matrix[statistic[1L], 1L]
      play_data$penalty_player_name <- player_matrix[statistic[1L], 2L]
      play_data$penalty_yards <- stat_yards[1L]
      play_data$penalty_team <- player_matrix[statistic[1L], 3L]
    }

    statistic <- stat_in(
      stat_id_vector,
      c(
        "rushing_yards",
        "rushing_yards_td",
        "lateral_rushing_yards",
        "lateral_rushing_yards_td",
        "passing_yards",
        "passing_yards_td",
        "sack_yards",
        "receiving_yards",
        "receiving_yards_td",
        "lateral_receiving_yards",
        "lateral_receiving_yards_td"
      )
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      statistic <- player_row_id_vector[statistic]
      play_data$yards_gained <- stat_yards[1L]
      play_data$penalty_override <- TRUE
      play_data$penalty_fix<-1
    }

    statistic <- stat_in(
      stat_id_vector,
      c(
        "interception_return_yards",
        "interception_return_yards_td",
        "lateral_interception_return_yards",
        "lateral_interception_return_yards_td",
        "punt_return_yards",
        "punt_return_yards_td",
        "lateral_punt_return_yards",
        "lateral_punt_return_yards_td",
        "kickoff_return_yards",
        "kickoff_return_yards_td",
        "lateral_kickoff_return_yards",
        "lateral_kickoff_return_yards_td"
      )
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      play_data$return_yards <- stat_yards[1L]
      play_data$penalty_override <- TRUE
      play_data$return_pentaly_fix<-1
    }

    # Air yards:
    statistic <- stat_in(
      stat_id_vector,
      c("air_yards_complete", "air_yards_incomplete")
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      play_data$air_yards <- stat_yards[1L]
    }

    # Yards after catch:
    statistic <- stat_in(
      stat_id_vector,
      c("yards_after_catch")
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      play_data$yards_after_catch <- stat_yards[1L]
    }

    # Kickoff, punt, and field goal distance:
    statistic <- stat_in(
      stat_id_vector,
      c(
        "punting_yards",
        "kickoff_yards",
        "field_goal_yards_missed",
        "field_goal_yards_made",
        "field_goal_yards_blocked"
      )
    )
    if (any(statistic)) {
      stat_yards <- stat_yards_vector[statistic]
      play_data$kick_distace <- stat_yards[1L]
    }
    statistic <- stat_in(
      stat_id_vector,
      c(
        "punt_inside_twenty",
        "punt_in_endzone",
        "punt_out_of_bounds",
        "punt_downed",
        "punt_fair_catch",
        "kickoff_inside_twenty",
        "kickoff_in_endzone",
        "kickoff_out_of_bounds",
        "kickoff_downed",
        "kickoff_fair_catch"
      )
    )
    if (any(statistic)) {
      play_data$penalty_override <- TRUE
      play_data$return_penalty_fix<-1
    }
    return(play_data)
  }

#' fill in indicator variable in the play by play table for a given play
#'
#' @param play_number the index of the play in the play_stat_matrix_list
#' @param game_environment contains preprocessed structures for analyzing the game
#'
#' @keywords internal
#'
#' @return a row of the play by play table with indicators filled in
create_binary_indicators <-
  function(play_number, game_environment) {
    play_data <- initialize_empty_play_data()
    unique_player_stats <-
      unique(game_environment$play_stat_matrix_list[[play_number]][, 2L])

    play_data[names(one_to_one_indicator_stats)[one_to_one_indicator_stats %in% unique_player_stats]] <- 1L


    play_data$rush_attempt <- as.numeric(any(rushing_indicator_stats %in% unique_player_stats))
    play_data$pass_attempt <- as.numeric(any(pass_attempt_indicator_stats %in% unique_player_stats))
    play_data$sack <- as.numeric(any(sack_indicator_stats %in% unique_player_stats))
    play_data$touchdown <- as.numeric(any(touchdown_indicator_stats %in% unique_player_stats))
    play_data$pass_touchdown <- as.numeric(any(pass_touchdown_indicator_stats %in% unique_player_stats))
    play_data$rush_touchdown <- as.numeric(any(rush_touchdown_indicator_stats %in% unique_player_stats))
    play_data$return_touchdown <- as.numeric(any(return_touchdown_indicator_stats %in% unique_player_stats))
    play_data$extra_point_attempt <- as.numeric(any(extra_point_attempt_indicator_stats %in% unique_player_stats))
    play_data$two_point_attempt <- as.numeric(any(two_point_attempt_indicator_stats %in% unique_player_stats))
    play_data$field_goal_attempt <- as.numeric(any(field_goal_attempt_indicator_stats %in% unique_player_stats))
    play_data$kickoff_attempt <- as.numeric(any(kickoff_attempt_indicator_stats %in% unique_player_stats))
    play_data$punt_attempt <- as.numeric(any(punt_attempt_indicator_stats %in% unique_player_stats))
    play_data$fumble <- as.numeric(any(fumble_indicator_stats %in% unique_player_stats))
    play_data$complete_pass <- as.numeric(any(complete_pass_indicator_stats %in% unique_player_stats))
    play_data$assist_tackle <- as.numeric(any(assist_tackle_indicator_stats %in% unique_player_stats))
    play_data$lateral_reception <- as.numeric(any(unique_player_stats %in% lateral_reception_indicator_stats))
    play_data$lateral_rush <- as.numeric(any(unique_player_stats %in% lateral_rush_indicator_stats))
    play_data$lateral_return <- as.numeric(any(unique_player_stats %in% lateral_return_indicator_stats))
    play_data$lateral_recovery <- as.numeric(any(unique_player_stats %in% lateral_recovery_indicator_stats))
  return(play_data)
    }





#' match integer stat id's to named stats
#'
#' @param play_stat_id_vector integer vector containing NFL stat id's of the stats accrued during a play
#' @param stats_to_check_for_vector character vector containing names of stats to check for
#'
#' @keywords internal
#'
#' @return a logical vector that tells which elements of the stat id vector are contained in the stat names
stat_in <-
  function(play_stat_id_vector,
             stats_to_check_for_vector) {
    return(play_stat_id_vector %in% stat_map[stats_to_check_for_vector])
  }
