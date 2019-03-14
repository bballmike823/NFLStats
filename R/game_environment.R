# TODO this doesn't need to be an environment
# TODO this is an organizational disaster.
# TODO restructure pre processing to make the program as a whole more organized


#' downloads the raw game data and creates preprocessing structures
#'
#' @param game_id the game id for which the environment is to be created
#' @param check_url whether or not to check the validity of the url created from the game id
#'
#' @return an environment that contains data structures to be used when processing the game
#' @export
generate_game_environment <- function(game_id, check_url = TRUE) {
  game_id <- as.integer(game_id)
  raw_data <- download_game_raw_data(game_id, check_url)
  generate_game_environment_from_raw_data(raw_data, game_id)
}

#' create preprocessing structures from raw game data
#'
#' @param raw_game_data the list structure returned from download_game_raw_data
#' @param game_id the id number for the game.
#'
#' @return a group of data structures that will be used when creating the game play by play
#' @export
generate_game_environment_from_raw_data <-
  function(raw_game_data, game_id = NULL) {
    if (!is.null(game_id)) {
      assert_that(as.integer(names(raw_game_data)[1]) == as.integer(game_id), msg = "supplied game id does not match the game id in JSON")
    } else {
      game_id <- as.integer(names(raw_game_data)[1])
    }
    home_team <- raw_game_data[[1]]$home$abbr
    away_team <- raw_game_data[[1]]$away$abbr

    # Store the number of drives in the game
    drive_list <- raw_game_data[[1]]$drives
    drive_list$crntdrv <- NULL
    drive_number_vector <- 1L:length(drive_list)
    # the if here is due to the one game that is not properly coded regarding its third drive):
    if (game_id == 2013092206) {
      drive_number_vector <-
        drive_number_vector[-3]
    }

    pl <-
      generate_play_list(game_id, drive_list, drive_number_vector, home_team, away_team)
    play_list <- pl$play_list
    play_count <- pl$play_count
    play_id_matrix <- pl$play_id_matrix
    team_columns <- pl$team_columns

    pt <- generate_player_table(home_team, away_team, play_list)
    player_matrix <- pt$player_matrix
    player_id_hash_table <- pt$player_id_hash_table
    num_players <- pt$num_players

    play_stat_matrix_list <-
      generate_play_stat_matrix_list(play_list, play_count, player_id_hash_table)

    game_environment <- new.env()

    game_environment$raw_game_data <- raw_game_data
    game_environment$game_id <- game_id
    game_environment$home_team <- home_team
    game_environment$away_team <- away_team
    game_environment$drive_list <- drive_list
    game_environment$drive_number_vector <- drive_number_vector

    game_environment$play_list <- play_list
    game_environment$play_count <- play_count
    game_environment$play_id_matrix <- play_id_matrix
    game_environment$team_columns <- team_columns

    game_environment$player_matrix <- player_matrix
    game_environment$player_id_hash_table <- player_id_hash_table
    game_environment$num_players <- num_players

    game_environment$play_stat_matrix_list <- play_stat_matrix_list

    return(game_environment)
  }

generate_play_list <-
  function(game_id, drive_list,
             drive_number_vector,
             home_team, away_team) {
    posteam_type <- character(0)
    posteam <- character(0)
    defteam <- character(0)
    play_list <- list()
    drive_ids <- integer(0)
    play_ids <- integer(0)
    play_counter <- 0L
    for (drive_number in drive_number_vector)
    {
      drive <- drive_list[[drive_number]]
      nplays <- drive$numplays
      if (nplays == 0) {
        next
      }
      posteam_type <-
        c(posteam_type, rep(ifelse(home_team == drive$posteam, "home", "away"), nplays))
      posteam <- c(posteam, rep(drive$posteam, nplays))
      defteam <-
        c(defteam, rep(ifelse(home_team == drive$posteam, away_team, home_team), nplays))
      drive_ids <- c(drive_ids, rep(drive_number, nplays))
      play_ids <- c(play_ids, as.integer(names(drive$plays)))
      for (play_number in 1L:drive$numplays)
      {
        play_counter <- play_counter + 1L
        play_list <- c(play_list, list(drive$plays[[play_number]]))
      }
    }
    result <- list()
    result$play_list <- play_list
    result$play_count <- play_counter
    result$play_id_matrix <-
      matrix(c(rep(game_id, play_counter), drive_ids, play_ids),
        ncol = 3,
        dimnames = list(NULL, c("game_id", "drive", "play_id"))
      )
    result$team_columns <- data.frame(home_team = rep(home_team, play_counter), away_team = rep(away_team, play_counter), posteam = posteam, posteam_type = posteam_type, defteam = defteam, stringsAsFactors = FALSE)
    return(result)
  }

#' create a compact matrix to hold all player data needed to process the game
#'
#' @param home_team the abbreviation of the home team
#' @param away_team the abbreviation of the away team
#' @param play_list the list of plays for the game
#'
#' @keywords internal
#'
#' @return a list containing
#' (1) player_matrix
#' contains the player id, name, and team for each unique player who recorded a stat in the game.
#' (2) player_id_hash_table
#' maps the NFL player id string to the appropriate row of the player_matrix.
#' (3) num_players
#' the number of players in the player_matrix
generate_player_table <- function(home_team, away_team, play_list) {
  player_id_hash_table <-
    hashmap::hashmap(keys = character(), values = integer())
  player_vect <-
    c(
      home_team,
      NA_character_,
      home_team,
      away_team,
      NA_character_,
      away_team
    )
  player_id_hash_table$insert(home_team, 1L)
  player_id_hash_table$insert(away_team, 2L)
  player_counter <- 2L
  for (play in play_list)
  {
    index <- 1L
    for (player_id in names(play$players))
    {
      player <- play$players[[index]]
      player_name <- player[[1L]]$playerName
      if (player_id == "0") {
        index <- index + 1L
        next
      }
      if (!player_id_hash_table$has_key(player_id)) {
        player_counter <- player_counter + 1L
        player_id_hash_table$insert(player_id, player_counter)
        player_vect <- c(
          player_vect, player_id,
          player_name,
          player[[1L]]$clubcode
        )
      }
      index <- index + 1L
    }
  }
  result <- list()
  result$player_id_hash_table <- player_id_hash_table
  result$player_matrix <-
    matrix(
      player_vect,
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c(
        "player_id", "player_name", "player_team"
      ))
    )
  result$num_players <- player_counter
  return(result)
}

#' Create a list of compact matrices to store the player stats for each play
#'
#' @param play_count the number of plays in the game
#' @param play_list list of plays in the game pulled from the NFL's JSON
#' @param player_id_hash_table hasmap that maps the NFL player_id's to rows in the player_matrix
#'
#' @keywords internal
#'
#' @return a list containing matrices of the player stats for each play in the game.
#' each matrix has 3 columns:
#' player_matrix_row_num - the row in the player_matrix which corresponds to the player who earned this stat
#' stat_id - The integer code that the NFL uses to designate different stat events
#' yards - the yards associated to this stat
#' the rows of the matrix are in order of the sequence specified in the NFL JSON
generate_play_stat_matrix_list <-
  function(play_list, play_count = length(play_list),
             player_id_hash_table) {
    play_stat_matrix_list <-
      vector("list", play_count)
    for (play_number in 1L:play_count)
    {
      play <- play_list[[play_number]]
      players <- play$players
      if (length(players) == 0) {
        next
      }
      play_length <- 0L
      for (player in players)
      {
        play_length <- play_length + length(player)
      }
      play_stat_matrix <-
        matrix(
          nrow = play_length,
          ncol = 3,
          dimnames = list(NULL, c(
            "player_matrix_row_num", "stat_id", "yards"
          ))
        )
      for (player_number in 1L:length(players))
      {
        player <- players[[player_number]]
        player_id <- names(players)[player_number]
        if (player_id != "0") {
          player_matrix_row <- player_id_hash_table$find(player_id)
          for (player_stat_number in 1L:length(player))
          {
            play_stat_matrix[player[[player_stat_number]]$sequence, ] <-
              c(
                player_matrix_row,
                as.integer(player[[player_stat_number]]$statId),
                as.integer(player[[player_stat_number]]$yards)
              )
          }
        } else {
          for (player_stat_number in 1L:length(player))
          {
            player_matrix_row <-
              player_id_hash_table$find(player[[player_stat_number]]$clubcode)
            play_stat_matrix[player[[player_stat_number]]$sequence, ] <-
              c(
                player_matrix_row,
                as.integer(player[[player_stat_number]]$statId),
                as.integer(player[[player_stat_number]]$yards)
              )
          }
        }
      }
      play_stat_matrix_list[[play_number]] <- play_stat_matrix
    }
    return(play_stat_matrix_list)
  }
