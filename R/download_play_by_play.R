#' @import dplyr
#' @importFrom assertthat is.count is.scalar assert_that
#' @import purrr
#' @import stringr


#' @title pull data from an nfl season.
#'
#' @param season the year for which you want stats
#' @param type pre or reg whether you want preseason or regular season stats
#'
#' @include parse_game.R
#' @include game_environment.R
#'
#' @return a tibble containing play by playstats for the specified nfl season
#'
#' @export
download_season_play_by_play_data <-
  function(season, type = "reg") {
    type <- check_type(type)
    season <- check_season(season)
    season_data <- tibble()
    week_data <- tibble()
    start_week <- case_when(
      type == "reg" ~ 1L,
      type == "pre" & season == 2011 ~ 1L,
      type == "pre" ~ 0L,
      type == "post" ~ 18L,
      TRUE ~ NA_integer_
    )
    end_week <- case_when(
      type == "reg" ~ 17L,
      type == "pre" ~ 4L,
      type == "post" ~ 18L,
      TRUE ~ NA_integer_
    )

    for (week in start_week:end_week)
    {
      week_data <-
        download_week_play_by_play_data(
          season,
          type,
          week
        )
      if (week == start_week) {
        season_data <- week_data
      }
      else {
        season_data <- bind_rows(season_data, week_data)
      }
    }
    return(season_data)
  }

#' pull data from an nfl week
#'
#' @param season the year for which you want stats
#' @param type pre or reg whether you want preseason or regular season stats
#' @param week the week of the season for wich you want stats
#'
#' @return a tibble containing play by playstats for the specified nfl season
#'
#' @export
download_week_play_by_play_data <-
  function(season, type = "reg", week = NULL) {
    type <- check_type(type)
    season <- check_season(season)

    game_ids <- find_game_ids(season, type, week, check_url = TRUE)

    week_data <- NULL
    # download play by play for each game
    for (id in game_ids)
    {
      if (is.null(week_data)) {
        week_data <-
          download_game_play_by_play_data(id, check_url = FALSE)
      } else {
        temp_game_data <-
          download_game_play_by_play_data(
            game_id_num = id,
            check_url = FALSE
          )
        week_data <- bind_rows(week_data, temp_game_data)
      }
    }
    return(week_data)
  }

#' pull data from an nfl game
#'
#' @param game_id_num the unique game identifier
#' @param check_url check the validity of the URL created for the game id
#'
#' @return a tibble containing play by playstats for the specified nfl season
#'
#' @export
download_game_play_by_play_data <-
  function(game_id_num, check_url = TRUE) {
    # TODO this function is pointless at the moment
    download_and_parse_game(game_id_num, check_url)
  }


#' retrieve the individual game ID's from the NFL schedules
#'
#' @param season the year
#' @param type pre reg or post
#' @param week the week of the season
#'
#' @return an integer vector containing the game id's from the NFL schedule page
#' @export
find_game_ids <-
  function(season,
             type,
             week = NULL,
             check_url = TRUE) {
    season <- check_season(season)
    type <- check_type(type)
    if (type == "post") {
      week <- NULL
    } else {
      assert_that(is.scalar(week) | is.null(week), msg = "week must be a single value or null")
      if (!is.null(week)) {
        if (is.character(week)) {
          assert_that((nchar(week) == 1 & grepl("[:digit:]", week)) | (nchar(week) == 2 & grepl("1[:digit:]", week)), msg = "week must have one or two digits")
          week <- as.integer(week)
        } else if (!is.integer(week)) {
          assert_that(week == 0 | is.count(week), msg = "week must be a whole number")
          week <- as.integer(week)
        }
        assert_that(((week >= 0L & week <= 4L & type == "pre") | (week >= 1L & week <= 17L & type == "reg")), msg = "invalid week. week must be between 0 and 4 for the preseason and 1 and 17 for the regular season")
      }
    }
    schedule_url <- paste0(
      "http://www.nfl.com/schedules/",
      season,
      "/",
      toupper(type),
      week
    )
    if (check_url) {
      assert_that(RCurl::url.exists(schedule_url),
        msg = paste0("unable to connect to: ", schedule_url)
      )
    }
    game_ids <-
      as.integer(str_match_all(
        RCurl::getURL(schedule_url),
        "(?:data-gameid=\")([0-9]{10})(?:\")"
      )[[1]][, 2])
    # remove games with corrupted data
    if (tolower(type) == "pre") {
      if (season == 2014) {
        warning("game 2014081503 has corrupted data and has been ommitted from results")
        game_ids <- game_ids[game_ids != 2014081503]
      } else if (season == 2016) {
        warning("game 2016080751 has corrupted data and has been ommitted from results")
        game_ids <- game_ids[game_ids != 2016080751]
      }
    } else if (season == 2013) {
      if(2013112401 %in% game_ids)
        warning(paste0("Due to a error by the NFL, game 2013112401 is missing yardage data"))
      if(2013120101 %in% game_ids)
        warning(paste0("Due to a error by the NFL, game 2013120101 is missing yardage data"))
    }
    return(game_ids)
  }

check_type <- function(type) {
  assert_that(is.scalar(type) & !is.list(type), msg = "type must have a single value")
  assert_that(is.character(type), msg = "type must be a character")
  type <- tolower(type)
  assert_that(type == "pre" | type == "reg" | type == "post", msg = paste0("season type must be pre, reg, or post. ", type, " is not a valid type."))
  return(type)
}

check_season <- function(season) {
  assert_that(is.scalar(season) & !is.list(season), msg = "season must be a single value")
  if (is.character(season)) {
    assert_that(nchar(season) == 4, msg = "season must have 4 digits")
    assert_that(grepl("[:digit:]{4}", season), msg = "season must be a number")
    season <- as.integer(season)
  } else if (!is.integer(season)) {
    assert_that(is.count(season), msg = "season must be an integer number")
    season <- as.integer(season)
  }
  assert_that(season >= 2009L, msg = "Years prior to 2009 are not supported")
  return(season)
}
