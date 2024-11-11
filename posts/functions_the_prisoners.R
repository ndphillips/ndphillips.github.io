# Simulate Prisoner Game | simulate_prisoner_game() ----------------------------

#' Simulate the prisoner game
#'
#' @param prisoners_n integer. The number of prisoners in each team
#' @param teams_n integer. The number of (separate) teams of prisoners
#' @param pick_max integer. The maximum number of boxes individual prisoners can open
#' @param method character. The method to use for simulation. Either "agent" or "environment"
#'
#' @return
#' @export
#'
#' @examples
simulate_prisoner_game <- function(prisoners_n = 100,
                                   teams_n = 10,
                                   pick_max = floor(prisoners_n / 2),
                                   method = c("environment", "agent")) {
  method <- match.arg(method)

  game <- create_prisoner_game(
    teams_n = teams_n,
    prisoners_n = prisoners_n,
    pick_max = pick_max,
    method = method
  )

  game <- add_game_outcomes(game)

  game <- add_game_summaries(game)

  return(game)
}

#' Create a new prisoner game object
#'
#' @param teams_n integer. The number of teams of prisoners
#' @param prisoners_n integer. The number of prisoners per team
#' @param pick_max integer. The maximum number of boxes individual prisoners can open
#' @param method character. The method to use for simulation. Either "agent" or "environment"
#'
#' @return list. A list with the structure of the game
#'
create_prisoner_game <- function(teams_n, prisoners_n, pick_max, method = "environment") {
  pb <- progress::progress_bar$new(
    format = "Team :current/:total [:bar] :percent eta: :eta",
    total = teams_n,
    clear = FALSE
  )

  params <- list(
    method = method,
    pick_max = pick_max,
    teams_n = teams_n,
    prisoners_n = prisoners_n
  )

  out <- structure(list(
    pb = pb,
    params = params,
    outcomes = NULL,
    summaries = NULL,
    processing = NULL
  ), class = c(method, "prisoner_game"))

  return(out)
}

add_game_outcomes <- function(prisoner_game) {
  sim_time_start <- Sys.time()

  prisoner_game <- dispatch_add_game_outcomes(prisoner_game)

  sim_time_end <- Sys.time()

  sim_duration <- as.numeric(difftime(sim_time_end, sim_time_start, units = "secs"))

  prisoner_game$processing$sim_duration <- sim_duration

  return(prisoner_game)
}

dispatch_add_game_outcomes <- function(x) {
  UseMethod("add_game_outcomes")
}

add_game_summaries <- function(prisoner_game) {
  p_success <- mean(prisoner_game$outcomes$is_success)

  prisoner_game$summaries$p_success <- p_success

  return(prisoner_game)
}

#' Add game outcomes using the environment method
#'
#' @param prisoner_game prisoner_game. A game created by `create prisoner_game`
#'
#' @return
#' @export
#'
#' @examples
add_game_outcomes.environment <- function(prisoner_game) {
  teams_n <- util_get_param(prisoner_game, "teams_n")
  prisoners_n <- util_get_param(prisoner_game, "prisoners_n")
  pick_max <- util_get_param(prisoner_game, "pick_max")

  rooms_ls <- purrr::map(
    1:teams_n,
    \(x) {
      prisoner_game$pb$tick()
      create_room(prisoners_n) |>
        add_loops()
    }
  )

  outcomes_tbl <- rooms_ls |>
    purrr::map(
      \(x) {
        tibble::tibble(
          is_success = max(x$index_in_loop) <= pick_max,
          boxes_opened_n_max = max(x$index_in_loop)
        )
      }
    ) |>
    purrr::list_rbind()

  prisoner_game$outcomes <- outcomes_tbl

  return(prisoner_game)
}

#' Add game outcomes using the agent method
#'
#' @param prisoner_game prisoner_game. A game created by `create prisoner_game`
#'
#' @return
#' @export
#'
#' @examples
add_game_outcomes.agent <- function(prisoner_game) {
  teams_n <- util_get_param(prisoner_game, "teams_n")
  prisoners_n <- util_get_param(prisoner_game, "prisoners_n")
  pick_max <- util_get_param(prisoner_game, "pick_max")

  rooms_ls <- purrr::map(1:teams_n, \(x) {
    create_room(x)
  })

  outcomes_tbl <- purrr::map(
    1:teams_n,
    \(x) {
      prisoner_game$pb$tick()
      simulate_agent_team(
        rooms_ls[[x]],
        pick_max = pick_max
      )
    }
  ) |>
    purrr::map_dfr(
      \(x) {
        tibble::tibble(
          boxes_opened_n_max = max(x$boxes_opened_n)
        )
      }
    ) |>
    dplyr::mutate(is_success = boxes_opened_n_max <= pick_max) |>
    dplyr::select(is_success, boxes_opened_n_max)

  prisoner_game$outcomes <- outcomes_tbl

  return(prisoner_game)
}

#' Simulate the Prisoners and Boxes Game
#'
#' @param prisoners_n integer. The number of prisoners
#'
#' @return list. A list with the following elements:
#'   - is_success: logical. Whether all prisoners were successful
#'   - boxes_opened_n_vec: integer vector. The number of boxes opened by each prisoner
#'   - team_result_ls: list. The results of each prisoner's search
#'
#' @examples
simulate_agent_team <- function(room,
                                pick_max = NULL) {
  prisoners_n <- nrow(room)

  team_result_ls <- purrr::map(
    1:prisoners_n,
    \(prisoner_i) {
      simulate_agent_prisoner(
        room = room,
        prisoner_i = prisoner_i,
        pick_max = pick_max
      )
    }
  )

  boxes_opened_n_vec <- purrr::map_int(
    team_result_ls,
    \(x) x$boxes_opened_n
  )

  out <- tibble::tibble(
    boxes_opened_n = boxes_opened_n_vec
  )

  return(out)
}

util_get_param <- function(game, param) {
  game$params[[param]]
}


#' Title
#'
#' @param room tbl. A tibble created by create_room()
#' @param prisoner_i integer. The prisoner number
#' @param pick_max integer. The maximum number of boxes to search
#'
#' @return  list
#'
#' @export
#'
#' @examples
simulate_agent_prisoner <- function(room = NULL,
                                    prisoner_i = 1,
                                    pick_max = NULL) {
  pick_i <- 1
  ticket_v <- room |>
    dplyr::filter(box == prisoner_i) |>
    dplyr::pull(ticket)

  is_success_i <- FALSE

  if (ticket_v == prisoner_i) {
    is_success_i <- TRUE
  }

  while (length(ticket_v) <= nrow(room) && is_success_i == FALSE) {
    pick_i <- pick_i + 1

    ticket_i <- room |>
      dplyr::filter(box == ticket_v[pick_i - 1]) |>
      dplyr::pull(ticket)

    ticket_v <- c(ticket_v, ticket_i)

    if (ticket_i == prisoner_i) {
      is_success_i <- TRUE
    }
  }

  is_success_i <- length(ticket_v) <= pick_max

  out <- list(
    prisoner = prisoner_i,
    is_success = is_success_i,
    ticket_v = ticket_v,
    boxes_opened_n = length(ticket_v)
  )

  return(out)
}

# Create Room | create_room() --------------------------------------------------

#' Create a room of boxes for prisoners to search in
#' @param boxes_n integer. The number of boxes in the room
#' @return tibble
#'
create_room <- function(boxes_n = 100) {
  tickets_v <- sample(1:boxes_n,
    size = boxes_n
  )

  room <- tibble::tibble(
    box = 1:boxes_n,
    ticket = tickets_v
  )

  room
}

# Add loops to room | add_loops() ----------------------------------------------

#' Add loop information to a room object created by `create_room()`
#'
#' @param room tbl. A tibble created by create_room()
#'
#' @return tbl. A tibble with the following columns added:
#'  - loop: integer. The loop number
#'  - index_in_loop: integer. The index of the box in the loop
add_loops <- function(room) {
  room <- room |>
    dplyr::mutate(
      loop = c(1, rep(NA_integer_, nrow(room) - 1)),
      index_in_loop = c(1, rep(NA_integer_, nrow(room) - 1))
    )

  while (any(is.na(room$loop))) {
    loop_current <- util_get_current_loop(room)
    loop_box_start <- util_get_start_box_in_loop(room, loop_current)
    last_ticket <- util_get_last_ticket(room)
    loop_current_last_index <- util_get_last_index_in_loop(room, loop_current)

    room <- room |>
      util_set_box_loop_and_index(
        .loop_current = loop_current,
        .last_ticket = last_ticket
      )
  }
  room
}

util_set_box_loop_and_index <- function(room, .last_ticket, .loop_current) {
  last_index_in_loop <- util_get_last_index_in_loop(room, .loop_current)
  start_box_in_loop <- util_get_start_box_in_loop(room, .loop_current)

  if (.last_ticket == start_box_in_loop) {
    room <- room |>
      dplyr::mutate(
        loop = dplyr::if_else(is.na(loop) & box == min(dplyr::if_else(is.na(loop), box, Inf)), .loop_current + 1, loop),
        index_in_loop = dplyr::if_else(loop == .loop_current + 1, 1, index_in_loop)
      )
  } else {
    room <- room |>
      dplyr::mutate(
        loop = dplyr::if_else(box == .last_ticket, .loop_current, loop),
        index_in_loop = dplyr::if_else(box == .last_ticket, last_index_in_loop + 1, index_in_loop)
      )
  }

  return(room)
}

util_get_last_ticket <- function(room) {
  ticket <- room |>
    dplyr::filter(loop == max(loop, na.rm = TRUE)) |>
    dplyr::filter(index_in_loop == max(index_in_loop, na.rm = TRUE)) |>
    dplyr::pull(ticket)

  return(ticket)
}

util_get_last_index_in_loop <- function(room, .loop) {
  last_index <- room |>
    dplyr::filter(loop == .loop) |>
    dplyr::filter(index_in_loop == max(index_in_loop, na.rm = TRUE)) |>
    dplyr::pull(index_in_loop)

  return(last_index)
}

util_get_start_box_in_loop <- function(room, .loop) {
  start_box <- room |>
    dplyr::filter(loop == .loop) |>
    dplyr::filter(index_in_loop == 1) |>
    dplyr::pull(box)

  return(start_box)
}

util_get_current_loop <- function(room) {
  current_loop <- room |>
    dplyr::filter(!is.na(loop)) |>
    dplyr::pull(loop) |>
    max(na.rm = TRUE)

  return(current_loop)
}

# IN PROGRESS ------------------------------------------------------------------

#' Title
#'
#' @param room
#' @param prisoner
#' @param arrange_loops
#'
#' @return
#' @export
#'
#' @examples
plot_prisoner_search <- function(room,
                                 prisoner,
                                 arrange_loops = TRUE) {
  # room <- create_room(16) |>
  #   add_loops()

  if (arrange_loops) {
    room <- room |>
      dplyr::arrange(loop, index_in_loop)
  }

  boxes_n <- nrow(room)

  rows_n <- floor(sqrt(boxes_n))
  columns_n <- ceiling(sqrt(boxes_n))

  room <- room |>
    dplyr::mutate(
      row = rep(1:rows_n, times = columns_n)[1:boxes_n],
      column = rev(rep(1:columns_n, each = rows_n))[1:boxes_n]
    )

  ggplot2::ggplot(room, ggplot2::aes(x = row, y = column, label = box)) +
    ggplot2::theme_void() +
    ggplot2::geom_tile(width = .2, height = .2, fill = "white", col = "black") +
    ggplot2::geom_text(nudge_y = .2) +
    ggplot2::geom_text(mapping = ggplot2::aes(label = ticket), col = gray(.4))
}
