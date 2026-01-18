
library(memoise)
library(R.utils)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(euroleaguer)
library(cowplot)
library(magick)
library(grid)
library(ggpp)

# ------------------ Helpers

# logos/photos 
team_logo_src    <- "https://media-cdn.cortextech.io/102de468-9e41-4b8e-b44d-23bebb9e916f.png?width=90&height=90&resizeType=fill&format=webp"
player_photo_src <- "C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/bar_chart/tarik_phillip_profile.png"

.norm_np <- function(x){
  x %>% iconv(from = "", to = "ASCII//TRANSLIT") %>%
    tolower() %>% gsub("[^a-z0-9 ]", " ", .) %>% str_squish()
}
get_img <- function(src) if (grepl("^https?://", src)) magick::image_read(src) else magick::image_read(path.expand(src))
.name_matches <- function(name_norm, query_norm){
  toks <- unlist(strsplit(query_norm, " +"))
  all(str_detect(name_norm, paste0("\\b", toks, "\\b")))
}

m_getPlayerStats      <- memoise(euroleaguer::getPlayerStats)
m_getCompetitionTeams <- memoise(euroleaguer::getCompetitionTeams)
m_getTeamStats        <- memoise(euroleaguer::getTeamStats)

uc_codes <- function(years) paste0("U", years)

pull_player_table <- function(sc){
  tryCatch(
    withTimeout(m_getPlayerStats(season_code = sc, statistic_mode = "perGame"),
                timeout = 8, onTimeout = "silent"),
    error = function(e) NULL
  )
}

# --- helper: facet-aware annotation_custom (works on older ggplot2)
annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf,
                               ymin = -Inf, ymax = Inf, data = NULL) {
  layer(
    data = data,
    stat = "identity",
    position = "identity",
    geom = ggplot2:::GeomCustomAnn,   # use internal geom
    inherit.aes = FALSE,
    params = list(grob = grob, xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax)
  )
}

sanitize_stat_frame <- function(df, season_code){
  df <- tibble::as_tibble(df)
  keep <- intersect(names(df), c("GP","PTS","2PM","2PA","3PM","3PA","FTM","FTA","FGM","FGA"))
  if (!length(keep)) keep <- names(df)
  to_num <- function(x){
    x <- as.character(x); x <- gsub("%","",x, fixed=TRUE); x <- gsub(",",".",x, fixed=TRUE)
    suppressWarnings(as.numeric(x))
  }
  for (col in keep) df[[col]] <- to_num(df[[col]])
  df <- df[, keep, drop = FALSE]
  df$season_code <- season_code
  df
}

pull_from_teams <- function(sc, q){
  tms <- tryCatch(
    withTimeout(m_getCompetitionTeams(season_code = sc),
                timeout = 8, onTimeout = "silent"),
    error = function(e) NULL
  )
  if (is.null(tms) || !nrow(tms)) return(tibble())
  
  tc <- names(tms)[grepl("team.*code$|^code$|^teamcode$", names(tms), ignore.case = TRUE)][1]
  codes <- unique(tms[[tc]])
  
  for (code in codes){
    Sys.sleep(0.25)
    lst <- tryCatch(
      withTimeout(m_getTeamStats(season_code = sc, team_code = code),
                  timeout = 6, onTimeout = "silent"),
      error = function(e) NULL
    )
    if (is.null(lst)) next
    pg <- lst$PlayerAveragePerGame; if (is.null(pg) || !nrow(pg)) pg <- lst$PlayerAccumulated
    if (is.null(pg) || !nrow(pg)) next
    nc <- names(pg)[grepl("^player$|player.?name", names(pg), ignore.case = TRUE)][1]
    if (is.na(nc)) next
    cand <- pg %>% mutate(.NAME = .norm_np(.data[[nc]]))
    keep <- vapply(cand$.NAME, .name_matches, logical(1), query_norm = q)
    if (any(keep)) return(sanitize_stat_frame(cand[keep, , drop = FALSE], sc))
  }
  tibble()
}

fetch_uc_player_rows_fast <- function(player_name,
                                      seasons = uc_codes(c(2019, 2021, 2023, 2024)),
                                      verbose = TRUE){
  q <- .norm_np(player_name)
  say <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  rows <- list()
  
  for (sc in seasons){
    tb <- pull_player_table(sc)
    if (!is.null(tb) && nrow(tb)){
      name_col <- names(tb)[grepl("^player$|player.?name", names(tb), ignore.case = TRUE)][1]
      if (!is.na(name_col)) {
        cand <- tb %>% mutate(.NAME = .norm_np(.data[[name_col]]))
        keep <- vapply(cand$.NAME, .name_matches, logical(1), query_norm = q)
        if (any(keep)) { say("✓ %s via player table", sc); rows[[sc]] <- sanitize_stat_frame(cand[keep, , drop = FALSE], sc); next }
      }
    }
    say("… %s player table miss; trying teams", sc)
    cand2 <- pull_from_teams(sc, q)
    if (nrow(cand2)) { say("✓ %s via team table", sc); rows[[sc]] <- cand2 } else say("× %s not found", sc)
  }
  
  df <- bind_rows(rows)
  if (!nrow(df)) return(df)
  df
}

aggregate_uc_player_by_season <- function(rows){
  if (!nrow(rows)) return(tibble())
  rows %>%
    group_by(season_code) %>%
    summarise(
      GP = sum(GP, na.rm = TRUE),
      PTS_PG = if_else(sum(GP, na.rm = TRUE) > 0,
                       sum(PTS * GP, na.rm = TRUE)/sum(GP, na.rm = TRUE),
                       NA_real_),
      `2PM` = sum(`2PM`, na.rm = TRUE),
      `2PA` = sum(`2PA`, na.rm = TRUE),
      `3PM` = sum(`3PM`, na.rm = TRUE),
      `3PA` = sum(`3PA`, na.rm = TRUE),
      FTM = sum(FTM, na.rm = TRUE),
      FTA = sum(FTA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      FG2_PCT = if_else(`2PA` > 0, `2PM`/`2PA`, NA_real_),
      FG3_PCT = if_else(`3PA` > 0, `3PM`/`3PA`, NA_real_),
      FT_PCT  = if_else( FTA  > 0,  FTM / FTA , NA_real_),
      slugSeason = {
        y <- as.integer(stringr::str_extract(season_code, "\\d{4}"))
        paste0(substr(y,3,4), "-", substr(y+1,3,4))
      }
    ) %>% arrange(season_code)
}

viz_theme <- theme(
  plot.title.position = "plot",
  plot.title   = element_text(hjust = .5, size = 14, face = "bold",
                              color = "black", margin = margin(b = 6)),
  plot.subtitle = element_text(hjust = .5, size = 8, color = "gray50",
                               margin = margin(b = 6)),
  legend.position = "none",
  plot.background  = element_rect(fill = "oldlace", color = "oldlace"),
  panel.background = element_rect(fill = "oldlace", color = "oldlace"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_line(colour = "oldlace"),
  panel.grid.major.x = element_blank(),
  axis.line   = element_line(colour = "black"),
  axis.title.x = element_text(colour = "black", size = 10),
  axis.title.y = element_text(colour = "black", size = 10),
  axis.text.x  = element_text(colour = "black", size = 8),
  axis.text.y  = element_text(colour = "black", size = 8),
  plot.margin  = margin(6, 6, 8, 6, "mm")   # trimmed
)

# ------------------ Tarik Phillip (Eurocup)
player <- "Tarik Phillip"

rows <- fetch_uc_player_rows_fast(player, verbose = TRUE)
if (!nrow(rows)) stop("Eurocup API returned no rows. Try again in a few minutes (rate-limited).")

tp <- aggregate_uc_player_by_season(rows)
tp <- tp %>% mutate(slugSeason = factor(slugSeason, levels = slugSeason))

y_max <- max(tp$PTS_PG, na.rm = TRUE)
y_top <- ceiling((y_max + 0.5) / 5) * 5

# Chart 1: PPG
ppg_plot <- ggplot(tp %>% filter(!is.na(PTS_PG)), aes(slugSeason, PTS_PG)) +
  geom_bar(stat = "identity", fill = "#00538C", color = "black", width = .6) +
  geom_text(aes(label = round(PTS_PG, 1)),
            vjust = 1.9, check_overlap = TRUE, color = "oldlace", fontface = "bold") +
  scale_y_continuous(limits = c(0, y_top), expand = expansion(mult = c(0, .04))) +
  labs(title = paste(player, "- Eurocup Points Per Game Trend"),
       subtitle = "Data: euroleaguer | Viz: Can Sahin",
       x = "Season", y = "Points Per Game\n") +
  viz_theme

print(ppg_plot)
ggsave("tarik_phillip_ppg.png", ppg_plot, height = 6, width = 6, dpi = "retina")

# Chart 2: Shooting % facets
tp_long <- tp %>%
  transmute(slugSeason,
            `2P%` = FG2_PCT, `3P%` = FG3_PCT, `FT%` = FT_PCT) %>%
  pivot_longer(!slugSeason, values_to = "value")

shoot_plot <- ggplot(tp_long, aes(slugSeason, value, fill = name, group = name)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black", width=.6, na.rm=TRUE) +
  geom_text(aes(label = ifelse(is.na(value), "—", scales::percent(value, accuracy = 1))),
            position=position_dodge(width=.9), vjust=2, size=2, color="oldlace", fontface="bold", na.rm=TRUE) +
  scale_y_continuous(labels=percent_format(accuracy = 1), limits=c(0,1), expand=c(0,0)) +
  scale_fill_manual(values = c("red4","gray50","gray10")) +
  facet_wrap(~name) +
  labs(title = paste(player, "- Eurocup Shooting % Trends"),
       subtitle = "Data: euroleaguer | Viz: Can Sahin",
       x = "Season", y = "Shooting Percentage") +
  viz_theme +
  theme(strip.background = element_rect(fill="oldlace"),
        strip.text = element_text(hjust=.5, size=8, face="bold", color="black"))

annotation_custom2 <- function(grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, data=NULL){
  layer(data = data, stat = "identity", position = "identity",
        geom = ggplot2:::GeomCustomAnn, inherit.aes = FALSE,
        params = list(grob=grob, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
}

# read images (URL/local OK) -> grobs
logo_grob   <- grid::rasterGrob(as.raster(get_img(team_logo_src)),   interpolate = TRUE)
player_grob <- grid::rasterGrob(as.raster(get_img(player_photo_src)), interpolate = TRUE)

# discrete-x math is easy: left edge = 0.5, right edge = n + 0.5
n <- length(unique(tp_long$slugSeason))
x0 <- 0.5; xW <- n        # x runs from 0.5 .. n+0.5 (width = n)
y0 <- 0;   yH <- 1        # y is 0..1 for % plots

# controls (tweak these 5 numbers)
pad_x <- 0.015          # in-panel left padding  (fraction of panel width)
pad_y <- 0.0            # in-panel top padding
logo_w_rel   <- 0.30      # logo width   (fraction of panel width)
logo_h_rel   <- 0.25      # logo height  (fraction of panel height)
player_w_rel <- 0.30      # player width
player_h_rel <- 0.25      # player height
overlap_dx   <- 0.06      # player overlaps logo by this fraction of panel width

# compute rectangles (in data units)
logo_shift_x <- 0.05

logo_xmin <- x0 + xW * (pad_x + logo_shift_x)
logo_xmax <- logo_xmin + xW * logo_w_rel
logo_ymax <- y0 + yH * (1 - pad_y)
logo_ymin <- logo_ymax - yH * logo_h_rel

player_xmin <- max(x0, logo_xmin - xW * overlap_dx)
player_xmax <- player_xmin + xW * player_w_rel
player_ymax <- logo_ymax
player_ymin <- player_ymax - yH * player_h_rel

facet_tag <- "2P%"   # must match exactly: check unique(tp_long$name)

shoot_with_badges <- shoot_plot +
  annotation_custom2(
    logo_grob,  xmin = logo_xmin,  xmax = logo_xmax,
    ymin = logo_ymin,  ymax = logo_ymax,
    data = data.frame(name = facet_tag)
  ) +
  annotation_custom2(
    player_grob, xmin = player_xmin, xmax = player_xmax,
    ymin = player_ymin, ymax = player_ymax,
    data = data.frame(name = facet_tag)
  )


print(shoot_with_badges)
ggsave("tarik_phillip_shooting_badged.png", shoot_with_badges, height = 5, width = 8, dpi = "retina")


