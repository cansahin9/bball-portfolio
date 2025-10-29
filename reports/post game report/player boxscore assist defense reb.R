# ───────────────────────────────────────────────
# 📁 SETUP
# ───────────────────────────────────────────────
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")

library(euroleaguer)
library(dplyr)
library(gt)
library(scales)
library(stringr)
library(glue)

# ───────────────────────────────────────────────
# 📊 LOAD GAME DATA
# ───────────────────────────────────────────────
game_data <- getGameBoxScore(season_code = "U2025", game_code = 36)

# ───────────────────────────────────────────────
# 🧮 PROCESS PLAYER STATS
# ───────────────────────────────────────────────
player_df <- game_data$PlayerStats %>%
  filter(TeamCode == "LLI") %>%  # team abbreviation
  mutate(
    MP = floor(Seconds / 60),
    Group = ifelse(IsStarter == 1, "Starters", "Bench"),
    STK = STL + BLK,  # Stocks = Steals + Blocks
    `AST/TO` = case_when(
      TO == 0 & AST == 0 ~ 0,
      TO == 0 ~ AST,
      TRUE ~ round(AST / TO, 2)
    )
  ) %>%
  select(Group, Player, MP, AST, TO, `AST/TO`, STL, BLK, STK, OREB, DREB, REB, FD, FC) %>%
  arrange(desc(Group), desc(MP)) %>%
  mutate(
    Number = str_extract(Player, "#\\d+"),
    Player = str_trim(str_to_title(gsub("#\\d+", "", Player))),
    Player = str_trim(glue("{Player} {Number}"))
  ) %>%
  select(-Number)

# ───────────────────────────────────────────────
# 🎨 GT THEME FUNCTION
# ───────────────────────────────────────────────
gt_theme_f5 <- function(gt_object, ...) {
  gt_object %>%
    opt_table_font(
      font = list(google_font("Roboto"), default_fonts()),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(font = google_font("Roboto"), weight = 700)
    ) %>%
    tab_style(
      style = list(
        cell_text(font = google_font("Roboto"), v_align = "bottom", size = px(12), weight = 'bold', color = "black"),
        cell_fill(color = "white")
      ),
      locations = cells_body()
    ) %>%
    tab_options(
      heading.align = "left",
      column_labels.font.weight = "bold",
      table.font.size = 12,
      data_row.padding = px(6),
      row_group.padding = px(6),
      column_labels.padding = px(6),
      ...
    )
}

# ───────────────────────────────────────────────
# 🧾 BUILD TABLE
# ───────────────────────────────────────────────
player_table <- player_df %>%
  gt(groupname_col = "Group") %>%
  gt_theme_f5() %>%
  cols_label(
    MP = "MP", AST = "AST", TO = "TO", `AST/TO` = "AST/TOV",
    STL = "STL", BLK = "BLK", STK = "STK",
    OREB = "OREB", DREB = "DREB", REB = "TRB",
    FD = "Taken", FC = "PF"
  ) %>%
  tab_spanner(label = "Playmaking", columns = c(AST, TO, `AST/TO`)) %>%
  tab_spanner(label = "Defense", columns = c(STL, BLK, STK)) %>%
  tab_spanner(label = "Rebounding", columns = c(OREB, DREB, REB)) %>%
  tab_spanner(label = "Fouls", columns = c(FD, FC)) %>%
  cols_align(align = "center", columns = -Player) %>%
  cols_width(Player ~ px(200), everything() ~ px(60)) %>%
  data_color(
    columns = c(MP),
    colors = col_numeric(
      palette = c("#f0f0f0", "#B34735"),  # reddish = burden
      domain = c(0, 40)
    )
  ) %>%
  data_color(
    columns = c(AST, STK, REB),
    colors = col_numeric(
      palette = c("#f0f0f0", "#52b788"),  # green = good
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TO),
    colors = col_numeric(
      palette = c("#f0f0f0", "#e63946"),  # red = bad
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(FC),
    colors = col_numeric(
      palette = c("#f0f0f0", "#e63946"),  # red = bad
      domain = c(0, 5)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray90"),
      cell_text(color = "black", weight = "bold", size = px(12)),
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2))
    ),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body(columns = c(MP, AST, TO, STK, REB, FC))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "black"),  # team color
      cell_text(color = "white", weight = "bold", size = px(12))
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  )

# ───────────────────────────────────────────────
# 💾 EXPORT
# ───────────────────────────────────────────────
gtsave(player_table, "lions_Player_Defense_Playmaking_Boxscore.png", expand = 10)
