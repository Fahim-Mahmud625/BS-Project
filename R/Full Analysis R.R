# ============================================================
# BDHS 2022 — FINAL CLEAN SURVEY-WEIGHTED PIPELINE (FIXED)
# Outputs: ALL tables + figures saved into: G.files/
# Colors: Met Need (#191970), Unmet Need (#4682B4),
#         SWPER Low/Med/High (#87CEEB/#4682B4/#191970)
# ============================================================

# ---- 0) Packages ----
pkgs <- c("haven","dplyr","survey","forcats","ggplot2","scales",
          "purrr","tibble","stringr","sf","lme4")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

options(survey.lonely.psu = "adjust")

# ---- 0.1) ONE output folder for EVERYTHING ----
out_dir <- "G.files"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 0.2) Consistent colors (YOUR scheme) ----
COL_MET   <- "#191970"  # midnight blue
COL_UNMET <- "#4682B4"  # steelblue
COL_LOW   <- "#87CEEB"  # sky blue
COL_MED   <- "#4682B4"  # steelblue
COL_HIGH  <- "#191970"  # midnight blue

# ============================================================
# 1) Load data + keep only required variables + label factors
# ============================================================

data_path <- "dat.dta"         # <-- change if needed
shp_path  <- "bgd_admin1.shp"  # <-- change if needed

df_raw <- read_dta(data_path)

keep_vars <- c(
  "v001","v023","v021","wt",
  "unmet_need",
  "att_cat","aut_cat","dec_cat",
  "age","child_cat","working_stat","wealth_index","religion",
  "sex_hh","dist_facility","place_of_res","division","comm_edu","comm_wealth","comm_fp_expo",
  "v626a"
)

missing_vars <- setdiff(keep_vars, names(df_raw))
if (length(missing_vars)) stop("Missing required variables: ", paste(missing_vars, collapse = ", "))

df <- df_raw %>%
  dplyr::select(all_of(keep_vars)) %>%
  mutate(wt = as.numeric(wt)) %>%
  filter(!is.na(unmet_need))

# ---- Outcome safety: handle 0/1 OR 1/2 coding ----
unmet_vals <- sort(unique(as.integer(df$unmet_need)))
if (length(unmet_vals) >= 2 && min(unmet_vals) == 1 && max(unmet_vals) == 2) {
  df <- df %>% mutate(unmet_need_num = as.integer(unmet_need) - 1L)
} else {
  df <- df %>% mutate(unmet_need_num = as.integer(unmet_need))
}

df <- df %>%
  mutate(
    unmet_need_bin = ifelse(unmet_need_num == 1, 1L, 0L),
    unmet_need_f   = factor(unmet_need_bin, levels = c(0,1), labels = c("Met Need","Unmet Need")),
    
    # SWPER domains
    att_cat = factor(att_cat, levels = c(1,2,3), labels = c("Low","Medium","High")),
    aut_cat = factor(aut_cat, levels = c(1,2,3), labels = c("Low","Medium","High")),
    dec_cat = factor(dec_cat, levels = c(1,2,3), labels = c("Low","Medium","High")),
    
    # individual
    age = factor(age, levels = c(1,2,3), labels = c("15-24","25-34","35-49")),
    child_cat = factor(child_cat, levels = c(0,1,2,3),
                       labels = c("No Child","1 Child","2 Children","3+ Children")),
    working_stat = factor(working_stat, levels = c(1,2), labels = c("Not Working","Working")),
    wealth_index = factor(wealth_index, levels = c(1,2,3), labels = c("Poor","Middle","Rich")),
    religion = factor(religion, levels = c(1,2), labels = c("Muslim","Non-Muslim")),
    
    # HH / community / region
    sex_hh = factor(sex_hh, levels = c(1,2), labels = c("Male","Female")),
    dist_facility = factor(dist_facility, levels = c(1,2), labels = c("Big Problem","Not Big Problem")),
    place_of_res = factor(place_of_res, levels = c(1,2), labels = c("Urban","Rural")),
    comm_edu = factor(comm_edu, levels = c(0,1), labels = c("Low","High")),
    comm_wealth = factor(comm_wealth, levels = c(0,1), labels = c("Low","High")),
    comm_fp_expo = factor(comm_fp_expo, levels = c(0,1), labels = c("Low","High")),
    
    division = as.character(haven::as_factor(division)),
    division = stringr::str_trim(division),
    division_std = dplyr::case_when(
      division %in% c("Barisal","Barishal") ~ "Barishal",
      division %in% c("Chittagong","Chattogram") ~ "Chattogram",
      TRUE ~ division
    )
  )

# ============================================================
# 2) Survey design
# ============================================================

des <- svydesign(
  id      = ~v001,
  strata  = ~v023,
  weights = ~wt,
  data    = df,
  nest    = TRUE
)

# ============================================================
# 3) Labels + helper functions
# ============================================================

var_labels <- c(
  unmet_need_f = "Unmet Need",
  att_cat      = "Attitude to Violence",
  aut_cat      = "Social Independence",
  dec_cat      = "Decision Making",
  age          = "Respondent's Age",
  child_cat    = "Parity (No. of Children)",
  working_stat = "Working Status",
  wealth_index = "Wealth Index",
  religion     = "Religion",
  sex_hh       = "Sex of Household Head",
  dist_facility= "Distance to Health Facility",
  place_of_res = "Place of Residence",
  division_std = "Administrative Division",
  comm_edu     = "Community Education",
  comm_wealth  = "Community Wealth",
  comm_fp_expo = "Community FP Exposure"
)

# safe univariate
svy_univar <- function(design, var, label = NULL) {
  lbl <- if (!is.null(label)) label else if (var %in% names(var_labels)) var_labels[[var]] else var
  
  wt_tab <- svytable(as.formula(paste0("~", var)), design)
  wt_df <- as.data.frame(wt_tab) %>%
    rename(level = 1, wt_N = Freq) %>%
    mutate(wt_pct = 100 * wt_N / sum(wt_N))
  
  un_df <- design$variables %>%
    count(.data[[var]], name = "n_unw") %>%
    rename(level = 1)
  
  left_join(un_df, wt_df, by = "level") %>%
    mutate(
      variable = var,
      label = lbl,
      wt_N = round(wt_N, 0),
      wt_pct = round(wt_pct, 1)
    ) %>%
    select(variable, label, level, n_unw, wt_N, wt_pct)
}

# bivariate
svy_bivar <- function(design, x, y = "unmet_need_f") {
  f <- as.formula(paste0("~", x, " + ", y))
  tab_wt <- svytable(f, design)
  row_pct <- prop.table(tab_wt, margin = 1) * 100
  chi <- suppressWarnings(svychisq(f, design, statistic = "F"))
  pval <- tryCatch(as.numeric(chi$p.value), error = function(e) NA_real_)
  
  as.data.frame(tab_wt) %>%
    rename(level = 1, outcome = 2, wt_N = Freq) %>%
    left_join(
      as.data.frame(row_pct) %>% rename(level = 1, outcome = 2, row_pct = Freq),
      by = c("level","outcome")
    ) %>%
    mutate(
      variable = x,
      label = if (x %in% names(var_labels)) var_labels[[x]] else x,
      wt_N = round(wt_N, 0),
      row_pct = round(row_pct, 1),
      p_value = pval
    ) %>%
    select(variable, label, level, outcome, wt_N, row_pct, p_value)
}

tidy_svy_or <- function(fit, design, digits = 3) {
  cf <- coef(summary(fit))
  dfree <- degf(design)
  crit <- qt(0.975, df = dfree)
  est <- cf[, "Estimate"]; se <- cf[, "Std. Error"]; p <- cf[, ncol(cf)]
  tibble(
    term    = rownames(cf),
    OR      = round(exp(est), digits),
    CI_low  = round(exp(est - crit * se), digits),
    CI_high = round(exp(est + crit * se), digits),
    p_value = signif(p, 3)
  )
}

tidy_glmer_or <- function(fit, digits = 3) {
  s <- summary(fit)$coef
  est <- s[, "Estimate"]; se <- s[, "Std. Error"]
  z <- est / se; p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  tibble(
    term    = rownames(s),
    OR      = round(exp(est), digits),
    CI_low  = round(exp(est - 1.96 * se), digits),
    CI_high = round(exp(est + 1.96 * se), digits),
    p_value = signif(p, 3)
  )
}

weighted_roc <- function(y01, p, w) {
  ok <- is.finite(y01) & is.finite(p) & is.finite(w)
  y01 <- y01[ok]; p <- p[ok]; w <- w[ok]
  ord <- order(p, decreasing = TRUE)
  y01 <- y01[ord]; p <- p[ord]; w <- w[ord]
  Wpos <- sum(w[y01 == 1]); Wneg <- sum(w[y01 == 0])
  tpr <- cumsum(w * (y01 == 1)) / Wpos
  fpr <- cumsum(w * (y01 == 0)) / Wneg
  tpr <- c(0, tpr); fpr <- c(0, fpr)
  auc <- sum((fpr[-1] - fpr[-length(fpr)]) * (tpr[-1] + tpr[-length(tpr)]) / 2)
  tibble(fpr = fpr, tpr = tpr, auc = auc)
}

# ============================================================
# 4) UNIVARIATE + BIVARIATE TABLES (SAVED IN G.files)
# ============================================================

uni_vars <- c(
  "unmet_need_f",
  "att_cat","aut_cat","dec_cat",
  "age","child_cat","working_stat","wealth_index","religion",
  "sex_hh","dist_facility","place_of_res","division_std",
  "comm_edu","comm_wealth","comm_fp_expo"
)

univariate_table <- bind_rows(
  svy_univar(des, "unmet_need_f", label = "Unmet Need"),
  purrr::map_dfr(setdiff(uni_vars, "unmet_need_f"), ~svy_univar(des, .x))
)

bivar_vars <- setdiff(uni_vars, "unmet_need_f")
bivariate_table <- purrr::map_dfr(bivar_vars, ~svy_bivar(des, .x))

write.csv(univariate_table, file.path(out_dir, "Univariate_BDHS2022.csv"), row.names = FALSE)
write.csv(bivariate_table, file.path(out_dir, "Bivariate_BDHS2022.csv"), row.names = FALSE)

# ============================================================
# 5) MULTIVARIATE — Survey-weighted logistic (Models I–IV)
# ============================================================

m1 <- svyglm(unmet_need_bin ~ att_cat + aut_cat + dec_cat, design = des, family = quasibinomial())

m2 <- svyglm(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               age + child_cat + working_stat + wealth_index + religion,
             design = des, family = quasibinomial())

m3 <- svyglm(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               sex_hh + dist_facility + place_of_res +
               comm_edu + comm_wealth + comm_fp_expo + division_std,
             design = des, family = quasibinomial())

m4 <- svyglm(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               age + child_cat + working_stat + wealth_index + religion +
               sex_hh + dist_facility + place_of_res +
               comm_edu + comm_wealth + comm_fp_expo + division_std,
             design = des, family = quasibinomial())

write.csv(tidy_svy_or(m1, des), file.path(out_dir, "ModelI_Svy_OR.csv"), row.names = FALSE)
write.csv(tidy_svy_or(m2, des), file.path(out_dir, "ModelII_Svy_OR.csv"), row.names = FALSE)
write.csv(tidy_svy_or(m3, des), file.path(out_dir, "ModelIII_Svy_OR.csv"), row.names = FALSE)
write.csv(tidy_svy_or(m4, des), file.path(out_dir, "ModelIV_Svy_OR.csv"), row.names = FALSE)

# ============================================================
# 6) MULTIVARIATE — Mixed-effects logistic (Null + I–IV)
# ============================================================

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

ml0 <- glmer(unmet_need_bin ~ 1 + (1 | v021), data = df, weights = wt, family = binomial(), control = ctrl)
ml1 <- glmer(unmet_need_bin ~ att_cat + aut_cat + dec_cat + (1 | v021), data = df, weights = wt, family = binomial(), control = ctrl)
ml2 <- glmer(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               age + child_cat + working_stat + wealth_index + religion + (1 | v021),
             data = df, weights = wt, family = binomial(), control = ctrl)
ml3 <- glmer(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               sex_hh + dist_facility + place_of_res +
               comm_edu + comm_wealth + comm_fp_expo + division_std + (1 | v021),
             data = df, weights = wt, family = binomial(), control = ctrl)
ml4 <- glmer(unmet_need_bin ~ att_cat + aut_cat + dec_cat +
               age + child_cat + working_stat + wealth_index + religion +
               sex_hh + dist_facility + place_of_res +
               comm_edu + comm_wealth + comm_fp_expo + division_std + (1 | v021),
             data = df, weights = wt, family = binomial(), control = ctrl)

write.csv(tidy_glmer_or(ml4), file.path(out_dir, "ModelIV_Mixed_OR.csv"), row.names = FALSE)

# ============================================================
# 7) FIGURES — ALL saved in G.files (FIXED COLORS)
# ============================================================

# ============================================================
# BAR/PANEL THEME: minimal + clear background (transparent-ready)
# ============================================================
theme_bar_clear <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_blank(),
      plot.background  = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.2, color = "grey85")
    )
}

# ---- 7.1 Weighted prevalence (Unmet Need) — FIXED COLORS + CLEAR BG ----
unmet_tab <- svytable(~unmet_need_f, des)
unmet_plot_df <- as.data.frame(unmet_tab) %>%
  rename(unmet_need = 1, wt_N = Freq) %>%
  mutate(pct = wt_N / sum(wt_N))

p_prev <- ggplot(unmet_plot_df, aes(x = unmet_need, y = pct, fill = unmet_need)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -0.4, fontface = "bold") +
  scale_fill_manual(values = c("Met Need" = COL_MET, "Unmet Need" = COL_UNMET)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Distribution of Unmet Need", x = NULL, y = "Percentage") +
  theme_bar_clear(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "prevelance of UNFP.png"),
       p_prev, width = 6.5, height = 5.5, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "prevelance of UNFP.pdf"),
       p_prev, width = 6.5, height = 5.5, dpi = 300, bg = "transparent")

# ---- 7.2 v626a unmet-need types (WEIGHTED) — FIXED COLORS + CLEAR BG ----
des_v626a <- subset(des, !is.na(v626a))
des_v626a <- update(des_v626a,
                    spacing  = as.integer(v626a == 1),
                    limiting = as.integer(v626a == 2)
)

m_v626a <- svymean(~spacing + limiting, des_v626a, na.rm = TRUE)

p_spacing  <- as.numeric(coef(m_v626a)["spacing"])
p_limiting <- as.numeric(coef(m_v626a)["limiting"])
p_total    <- p_spacing + p_limiting

plot_v626a <- tibble(
  category = factor(
    c("Unmet Need\n(for limiting)", "Unmet Need\n(for spacing)", "Total\nUnmet Needs"),
    levels = c("Unmet Need\n(for limiting)", "Unmet Need\n(for spacing)", "Total\nUnmet Needs")
  ),
  pct = c(p_limiting, p_spacing, p_total)
)

p_v626a <- ggplot(plot_v626a, aes(x = category, y = pct, fill = category)) +
  geom_col(color = "black", linewidth = 0.8, width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 0.01)),
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Unmet Need\n(for limiting)" = COL_LOW,
    "Unmet Need\n(for spacing)"  = COL_UNMET,
    "Total\nUnmet Needs"         = COL_MET
  )) +
  scale_y_continuous(
    labels = percent,
    limits = c(0, max(plot_v626a$pct) + 0.05),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Prevalence of Unmet Need Categories",
    x = NULL, y = "Percent (%)"
  ) +
  theme_bar_clear(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(file.path(out_dir, "UNFP types.png"),
       p_v626a, width = 6.5, height = 6, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "UNFP types.pdf"),
       p_v626a, width = 6.5, height = 6, dpi = 300, bg = "transparent")

# ---- 7.3 Empowerment domains distribution (weighted stacked) — FIXED COLORS + CLEAR BG ----
emp_vars <- c("att_cat","aut_cat","dec_cat")
emp_plot_data <- purrr::map_dfr(emp_vars, function(v) {
  tab <- svytable(as.formula(paste0("~", v)), des)
  as.data.frame(prop.table(tab)) %>%
    rename(level = 1, pct = Freq) %>%
    mutate(domain = v)
}) %>%
  mutate(
    level = factor(level, levels = c("Low","Medium","High")),
    domain = factor(domain, levels = emp_vars)
  )

domain_labels <- c(
  att_cat = "Attitude\ntoward violence",
  aut_cat = "Social\nIndependence",
  dec_cat = "Decision\nMaking"
)

p_emp <- ggplot(emp_plot_data, aes(x = domain, y = pct, fill = level)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white", fontface = "bold", size = 3.5
  ) +
  scale_fill_manual(values = c("Low" = COL_LOW, "Medium" = COL_MED, "High" = COL_HIGH)) +
  scale_x_discrete(labels = domain_labels) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Distribution of Empowerment Domains",
    x = NULL, y = "Percentage", fill = "Level"
  ) +
  theme_bar_clear(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()  # looks cleaner for stacked bars
  )

ggsave(file.path(out_dir, "Empowerment Distn.png"),
       p_emp, width = 6.8, height = 5.8, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "Empowerment Distn.pdf"),
       p_emp, width = 6.8, height = 5.8, dpi = 300, bg = "transparent")

# ---- 7.4 Panel plots: unmet need % by covariates — CLEAR BG ----
get_panel_data <- function(design, var_list) {
  purrr::map_dfr(var_list, function(v) {
    tab <- svytable(as.formula(paste0("~", v, " + unmet_need_f")), design)
    pct <- prop.table(tab, margin = 1) * 100
    as.data.frame(pct) %>%
      rename(level = 1, status = 2, pct = Freq) %>%
      filter(status == "Unmet Need") %>%
      mutate(label = if (v %in% names(var_labels)) var_labels[[v]] else v)
  })
}

plot_panels <- function(df_pan, title) {
  df_pan$label <- factor(df_pan$label, levels = unique(df_pan$label))
  ggplot(df_pan, aes(x = forcats::fct_rev(level), y = pct)) +
    geom_col(width = 0.7, fill = COL_UNMET) +
    geom_text(aes(label = paste0(round(pct, 1), "%")),
              hjust = -0.15, fontface = "bold", size = 3.2) +
    facet_grid(label ~ ., scales = "free_y", space = "free_y", switch = "y") +
    coord_flip(ylim = c(0, max(df_pan$pct) + 10)) +
    labs(
      title = title,
      subtitle = "Unmet Need prevalence (weighted %)",
      x = NULL, y = "Percent (%)"
    ) +
    theme_bar_clear(base_size = 12) +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, face = "bold"),
      panel.grid.major.x = element_line(linewidth = 0.2, color = "grey85")
    )
}

p_swper <- plot_panels(get_panel_data(des, c("att_cat","aut_cat","dec_cat")), "SWPER domains")
p_ind   <- plot_panels(get_panel_data(des, c("age","child_cat","working_stat","wealth_index","religion")), "Individual factors")
p_hh    <- plot_panels(get_panel_data(des, c("sex_hh","dist_facility","place_of_res","division_std","comm_edu","comm_wealth","comm_fp_expo")),
                       "Household / community / region factors")

ggsave(file.path(out_dir, "Fig_Panels_SWPER.png"),      p_swper, width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "Fig_Panels_Individual.png"), p_ind,   width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "Fig_Panels_HH_Community.png"), p_hh,  width = 8, height = 6, dpi = 300, bg = "transparent")

ggsave(file.path(out_dir, "Fig_Panels_SWPER.pdf"),      p_swper, width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "Fig_Panels_Individual.pdf"), p_ind,   width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave(file.path(out_dir, "Fig_Panels_HH_Community.pdf"), p_hh,  width = 8, height = 6, dpi = 300, bg = "transparent")


# ---- 7.5 Choropleths: unmet need + high att/aut/dec by division ----
bgd_sf <- st_read(shp_path, quiet = TRUE)

# detect division name column
name_candidates <- names(bgd_sf)[stringr::str_detect(names(bgd_sf),
                                                     regex("adm1|name_1|division|adm1_name|name", ignore_case = TRUE)
)]
if (length(name_candidates) == 0) stop("Could not find a division name field in shapefile.")
name_col <- name_candidates[1]

bgd_sf <- bgd_sf %>% mutate(division_shape = as.character(.data[[name_col]]))

norm_name <- function(x) {
  x %>% stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z]", "") %>% stringr::str_trim()
}

bgd_sf <- bgd_sf %>% mutate(div_key = norm_name(division_shape))
df <- df %>% mutate(div_key = norm_name(division_std))
des <- update(des, div_key = df$div_key)

bgd_pts <- bgd_sf %>%
  st_point_on_surface() %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(div_key, lon, lat)

unmet_by_div <- svyby(~unmet_need_bin, ~div_key, des, svymean, na.rm = TRUE) %>%
  as.data.frame() %>% transmute(div_key, pct = 100 * unmet_need_bin)

des_map <- update(des,
                  att_hi = as.integer(df$att_cat == "High"),
                  aut_hi = as.integer(df$aut_cat == "High"),
                  dec_hi = as.integer(df$dec_cat == "High")
)

att_hi_by_div <- svyby(~att_hi, ~div_key, des_map, svymean, na.rm = TRUE) %>%
  as.data.frame() %>% transmute(div_key, pct = 100 * att_hi)
aut_hi_by_div <- svyby(~aut_hi, ~div_key, des_map, svymean, na.rm = TRUE) %>%
  as.data.frame() %>% transmute(div_key, pct = 100 * aut_hi)
dec_hi_by_div <- svyby(~dec_hi, ~div_key, des_map, svymean, na.rm = TRUE) %>%
  as.data.frame() %>% transmute(div_key, pct = 100 * dec_hi)

map_theme <- theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        legend.title = element_text(face = "bold", size = 10))

# --- Update ONLY your make_map() to show division names (smaller font) ---

make_map <- function(est_df, title, legend_title, fname) {
  
  map_df  <- bgd_sf %>% left_join(est_df, by = "div_key")
  pts_df  <- bgd_pts %>% left_join(est_df, by = "div_key") %>%
    left_join(bgd_sf %>% st_drop_geometry() %>% select(div_key, division_shape), by = "div_key")
  
  p <- ggplot(map_df) +
    geom_sf(aes(fill = pct), color = "white", linewidth = 0.2) +
    
    # % label (as before)
    geom_text(
      data = pts_df,
      aes(x = lon, y = lat, label = sprintf("%.1f", pct)),
      color = "white", fontface = "bold", size = 4
    ) +
    
    # Division name label (smaller, below the %)
    geom_text(
      data = pts_df,
      aes(x = lon, y = lat, label = division_shape),
      vjust = 3,              # pushes text below the % label
      color = "white",
      fontface = "bold",
      size = 1.5              # <-- smaller font for division names
    ) +
    
    scale_fill_gradient(low = COL_UNMET, high = COL_MET, name = legend_title) +
    labs(title = title) +
    coord_sf(datum = NA) + map_theme
  
  ggsave(file.path(out_dir, fname), p, width = 8.5, height = 6, dpi = 300)
  p
}

make_map(unmet_by_div, "Unmet Need for Family Planning", "Unmet Need (%)",
         "UNFP Div.png")
make_map(unmet_by_div, "Unmet Need for Family Planning", "Unmet Need (%)",
         "UNFP Div.pdf")
make_map(att_hi_by_div, "High Attitude to Violence (SWPER)", "High AV (%)",
         "Map_High_AttitudeToViolence_ByDivision.png")
make_map(aut_hi_by_div, "High Social Independence (SWPER)", "High SI (%)",
         "Map_High_SocialIndependence_ByDivision.png")
make_map(dec_hi_by_div, "High Decision Making (SWPER)", "High DM (%)",
         "Map_High_DecisionMaking_ByDivision.png")

# ---- 7.6 ROC curves — FIXED COLORS + combined plot ----
df$pred_svy_m4   <- as.numeric(predict(m4,  type = "response"))
df$pred_mixed_m4 <- as.numeric(predict(ml4, type = "response"))

roc_svy   <- weighted_roc(df$unmet_need_bin, df$pred_svy_m4,   df$wt) %>% mutate(model = "Survey logistic")
roc_mixed <- weighted_roc(df$unmet_need_bin, df$pred_mixed_m4, df$wt) %>% mutate(model = "Mixed logistic")

auc_svy   <- roc_svy$auc[1]
auc_mixed <- roc_mixed$auc[1]

p_roc_svy <- ggplot(roc_svy, aes(x = fpr, y = tpr)) +
  geom_line(linewidth = 1.2, color = COL_UNMET) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  labs(title = "ROC — Logistic (Model IV)",
       subtitle = paste0("Weighted AUC = ", round(auc_svy, 3)),
       x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 12)

p_roc_mixed <- ggplot(roc_mixed, aes(x = fpr, y = tpr)) +
  geom_line(linewidth = 1.2, color = COL_MET) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  labs(title = "ROC — Mixed-effects Logistic (Model IV)",
       subtitle = paste0("Weighted AUC = ", round(auc_mixed, 3)),
       x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "ROC_Survey_ModelIV.png"), p_roc_svy, width = 6.5, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "ROC_Mixed_ModelIV.png"),  p_roc_mixed, width = 6.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "ROC_Survey_ModelIV.pdf"), p_roc_svy, width = 6.5, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "ROC_Mixed_ModelIV.pdf"),  p_roc_mixed, width = 6.5, height = 5.5, dpi = 300)


roc_combined <- bind_rows(roc_svy, roc_mixed)

p_roc_comp <- ggplot(roc_combined, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1.2) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  scale_color_manual(values = c("Survey logistic" = COL_UNMET,
                                "Mixed logistic"  = COL_MET),
                     labels = c("Survey logistic" = "Logistic",
                                "Mixed logistic"  = "Mixed-Effect Logistic")) +
  labs(title = "ROC Comparison — Final Models (Model IV)",
       subtitle = paste0("Weighted AUC — Logistic: ", round(auc_svy, 3),
                         " | Mixed: ", round(auc_mixed, 3)),
       x = "1 - Specificity", y = "Sensitivity",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "ROC_Comparison_ModelIV.png"), p_roc_comp, width = 7, height = 5.8, dpi = 300)
ggsave(file.path(out_dir, "ROC model IV comparison.pdf"), p_roc_comp, width = 7, height = 5.8, dpi = 300)

# ============================================================
# DONE: EVERYTHING is saved inside "G.files/"
# ============================================================

# ============================================================
# 8) ADDITIONAL RECOMMENDED FIGURES (append at bottom)
# ============================================================

# ---- 8.1 Forest plot: aOR (Model IV) survey vs mixed ----
svy_or4   <- tidy_svy_or(m4, des)   %>% dplyr::filter(term != "(Intercept)") %>% mutate(model = "Survey logistic")
mixed_or4 <- tidy_glmer_or(ml4)     %>% dplyr::filter(term != "(Intercept)") %>% mutate(model = "Mixed logistic")

make_pretty_term <- function(term) {
  # factor terms look like varLEVEL (e.g., att_catMedium)
  # numeric terms would show as-is (none in your current set)
  for (v in c("att_cat","aut_cat","dec_cat","age","child_cat","working_stat","wealth_index","religion",
              "sex_hh","dist_facility","place_of_res","comm_edu","comm_wealth","comm_fp_expo","division_std")) {
    if (startsWith(term, v)) {
      lvl <- substring(term, nchar(v) + 1)
      # clean up level text if needed
      lvl <- gsub("^", "", lvl)
      return(paste0(var_labels[[v]], ": ", lvl))
    }
  }
  term
}

forest_df <- bind_rows(svy_or4, mixed_or4) %>%
  mutate(
    term_pretty = purrr::map_chr(term, make_pretty_term),
    model = factor(model, levels = c("Survey logistic","Mixed logistic"))
  )

# Order terms by survey model appearance (top-to-bottom)
term_order <- svy_or4$term %>% purrr::map_chr(make_pretty_term) %>% unique()
forest_df$term_pretty <- factor(forest_df$term_pretty, levels = rev(term_order))

p_forest <- ggplot(forest_df, aes(x = OR, y = term_pretty, color = model)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.6), height = 0.2, linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.6), size = 2.2) +
  scale_x_log10() +
  scale_color_manual(values = c("Survey logistic" = COL_UNMET, "Mixed logistic" = COL_MET)) +
  labs(
    title = "Adjusted Odds Ratios (Model IV): Survey vs Mixed-effects",
    x = "Adjusted OR (log scale)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "Fig_Forest_aOR_ModelIV_Compare.png"),
       p_forest, width = 10, height = 7, dpi = 300)


# ---- 8.2 Marginal predicted probability by SWPER levels (Model IV) ----
# Uses g-computation: set domain to each level for everyone, predict, then take survey-weighted mean.
# NOTE: CIs here reflect design-based variability of predicted means (not full model-parameter uncertainty).

gcomp_mean <- function(fit, data, setvar, lev) {
  nd <- data
  nd[[setvar]] <- factor(lev, levels = levels(data[[setvar]]))
  
  if (inherits(fit, "svyglm")) {
    p <- as.numeric(predict(fit, newdata = nd, type = "response"))
  } else {
    p <- as.numeric(predict(fit, newdata = nd, type = "response", re.form = NA)) # population-average
  }
  
  nd$p <- p
  dtmp <- svydesign(id = ~v001, strata = ~v023, weights = ~wt, data = nd, nest = TRUE)
  m <- svymean(~p, dtmp, na.rm = TRUE)
  
  tibble(
    level = lev,
    pred  = as.numeric(coef(m)[1]),
    se    = as.numeric(SE(m)[1]),
    lcl   = pred - 1.96 * se,
    ucl   = pred + 1.96 * se
  )
}

swper_vars <- c("att_cat","aut_cat","dec_cat")

marg_df <- purrr::map_dfr(swper_vars, function(v) {
  levs <- levels(df[[v]])
  
  svy_part <- purrr::map_dfr(levs, ~gcomp_mean(m4,  df, v, .x)) %>%
    mutate(model = "Survey logistic", domain = v)
  
  mix_part <- purrr::map_dfr(levs, ~gcomp_mean(ml4, df, v, .x)) %>%
    mutate(model = "Mixed logistic", domain = v)
  
  bind_rows(svy_part, mix_part)
}) %>%
  mutate(
    domain = factor(domain, levels = swper_vars,
                    labels = c("Attitude to Violence","Social Independence","Decision Making")),
    level = factor(level, levels = c("Low","Medium","High")),
    model = factor(model, levels = c("Survey logistic","Mixed logistic"))
  )

p_marg <- ggplot(marg_df, aes(x = level, y = pred, color = model, group = model)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25), linewidth = 0.7) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.08,
                position = position_dodge(width = 0.25), linewidth = 0.6) +
  scale_color_manual(values = c("Survey logistic" = COL_UNMET, "Mixed logistic" = COL_MET)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  facet_wrap(~domain, nrow = 1) +
  labs(
    title = "Marginal Predicted Probability of Unmet Need by SWPER Domains (Model IV)",
    x = NULL,
    y = "Predicted probability (weighted mean)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "Fig_MarginalPred_SWPER_ModelIV.png"),
       p_marg, width = 11, height = 4.5, dpi = 300)


# ---- 8.3 Division caterpillar plot (weighted prevalence + 95% CI) ----
div_est <- svyby(~unmet_need_bin, ~division_std, des, svymean,
                 vartype = c("ci"), level = 0.95, na.rm = TRUE) %>%
  as.data.frame() %>%
  transmute(
    division = division_std,
    pct = 100 * unmet_need_bin,
    lcl = 100 * ci_l,
    ucl = 100 * ci_u
  ) %>%
  arrange(pct) %>%
  mutate(division = factor(division, levels = division))

p_div <- ggplot(div_est, aes(x = pct, y = division)) +
  geom_errorbarh(aes(xmin = lcl, xmax = ucl), height = 0.2, linewidth = 0.7, color = COL_UNMET) +
  geom_point(size = 2.4, color = COL_UNMET) +
  labs(
    title = "Unmet Need by Division (Weighted % with 95% CI)",
    x = "Percent (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "Fig_Caterpillar_UnmetNeed_ByDivision.png"),
       p_div, width = 8.5, height = 4.8, dpi = 300)


# ---- 8.4 Calibration plot (deciles) comparing final models ----
# Bin by survey-logistic predicted probability deciles (weighted), then compare observed vs predicted.

# ---- FIXED calibration plot (deciles) comparing final models ----
des_cal <- update(des,
                  pred_svy_m4   = df$pred_svy_m4,
                  pred_mixed_m4 = df$pred_mixed_m4
)

q_obj <- svyquantile(~pred_svy_m4, des_cal,
                     quantiles = seq(0, 1, 0.1),
                     ci = FALSE, na.rm = TRUE)
cuts <- as.numeric(coef(q_obj))

cuts[1] <- 0
cuts[length(cuts)] <- 1
cuts <- unique(cuts)
if (length(cuts) < 4) stop("Too few unique decile cutpoints for calibration. Check predictions.")

df$cal_bin <- cut(df$pred_svy_m4, breaks = cuts, include.lowest = TRUE, labels = FALSE)
des_cal <- update(des_cal, cal_bin = df$cal_bin)

obs <- svyby(~unmet_need_bin, ~cal_bin, des_cal, svymean, na.rm = TRUE) %>% as.data.frame()
p1  <- svyby(~pred_svy_m4,   ~cal_bin, des_cal, svymean, na.rm = TRUE) %>% as.data.frame()
p2  <- svyby(~pred_mixed_m4, ~cal_bin, des_cal, svymean, na.rm = TRUE) %>% as.data.frame()

cal_df <- tibble(
  bin = obs$cal_bin,
  observed = obs$unmet_need_bin,
  pred_svy = p1$pred_svy_m4,
  pred_mixed = p2$pred_mixed_m4
)

cal_long <- bind_rows(
  cal_df %>% transmute(bin, model = "Survey logistic", predicted = pred_svy, observed),
  cal_df %>% transmute(bin, model = "Mixed logistic", predicted = pred_mixed, observed)
) %>%
  mutate(model = factor(model, levels = c("Survey logistic","Mixed logistic")))

p_cal <- ggplot(cal_long, aes(x = predicted, y = observed, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("Survey logistic" = COL_UNMET, "Mixed logistic" = COL_MET)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Calibration Plot (Deciles) — Model IV",
    subtitle = "Observed vs predicted (weighted within deciles)",
    x = "Mean predicted probability",
    y = "Observed prevalence",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "Fig_Calibration_ModelIV_Compare.png"),
       p_cal, width = 7.5, height = 6, dpi = 300)

# ============================================================
# End of extra figures
# ============================================================

# ============================================================
# 9) REPORT-READY TABLE EXPORTS (CSV + LaTeX) -> G.files/
# ============================================================

# packages for table export (won't break if already installed)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

library(tidyr)
library(knitr)
library(kableExtra)

# ---------- helpers ----------
fmt_int <- function(x) formatC(round(as.numeric(x), 0), format = "f", digits = 0, big.mark = ",")
fmt_pct <- function(x, d = 1) sprintf(paste0("%.", d, "f"), as.numeric(x))
fmt_p   <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

write_tex <- function(df, file, caption = NULL, label = NULL) {
  tex <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = caption,
    label = label,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"), font_size = 9)
  
  writeLines(tex, file)
}

# ---------- 9.1 Univariate: clean & readable ----------
uni_ready <- univariate_table %>%
  mutate(
    Variable = label,
    Level = as.character(level),
    `n (unweighted)` = as.integer(n_unw),
    `Weighted N` = fmt_int(wt_N),
    `Weighted %` = fmt_pct(wt_pct, 1)
  ) %>%
  group_by(Variable) %>%
  mutate(Variable = ifelse(row_number() == 1, Variable, "")) %>%
  ungroup() %>%
  select(Variable, Level, `n (unweighted)`, `Weighted N`, `Weighted %`)

write.csv(uni_ready, file.path(out_dir, "Table1_Univariate_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(uni_ready, file.path(out_dir, "Table1_Univariate_ReportReady.tex"),
          caption = "Univariate distribution (unweighted n; weighted N and %).",
          label = "tab:univariate")

# ---------- 9.2 Bivariate: wide layout like paper tables ----------
# cell = "Weighted N (row %)" for each outcome
bivar_ready <- bivariate_table %>%
  mutate(
    cell = paste0(fmt_int(wt_N), " (", fmt_pct(row_pct, 1), ")"),
    p_value_fmt = fmt_p(p_value),
    Variable = label,
    Level = as.character(level)
  ) %>%
  select(variable, Variable, Level, outcome, cell, p_value_fmt) %>%
  tidyr::pivot_wider(names_from = outcome, values_from = cell) %>%
  group_by(variable, Variable) %>%
  mutate(`p-value` = ifelse(row_number() == 1, first(p_value_fmt), "")) %>%
  ungroup() %>%
  select(Variable, Level, `Met Need`, `Unmet Need`, `p-value`)

write.csv(bivar_ready, file.path(out_dir, "Table2_Bivariate_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(bivar_ready, file.path(out_dir, "Table2_Bivariate_ReportReady.tex"),
          caption = "Bivariate association with unmet need (weighted N and row %, survey-adjusted p-value).",
          label = "tab:bivariate")

# ---------- 9.3 Final model OR table (survey logistic + mixed side-by-side) ----------
make_pretty_term2 <- function(term) {
  for (v in c("att_cat","aut_cat","dec_cat","age","child_cat","working_stat","wealth_index","religion",
              "sex_hh","dist_facility","place_of_res","comm_edu","comm_wealth","comm_fp_expo","division_std")) {
    if (startsWith(term, v)) {
      lvl <- substring(term, nchar(v) + 1)
      return(paste0(var_labels[[v]], ": ", lvl))
    }
  }
  term
}

svy_or4   <- tidy_svy_or(m4, des) %>% filter(term != "(Intercept)") %>%
  mutate(term_pretty = purrr::map_chr(term, make_pretty_term2),
         aOR = paste0(OR, " (", CI_low, "–", CI_high, ")"),
         p = fmt_p(as.numeric(p_value))) %>%
  select(term, term_pretty, aOR, p) %>%
  rename(`aOR (95% CI) — Survey logistic` = aOR,
         `p — Survey` = p)

mix_or4 <- tidy_glmer_or(ml4) %>% filter(term != "(Intercept)") %>%
  mutate(term_pretty = purrr::map_chr(term, make_pretty_term2),
         aOR = paste0(OR, " (", CI_low, "–", CI_high, ")"),
         p = fmt_p(as.numeric(p_value))) %>%
  select(term, term_pretty, aOR, p) %>%
  rename(`aOR (95% CI) — Mixed logistic` = aOR,
         `p — Mixed` = p)

model_ready <- full_join(svy_or4, mix_or4, by = c("term","term_pretty")) %>%
  arrange(match(term_pretty, svy_or4$term_pretty)) %>%
  transmute(
    Covariate = term_pretty,
    `aOR (95% CI) — Survey logistic`,
    `p — Survey`,
    `aOR (95% CI) — Mixed logistic`,
    `p — Mixed`
  )

write.csv(model_ready, file.path(out_dir, "Table3_ModelIV_OR_compare_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(model_ready, file.path(out_dir, "Table3_ModelIV_OR_compare_ReportReady.tex"),
          caption = "Adjusted odds ratios from final Model IV (survey-weighted logistic vs mixed-effects logistic).",
          label = "tab:model4")
# ============================================================
# 9.4) REPORT-READY TABLES for Model I–III (SVY) -> G.files/
# Paste at bottom (after Section 9 block)
# Requires: tidy_svy_or(), var_labels, out_dir, and m1/m2/m3 already created
# ============================================================

# If not already loaded above:
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")
library(knitr)
library(kableExtra)

# helpers (reuse if already defined)
fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

write_tex <- function(df, file, caption = NULL, label = NULL) {
  tex <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = caption,
    label = label,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"), font_size = 9)
  
  writeLines(tex, file)
}

make_pretty_term2 <- function(term) {
  for (v in c("att_cat","aut_cat","dec_cat","age","child_cat","working_stat","wealth_index","religion",
              "sex_hh","dist_facility","place_of_res","comm_edu","comm_wealth","comm_fp_expo","division_std")) {
    if (startsWith(term, v)) {
      lvl <- substring(term, nchar(v) + 1)
      # Handle empty lvl just in case
      if (nchar(lvl) == 0) return(var_labels[[v]])
      return(paste0(var_labels[[v]], ": ", lvl))
    }
  }
  term
}

svy_model_report_ready <- function(fit, design, model_name, out_prefix) {
  tab <- tidy_svy_or(fit, design) %>%
    dplyr::filter(term != "(Intercept)") %>%
    mutate(
      Covariate = purrr::map_chr(term, make_pretty_term2),
      `aOR (95% CI)` = paste0(OR, " (", CI_low, "–", CI_high, ")"),
      `p-value` = fmt_p(as.numeric(p_value))
    ) %>%
    select(Covariate, `aOR (95% CI)`, `p-value`)
  
  # Save CSV + LaTeX
  write.csv(tab, file.path(out_dir, paste0(out_prefix, ".csv")), row.names = FALSE, na = "")
  write_tex(tab,
            file.path(out_dir, paste0(out_prefix, ".tex")),
            caption = paste0("Adjusted odds ratios (survey-weighted logistic), ", model_name, "."),
            label = paste0("tab:", tolower(gsub("[^a-zA-Z0-9]+", "", out_prefix)))
  )
  tab
}

# ---- Generate report-ready tables for Model I–III (SVY) ----
tab_m1 <- svy_model_report_ready(m1, des, "Model I",   "Table_Model_I_Svy_ReportReady")
tab_m2 <- svy_model_report_ready(m2, des, "Model II",  "Table_Model_II_Svy_ReportReady")
tab_m3 <- svy_model_report_ready(m3, des, "Model III", "Table_Model_III_Svy_ReportReady")


# ============================================================
# 12) REPORT-READY TABLES: SVY Model IV + Mixed Model IV -> G.files/
# Paste at bottom
# Requires: tidy_svy_or(), tidy_glmer_or(), m4, ml4, des, out_dir, var_labels
# ============================================================

if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")
library(knitr)
library(kableExtra)

fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

write_tex <- function(df, file, caption = NULL, label = NULL) {
  tex <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = caption,
    label = label,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"), font_size = 9)
  writeLines(tex, file)
}

make_pretty_term_rr <- function(term) {
  for (v in c("att_cat","aut_cat","dec_cat","age","child_cat","working_stat","wealth_index","religion",
              "sex_hh","dist_facility","place_of_res","comm_edu","comm_wealth","comm_fp_expo","division_std")) {
    if (startsWith(term, v)) {
      lvl <- substring(term, nchar(v) + 1)
      return(paste0(var_labels[[v]], ": ", lvl))
    }
  }
  term
}

# ---------- 12.1 SVY Model IV (report-ready) ----------
tab_svy4 <- tidy_svy_or(m4, des) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    Covariate = purrr::map_chr(term, make_pretty_term_rr),
    `aOR (95% CI)` = paste0(OR, " (", CI_low, "–", CI_high, ")"),
    `p-value` = fmt_p(as.numeric(p_value))
  ) %>%
  dplyr::select(Covariate, `aOR (95% CI)`, `p-value`)

write.csv(tab_svy4, file.path(out_dir, "Table_ModelIV_Svy_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(tab_svy4, file.path(out_dir, "Table_ModelIV_Svy_ReportReady.tex"),
          caption = "Adjusted odds ratios (survey-weighted logistic), Model IV.",
          label = "tab:model4_svy")

# ---------- 12.2 Mixed Model IV (report-ready) ----------
tab_mix4 <- tidy_glmer_or(ml4) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    Covariate = purrr::map_chr(term, make_pretty_term_rr),
    `aOR (95% CI)` = paste0(OR, " (", CI_low, "–", CI_high, ")"),
    `p-value` = fmt_p(as.numeric(p_value))
  ) %>%
  dplyr::select(Covariate, `aOR (95% CI)`, `p-value`)

write.csv(tab_mix4, file.path(out_dir, "Table_ModelIV_Mixed_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(tab_mix4, file.path(out_dir, "Table_ModelIV_Mixed_ReportReady.tex"),
          caption = "Adjusted odds ratios (mixed-effects logistic), Model IV.",
          label = "tab:model4_mixed")

# ---------- 12.3 Side-by-side: SVY vs Mixed (report-ready) ----------
svy_wide <- tidy_svy_or(m4, des) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    Covariate = purrr::map_chr(term, make_pretty_term_rr),
    `SVY aOR (95% CI)` = paste0(OR, " (", CI_low, "–", CI_high, ")"),
    `SVY p` = fmt_p(as.numeric(p_value))
  ) %>%
  dplyr::select(Covariate, `SVY aOR (95% CI)`, `SVY p`)

mix_wide <- tidy_glmer_or(ml4) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    Covariate = purrr::map_chr(term, make_pretty_term_rr),
    `Mixed aOR (95% CI)` = paste0(OR, " (", CI_low, "–", CI_high, ")"),
    `Mixed p` = fmt_p(as.numeric(p_value))
  ) %>%
  dplyr::select(Covariate, `Mixed aOR (95% CI)`, `Mixed p`)

tab_compare4 <- dplyr::full_join(svy_wide, mix_wide, by = "Covariate") %>%
  dplyr::arrange(match(Covariate, svy_wide$Covariate))

write.csv(tab_compare4, file.path(out_dir, "Table_ModelIV_Svy_vs_Mixed_ReportReady.csv"),
          row.names = FALSE, na = "")
write_tex(tab_compare4, file.path(out_dir, "Table_ModelIV_Svy_vs_Mixed_ReportReady.tex"),
          caption = "Model IV comparison: survey-weighted logistic vs mixed-effects logistic.",
          label = "tab:model4_compare")

# ============================================================
# 13) FLOWCHARTS (ORGANIZED) + SAVE TO G.files/
# Paste at bottom
# Requires: out_dir already set to "G.files"
# ============================================================

# ---- Packages (load once) ----
if (!requireNamespace("DiagrammeR", quietly = TRUE)) install.packages("DiagrammeR")
if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) install.packages("DiagrammeRsvg")
if (!requireNamespace("rsvg", quietly = TRUE)) install.packages("rsvg")

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# ---- Ensure output folder exists (G.files) ----
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Export helper: save PNG (and optional SVG) into out_dir ----
# --- UPDATED: save_flowchart() with PDF option ---
# Requires: DiagrammeRsvg + rsvg already loaded
# Saves to: file.path(out_dir, ...)

save_flowchart <- function(viz,
                           filename_base,
                           width_px = 2400,
                           save_svg = TRUE,
                           save_pdf = TRUE) {
  
  svg_txt <- export_svg(viz)
  
  # PNG
  rsvg_png(
    charToRaw(svg_txt),
    file  = file.path(out_dir, paste0(filename_base, ".png")),
    width = width_px
  )
  
  # SVG (optional)
  if (isTRUE(save_svg)) {
    rsvg_svg(
      charToRaw(svg_txt),
      file = file.path(out_dir, paste0(filename_base, ".svg"))
    )
  }
  
  # PDF (optional)
  if (isTRUE(save_pdf)) {
    rsvg_pdf(
      charToRaw(svg_txt),
      file = file.path(out_dir, paste0(filename_base, ".pdf"))
    )
  }
  
  invisible(TRUE)
}

# ============================================================
# 13.1 Flowchart: Why Female HH Head -> higher unmet need
# ============================================================
viz_sexhhh <- grViz("
digraph flowchart {
  graph [layout = dot, rankdir = TB, nodesep = 0.6, ranksep = 0.5, fontname = 'Times-Roman']
  node  [shape = rect, style = 'filled,rounded', fontname = 'Times-Roman', penwidth = 1.5, margin = 0.2]
  edge  [color = '#2c3e50', penwidth = 1.5, arrowsize = 0.8]

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970']
  A [label = 'Female-Headed\\nHousehold']

  node [fillcolor = '#B0C4DE', fontcolor = 'black', color = '#4682B4']
  B [label = 'Common underlying situations\\nand constraints']

  node [fillcolor = '#F0F8FF', fontcolor = 'black', color = '#4682B4']
  C1 [label = 'Male partner absent\\nor infrequent presence\\n\\nIrregular contraceptive\\nuse, higher exposure risk', align = 'left']
  C2 [label = 'Limited time, money,\\nand support for\\naccessing services\\n\\nChildcare burden,\\neconomic strain,\\npoor mobility', align = 'left']
  C3 [label = 'Social and logistic\\nbarriers to seeking care\\n\\nStigma in community,\\nfears, clinic gatekeeping', align = 'left']

  node [fillcolor = '#4682B4', fontcolor = 'white', color = '#4682B4']
  D [label = 'Not using a method despite\\nwanting to delay/stop births']

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970']
  E [label = 'Higher Measured Unmet Need\\n\\nObserved large adjusted Odds Ratio']

  A -> B
  B -> C1
  B -> C2
  B -> C3
  C1 -> D
  C2 -> D
  C3 -> D
  D -> E

  { rank = same; C1; C2; C3 }
}
")
viz_sexhhh
save_flowchart(viz_sexhhh, "Flow_Why_FemaleHHH_Higher_UnmetNeed", width_px = 2400, save_svg = TRUE)

# ============================================================
# 13.2 Flowchart: SWPER Index (domains and items)
# ============================================================
viz_swper <- grViz("
digraph swper_flow {
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.5, splines = ortho, fontname = 'Times-Roman']
  node  [shape = rect, style = 'filled,rounded', fontname = 'Times-Roman', penwidth = 1.5, margin = 0.2]
  edge  [color = '#2c3e50', penwidth = 1.5, arrowsize = 0.8]

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970', fontsize=20]
  Root [label = 'SWPER Index']

  node [fillcolor = '#F0F8FF', fontcolor = 'black', color = '#4682B4', fontsize=18]
  Desc [label = 'A measure of women\\'s empowerment based on DHS\\ndata, developed in 2015 by Ewerling et al. in 34\\nAfrican, Latin American, and Asian countries.', width=4]

  node [fillcolor = '#E6E6FA', fontcolor = 'black', color = '#4682B4', fontsize=16]
  Calc [label = 'How is it calculated?']

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970', width=2, fontsize=16]
  Head1 [label = 'Social\\nIndependence']
  Head2 [label = 'Decision-\\nMaking']
  Head3 [label = 'Attitude toward\\nViolence']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', shape=box, align=left, width=2, fontsize=16]
  List1 [label = '• Education\\n• Information access\\n• Gaps with spouse in age and education']
  List2 [label = '• Healthcare decisions\\n• Major household purchases\\n• Family visit decisions']
  List3 [label = '• Justification of\\nwife beating\\n• Refusal of sex\\n• Spousal violence reporting']

  Root -> Desc
  Desc -> Calc
  Calc -> Head1
  Calc -> Head2
  Calc -> Head3

  edge [dir = none]
  Head1 -> List1
  Head2 -> List2
  Head3 -> List3

  { rank = same; Head1; Head2; Head3 }
  { rank = same; List1; List2; List3 }
}
")
viz_swper
save_flowchart(viz_swper, "flowchartswper", width_px = 2400, save_svg = TRUE)
save_flowchart(viz_swper, "flowchartswper", width_px = 2400, save_pdf = TRUE)

# ============================================================
# 13.3 Flowchart: Unmet Need definition (blue theme)
# Keep only ONE version (you had two). This is the blue theme one.
# ============================================================
viz_unmet_def <- grViz("
digraph unmet_need_flow {
  graph [layout = dot, rankdir = TB, nodesep = 0.8, ranksep = 0.8, splines = polyline, fontname = 'Helvetica']
  node  [shape = rect, style = 'filled,rounded', fontname = 'Helvetica', penwidth = 1.5, margin = 0.25]
  edge  [color = '#2c3e50', penwidth = 1.2, arrowsize = 0.8, fontname = 'Helvetica', fontsize = 16]

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970', fontsize = 16]
  Start [label = 'Target Population\\n(Women 15-49, Married, Fecund)']

  node [fillcolor = '#B0C4DE', fontcolor = 'black', color = '#4682B4', fontsize = 16]
  Intent [label = 'What is her reproductive intention?']

  node [fillcolor = '#4682B4', fontcolor = 'white', color = '#191970', width = 2.5]
  Space [label = 'Wants to Space Births\\n(Wait 2+ years)']
  Limit [label = 'Wants to Limit Births\\n(No more children)']

  node [fillcolor = '#DCDCDC', fontcolor = 'black', color = '#808080', width = 2]
  Soon [label = 'Wants child soon\\n(Within 2 years)']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', width = 2.5]
  CheckSpace [label = 'Using modern contraception?']
  CheckLimit [label = 'Using modern contraception?']

  node [fillcolor = '#191970', fontcolor = 'white', color = '#000080', width = 2.5]
  UnmetSpace [label = 'UNMET NEED (SPACING)']
  UnmetLimit [label = 'UNMET NEED (LIMITING)']

  node [fillcolor = '#B0C4DE', fontcolor = 'black', color = '#778899', width = 2.5]
  MetSpace [label = 'Met Need\\n(Spacing)']
  MetLimit [label = 'Met Need\\n(Limiting)']

  node [fillcolor = '#DCDCDC', fontcolor = 'black', color = '#808080', width = 2]
  NoNeed [label = 'No Unmet Need']

  Start -> Intent
  Intent -> Space
  Intent -> Limit
  Intent -> Soon
  Soon -> NoNeed [color = '#808080']

  Space -> CheckSpace
  CheckSpace -> MetSpace   [label = ' Yes']
  CheckSpace -> UnmetSpace [label = ' No']

  Limit -> CheckLimit
  CheckLimit -> MetLimit   [label = ' Yes']
  CheckLimit -> UnmetLimit [label = ' No']

  { rank = same; Space; Limit; Soon }
  { rank = same; CheckSpace; CheckLimit }
  { rank = same; UnmetSpace; MetSpace; UnmetLimit; MetLimit; NoNeed }
}
")
viz_unmet_def
save_flowchart(viz_unmet_def, "unfpflowchart", width_px = 2400, save_svg = TRUE)
save_flowchart(viz_unmet_def, "unfpflowchart", width_px = 2400, save_pdf = TRUE)
save_flowchart(viz_unmet_def, "unfpflowchart2", save_pdf = TRUE)

# ============================================================
# 13.4 Flowchart: Sampling (TB version) — save in G.files
# ============================================================
viz_sampling_tb <- grViz("
digraph sampling_flow {
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.5, splines = polyline, fontname = 'Helvetica']

  node [shape = box, style = 'filled,rounded', fontname = 'Helvetica', fontsize = 11,
        penwidth = 1.5, margin = 0.2]
  edge [color = '#2c3e50', penwidth = 1.2, arrowsize = 0.8]

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970', width = 3]
  Clusters [label = 'Total selected clusters (EAs): 675\\n(1 rural cluster excluded due to security)\\nSurvey conducted in 674 clusters']

  node [fillcolor = '#4682B4', fontcolor = 'white', color = '#191970', width = 2]
  Urban [label = 'Urban\\n250 clusters']
  Rural [label = 'Rural\\n424 clusters']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', width = 2.5]
  Selection_Step [label = '45 households were selected\\nfrom each cluster']

  node [fillcolor = '#B0C4DE', fontcolor = 'black', color = '#4682B4', width = 2.5]
  Total_HH [label = 'Total 30,330 households selected\\n(Equal probability selection)']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', width = 2.5]
  HH_Interview [label = '30,018 households interviewed\\n(99.6% response rate)']

  Women_Eligible [label = '30,358 women eligible for interview']
  Women_Interview [label = '30,078 eligible women interviewed\\n(99.1% response rate)']
  Full_Q [label = '19,987 eligible women interviewed\\nfor full questionnaire\\n(98.9% response rate)']
  Married [label = '18,987 currently married women']

  node [fillcolor = '#191970', fontcolor = 'white', color = '#000080', width = 3]
  Final [label = '16,639 (unweighted) women included in study\\n(16,752 weighted)']

  Clusters -> Urban
  Clusters -> Rural
  Urban -> Selection_Step
  Rural -> Selection_Step
  Selection_Step -> Total_HH
  Total_HH -> HH_Interview
  HH_Interview -> Women_Eligible
  Women_Eligible -> Women_Interview
  Women_Interview -> Full_Q
  Full_Q -> Married
  Married -> Final

  { rank = same; Urban; Rural }
}
")
viz_sampling_tb
save_flowchart(viz_sampling_tb, "Flow_Sampling_TB", width_px = 2400, save_svg = TRUE)

# ============================================================
# 13.5 Flowchart: Sampling (LR version) — save in G.files
# ============================================================
viz_sampling_lr <- grViz("
digraph sampling_flow {
  graph [layout = dot, rankdir = LR, nodesep = 0.35, ranksep = 0.7, splines = polyline, fontname = 'Helvetica']
  node [shape = box, style = 'filled,rounded', fontname = 'Helvetica', fontsize = 20, penwidth = 1.5, margin = 0.2]
  edge [color = '#2c3e50', penwidth = 1.2, arrowsize = 0.8]

  node [fillcolor = '#191970', fontcolor = 'white', color = '#191970', width = 3]
  Clusters [label = 'Total selected clusters (EAs): 675\\n(1 rural cluster excluded due to security)\\nSurvey conducted in 674 clusters']

  node [fillcolor = '#4682B4', fontcolor = 'white', color = '#191970', width = 2]
  Urban [label = 'Urban\\n250 clusters']
  Rural [label = 'Rural\\n424 clusters']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', width = 2.5]
  Selection_Step [label = '45 households were selected\\nfrom each cluster']

  node [fillcolor = '#B0C4DE', fontcolor = 'black', color = '#4682B4', width = 2.5]
  Total_HH [label = 'Total 30,330 households selected']

  node [fillcolor = 'white', fontcolor = 'black', color = '#4682B4', width = 2.5]
  HH_Interview [label = '30,018 households interviewed']

  Women_Eligible [label = '30,358 women eligible for interview']
  Women_Interview [label = '30,078 eligible women interviewed']
  Full_Q [label = '19,987 eligible women interviewed\\nfor full questionnaire']
  Married [label = '18,987 currently married women']

  node [fillcolor = '#191970', fontcolor = 'white', color = '#000080', width = 3]
  Final [label = '16,639 women included in study']

  Clusters -> Urban
  Clusters -> Rural
  Urban -> Selection_Step
  Rural -> Selection_Step

  Selection_Step -> Total_HH [minlen=1]
  Total_HH -> HH_Interview [constraint=false]
  HH_Interview -> Women_Eligible [minlen=1]
  Women_Eligible  -> Women_Interview [constraint=false]
  Women_Interview -> Full_Q [constraint=false]
  Full_Q -> Married [constraint=false]
  Married -> Final [minlen=1]

  { rank = same; Urban; Rural }
}
")
viz_sampling_lr
save_flowchart(viz_sampling_lr, "samplingflowchart", width_px = 4200, save_svg = TRUE)
save_flowchart(viz_sampling_lr, "samplingflowchart", width_px = 4200, save_pdf = TRUE)

# ============================================================
# 13.6 Flowchart: Conceptual Framework — save in G.files
# ============================================================
viz_framework <- grViz("
digraph conceptual_framework {
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.8, splines = ortho, fontname = 'Helvetica']
  node [shape = plain, fontname = 'Helvetica', fontsize = 18]
  edge [color = '#555555', penwidth = 1.2, arrowsize = 0.8]

  SWPER [label = <
    <table border='0' cellborder='1' cellspacing='0' cellpadding='8' color='#708090'>
      <tr><td bgcolor='#B0C4DE' sides='B'><b>SWPER Index</b></td></tr>
      <tr><td bgcolor='white' align='left' balign='left'>
        • Attitude towards violence<br/>
        • Social independence<br/>
        • Decision making
      </td></tr>
    </table>
  >]

  Individual [label = <
    <table border='0' cellborder='1' cellspacing='0' cellpadding='8' color='#708090'>
      <tr><td bgcolor='#B0C4DE' sides='B'><b>Individual-level<br/>Characteristics</b></td></tr>
      <tr><td bgcolor='white' align='left' balign='left'>
        • Respondent age group<br/>
        • Number of living children<br/>
        • Employment status<br/>
        • Wealth index<br/>
        • Religion<br/>
        • FP Media exposure
      </td></tr>
    </table>
  >]

  Community [label = <
    <table border='0' cellborder='1' cellspacing='0' cellpadding='30' color='#708090'>
      <tr><td bgcolor='#B0C4DE' sides='B'><b>Household, Community, Region-level<br/>Characteristics</b></td></tr>
      <tr><td bgcolor='white' align='left' balign='left'>
        • Sex of household head<br/>
        • Distance to health facility<br/>
        • Place of residence<br/>
        • Administrative division<br/>
        • Community female education<br/>
        • Community wealth status<br/>
        • Community FP exposure
      </td></tr>
    </table>
  >]

  Outcome [label = <
    <table border='0' cellborder='1' cellspacing='0' cellpadding='30' color='#191970'>
      <tr><td bgcolor='#191970'><font color='white'><b>Unmet Need for<br/>Family Planning</b></font></td></tr>
    </table>
  >]

  Individual -> Outcome 
  SWPER -> Individual
  SWPER -> Outcome
  Individual -> Community
  Community -> Outcome

  { rank = same; SWPER; Individual; Community }
}
")
viz_framework
save_flowchart(viz_framework, "concept framework", width_px = 2400, save_svg = TRUE)
save_flowchart(viz_framework, "concept framework", width_px = 4200, save_pdf = TRUE)

# ============================================================
# DONE: Flowcharts saved to out_dir (G.files)
# ============================================================


# ============================================================
# ORDERED GVIF / Adjusted GVIF (VIF) CODE — REPORT READY
# Paste at bottom
# Saves: CSV + LaTeX into "G.files/"
# Requires: df, out_dir, m4, ml4 already exist
# ============================================================

# ---- Packages ----
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(car)
library(dplyr)
library(tibble)
library(knitr)
library(kableExtra)
library(stringr)

# ---- Output folder ----
if (!exists("out_dir") || is.null(out_dir) || !nzchar(out_dir)) out_dir <- "G.files"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Make sure you have a binary outcome for glm() ----
# (VIF/GVIF uses the predictor X matrix; outcome can be any binary)
if (!("unmet_need_bin" %in% names(df))) {
  if ("unmet_need_f" %in% names(df)) {
    df$unmet_need_bin <- ifelse(as.character(df$unmet_need_f) == "Unmet Need", 1, 0)
  } else if ("unmet_need" %in% names(df)) {
    if (is.factor(df$unmet_need)) {
      df$unmet_need_bin <- ifelse(as.character(df$unmet_need) == "Unmet Need", 1, 0)
    } else {
      df$unmet_need_bin <- ifelse(df$unmet_need == 1, 1, 0)
    }
  } else {
    stop("No unmet_need / unmet_need_f found to create unmet_need_bin.")
  }
}

# ---- If wt missing or not numeric, handle safely ----
if (!("wt" %in% names(df))) stop("Weight variable 'wt' not found in df.")
df$wt <- as.numeric(df$wt)

# ---- LaTeX writer ----
write_tex <- function(df_out, file, caption = NULL, label = NULL) {
  tex <- knitr::kable(
    df_out,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = caption,
    label = label,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"), font_size = 9)
  writeLines(tex, file)
}

# ============================================================
# 1) FUNCTION: GVIF table from ANY model object (m4 or ml4)
# ============================================================
# ============================================================
# UPDATED: ORDERED GVIF with "Adj GVIF (squared)" INCLUDED
# (Adj GVIF)^2 = GVIF^(1/Df)
# This replaces/updates your gvif_ordered_report() and the run block
# ============================================================

# ---- Updated function ----
gvif_ordered_report <- function(model_obj,
                                data,
                                weight_var = "wt",
                                outcome_bin = "unmet_need_bin",
                                order_terms,
                                file_base) {
  
  # Extract RHS terms
  rhs_terms <- attr(terms(model_obj), "term.labels")
  if (length(rhs_terms) == 0) stop("No RHS terms detected from model_obj.")
  
  # Build a glm with same RHS (VIF depends on X matrix)
  f_vif <- as.formula(paste(outcome_bin, "~", paste(rhs_terms, collapse = " + ")))
  fit_vif <- glm(f_vif, data = data, family = binomial(), weights = data[[weight_var]])
  
  v <- car::vif(fit_vif)
  
  # Convert vif output to table
  if (is.matrix(v)) {
    out <- tibble::tibble(
      Predictor = rownames(v),
      GVIF      = as.numeric(v[, "GVIF"]),
      Df        = as.numeric(v[, "Df"])
    )
  } else {
    out <- tibble::tibble(
      Predictor = names(v),
      GVIF      = as.numeric(v),
      Df        = 1
    )
  }
  
  # Add adjusted metrics (including squared adjusted GVIF)
  out <- out %>%
    dplyr::mutate(
      GVIF = round(GVIF, 3),
      `GVIF^(1/(2*Df))` = round(GVIF^(1 / (2 * Df)), 3),
      `Adj GVIF` = `GVIF^(1/(2*Df))`,
      `Adj GVIF (squared)` = round((`GVIF^(1/(2*Df))`^2), 3),  # = GVIF^(1/Df)
      Flag = dplyr::case_when(
        `Adj GVIF` < 2 ~ "OK (<2)",
        `Adj GVIF` < 5 ~ "Moderate (2–<5)",
        TRUE ~ "High (≥5)"
      )
    )
  
  # Order by your Model IV sequence (Serial)
  serial_map <- tibble::tibble(Predictor = order_terms, Serial = seq_along(order_terms))
  
  out_ord <- out %>%
    dplyr::left_join(serial_map, by = "Predictor") %>%
    dplyr::mutate(Serial = ifelse(is.na(Serial), 999L, as.integer(Serial))) %>%
    dplyr::arrange(Serial, Predictor) %>%
    dplyr::mutate(Serial = ifelse(Serial == 999L, NA_integer_, Serial)) %>%
    dplyr::select(Serial, everything())
  
  # Save CSV + TEX
  csv_path <- file.path(out_dir, paste0(file_base, ".csv"))
  tex_path <- file.path(out_dir, paste0(file_base, ".tex"))
  
  write.csv(out_ord, csv_path, row.names = FALSE, na = "")
  write_tex(out_ord, tex_path,
            caption = "GVIF, adjusted GVIF (VIF-comparable), and squared adjusted GVIF (GVIF^(1/Df)), ordered by Model IV sequence.",
            label = paste0("tab:", tolower(gsub("[^a-zA-Z0-9]+", "", file_base))))
  
  message("Saved: ", csv_path)
  message("Saved: ", tex_path)
  
  out_ord
}

# ============================================================
# UPDATED RUN BLOCK (includes squared columns automatically)
# ============================================================

# SVY model IV ordered GVIF
gvif_svy_m4_ordered <- gvif_ordered_report(
  model_obj   = m4,
  data        = df,
  weight_var  = "wt",
  outcome_bin = "unmet_need_bin",
  order_terms = model4_order,
  file_base   = "Table_GVIF_SVY_ModelIV_ORDERED"
)

# Mixed model IV ordered GVIF (same fixed-effects set)
gvif_mixed_m4_ordered <- gvif_ordered_report(
  model_obj   = ml4,
  data        = df,
  weight_var  = "wt",
  outcome_bin = "unmet_need_bin",
  order_terms = model4_order,
  file_base   = "Table_GVIF_Mixed_ModelIV_ORDERED"
)

# Optional: ordered comparison table (adjusted + squared adjusted GVIF side-by-side)
gvif_compare <- full_join(
  gvif_svy_m4_ordered %>% select(
    Serial, Predictor,
    adj_svy = `Adj GVIF`,
    adj2_svy = `Adj GVIF (squared)`
  ),
  gvif_mixed_m4_ordered %>% select(
    Predictor,
    adj_mixed = `Adj GVIF`,
    adj2_mixed = `Adj GVIF (squared)`
  ),
  by = "Predictor"
) %>%
  arrange(Serial)

write.csv(gvif_compare,
          file.path(out_dir, "Table_GVIF_Compare_SVY_vs_Mixed_ModelIV_ORDERED.csv"),
          row.names = FALSE, na = "")

write_tex(gvif_compare,
          file.path(out_dir, "Table_GVIF_Compare_SVY_vs_Mixed_ModelIV_ORDERED.tex"),
          caption = "Adjusted GVIF (GVIF^(1/(2Df))) and squared adjusted GVIF (GVIF^(1/Df)) comparison for Model IV predictors (SVY vs mixed), ordered by Model IV sequence.",
          label = "tab:gvif_compare_modeliv")

# ============================================================
# ROC curves in ONE plot:
#  - SVY Models I–IV (survey-weighted predictions)
#  - Mixed-effects Model IV (melogit/glmer)
# Colors:
#  - Model I:  COL_LOW   (#87CEEB)
#  - Model II: COL_MED   (#4682B4)
#  - Model III: COL_MET  (#191970)  (dark)
#  - Model IV (SVY): COL_UNMET (#4682B4)  (steelblue)
#  - Model IV (Mixed): COL_MET (#191970)  (midnight blue)
# Saves to: G.files/ROC_All_Models_OnePlot.png
# ============================================================

# ---- safety: needed packages ----
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

library(dplyr)
library(tibble)
library(ggplot2)
library(scales)

# ---- Ensure binary outcome exists ----
if (!("unmet_need_bin" %in% names(df))) {
  if ("unmet_need_f" %in% names(df)) {
    df$unmet_need_bin <- ifelse(as.character(df$unmet_need_f) == "Unmet Need", 1, 0)
  } else if ("unmet_need" %in% names(df)) {
    df$unmet_need_bin <- ifelse(df$unmet_need == 1, 1, 0)
  } else {
    stop("No unmet_need / unmet_need_f found.")
  }
}
df$wt <- as.numeric(df$wt)

# ---- Weighted ROC helper (no pROC; works with survey weights) ----
weighted_roc <- function(y, p, w, n_bins = 250) {
  keep <- is.finite(y) & is.finite(p) & is.finite(w)
  y <- y[keep]; p <- p[keep]; w <- w[keep]
  
  if (length(unique(y)) < 2) stop("ROC needs both 0 and 1 outcomes.")
  
  # thresholds
  uniq_p <- unique(p)
  if (length(uniq_p) > n_bins) {
    thr <- quantile(uniq_p, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE)
    thr <- sort(unique(thr), decreasing = TRUE)
  } else {
    thr <- sort(unique(uniq_p), decreasing = TRUE)
  }
  
  P <- sum(w[y == 1]); N <- sum(w[y == 0])
  if (P == 0 || N == 0) stop("ROC cannot be computed: no positives or negatives after filtering.")
  
  tpr <- numeric(length(thr))
  fpr <- numeric(length(thr))
  
  for (i in seq_along(thr)) {
    pred1 <- p >= thr[i]
    tp <- sum(w[pred1 & y == 1])
    fp <- sum(w[pred1 & y == 0])
    tpr[i] <- tp / P
    fpr[i] <- fp / N
  }
  
  # add endpoints and compute AUC (trapezoid)
  dfroc <- tibble(fpr = c(0, fpr, 1), tpr = c(0, tpr, 1))
  ord <- order(dfroc$fpr)
  x <- dfroc$fpr[ord]; y2 <- dfroc$tpr[ord]
  auc <- sum(diff(x) * (head(y2, -1) + tail(y2, -1)) / 2)
  
  dfroc$auc <- auc
  dfroc
}

# ---- Get predicted probabilities for all models ----
df$pred_m1 <- as.numeric(predict(m1, type = "response"))
df$pred_m2 <- as.numeric(predict(m2, type = "response"))
df$pred_m3 <- as.numeric(predict(m3, type = "response"))
df$pred_m4 <- as.numeric(predict(m4, type = "response"))

# Mixed model IV predictions
df$pred_ml4 <- as.numeric(predict(ml4, type = "response"))

# ---- Build ROC dataframes ----
roc_m1  <- weighted_roc(df$unmet_need_bin, df$pred_m1,  df$wt) %>% mutate(model = "SVY Model I")
roc_m2  <- weighted_roc(df$unmet_need_bin, df$pred_m2,  df$wt) %>% mutate(model = "SVY Model II")
roc_m3  <- weighted_roc(df$unmet_need_bin, df$pred_m3,  df$wt) %>% mutate(model = "SVY Model III")
roc_m4  <- weighted_roc(df$unmet_need_bin, df$pred_m4,  df$wt) %>% mutate(model = "SVY Model IV")
roc_ml4 <- weighted_roc(df$unmet_need_bin, df$pred_ml4, df$wt) %>% mutate(model = "Mixed Model IV")

roc_all <- bind_rows(roc_m1, roc_m2, roc_m3, roc_m4) %>%
  mutate(model = factor(model, levels = c("SVY Model I","SVY Model II","SVY Model III","SVY Model IV","Mixed Model IV")))

# AUC labels
auc_tbl <- roc_all %>%
  group_by(model) %>%
  summarise(AUC = first(auc), .groups = "drop") %>%
  mutate(lbl = paste0(model, " (AUC=", sprintf("%.3f", AUC), ")"))

auc_labs <- setNames(auc_tbl$lbl, auc_tbl$model)

# ---- Colors (you can edit if you want exact fixed mapping) ----
roc_cols <- c(
  "SVY Model I"    = "yellow",
  "SVY Model II"   = "orange",
  "SVY Model III"  = "red",
  "SVY Model IV"   = COL_UNMET,  # steelblue
  "Mixed Model IV" = COL_MET     # midnight blue
)
# ---- Plot ----
p_roc_all <- ggplot(roc_all, aes(fpr, tpr, color = model)) +
  geom_line(linewidth = 1.25) +
  geom_abline(linetype = "dashed", color = "grey50") +
  coord_equal() +
  scale_color_manual(values = roc_cols, labels = c(
    "SVY Model I"    = "Model I",
    "SVY Model II"   = "Model II",
    "SVY Model III"  = "Model III",
    "SVY Model IV"   = "Model IV",  # steelblue
    "Mixed Model IV" = "Mixed Effect Model IV"
  )) +
  labs(
    title = "ROC Curves: Logistic Models I–IV and Mixed-effects Logistic Model IV",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.box       = "vertical"
  )

print(p_roc_all)

ggsave(
  filename = file.path(out_dir, "ROC_All_Models_OnePlot.png"),
  plot = p_roc_all, width = 10, height = 7.5, dpi = 300, bg = "transparent"
)

ggsave(
  filename = file.path(out_dir, "ROC all.pdf"),
  plot = p_roc_all, width = 10, height = 7.5, dpi = 300, bg = "transparent"
)

