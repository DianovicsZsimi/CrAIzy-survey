# Preserve Unicode symbols in plot text on Windows.
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
.libPaths(c(".r_library", .libPaths()))
library(scico)
library(tidyverse)
library(ggplot2)
library(scales)
library(rnaturalearth)
library(sf)

# Source with local = knitr::knit_global() so helpers share the analysis context.


# Variable definitions, question titles, and response options

question_text = c(
  discipline_of_interest = "Select your primary research discipline.",
  stage = "Select your career stage.",
  ai_use = "How much do you currently use AI?",
  awareness = "Before reading the introduction above, how familiar were you with the recent reports of AI-generated mathematics research?",
  capability = "Within the next three years, what level of scientific research capability do you expect AI systems to achieve in your field?",
  displacement = "Advances in AI will significantly reduce the demand for human researchers in my field in the next three years.",
  threat = "Advances in AI threaten the continued existence and long-term viability of academic research as a human profession.",
  opportunity = "AI-driven research will increase the overall rate of scientific progress.",
  concern = "I am concerned about the implications of AI-driven scientific research for my own job security.",
  ai_roles = "Highest level of AI autonomy you consider acceptable by research roles",
  change = "Have recent advances in AI-driven scientific research caused you to reconsider your research or academic career?",
  country = ""
)

agreement_order = c("Strongly disagree", "Somewhat disagree",
                    "Neither agree nor disagree", "Somewhat agree", "Strongly agree")

response_orders = list(
  displacement = agreement_order,
  threat = agreement_order,
  opportunity = agreement_order,
  concern = agreement_order,
  awareness = c("Not aware", "Heard about them, but know little",
                "Somewhat familiar", "Very familiar"),
  ai_use = c("Not at all",
             "I have only tried it occasionally",
             "I use it for a few specific tasks",
             "I regularly use it for some of my research tasks",
             "I use it for many of my research tasks",
             "It is integral to nearly all of my research work"),
  capability = c(
    "Cannot help researchers.",
    "Assist human researchers in carrying out research tasks.",
    "Perform research tasks independently, but produce lower-quality results than competent human researchers.",
    "Perform research tasks independently, producing results of comparable quality to those of competent human researchers.",
    "Perform research tasks independently, producing higher-quality results than competent human researchers."
  ),
  stage = c("PhD student", "Postdoctoral researcher", "Assistant professor",
            "Associate professor", "Full professor",
            "Currently not conducting research within an academic setting", "Other"),
  change = c("I do not intend to reconsider my research or career.",
             "I am reconsidering research topics.",
             "I am increasing AI adoption in my research.",
             "I am reconsidering my academic career.",
             "I am considering sacrificing less of my life to science.",
             "I am considering training fewer researchers.",
             "Other", "I prefer not to answer.")
)

demographics = c("discipline_of_interest", "stage", "ai_use")

ai_questions = c("awareness", "capability", "displacement", "threat", "opportunity", "concern")

ai_roles = c(role_ra = "Research assistant",
             role_ir = "Independent researcher",
             role_pr = "Manuscript peer reviewer",
             role_je = "Journal editor",
             role_gr = "Grant reviewer",
             role_dm = "Research funding decision-maker")

role_response_order = c("Not permitted", "Permitted with human supervision",
                        "Permitted autonomously", "Unsure")

summary_disciplines = c(
  "Natural sciences", "Engineering and technology",
  "Medical, health and life sciences", "Agricultural and veterinary sciences",
  "Social sciences", "Humanities and the arts"
)

training_option = "I am considering training fewer researchers."

discipline_rank_responses = list(
  awareness = "Very familiar",
  capability = response_orders$capability[5],
  displacement = c("Somewhat agree", "Strongly agree"),
  threat = c("Somewhat agree", "Strongly agree"),
  opportunity = c("Somewhat agree", "Strongly agree"),
  concern = c("Somewhat agree", "Strongly agree")
)

subfield_disciplines = c(
  social_sciences = "Social sciences",
  medical_life_sciences = "Medical, health and life sciences",
  engineering = "Engineering and technology",
  humanities = "Humanities and the arts"
)

subfield_other_labels = c(
  social_sciences = "Other social sciences",
  medical_life_sciences = "Other medical sciences",
  engineering = "Other engineering and technologies",
  humanities = "Other humanities"
)

subfield_labels = c(
  "Electrical engineering, computer engineering, and information engineering" =
    "Electrical, computer & information engineering",
  "Civil engineering, maritime engineering, transport engineering, and mining engineering" =
    "Civil, maritime, transport & mining eng.",
  "Environmental engineering and related engineering" =
    "Environmental & related engineering",
  "Materials engineering and resources engineering" =
    "Materials & resources engineering",
  "Industrial, systems and processes engineering" =
    "Industrial, systems & process engineering"
)

country_group_order = c("USA", "EU + UK", "China", "India", "Other countries")

tokyo_colours = scico::scico(5, palette = "vik")

academic_colours = list(
  responses = tokyo_colours,
  bar = tokyo_colours[2],
  missing = "#DDDDDD"
)

academic_page_width = 16 / 2.54

# Shared percentage-label utility (also used during data preparation)

stacked_percent_label = function(proportion) {
  percentage = round(100 * proportion)
  ifelse(!is.na(percentage) & percentage >= 5, paste0(percentage, "%"), "")
}

# Supporting data, response orders, summaries, and validation
# Run when this file is sourced from the report.

# Load cleaned survey data
# Render data_wrangling.Rmd before running this document.
data_filtered = read.csv("data_clean.csv", check.names = FALSE, na.strings = "", fileEncoding = "UTF-8")
# Show the other natural-science field only when at least 100 respondents selected it.
other_natural_sciences_n = sum(data_filtered$discipline_1 %in% "Natural sciences" &
                                data_filtered$discipline_2 %in% "Other natural sciences")
other_natural_sciences_rows = data_filtered$discipline_1 %in% "Natural sciences" &
  data_filtered$discipline_2 %in% "Other natural sciences"
data_filtered$discipline_of_interest[other_natural_sciences_rows] =
  if (other_natural_sciences_n >= 100) "Other natural sciences" else NA_character_


# analysis-groups
grouped_plots = list()
# Omit career stages with no respondents in the cleaned sample.
response_orders$stage = response_orders$stage[response_orders$stage %in% data_filtered$stage]

# Natural-science fields first, then the other disciplines; alphabetical within each.
discipline_order = data_filtered |>
  filter(!is.na(discipline_of_interest), nzchar(trimws(discipline_of_interest))) |>
  distinct(discipline_1, discipline_of_interest) |>
  arrange(desc(discipline_1 == "Natural sciences"), discipline_of_interest) |>
  pull(discipline_of_interest) |>
  unique()
response_orders$discipline_of_interest = discipline_order

country_check = data_filtered |>
  distinct(country, country_iso3, continent) |>
  arrange(country)
country_counts = data_filtered |>
  filter(!is.na(country_iso3)) |>
  count(country_iso3, name = "n") |>
  mutate(percentage = n / sum(n))


# participant-summary
summary_rows = function(variable, heading, responses, labels = responses) {
  answers = trimws(as.character(data_filtered[[variable]]))
  answers = answers[!is.na(answers) & nzchar(answers)]
  counts = as.integer(table(factor(answers, levels = responses)))
  data.frame(
    Variable = c(heading, labels),
    Count = c(NA_integer_, counts),
    `Percentage (%)` = c(NA_real_, round(100 * counts / length(answers), 1)),
    check.names = FALSE
  )
}

participant_summary = rbind(
  summary_rows("stage", "Stage", response_orders$stage),
  summary_rows("discipline_1", "Discipline", summary_disciplines,
               labels = replace(summary_disciplines, 5, "Social Sciences"))
)
rownames(participant_summary) = NULL
write.csv(participant_summary, "participant_summary.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")


# ai-background-summary
ai_background_summary = rbind(
  summary_rows("awareness", "Awareness", response_orders$awareness),
  summary_rows("ai_use", "AI use", response_orders$ai_use)
)
rownames(ai_background_summary) = NULL
write.csv(ai_background_summary, "ai_background_summary.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")


# ai-roles preparation

role_responses = data_filtered |>
  select(all_of(names(ai_roles))) |>
  pivot_longer(everything(), names_to = "role", values_to = "response") |>
  mutate(response = trimws(response)) |>
  filter(!is.na(response), nzchar(response))

role_counts = bind_rows(role_responses, role_responses |> mutate(role = ".all_roles")) |>
  mutate(role = factor(role, levels = c(rev(names(ai_roles)), ".all_roles")),
         response = factor(response, levels = role_response_order)) |>
  count(role, response, .drop = FALSE) |>
  group_by(role) |>
  mutate(total = sum(n),
         percentage = if_else(total > 0, n / total, NA_real_)) |>
  ungroup()

# Explicit cumulative positions keep the requested left-to-right stack order.
role_counts = role_counts |>
  arrange(role, response) |>
  group_by(role) |>
  mutate(xmax = cumsum(percentage), xmin = xmax - percentage,
         label = stacked_percent_label(percentage)) |>
  ungroup()


# ai-change preparation
# Multiple selections: denominators count people answering the question, not selections.
change_respondents = data_filtered |>
  select(respondent, stage, change) |>
  filter(!is.na(change), nzchar(trimws(change)))

change_long = change_respondents |>
  select(respondent, change) |>
  separate_longer_delim(change, delim = ",") |>
  mutate(change = trimws(change)) |>
  filter(nzchar(change)) |>
  distinct(respondent, change)
# Exclude PhD students from both numerator and denominator for training only.
change_eligible = tidyr::expand_grid(
  respondent = change_respondents$respondent,
  response = response_orders$change
) |>
  left_join(select(change_respondents, respondent, stage), by = "respondent") |>
  filter(!(response == training_option & coalesce(stage == "PhD student", FALSE))) |>
  left_join(change_long |> transmute(respondent, response = change, chosen = TRUE),
            by = c("respondent", "response"))

change_counts = change_eligible |>
  group_by(response) |>
  summarise(n = sum(chosen %in% TRUE), denominator = n(), .groups = "drop")
stopifnot(all(change_counts$n <= change_counts$denominator))


# Prepare the four subfield datasets and their pooled categories once.
subfield_data = lapply(names(subfield_disciplines), function(discipline) {
  parent = subfield_disciplines[[discipline]]
  discipline_data = data_filtered |> filter(discipline_1 == parent)
  original_n = nrow(discipline_data)
  other_label = unname(subfield_other_labels[discipline])
  rare_subfields = discipline_data |>
    filter(!is.na(discipline_2), nzchar(trimws(discipline_2))) |>
    count(discipline_2) |>
    filter(n < 10, discipline_2 != other_label) |>
    pull(discipline_2)
  discipline_data = discipline_data |>
    mutate(discipline_2 = if_else(discipline_2 %in% rare_subfields,
                                  other_label, discipline_2))
  stopifnot(nrow(discipline_data) == original_n,
            !any(discipline_data$discipline_2 %in% rare_subfields))
  subfield_order = discipline_data$discipline_2
  subfield_order = sort(unique(subfield_order[!is.na(subfield_order) & nzchar(trimws(subfield_order))]))

  list(data = discipline_data, order = subfield_order)
})
names(subfield_data) = names(subfield_disciplines)


# country-respondent-counts preparation
country_bars = country_counts |>
  arrange(desc(n), country_iso3) |>
  mutate(country_iso3 = factor(country_iso3, levels = rev(country_iso3)),
         # Read down each column, with one common count scale across columns.
         column = ceiling(row_number() / ceiling(n() / 3)))


# world-map preparation
world = ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_data = world |>
  left_join(
    country_counts,
    by = c("adm0_a3" = "country_iso3")
  )

# Every observed country must match map geometry; fail visibly if a future code does not.
stopifnot(all(country_counts$country_iso3 %in% world$adm0_a3))


demographic_counts = setNames(lapply(demographics, function(question) {
  data_filtered |> count(response = .data[[question]])
}), demographics)


# Countries not matched to map geometry (displayed below the map).
unmatched_map_countries = country_check |>
  filter(!is.na(country_iso3)) |>
  anti_join(
    sf::st_drop_geometry(world),
    by = c("country_iso3" = "adm0_a3")
  )


# Visualisation functions: themes, plots, layout, and export

configure_visualisation = function() {

  knitr::opts_chunk$set(echo = FALSE, fig.width = 12, fig.height = 5, dpi = 300, fig.retina = 1,
                        fig.align = "center", dev = "ragg_png")
  

  plot_export_dir = file.path(getwd(), "plots_png", "academic")
  dir.create(plot_export_dir, recursive = TRUE, showWarnings = FALSE)
  plot_filename = function(path) {
    name = sub("-[0-9]+[.]png$", "", basename(path))
    name = gsub("-", "_", name)
    if (!endsWith(name, "_academic")) name = paste0(name, "_academic")
    paste0(name, ".png")
  }
  
  default_plot_hook = knitr::knit_hooks$get("plot")
  knitr::knit_hooks$set(plot = function(x, options) {
    for (path in x) {
      filename = plot_filename(path)
      file.copy(path, file.path(plot_export_dir, filename), overwrite = TRUE)
    }
    default_plot_hook(x, options)
  })
}


question_title = function(question, width = 85) {
  stringr::str_wrap(unname(question_text[question]), width = width)
}

title_height = function(title) {
  0.27 * (stringr::str_count(title, "\n") + 1)
}


participant_subtitle = function(n) {
  paste0("<i>N</i> = ", format(n, big.mark = ",", scientific = FALSE, trim = TRUE))
}


response_palette = function(answers) {
  positions = if (length(answers) == 4) c(1, 2, 4, 5) else seq_along(answers)
  setNames(academic_colours$responses[positions], answers)
}

# Explicit line breaks
legend_response_labels = function(question, answers) {
  labels = if (question == "capability") {
    c("Cannot help", "Assist humans",
      "Work independently:\nachieving lower quality",
      "Work independently:\nachieving comparable quality",
      "Work independently:\nachieving higher quality")
  } else {
    wrapped = stringr::str_wrap(answers, width = 22)
    wrapped[answers == "Neither agree nor disagree"] = "Neither agree nor disagree"
    wrapped
  }

  labels
}

draw_key_square = function(data, params, size) {
  grid::rectGrob(width = grid::unit(12, "pt"), height = grid::unit(12, "pt"),
                 gp = grid::gpar(fill = data$fill, col = NA))
}

theme_academic = function(base_size = 10, base_family = "Times New Roman",
                          manuscript_family = "Times New Roman") {
  stopifnot(base_size >= 8, base_size <= 14)
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black"),
      geom = element_geom(family = base_family, fontsize = 8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.title = element_text(size = base_size, face = "plain"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = base_size, color = "black"),
      axis.text.y = element_text(hjust = 1, margin = margin(r = 6)),
      plot.tag = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(family = manuscript_family, size = 12,
                                face = "italic", hjust = 0, margin = margin(b = 12)),
      plot.subtitle = ggtext::element_markdown(family = base_family, size = base_size,
                                               color = "black", margin = margin(b = 8)),
      legend.position = "top",
      legend.location = "plot",
      legend.justification = "center",
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = grid::unit(12, "pt"),
      legend.box.spacing = grid::unit(4, "pt"),
      legend.margin = margin(b = 4),
      legend.title = element_text(size = base_size, face = "plain"),
      legend.text = element_text(size = base_size),
      strip.background = element_blank(),
      strip.text = element_text(size = base_size, face = "plain"),
      plot.margin = margin(6, 26, 6, 6)
    )
}

category_plot = function(counts, title,
                         wrap_width = 32,
                         response_order = NULL, denominator = NULL, participants = NULL) {
  counts = counts |>
    mutate(response = as.character(response)) |>
    filter(!is.na(response), nzchar(trimws(response)))
  if (nrow(counts) == 0) return(NULL)
  if (is.null(participants)) participants = sum(counts$n)

  # Never sort by count. Reverse levels because ggplot draws the first at bottom.
  if (is.null(response_order)) response_order = sort(unique(counts$response))
  if (is.null(denominator)) {
    denominator = if ("denominator" %in% names(counts))
      setNames(counts$denominator, counts$response) else sum(counts$n)
  }
  stopifnot(all(is.finite(denominator)), all(denominator > 0))
  counts = counts |>
    tidyr::complete(response = response_order, fill = list(n = 0)) |>
    mutate(value = n / if (length(.env$denominator) == 1) .env$denominator else
             unname(.env$denominator[response]),
           label = scales::percent(value, accuracy = 1),
           response = factor(response, levels = rev(response_order)))
  wrapped = stringr::str_wrap(levels(counts$response), width = wrap_width)
  
  height = 1.8 + sum(pmax(0.42, 0.19 * (stringr::str_count(wrapped, "\n") + 1) + 0.16))

  p = ggplot(counts, aes(x = value, y = response)) +
    geom_col(fill = academic_colours$bar, width = 0.68) +
    geom_text(aes(label = label), hjust = -0.2,
              color = "#222222") +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width)) +
    scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = function(x) {
        ticks = scales::breaks_pretty(n = 5)(x)
        ticks[ticks >= 0 & ticks <= 1]
      },
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(title = stringr::str_wrap(title, width = 62),
         subtitle = participant_subtitle(participants),
         x = "Share of respondents",
         y = NULL) +
    theme_academic()
  attr(p, "figure_height") = max(4, height) + 0.4 + title_height(stringr::str_wrap(title, 62))
  p
}

# Group comparisons: one complete response distribution per group.
grouped_ai_plot = function(data, question, group, group_order,
                            rank_responses = NULL, hierarchy_parent = NULL, fixed_order = FALSE) {
  answers = response_orders[[question]]
  plot_data = data |>
    transmute(group = as.character(.data[[group]]),
              response = as.character(.data[[question]])) |>
    filter(!is.na(group), nzchar(trimws(group)),
           !is.na(response), nzchar(trimws(response)))
  if (nrow(plot_data) == 0) return(NULL)
  all_responses = as.character(data[[question]])
  all_responses = all_responses[!is.na(all_responses) & nzchar(trimws(all_responses))]

  child_groups = character()
  if (!is.null(hierarchy_parent)) {
    child_groups = unique(as.character(data[[group]][data$discipline_1 %in% hierarchy_parent]))
    child_groups = intersect(group_order, setdiff(child_groups, hierarchy_parent))
    parent_data = data |>
      filter(discipline_1 == hierarchy_parent) |>
      transmute(group = hierarchy_parent, response = as.character(.data[[question]])) |>
      filter(!is.na(response), nzchar(trimws(response)))
    plot_data = bind_rows(plot_data, parent_data)
    group_order = unique(c(group_order, hierarchy_parent))
  }
  groups = group_order[group_order %in% plot_data$group]
  is_agreement = identical(answers, agreement_order)

  if (is_agreement && !fixed_order) {
    ranking = plot_data |>
      group_by(group) |>
      summarise(strong_agree_share = mean(response == "Strongly agree"),
                somewhat_agree_share = mean(response == "Somewhat agree"), .groups = "drop") |>
      mutate(tie_order = match(group, groups)) |>
      arrange(desc(strong_agree_share),
              if (is.null(hierarchy_parent)) desc(somewhat_agree_share) else tie_order,
              tie_order)
    groups = ranking$group
  } else if (!fixed_order && !is.null(rank_responses)) {
    ranking = plot_data |>
      group_by(group) |>
      summarise(rank_share = mean(response %in% rank_responses), .groups = "drop") |>
      mutate(tie_order = match(group, groups)) |>
      arrange(desc(rank_share), tie_order)
    groups = ranking$group
  }

  if (!is.null(hierarchy_parent) && hierarchy_parent %in% groups) {
    children = groups[groups %in% child_groups]
    parents = groups[!groups %in% child_groups]
    groups = append(parents, children, after = match(hierarchy_parent, parents))
  }
  groups = c("All respondents", groups)
  segments = bind_rows(plot_data, tibble(group = "All respondents", response = all_responses)) |>
    count(group, response) |>
    complete(group = groups, response = answers, fill = list(n = 0)) |>
    mutate(response = factor(response, levels = answers)) |>
    arrange(group, response) |>
    group_by(group) |>
    mutate(total = sum(n), proportion = n / total,
           xmax = cumsum(proportion),
           xmin = xmax - proportion) |>
    ungroup() |>
    mutate(y = match(group, rev(groups)) + if_else(group == "All respondents", 0.5, 0),
           label = stacked_percent_label(proportion))
  stopifnot(all(abs(tapply(segments$proportion, segments$group, sum) - 1) < 1e-10),
            all(segments$xmax >= segments$xmin))

  palette = response_palette(answers)

  segments$bar_alpha = ifelse(segments$group %in% child_groups, 0.55, 1)

  rgb = grDevices::col2rgb(palette[as.integer(segments$response)]) / 255
  rgb = sweep(rgb, 2, segments$bar_alpha, "*") +
    matrix(rep(1 - segments$bar_alpha, each = 3), nrow = 3)
  linear_rgb = ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  luminance = colSums(linear_rgb * c(0.2126, 0.7152, 0.0722))
  segments$text_color = ifelse((1.05 / (luminance + 0.05)) > ((luminance + 0.05) / 0.05),
                               "white", "black")
  legend_labels = legend_response_labels(question, answers)
  axis_labels = stringr::str_wrap(rev(groups), 31)
  if (!is.null(hierarchy_parent)) {
    child_rows = rev(groups) %in% child_groups
    axis_labels[child_rows] = paste0("        ", gsub("\n", "\n        ", axis_labels[child_rows], fixed = TRUE))
  }
  title = question_title(question, width = 90)
  p = ggplot(segments) +
    geom_rect(key_glyph = draw_key_square, aes(xmin = xmin, xmax = xmax, ymin = y - 0.36,
                  ymax = y + 0.36, fill = response, alpha = bar_alpha), color = "white", linewidth = 0.3) +
    geom_text(aes(x = (xmin + xmax) / 2, y = y, label = label, color = text_color),
              show.legend = FALSE) +
    scale_color_identity() +
    scale_alpha_identity(guide = "none") +
    scale_fill_manual(values = setNames(palette, answers), breaks = answers,
                      labels = legend_labels, drop = FALSE) +
    scale_y_continuous(breaks = sort(unique(segments$y)), labels = axis_labels,
                       limits = c(0.35, length(groups) + 1.25),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = scales::label_percent(accuracy = 1), oob = scales::oob_keep,
                       expand = expansion(mult = c(0, 0))) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = participant_subtitle(length(all_responses)),
         x = "Share of respondents within each group",
         y = NULL, fill = NULL) +
    theme_academic() +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(axis.text.x = element_text(margin = margin(t = 8), lineheight = 1.1),
          plot.margin = margin(16, 65, 16, 16),
          plot.title = element_text(margin = margin(b = 12)),
          plot.subtitle = ggtext::element_markdown(margin = margin(b = 6), lineheight = 1.15),
          legend.text = element_text(lineheight = 1.05))
  if (!is.null(hierarchy_parent)) {
    p = p + theme(axis.text.y = element_text(hjust = 0))
    attr(p, "indented_groups") = child_groups
  }
  # Equal row spacing must accommodate the longest wrapped group label.
  row_height = max(0.85, max(stringr::str_count(axis_labels, "\n") + 1) * 0.20 + 0.22)
  # Reserve title/legend space separately so even two-group charts have thick bars.
  attr(p, "figure_height") = max(6, 3.9 + (length(groups) + 0.5) * row_height + title_height(title) + if (is_agreement) 0.6 else 0)
  attr(p, "figure_width") = 12
  attr(p, "figure_label") = paste("grouped", group, question, sep = "-")
  p
}


# Child chunks give every plot in a loop its own device height when knitting.
render_survey_plot = function(p, name = NULL) {
  if (is.null(p)) return(invisible(NULL))
  if (!isTRUE(getOption("knitr.in.progress"))) {
    print(p)
    return(invisible(p))
  }
  height = attr(p, "figure_height")
  width = attr(p, "figure_width")
  if (is.null(width)) width = 12
  label = if (is.null(name)) attr(p, "figure_label") else name
  if (is.null(label)) label = ""
  output = knitr::knit_child(
    text = sprintf("```{r %s, echo=FALSE, fig.width=%.2f, fig.height=%.2f}\nprint(p)\n```",
                   label, width, height),
    envir = environment(), quiet = TRUE
  )
  cat(output, sep = "\n")
}


academic_title_case = function(text) {
  tools::toTitleCase(text)
}
show_survey_plot = function(p, name = NULL, kind = "standard") {
  if (is.null(p)) return(invisible(NULL))
  if (is.null(name)) name = attr(p, "figure_label")
  title = academic_title_case(gsub("\n", " ", p$labels$title, fixed = TRUE))
  title = stringr::str_wrap(title, width = 68)
  if (!is.null(p$labels$subtitle)) {
    title = paste0(title, " (", gsub("<[^>]+>", "", p$labels$subtitle), ")")
  }
  p = p + theme_academic() + labs(tag = NULL, title = title, subtitle = NULL)
  legend_rows = 0
  # Keep the shared vik colours; change only the displayed legend's capitalization.
  fill_scale = p$scales$get_scales("fill")
  if (!is.null(fill_scale) && is.character(fill_scale$labels)) {
    fill_scale = fill_scale$clone()
    fill_scale$labels = academic_title_case(fill_scale$labels)
    if (inherits(fill_scale, "ScaleDiscrete")) {
      # One row of legend entries; wrap each label to fit the manuscript width.
      fill_scale$labels = stringr::str_wrap(
        gsub("\n", " ", fill_scale$labels, fixed = TRUE), width = 14)
      legend_rows = max(stringr::str_count(fill_scale$labels, "\n") + 1) * 0.4
      p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        theme(legend.text = element_text(size = 8, lineheight = 0.95),
              legend.key.size = grid::unit(8, "pt"),
              legend.key.spacing.x = grid::unit(10, "pt"))
    }
    p$scales = p$scales$clone()
    suppressMessages(p$scales$add(fill_scale))
  }
  # Rewrap row labels for the final printed width, rather than shrinking a wide plot.
  row_labels = character()
  if (!kind %in% c("map", "country-count")) {
    y_scale = p$scales$get_scales("y")$clone()
    original_labels = y_scale$labels
    if (is.character(original_labels)) {
      row_labels = stringr::str_wrap(gsub("\n", " ", original_labels, fixed = TRUE), 24)
      y_scale$labels = row_labels
    } else if (is.function(original_labels)) {
      keys = if ("role" %in% names(p$data)) levels(p$data$role) else levels(p$data$response)
      row_labels = stringr::str_wrap(gsub("\n", " ", original_labels(keys), fixed = TRUE), 24)
      y_scale$labels = function(x) stringr::str_wrap(
        gsub("\n", " ", original_labels(x), fixed = TRUE), 24)
    }
    if (length(attr(p, "indented_groups")) > 0) {
      axis_groups = p$data |> distinct(group, y) |> arrange(y) |> pull(group)
      child_rows = axis_groups %in% attr(p, "indented_groups")
      row_labels[child_rows] = paste0("        ", gsub("\n", "\n        ", row_labels[child_rows], fixed = TRUE))
      y_scale$labels = row_labels
      p = p + theme(axis.text.y = element_text(hjust = 0))
    }
    p$scales = p$scales$clone()
    suppressMessages(p$scales$add(y_scale))
  }
  if (kind == "map") {
    p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
                  axis.title.x = element_blank(), axis.title.y = element_blank(),
                  axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                  axis.line = element_blank())
  }
  if (kind == "country-count") {
    p = p + theme(strip.text = element_blank(), panel.spacing.x = grid::unit(12, "pt"))
  }
  width = academic_page_width
  title_lines = stringr::str_count(title, "\n") + 1
  overhead = 0.9 + 0.18 * title_lines + 0.38 * legend_rows
  lines_per_row = stringr::str_count(row_labels, "\n") + 1
  height = if (kind == "map") 4.8 else
    if (kind == "country-count") overhead + 0.18 * ceiling(nrow(p$data) / 3) else
    if ("group" %in% names(p$data)) overhead +
      (n_distinct(p$data$group) + 0.5) * max(0.40, 0.145 * max(lines_per_row) + 0.08) else
    if ("role" %in% names(p$data)) overhead +
      n_distinct(p$data$role) * max(0.40, 0.145 * max(lines_per_row) + 0.08) else
      overhead + length(row_labels) * max(0.40, 0.145 * max(lines_per_row) + 0.08)
  height = min(9.2, max(3, height))
  attr(p, "figure_width") = width
  attr(p, "figure_height") = height
  render_survey_plot(p, name = paste0(name, "-academic"))
}


make_ai_roles_plot = function(role_counts, role_response_order, ai_roles, data_filtered) {
  role_palette = setNames(academic_colours$responses[c(1, 4, 5, 3)],
                           role_response_order)
  role_rgb = grDevices::col2rgb(role_palette) / 255
  role_linear = ifelse(role_rgb <= 0.04045, role_rgb / 12.92,
                       ((role_rgb + 0.055) / 1.055)^2.4)
  role_luminance = colSums(role_linear * c(0.2126, 0.7152, 0.0722))
  role_text = setNames(ifelse(role_luminance < 0.179, "white", "black"),
                       names(role_palette))
  role_counts$text_colour = unname(role_text[as.character(role_counts$response)])
  role_labels = c(ai_roles, .all_roles = "All roles")
  
  ai_roles_plot = ggplot(role_counts, aes(y = role, fill = response)) +
    geom_rect(key_glyph = draw_key_square, aes(xmin = xmin, xmax = xmax,
                  ymin = as.numeric(role) - 0.34, ymax = as.numeric(role) + 0.34),
              color = "white", linewidth = 0.4, na.rm = TRUE) +
    geom_text(aes(x = (xmin + xmax) / 2, label = label, color = text_colour),
              show.legend = FALSE, na.rm = TRUE) +
    scale_color_identity() +
    scale_fill_manual(values = role_palette, breaks = role_response_order,
                      labels = c("Not permitted", "With human supervision", "Autonomously", "Unsure"),
                      drop = FALSE) +
    scale_y_discrete(drop = FALSE,
                     labels = function(x) stringr::str_wrap(unname(role_labels[x]), width = 30)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = scales::label_percent(accuracy = 1),
                       expand = expansion(mult = c(0, 0))) +
    coord_cartesian(clip = "off") +
    labs(title = question_title("ai_roles"),
         subtitle = participant_subtitle(nrow(data_filtered |>
           filter(if_any(all_of(names(ai_roles)), ~ !is.na(.x) & nzchar(trimws(.x)))))),
  
         x = "Share of respondents within each role", y = NULL, fill = NULL) +
    theme_academic() +
    guides(fill = guide_legend(nrow = 1)) +
    theme(plot.subtitle = ggtext::element_markdown(margin = margin(b = 6)))
  ai_roles_plot
}


make_country_count_plot = function(country_bars, country_counts) {
  country_count_plot = ggplot(country_bars, aes(x = n, y = country_iso3)) +
    geom_col(fill = academic_colours$bar, width = 0.75) +
    geom_text(aes(label = n), hjust = -0.2) +
    facet_wrap(vars(column), nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 4),
                       labels = scales::label_number(accuracy = 1),
                       expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Number of respondents per country",
         subtitle = participant_subtitle(sum(country_counts$n)), x = "Number of respondents",
         y = NULL) +
    theme_academic() +
    theme(strip.text = element_blank(), panel.spacing.x = grid::unit(25, "pt"))
  attr(country_count_plot, "figure_height") = 2 + ceiling(nrow(country_bars) / 3) * 0.32
  attr(country_count_plot, "figure_width") = 12
  attr(country_count_plot, "figure_label") = "country-count-bars"
  country_count_plot
}


make_world_map = function(world_data, country_counts) {
  world_plot = ggplot(world_data) +
    geom_sf(
      aes(fill = percentage),
      color = "white",
      linewidth = 0.15
    ) +
    scale_fill_gradient(
      low = "#BFDDEB", high = "#08105D",
      limits = c(0, max(country_counts$percentage)),
      na.value = academic_colours$missing,
      labels = scales::label_percent(accuracy = 0.1),
      name = "Share of respondents"
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = "Geographical distribution of respondents",
      subtitle = participant_subtitle(sum(country_counts$n))
    ) +
    theme_academic() +
    theme(
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
      panel.grid = element_blank()
    ) +
    guides(fill = guide_colorbar(
      barwidth = grid::unit(220, "pt"),
      barheight = grid::unit(9, "pt"), title.position = "top"
    ))
  world_plot
}


