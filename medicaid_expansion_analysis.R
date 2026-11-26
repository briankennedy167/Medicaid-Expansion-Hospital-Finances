sink("pset3_log.txt", split = TRUE)

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr) 
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(fixest)


#Problem One

hosp <- read_csv("hospitals.csv", show_col_types = FALSE) %>%
  mutate(state = str_to_upper(str_trim(state)))

nonexp_2014 <- c(
  "AL","AK","FL","GA","ID","KS","LA","ME","MS","MO","MT","NE","NC",
  "OK","SC","SD","TX","UT","VA","WI","WY"
)

hosp <- hosp %>%
  mutate(
    expanded = case_when(
      state %in% nonexp_2014 ~ 0L,
      TRUE                   ~ 1L
    )
  )

setdiff(unique(hosp$state), c(state.abb, "DC"))

hosp %>%
  count(expanded) 

#Problem Two

vars_to_plot <- c("charity_care_per_bed", "revenue_per_bed", "days_per_bed")

hosp_summary <- hosp %>%
  group_by(year, expanded) %>%
  summarize(across(all_of(vars_to_plot), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Plot- Charity per Bed
ggplot(hosp_summary, aes(x = year, y = charity_care_per_bed)) +
  geom_line(aes(color = factor(expanded), linetype = factor(expanded)), linewidth = 1.1) +
  geom_vline(aes(xintercept = 2014, color = "2014 ACA Medicaid expansion", 
                 linetype = "2014 ACA Medicaid expansion"), linewidth = 1) +
  scale_color_manual(
    name = "Key",
    values = c("0" = "#C44E52", "1" = "#4C72B0", "2014 ACA Medicaid expansion" = "black"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  scale_linetype_manual(
    name = "Key",
    values = c("0" = "solid", "1" = "solid", "2014 ACA Medicaid expansion" = "dashed"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  labs(
    title = "Charity Care per Bed",
    x = "Year",
    y = "Average Charity Care per Bed"
  ) +
  theme_minimal(base_size = 13)

# Plot 2 - Revenue per Bed
ggplot(hosp_summary, aes(x = year, y = revenue_per_bed)) +
  geom_line(aes(color = factor(expanded), linetype = factor(expanded)), linewidth = 1.1) +
  geom_vline(aes(xintercept = 2014, color = "2014 ACA Medicaid expansion", 
                 linetype = "2014 ACA Medicaid expansion"), linewidth = 1) +
  scale_color_manual(
    name = "Key",
    values = c("0" = "#C44E52", "1" = "#4C72B0", "2014 ACA Medicaid expansion" = "black"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  scale_linetype_manual(
    name = "Key",
    values = c("0" = "solid", "1" = "solid", "2014 ACA Medicaid expansion" = "dashed"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  labs(
    title = "Revenue per Bed",
    x = "Year",
    y = "Average Revenue per Bed"
  ) +
  theme_minimal(base_size = 13)

# Plot 3 - Days per Bed (Quantity)
ggplot(hosp_summary, aes(x = year, y = days_per_bed)) +
  geom_line(aes(color = factor(expanded), linetype = factor(expanded)), linewidth = 1.1) +
  geom_vline(aes(xintercept = 2014, color = "2014 ACA Medicaid expansion", 
                 linetype = "2014 ACA Medicaid expansion"), linewidth = 1) +
  scale_color_manual(
    name = "Key",
    values = c("0" = "#C44E52", "1" = "#4C72B0", "2014 ACA Medicaid expansion" = "black"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  scale_linetype_manual(
    name = "Key",
    values = c("0" = "solid", "1" = "solid", "2014 ACA Medicaid expansion" = "dashed"),
    labels = c("0" = "Non-expansion states", 
               "1" = "Expansion states", 
               "2014 ACA Medicaid expansion" = "2014 ACA Medicaid expansion")
  ) +
  labs(
    title = "Days per Bed",
    x = "Year",
    y = "Average Days per Bed"
  ) +
  theme_minimal(base_size = 13)

#Problem Three

hosp <- hosp %>%
  mutate(post = as.integer(year >= 2014))

did_model <- function(var) {
  fml <- as.formula(paste(var, "~ expanded + post + expanded:post"))
  mod <- lm(fml, data = hosp)
  coeftest(mod, vcov = vcovCL, cluster = ~state)
}


did_charity <- did_model("charity_care_per_bed")
did_revenue <- did_model("revenue_per_bed")
did_days    <- did_model("days_per_bed")

# Results
did_charity
did_revenue
did_days


#Question Four

hosp <- hosp %>%
  mutate(time = year - 2006L)

run_trend <- function(yvar) {
  fml <- as.formula(paste(yvar, "~ expanded + time + expanded:time"))
  m   <- lm(fml, data = hosp)
  ct  <- coeftest(m, vcov = vcovCL, cluster = ~ state)        # state clustering
  tid <- tidy(ct)

  
  a3   <- tid$estimate[tid$term == "expanded:time"]
  se3  <- tid$std.error[tid$term == "expanded:time"]
  
  eff_2015 <- 9 * a3;  se_2015 <- 9 * se3
  eff_2019 <- 13 * a3; se_2019 <- 13 * se3
  
  list(model = m, table = tid,
       effect_2015 = c(est = eff_2015, se = se_2015),
       effect_2019 = c(est = eff_2019, se = se_2019))
}

#Charity care per bed
res_charity <- run_trend("charity_care_per_bed")
res_charity$table
res_charity$effect_2015
res_charity$effect_2019

#Revenue per bed
res_revenue <- run_trend("revenue_per_bed")
res_revenue$table
res_revenue$effect_2015
res_revenue$effect_2019

# Patient days per bed
res_days <- run_trend("days_per_bed")
res_days$table
res_days$effect_2015
res_days$effect_2019

#Problem Five

hosp <- hosp %>%
  mutate(year = factor(year))  # R automatically omits one level (2007)

run_event <- function(yvar) {
  fml <- as.formula(paste(
    yvar, "~ expanded + year + expanded:year"
  ))
  m <- lm(fml, data = hosp)
  ct <- coeftest(m, vcov = vcovCL, cluster = ~ state)
  tidy(ct)
}

ev_charity <- run_event("charity_care_per_bed")
ev_revenue <- run_event("revenue_per_bed")
ev_days    <- run_event("days_per_bed")

gamma_charity <- ev_charity %>% filter(grepl("expanded:year", term))
gamma_revenue <- ev_revenue %>% filter(grepl("expanded:year", term))
gamma_days    <- ev_days    %>% filter(grepl("expanded:year", term))

head(gamma_charity)


plot_event <- function(df, title) {
  df <- df %>%
    mutate(year = as.numeric(gsub("expanded:year", "", term)))
  
  ggplot(df, aes(x = year, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
    labs(title = title,
         x = "Year",
         y = "Estimated Effect (Yt, relative to 2007)") +
    theme_minimal(base_size = 13)
}

plot_event(gamma_charity, "Event-Study: Charity Care per Bed")
plot_event(gamma_revenue, "Event-Study: Revenue per Bed")
plot_event(gamma_days, "Event-Study: Days per Bed")

#Problem Six

hosp <- hosp %>% mutate(year = factor(year))  # 2007 will be baseline if present


m_charity <- lm(charity_care_per_bed ~ expanded + year + expanded:year, data = hosp)
m_revenue <- lm(revenue_per_bed      ~ expanded + year + expanded:year, data = hosp)
m_days    <- lm(days_per_bed         ~ expanded + year + expanded:year, data = hosp)


tidy_charity <- tidy(coeftest(m_charity, vcov = vcovCL, cluster = ~ state))
tidy_revenue <- tidy(coeftest(m_revenue, vcov = vcovCL, cluster = ~ state))
tidy_days    <- tidy(coeftest(m_days,    vcov = vcovCL, cluster = ~ state))


pre_hyps <- c(
  "expanded:year2008 = 0",
  "expanded:year2009 = 0",
  "expanded:year2010 = 0",
  "expanded:year2011 = 0",
  "expanded:year2012 = 0",
  "expanded:year2013 = 0"
)

wald_pre <- function(mod) {
  car::linearHypothesis(
    mod,
    hypothesis.matrix = pre_hyps,
    vcov. = sandwich::vcovCL(mod, cluster = ~ state),  # <-- note vcov.
    test = "F"
  )
}

pre_charity <- wald_pre(m_charity)
pre_revenue <- wald_pre(m_revenue)
pre_days    <- wald_pre(m_days)

pre_charity
pre_revenue
pre_days

# p-values:
p_charity <- pre_charity[["Pr(>F)"]][2]
p_revenue <- pre_revenue[["Pr(>F)"]][2]
p_days    <- pre_days[["Pr(>F)"]][2]
c(charity = p_charity, revenue = p_revenue, days = p_days)

#Problem Seven

did_charity_controls <- lm(
  charity_care_per_bed ~ expanded * post + forprofit + govt + psychiatric + childrens,
  data = hosp
)

summary(did_charity_controls)

did_charity_controls <- lm(
  charity_care_per_bed ~ expanded * post + forprofit + govt + psychiatric + childrens,
  data = hosp
)

summary(did_charity_controls)

coeftest(did_charity_controls, vcov = vcovCL, cluster = ~ state)

#Problem Eight
fe_did <- function(yvar, cluster_on = ~ state) {
  fml <- as.formula(paste0(yvar, " ~ post:expanded | hospital_id + year"))
  m <- feols(fml, data = hosp, cluster = cluster_on)
  list(model = m, table = broom::tidy(m))
}

# Charity care per bed
fe_charity <- fe_did("charity_care_per_bed")
fe_charity$table

# Revenue per bed
fe_revenue <- fe_did("revenue_per_bed")
fe_revenue$table

# Days per bed (quantity)
fe_days <- fe_did("days_per_bed")
fe_days$table

#Question Nine

hosp <- hosp %>% mutate(year = as.numeric(as.character(year)))


late_years <- c(AK = 2016, MT = 2016, LA = 2017, VA = 2019, ME = 2019)


state_cohorts <- hosp %>%
  distinct(state, expanded) %>%
  mutate(expand_year = dplyr::case_when(
    state %in% names(late_years) ~ unname(late_years[state]),
    expanded == 1                ~ 2014,      # 2014 adopters
    TRUE                         ~ NA_real_   # never adopters
  ))

hosp <- hosp %>%
  select(-any_of("expand_year")) %>%
  left_join(state_cohorts %>% select(state, expand_year), by = "state") %>%
  mutate(expand_year = as.numeric(expand_year),
         treat       = as.integer(!is.na(expand_year) & year >= expand_year))


print(table(hosp$treat, hosp$year))


run_staggered <- function(yvar, cluster_on = ~ state) {
  fml <- as.formula(paste0(yvar, " ~ treat | hospital_id + year"))
  feols(fml, data = hosp, cluster = cluster_on)
}

m9_charity <- run_staggered("charity_care_per_bed")
m9_revenue <- run_staggered("revenue_per_bed")
m9_days    <- run_staggered("days_per_bed")

etable(m9_charity, m9_revenue, m9_days, digits = 1,
       headers = c("Charity care / bed", "Revenue / bed", "Days / bed"))

staggered_key <- dplyr::bind_rows(
  tidy(m9_charity) %>% mutate(outcome = "Charity care / bed"),
  tidy(m9_revenue) %>% mutate(outcome = "Revenue / bed"),
  tidy(m9_days)    %>% mutate(outcome = "Days / bed")
) %>%
  filter(term == "treat") %>%
  select(outcome, estimate, std.error, p.value)

print(staggered_key)


m9_es_charity <- feols(
  charity_care_per_bed ~ sunab(expand_year, year) | hospital_id + year,
  data = hosp, cluster = ~ state
)


tid_es <- broom::tidy(m9_es_charity)


event_terms <- tid_es$term[grepl("year::", tid_es$term) & grepl("cohort::", tid_es$term)]


if (length(event_terms) > 0) {
  etable(m9_es_charity, keep = event_terms, digits = 1)
} else {
  message("No event-time coefficients found to display (after collinearity pruning).")
}


iplot(
  m9_es_charity, ref.line = 0,
  xlab = "Event time (years relative to expansion)",
  ylab = "Effect on charity care per bed",
  main = "Event-study: Medicaid expansion (charity care per bed)"
)


pre_terms <- tid_es$term[
  grepl("^year::-[0-9]+:cohort::", tid_es$term) & !is.na(tid_es$estimate)
]

if (length(pre_terms) >= 1) {

  pretrend_test <- tryCatch(
    wald(m9_es_charity, pre_terms, vcov = ~ state),
    error = function(e) e
  )
  
  cat("\nTWFE DiD estimate (charity care per bed):\n")
  print(tidy(m9_charity) %>%
          filter(term == "treat") %>%
          select(estimate, std.error, p.value))
  
  cat("\nEvent-study joint pre-trend test (all pre = 0):\n")
  if (inherits(pretrend_test, "error")) {
    cat("Wald test could not be computed (near-singular VCOV after pruning).\n")
    cat("Tip: fewer/more flexible pre bins or a different VCOV may help.\n")
  } else {
    print(pretrend_test)
  }
} else {
  cat("\nNo negative event-time coefficients were estimated (after pruning),\n",
      "so a joint pre-trend test cannot be computed.\n", sep = "")
}

sink()
