# Analysis ----

# Libraries ----
library(crosstable)
library(magrittr)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(zoo)
library(scales)
library(RColorBrewer)
library(forcats)
library(janitor)


# Setup ----
## Load & prepare data ----
getwd()
# Set the working directory to the github repository 
# folder contrarian-discourses-against-cap-and-trade
setwd("../")
# Create a folder for the Plots
dir.create("Plots")
# Save the filepath to the folder called Plots
filepath_plots = paste0(toString(getwd()), "/Plots/")

### Labelled contrarian paragraphs ----
df <- read.csv("Data/contrarian_witnesses_03_10_utterances_witnesses_MoCs_labels_for_plotting.csv")

# Save dataframe for second labelling
# write.csv(df %>% select(id, text), 
#           "Data/CDACAT_contrarian_paragraphs_for_second_labelling.csv",
#           row.names = F)

#### Integrate final taxonomy revisions ----

# Combine L2dummy_2_4 CO2 is not rising L3dummy_2_3_6 CO2 was higher in the past
df$L3dummy_2_3_6 <- ifelse(df$L3dummy_2_3_6 + df$L2dummy_2_4 > 0, 1, 0)
df <- df %>% select(!c(L2dummy_2_4))
# Include L3dummy_2_3_6 count in new parent category L2dummy_2_3 
df$L2dummy_2_3 <- ifelse(df$L2dummy_2_3 + df$L3dummy_2_3_6 > 0, 1, 0)


# Combine L2dummy_2_5 Human CO2 emissions are miniscule with L3dummy_2_1_5 
# Human CO2 emissions are tiny compared to natural CO2 emission
df$L3dummy_2_1_5 <- ifelse(df$L3dummy_2_1_5 + df$L2dummy_2_5 > 0, 1, 0)
df <- df %>% select(!c(L2dummy_2_5))
# Include L3dummy_2_1_5 count in new parent category L2dummy_2_1 
df$L2dummy_2_1 <- ifelse(df$L2dummy_2_1 + df$L3dummy_2_1_5 > 0, 1, 0)


# Make L2dummy_2_2 It's non-greenhouse gas human climate forcings a sub-claim of 
# There's no evidence for greenhouse effect/carbon dioxide driving climate change
df <- df %>% rename("L3dummy_2_2_7" = "L2dummy_2_2")
# Update the numbering of the remaining claim 2 sub-claims
df <- df %>% rename("L2dummy_2_2" = "L2dummy_2_3",
                    "L3dummy_2_2_1" = "L3dummy_2_3_1",
                    "L3dummy_2_2_2" = "L3dummy_2_3_2",
                    "L3dummy_2_2_3" = "L3dummy_2_3_3",
                    "L3dummy_2_2_4" = "L3dummy_2_3_4",
                    "L3dummy_2_2_5" = "L3dummy_2_3_5",
                    "L3dummy_2_2_6" = "L3dummy_2_3_6")
# Include L3dummy_2_2_7 count in new parent category L2dummy_2_2 
df$L2dummy_2_2 <- ifelse(df$L2dummy_2_2 + df$L3dummy_2_2_7 > 0, 1, 0)

# Combine Climate change doesn't negatively impact the economy and Warmer 
# climates are associated with prosperity
df$L2dummy_3_7 <- ifelse(df$L2dummy_3_7 + df$L2dummy_3_8 > 0, 1, 0)
df <- df %>% select(!c(L2dummy_3_8))

# Move It's the clouds in Water vapor is the most powerful greenhouse gas
df$L3dummy_2_2_4 <- ifelse(df$L3dummy_2_2_4 + df$L3dummy_2_1_6 > 0, 1, 0)
df <- df %>% select(!c(L3dummy_2_1_6))
# Limit parent category L2dummy_2_1 to remaining sub-claims
df$L2dummy_2_1 <- df %>% select(L3dummy_2_1_1:L3dummy_2_1_5) %>% 
  mutate(L2dummy_2_1 = ifelse(rowSums(.)>0,1,0)) %>% pull(L2dummy_2_1)
# Include L3dummy_2_2_4 count in new parent category L2dummy_2_2 
df$L2dummy_2_2 <- ifelse(df$L2dummy_2_2 + df$L3dummy_2_2_4 > 0, 1, 0)

# Move 4.3.4.3 We should focus on helping other countries reduce their emissions 
# a level up in the taxonomy
df <- df %>% rename("L3dummy_4_3_6" = "L4dummy_4_3_4_3")
# Limit parent category L3dummy_4_3_4 to remaining sub-claims
df$L3dummy_4_3_4 <- df %>% select(L4dummy_4_3_4_1:L4dummy_4_3_4_2) %>%
  mutate(L3dummy_4_3_4 = ifelse(rowSums(.)>0,1,0)) %>% pull(L3dummy_4_3_4)

df %>% filter(L2dummy_3_1 == "1") %>% 
  tail()# %>% select(text)

# Integrate final labeling corrections for sub-claim 3.1
# limiting the paragraphs to explicit sensitivity claims
# Most of these say that the impacts are not bad, but only implicitly 
# state that the sensitivity is low

df[df$id %in% c(15771L, 18343L, 18641L, 33566L, 37409L,
                37455L, 38519L, 67183L, 68006L, 68013L), "L2dummy_3_1"] = 0
df[df$id %in% c(38755L, 67957L), "C0"] = 1
df[df$id %in% c(38755L, 67957L), 55:156] = 0
df[df$id %in% c(43884L), "L2dummy_3_1"] = 0
df[df$id %in% c(43884L), "L2dummy_3_4"] = 1
df[df$id %in% c(45145L), "L2dummy_3_1"] = 0
df[df$id %in% c(45145L), "L1dummy_2"] = 1
df[df$id %in% c(67407L), "L2dummy_3_1"] = 0
df[df$id %in% c(67407L), "L2dummy_3_2"] = 1
df[df$id %in% c(67407L), "L3dummy_3_2_3"] = 1

## Define claim columns ----
SuperClaims = sort(names(df)[startsWith(names(df),"L1dummy")])
SubClaims_lvl2 = sort(names(df)[startsWith(names(df),"L2dummy")])
SubClaims_lvl3 = sort(names(df)[startsWith(names(df),"L3dummy")])
SubClaims_lvl4 = sort(names(df)[startsWith(names(df),"L4dummy")])

# Save the recoded version of the data for future classifier training

congress_capandtrade_03_10 <- df %>%
  select(id, text, SuperClaims, SubClaims_lvl2, SubClaims_lvl3)

# write.csv(congress_capandtrade_03_10,
#           "Data/congress_capandtrade_03_10.csv", 
#           row.names = F)

### All witness paragraphs with wordcount >=10 with claim 4 predictions----
df4 <- read.csv("Data/inference_witnesses_03_10_utterances_witnesses_MoCs_labels_for_plotting.csv") %>%
  subset(type == "witness") %>% 
  subset(word_count >= 10)

#### Load the correction metrics ----

# Precision and recall used to correct the predictions
claim <- c("4_1", "4_2", "4_3", "4")
FFI_precision_sample <- c(0.4516129, 0.60000000, 0.35483871, 0.53333333)
FFI_precision_boosted <- c(0.4893617 , 0.6, 0.35926773, 0.44918999)
FFI_recall_sample <- c(0.77777778, 0.93750000, 0.61111111, 0.86956522)
FFI_recall_boosted <-  c(0.46938776, 0.18987342, 0.36175115, 0.44267054)
CII_precision_sample <- c(0.62962963, 0.16000000, 0.03030303, 0.32000000)
CII_precision_boosted <-  c(0.63385827, 0.17073171, 0.0407767, 0.24185464)
CII_recall_sample <- c(0.85000000, 0.66666667, 0.33333333, 1.00000000)
CII_recall_boosted <-  c(0.96987952, 0.25925926, 0.63636364, 1)
CON_precision_sample <- c(0.66666667, 0.4375, 0.53061224, 0.67326733)
CON_precision_boosted <-  c(0.66, 0.5, 0.51351351, 0.65988372)
CON_recall_sample <- c(0.84210526, 0.41176471, 0.74285714, 0.90666667)
CON_recall_boosted <-  c(0.84615385, 0.33846154, 0.57926829, 0.76430976)

PR <- data.frame(claim, 
                 FFI_precision_sample,
                 FFI_recall_sample, 
                 FFI_precision_boosted, 
                 FFI_recall_boosted,
                 CII_precision_sample, 
                 CII_recall_sample, 
                 CII_precision_boosted, 
                 CII_recall_boosted,
                 CON_precision_sample, 
                 CON_recall_sample, 
                 CON_precision_boosted, 
                 CON_recall_boosted)


## Climate legislation data----
legislation_complete <- data.frame(
  id = c("S.139", "H.R.4067", "S.843", "S.150",
         "H.R.759", "S.342", "S.1151", "S.3698",
         "S.280", "S.317", "H.R.620","S.485", 
         "H.R.1590", "S.1201", "S.1766", "S.2191",
         "H.R.4226", "S.3036", "H.R.1862", "H.R.2454", 
         "S.1733", "S.2877"),
  date = as.Date(c("01-09-2003", "03-30-2004", "04-09-2003", "01-25-2005",
                   "02-10-2005", "02-10-2005", "05-26-2005", "07-20-2006",
                   "01-12-2007", "01-17-2007", "01-22-2007", "02-01-2007",
                   "03-20-2007", "04-24-2007", "07-11-2007", "10-18-2007",
                   "11-15-2007", "05-20-2008", "04-01-2009", "05-15-2009", 
                   "09-30-2009", "12-11-2009"),
                 format = "%m-%d-%Y",  origin='01-01-1970')) %>% 
  mutate(year = lubridate::year(date),
         date_numeric = as.numeric(date))

legislation_important <- c("2003-01-09", "2003-10-30", "2005-05-26", "2005-06-22", 
                           "2007-01-12", "2007-10-18", "2008-06-06", "2009-06-26", 
                           "2010-07-22") %>% as.Date() %>% as.numeric()

## Time variable setup ----

halfyears <-
  data.frame(
    date = seq(as.Date("2003-01-01"), by = "day",
               length.out = as.Date("2011-01-02") - as.Date("2003-01-01"))) %>%
  mutate(term = floor_date(date, "halfyear"))

halfyears_labels <-
  halfyears %>% 
  select(!date) %>%
  unique() %>%
  mutate(month = format(as.Date(term), "%b"),
         year = format(as.Date(term), "%Y"),
         halfyears_labels = ifelse(month == "Jan", year, ""))

quarters <-
  data.frame(
    date = seq(as.Date("2003-01-01"), by = "day",
               length.out = as.Date("2011-01-02") - as.Date("2003-01-01"))) %>%
  mutate(quarter = floor_date(date, "quarter"))

quarters_labels <-
  quarters %>% 
  select(!date) %>%
  unique() %>%
  mutate(month = format(as.Date(quarter), "%b"),
         year = format(as.Date(quarter), "%Y"),
         quarters_labels = ifelse(month == "Jan", year, ""))


df <- df %>%
  mutate(term = halfyears$term[match(as.Date(date), halfyears$date)],
         quarter = quarters$quarter[match(as.Date(date), quarters$date)])

df4 <- df4 %>%
  mutate(term = halfyears$term[match(as.Date(date), halfyears$date)],
         quarter = quarters$quarter[match(as.Date(date), quarters$date)])

#### Subset CON, FFI, & CII ----

FFI <- df4 %>%
  filter(witness_category == "Fossil Fuel Industry" & 
           witness_contrarian != "Contrarian")

CII <- df4 %>%
  filter(witness_category == "Carbon-intensive Industry" & 
           witness_contrarian != "Contrarian")

CON_labelled <- df4 %>%
  filter(witness_contrarian == "Contrarian" &
           batch != "")

CON_predicted <- df4 %>%
  filter(witness_contrarian == "Contrarian" &
           batch == "")

## Labelled FFI and CII validation samples ----

FFI_val <-  read.csv("Data/codacat_validation_samples_ffi_labelled.csv")
CII_val <-  read.csv("Data/codacat_validation_samples_cii_labelled.csv")


# Part 1: Overview ----

## Table 1: Hearings overview ---- 

overview_table <-
  df4 %>% 
  mutate(witness_con = ifelse(witness_contrarian == "Contrarian", 1, 0),
         witness_ffi = ifelse(witness_con==0 &
                                witness_category=="Fossil Fuel Industry", 1, 0),
         witness_cii = ifelse(witness_con==0 & witness_category=="Carbon-intensive Industry", 1, 0)) %>% 
  group_by(date, hearing_id, title, witness, witness_con, witness_ffi, witness_cii) %>% 
  summarise(witness_paragraphs = n()) %>% 
  group_by(date, title) %>% 
  summarise(witnesses = n_distinct(witness),
            witness_con = sum(witness_con),
            witness_ffi = sum(witness_ffi),
            witness_cii = sum(witness_cii),
            paragraphs = sum(witness_paragraphs))


# Proportion of hearings containing any claim / no claim
round(prop.table(table(df$C0)), 2)

overview_table %>% xtable::xtable(digits=c(0,0,0,0,0,0,0,0))

overview_table %>% 
  ungroup() %>% 
  select(witnesses:paragraphs) %>% 
  colSums()

# Part 2: Contrarian claims (labelled) ----

## Plot A1: Super-claims per hearing  ----

claims_per_hearings <- 
  left_join(
    df4 %>%
      group_by(date, title, hearing_id) %>%
      summarise(TotalParagraphs = n()) %>%
      ungroup() %>% 
      arrange(desc(date)),
    df %>% 
      group_by(date, title) %>% 
      summarise(across(L1dummy_1:L1dummy_5, ~sum(.)))) %>% 
  replace(is.na(.), 0) %>% 
  mutate(L1dummy_SUM = rowSums(select(., contains("L1dummy_")))) %>% 
  pivot_longer(cols = starts_with("L1dummy_"),
               names_to = "claim",
               values_to = "count",
               names_prefix = "L1dummy_") %>% 
  mutate(count_cat = cut(count, 
                         breaks = c(-1, 0, 5, 10, 20, 30, 40, 50, 60, 70, 80),
                         labels = c("0", "1-5", "6-10", "11-20",
                                    "21-30", "31-40", "41-50", 
                                    "51-60", "61-70", "71-80")),
         per_paragraphs = count/TotalParagraphs*100,
         per_cat = cut(per_paragraphs, 
                       breaks = c(-1, 0, 5, 10, 15, 20, 25, 30, 35),
                       labels = c("0", "1-5", "6-10", "11-15", "16-20",
                                  "21-25", "26-30", "31-35")),
         title_date = paste(title, date),
         label = ifelse(count == 0, NA, count))

PA1 <-
  claims_per_hearings %>% 
  ggplot(aes(x = claim, y = fct_inorder(title_date), 
             fill = per_cat, label = label)) +
  geom_tile(alpha = .8) +
  geom_text(alpha = .7, size = 3) +
  scale_fill_manual(values = c("white", brewer.pal(name = "YlOrRd", 7))) +
  labs(x = "Super-claim",
       fill = "Proportion of\nwitness\nparagraphs\ncontaining\na claim (%)", 
       y = element_blank()) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Export ----

ggsave(
  filename = "HearingsClaimsProportion.pdf",
  plot = PA1,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 210,
  height = 255,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

## Plot 1: Super-claims over time and sub-claims by type of contrarian  ----

# Claim labels, colors, and shapes
superclaim_labels = c("1. It's not happening",
                      "2. It's not us",
                      "3. It's not bad",
                      "4. Solutions won't work",
                      "5. Science is unreliable")

superclaim_colors = c("#fe6100","#ffb000","#785ef0","#dc267f","#648fff")
superclaim_shapes = c(21, 22, 23, 24, 25)

### 1a) Super-claims abs/rel ----

# Any claim over time: absolute and proportion of all paragraphs

SuperClaims_over_time_abs_rel_term <- merge(
  df4 %>% 
    group_by(term) %>% 
    summarise(term_paragraphs = n()),
  df %>% 
    group_by(term) %>% 
    summarise(term_claims = sum(abs(C0-1)))) %>%
  complete(term = halfyears$term) %>%
  mutate(per = term_claims/term_paragraphs*100) %>% 
  replace(is.na(.), 0)

scale_value <- 17.5

P1a <-
  SuperClaims_over_time_abs_rel_term %>% 
  select(!term_paragraphs) %>% 
  mutate(per = per * scale_value) %>%
  ggplot(aes(x = term + 90)) +
  geom_bar(aes(y = term_claims), stat = "identity", fill = "lightgray") +
  geom_text(aes(y = term_claims, label = term_claims), vjust = -0.5) +
  geom_line(aes(y=per), alpha = .7) +
  scale_x_continuous(name = element_blank(),
                     breaks = halfyears_labels$term,
                     limits = c(min(halfyears$term), max(halfyears$term)),
                     labels = halfyears_labels$halfyears_labels,
                     expand = c(0.005,0.005)) +
  scale_y_continuous(
    name = "Super-claim count", 
    limits = c(0, 390),
    breaks = pretty_breaks(6),
    expand = c(0.02,0.02),
    position = "right",
    sec.axis = sec_axis(~ . / scale_value, 
                        name = "Proportion of paragraphs",
                        breaks = pretty_breaks(7),
                        labels = label_percent(scale = 1))) +
  theme_classic() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)),
        axis.text.y.right=element_blank(),
        axis.ticks.y.right=element_blank(),
        axis.title.y.right = element_blank())

### 1b) Super-claim prevalence ----

SuperClaims_over_time_per_witness <- df %>%
  select("term", "hearing_id", "last_name", SuperClaims) %>% 
  group_by(term, hearing_id, last_name) %>% 
  mutate_at(vars(starts_with("L1dummy")), list(~sum(.))) %>%
  ungroup() %>%
  mutate(ClaimTotal = rowSums(select(., starts_with("L1dummy")))) %>% 
  group_by(term, hearing_id, last_name) %>% 
  mutate(utterance_count = n()) %>%
  ungroup() %>% 
  arrange(term, hearing_id, last_name) %>% 
  distinct() %>%
  setNames(gsub("L1dummy_", "Claim", names(.))) %>% 
  mutate_at(vars(starts_with("Claim")), funs("per" = ./utterance_count*100))

SuperClaims_over_time <-
  SuperClaims_over_time_per_witness %>% 
  select(!utterance_count:ClaimTotal_per) %>% 
  pivot_longer(cols = starts_with("C"),
               names_to = "claim",
               values_to = "claim_count") %>% 
  group_by(term, claim) %>% 
  summarise(claim_count = sum(claim_count)) %>% 
  filter(claim != "ClaimTotal") %>% 
  group_by(term) %>% 
  mutate(claim_total = sum(claim_count)) %>% 
  ungroup() %>% 
  mutate(claim_prevalence = claim_count/claim_total,
         low = ifelse(term <= "2005-01-01", 1, 0),
         high = ifelse(term >= "2005-01-01", 1, 0))

P1b <-
  ggplot(SuperClaims_over_time,
         aes(x = term + 90, y = claim_prevalence, color = claim, shape = claim)) +
  geom_line(data = filter(SuperClaims_over_time, low == 1), size=1, alpha = .7) +
  geom_line(data = filter(SuperClaims_over_time, high == 1), size=1,  alpha = .7) +
  geom_point(data = filter(SuperClaims_over_time, low == 1), size=3,  alpha = .9) + 
  geom_point(data = filter(SuperClaims_over_time, high == 1), size=3,  alpha = .9) +
  scale_x_continuous(name = "Half-year",
                     breaks = halfyears_labels$term,
                     limits = c(min(halfyears$term), max(halfyears$term)),
                     labels = halfyears_labels$halfyears_labels,
                     expand = c(0.005,0.005)) +
  scale_y_continuous(name = "Proportion of claims",
                     breaks = pretty_breaks(n=10),
                     labels = label_percent(scale=100),
                     expand = c(0.02,0.02))+
  scale_color_manual(values = superclaim_colors,
                     labels = superclaim_labels) +
  scale_shape_manual(values = superclaim_shapes,
                     labels = superclaim_labels) +
  scale_alpha_manual(values = c(.9, 0.5), guide = FALSE) +
  labs(color = "Super-claim", shape = "Super-claim") +
  theme_classic() +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(.1, "cm"),
        legend.spacing.y = unit(.0, "cm"),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)))

ggarrange(P1a, P1b, nrow = 2,
          heights = c(1, 1.2),
          align = "v")

### 1c) Sub-claim prevalence ----

SubClaims_2_over_time_per_witness <- df %>%
  select("congress", "hearing_id", "last_name", "witness_denialist", SubClaims_lvl2) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate_at(vars(starts_with("L2dummy")), list(~sum(.))) %>%
  ungroup() %>%
  mutate(ClaimTotal = rowSums(select(., starts_with("L2dummy")))) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate(utterance_count = n()) %>%
  ungroup() %>% 
  arrange(congress, hearing_id, last_name) %>% 
  distinct() %>%
  setNames(gsub("L2dummy_", "Claim", names(.))) %>% 
  mutate_at(vars(starts_with("Claim")), funs("per" = ./utterance_count*100))

SubClaims_2_totals_and_per <-
  merge(SubClaims_2_over_time_per_witness %>% 
          select(Claim1_1:Claim5_3) %>% 
          mutate(group_var = "x") %>%
          group_by(group_var) %>% 
          summarise(across(everything(), sum)) %>% 
          pivot_longer(cols = starts_with("C"),
                       names_to = "claim",
                       values_to = "claim_count") %>% 
          select(claim, claim_count) %>% 
          mutate(claim_per = claim_count/sum(claim_count)*100),
        SubClaims_2_over_time_per_witness %>% 
          select(witness_denialist:Claim5_3) %>% 
          group_by(witness_denialist) %>% 
          summarise(across(everything(), sum)) %>% 
          pivot_longer(cols = starts_with("C"),
                       names_to = "claim",
                       values_to = "claim_count") %>% 
          select(claim, witness_denialist, claim_count) %>% 
          group_by(witness_denialist) %>% 
          mutate(claim_per = claim_count/sum(claim_count)*100,
                 witness_denialist = ifelse(witness_denialist == "Denialist", 
                                            "denialist", "other")) %>% 
          ungroup() %>% 
          pivot_wider(names_from = witness_denialist,
                      values_from = c(claim_count, claim_per)),
        by = "claim") %>% 
  mutate(subclaim = str_replace_all(claim, "Claim", ""),
         subclaim = str_replace_all(subclaim, "_", "."))

subclaim_order <- 
  SubClaims_2_totals_and_per %>% 
  select(!starts_with("claim_count")) %>%
  arrange(claim_per) %$% subclaim

subclaim_labels <- 
  c("Ice isn't melting", 
    "Extremes aren't increasing",
    "Only positive health impacts",
    "Heading into ice-age",
    "No species impact",
    "Climate change is conspiracy",
    "CO2 is not a pollutant",
    "Only positive economic impacts",
    "Only a few degrees", 
    "Sensitivity is low",
    "Climate friendly alternatives won't work",
    "It's natural cycles",
    "No evidence for Greenhouse Effect",
    "Climate policy is too difficult",
    "We need fossil fuels",
    "Policies are unnecessary",
    "Climate science is unreliable",
    "Climate policies are ineffective",
    "Climate movement is unreliable",
    "Climate policies are harmful")

SubClaims_2_per <- 
  SubClaims_2_totals_and_per %>% 
  select(!starts_with("claim_count")) %>%
  arrange(desc(claim_per)) %>% 
  pivot_longer(cols = starts_with("claim_per"),
               names_to = "type",
               values_to = "claim_per") %>% 
  mutate(type = ifelse(type == "claim_per", "TOTAL", 
                       ifelse(type == "claim_per_denialist",
                              "Denialist", "Other contrarian")),
         subclaim = factor(subclaim, levels = subclaim_order),
         superclaim = substr(claim,1,6)) %>% 
  head(60)

P1c <-
  SubClaims_2_per %>% 
  mutate(claim_per = ifelse(claim_per == 0, NA, claim_per)) %>% 
  filter(type != "TOTAL") %>% 
  ggplot(aes(x = claim_per, y = subclaim, 
             shape = type, color = superclaim)) + 
  geom_point(size = 1.5, stroke = 1, alpha = 0.7) +
  labs(x = "Proportion of claims", y = element_blank(),
       shape = "Witness type", color = "Super-claim") +
  scale_color_manual(labels = superclaim_labels, 
                     values = superclaim_colors) +
  scale_shape_manual(values = c(3, 8)) + 
  scale_y_discrete(position = "right",
                   labels = subclaim_labels) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  theme(legend.position="top") +
  theme_bw() +
  guides(shape = guide_legend(nrow = 2, byrow = TRUE),
         color = "none") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.spacing.x = unit(.1, "cm"),
        legend.spacing.y = unit(.0, "cm"),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "gray", fill=NA, 
                                    linewidth=1))

### Export ----

P1b_legend = as_ggplot(get_legend(P1b, position = "bottom"))
P1c_legend = as_ggplot(get_legend(P1c, position = "bottom"))

P1 <- 
  ggarrange(
    ggarrange(
      ggarrange(P1a, P1b, nrow = 2,
                heights = c(1, 1),
                align = "v",
                legend = "none"),
      P1c,
      widths = c(1, 1),
      legend = "none"),
    ggarrange(NULL, P1b_legend, NULL, P1c_legend, NULL,
              nrow = 1),
    nrow = 2,
    heights = c(1, .1)); P1

ggsave(
  filename = "Contrarian_claims_overview_horizontal.pdf",
  plot = P1,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 190,
  height = 130,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

## Sub-claims ----

### Level 3 sub-claims ----

#### Counts per witness -----
SubClaims_3_over_time_per_witness <- df %>%
  select("congress", "hearing_id", "last_name", "witness_denialist", SubClaims_lvl3) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate_at(vars(starts_with("L3dummy")), list(~sum(.))) %>%
  ungroup() %>%
  mutate(ClaimTotal = rowSums(select(., starts_with("L3dummy")))) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate(utterance_count = n()) %>%
  ungroup() %>% 
  arrange(congress, hearing_id, last_name) %>% 
  distinct() %>%
  setNames(gsub("L3dummy_", "Claim", names(.))) %>% 
  mutate_at(vars(starts_with("Claim")), funs("per" = ./utterance_count*100))

##### Check for notable trends ----

#### Counts per sub-claim -----
SubClaims_3_totals_and_per <- 
  SubClaims_3_over_time_per_witness %>% 
  select(Claim1_1_1:Claim5_3_2) %>% 
  mutate(group_var = "x") %>%
  group_by(group_var) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(cols = starts_with("C"),
               names_to = "level2subclaim",
               values_to = "level2subclaim_count") %>% 
  select(!group_var) %>% 
  mutate(subclaim = substr(level2subclaim, 6, 8),
         claim = substr(level2subclaim, 6, 6)) %>% 
  group_by(subclaim) %>% 
  mutate(per_subclaim = level2subclaim_count/sum(level2subclaim_count)*100)

### Level 4 sub-claims ----

#### Counts per witness -----
SubClaims_4_over_time_per_witness <- df %>%
  select("congress", "hearing_id", "last_name", "witness_denialist", SubClaims_lvl4) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate_at(vars(starts_with("L4dummy")), list(~sum(.))) %>%
  ungroup() %>%
  mutate(ClaimTotal = rowSums(select(., starts_with("L4dummy")))) %>% 
  group_by(congress, hearing_id, last_name, witness_denialist) %>% 
  mutate(utterance_count = n()) %>%
  ungroup() %>% 
  arrange(congress, hearing_id, last_name) %>% 
  distinct() %>%
  setNames(gsub("L4dummy", "Claim", names(.))) %>% 
  mutate_at(vars(starts_with("Claim")), funs("per" = ./utterance_count*100))

#### Counts per sub-claim -----
SubClaims_4_totals_and_per <- 
  SubClaims_4_over_time_per_witness %>% 
  select(Claim_4_1_1_1:Claim_4_5_2_2) %>% 
  mutate(group_var = "x") %>%
  group_by(group_var) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(cols = starts_with("C"),
               names_to = "level3subclaim",
               values_to = "level3subclaim_count") %>%
  select(!group_var) %>% 
  mutate(level2subclaim = substr(level3subclaim, 7, 11)) %>% 
  group_by(level2subclaim) %>% 
  mutate(per_level2subclaim = level3subclaim_count/sum(level3subclaim_count)*100)

### Inspect specific sub-claims ----

# Level 2
SubClaims_2_totals_and_per %>% 
  filter(substring(subclaim, 1, 1) == 2)

# Level 3
SubClaims_3_totals_and_per %>% 
  filter(subclaim == "2_2")

# Level 4
SubClaims_4_totals_and_per %>% 
  filter(level2subclaim == "4_5_1")

### Extract text for specific sub-claims ----

df %>% filter(L3dummy_4_2_1 == "1") %>% select(text)

df %>% filter(L4dummy_4_5_2_1 == "1") %>% select(text)

## Descriptives over time ----

### Hearings ----

### Number of hearings per congress
df4 %>% select(congress, year, hearing_id) %>% 
  distinct() %>% 
  group_by(congress) %>% 
  summarise(n())

### Number of hearings per halfyear
df4 %>% select(congress, term, hearing_id) %>% 
  distinct() %>% 
  group_by(congress, term) %>% 
  summarise(n())

### Super-claims ----

#### Super-claim totals ----
SuperClaims_over_time %>% select(claim, claim_count) %>%
  group_by(claim) %>% 
  summarise(claim_count = sum(claim_count)) %>% 
  mutate(claim_per = claim_count/sum(claim_count)*100)

#### Super-claims over time ----
SuperClaims_over_time_abs_rel_term

#### Super-claim counts by claim over time ----
SuperClaims_over_time %>% select(term, claim, claim_count) %>%
  pivot_wider(names_from = "claim", values_from = claim_count)

#### Super-claim proportions by claim over time ----
SuperClaims_over_time %>% select(term, claim, claim_prevalence) %>%
  mutate(claim_prevalence = round(claim_prevalence*100,2)) %>% 
  pivot_wider(names_from = "claim", values_from = claim_prevalence)

#### Super-claim in specific hearings / timeframes ----

CONGRESS = 109
# Number of claims made by each contrarian witness during a specific congress
df %>% 
  filter(congress == CONGRESS) %>% 
  group_by(year, title, witness) %>% 
  summarise(N_paragraphs = n(),
            N_claims = sum(abs(C0-1))) %>% 
  print(n=50)

# Number of super-claims by claim made per term in a specific congress
df %>% 
  filter(congress == CONGRESS) %>%
  group_by(term) %>% 
  summarise(across(L1dummy_1:L1dummy_5, ~sum(.)))

# Number of super-claims by claim made per hearing in a specific congress
df %>% 
  filter(congress == CONGRESS) %>%
  group_by(date, title) %>% 
  summarise(across(L1dummy_1:L1dummy_5, ~sum(.))) %>% 
  print(n=50)

# Number of policy claims by sub-claim made per term in a specific congress
df %>% 
  filter(congress == CONGRESS) %>% 
  group_by(term) %>% 
  summarise(across(L2dummy_4_1:L2dummy_4_7, ~sum(.)))

### Witnesses ----

CONGRESS = 109

#### Halfyearly counts/proportions for a specific congress  ----

##### By contrarian status and witness category  ----
for (i in 1:4) {
  terms = df4 %>% filter(congress == CONGRESS) %$% unique(term)
  print(terms[i])
  tab = df4 %>% 
    filter(term ==  terms[i]) %>%
    select("date", "witness_category", "witness_contrarian", "witness") %>% 
    distinct() %>% 
    select(witness_category, witness_contrarian) %>% 
    group_by(witness_category, witness_contrarian) %>% 
    table()
  tab = rbind(tab, margin.table(tab, margin = 2))
  print("COUNTS")
  print(tab)
  prop_tab = prop.table(tab, margin = 1) %>%
    round(2) %>% ftable()
  print("PROPORTIONS")
  print(prop_tab)
} 

##### By denialist status and witness category  ----
for (i in 1:4) {
  terms = df4 %>% filter(congress == CONGRESS) %$% unique(term)
  print(terms[i])
  tab = df4 %>% 
    filter(term ==  terms[i]) %>%
    select("date", "witness_category", "witness_denialist", "witness") %>% 
    distinct() %>% 
    select(witness_category, witness_denialist) %>% 
    group_by(witness_category, witness_denialist) %>% 
    table()
  tab = rbind(tab, margin.table(tab, margin = 2))
  print("COUNTS")
  print(tab)
  prop_tab = prop.table(tab, margin = 1) %>%
    round(2) %>% ftable()
  print("PROPORTIONS")
  print(prop_tab)
} 

#### Count per hearing for a specific congress  ----

##### By contrarian status ----
df4 %>% filter(congress == CONGRESS) %>%
  select(date, hearing_id, title, committee_short, witness, witness_contrarian) %>% 
  distinct() %>% 
  group_by(date, hearing_id, title, committee_short, witness_contrarian) %>% 
  summarise(witness_count = n()) %>% 
  pivot_wider(names_from = witness_contrarian, values_from = witness_count) %>% 
  janitor::clean_names() %>% 
  replace(is.na(.), 0) %>% 
  mutate(prop_contrarian = contrarian/(contrarian+other_witness)) %>% 
  print(n=50)

##### By contrarian status and witness category ----
for (i in 1:4) {
  terms = df4 %>% filter(congress == CONGRESS) %$% unique(term)
  df4 %>% 
    filter(term == terms[i]) %>%
    select("date", "hearing_id", "witness_category", "witness_contrarian", "witness") %>% 
    distinct() %>% 
    select(hearing_id, date, witness_category, witness_contrarian) %>% 
    group_by(hearing_id, date, witness_category, witness_contrarian) %>% 
    table()  %>% addmargins() %>%
    ftable() %>% 
    print()
} 

#### Witnesses at a specific hearing ----
df4 %>% filter(hearing_id == "110shrg88902") %>%
  select(hearing_id, witness, witness_denialist) %>% distinct()

#### Contrarian access ----
##### Per congress ----
df4 %>% 
  select(congress, date, year, term, hearing_id, witness, witness_contrarian) %>% 
  distinct() %>% 
  group_by(congress, date, year, term, hearing_id, witness_contrarian) %>% 
  summarise(witness_count = n()) %>% 
  pivot_wider(names_from = witness_contrarian, values_from = witness_count) %>% 
  janitor::clean_names() %>% 
  replace(is.na(.), 0) %>% 
  mutate(any_contrarians = ifelse(contrarian == 0, "no", "yes")) %>% 
  group_by(congress, year, term, any_contrarians) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = any_contrarians, values_from = count,
              names_prefix = "any_contrarians_") %>% 
  mutate(prop_no_contrarians = any_contrarians_no/(any_contrarians_no+any_contrarians_yes),
         prop_any_contrarians = 1 - prop_no_contrarians)

##### Per halfyear ----
df4 %>% 
  select(congress, year, term, date, hearing_id, witness, witness_contrarian) %>% 
  distinct() %>% 
  group_by(congress, year, term, date, hearing_id, witness_contrarian) %>% 
  summarise(witness_count = n()) %>% 
  pivot_wider(names_from = witness_contrarian, values_from = witness_count) %>% 
  janitor::clean_names() %>% 
  replace(is.na(.), 0) %>% 
  mutate(any_contrarians = ifelse(contrarian == 0, "no", "yes")) %>% 
  group_by(congress, year, any_contrarians) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = any_contrarians, values_from = count,
              names_prefix = "any_contrarians_") %>% 
  replace(is.na(.), 0) %>% 
  mutate(prop_no_contrarians = any_contrarians_no/(any_contrarians_no+any_contrarians_yes),
         prop_any_contrarians = 1 - prop_no_contrarians)

## Multi-claim paragraphs ----

### Extract paragraphs with at least x lvl 1 subclaims
df %>%
  select("congress", "hearing_id", "text", "last_name", "witness_denialist", SuperClaims, SubClaims_lvl2, SubClaims_lvl3) %>% 
  mutate(subclaims = rowSums(across(SubClaims_lvl2))) %>% 
  filter(subclaims >= 3) %>%
  select(last_name, witness_denialist, SuperClaims, text, SubClaims_lvl3)


df %>% 
  filter(term == "2009-07-01") %>%
  select("date", "hearing_id", "title", SubClaims_lvl2) %>% 
  group_by(date, hearing_id, title) %>% 
  summarise(across(L2dummy_5_1:L2dummy_5_3, ~sum(.)))

df %>% 
  filter(L2dummy_3_1==1) %>%
  select("date", "hearing_id", "text")


df %>% 
  filter(term >= "2009-07-01") %>%
  select("date", "hearing_id", "title", "L1dummy_1", "L1dummy_2",
         "L1dummy_3", "L1dummy_4", "L1dummy_5") %>% 
  group_by(date, hearing_id, title) %>% 
  summarise(across(L1dummy_1:L1dummy_5, ~sum(.)))

df4 %>% 
  filter(term == "2006-07-01") %>%
  select("date", "hearing_id", "witness_category", "witness_contrarian", "witness") %>% 
  distinct() %>% 
  select(date, witness_category, witness_contrarian) %>% 
  group_by(date, witness_category, witness_contrarian) %>% 
  table()  %>% addmargins() %>% ftable()

## Hearings containing a specific keyword ----
df4[grep("climate.{0,3}gate", tolower(df4$text)),] %>% 
  select("date", "hearing_id", "title") %>% 
  group_by(date, hearing_id, title) %>% 
  summarise(occurrence = n())

# Part 3: CON, FFI, & CII policy sub-claims (predicted) ----

## Data preparation ----

### Quarterly ----

#### CON ----

#### CON_labelled

CON_labelled_quarter <- CON_labelled %>% 
  group_by(quarter) %>% 
  mutate(total_labelled = n()) %>% 
  group_by(quarter, total_labelled) %>% 
  summarise_at(vars(label_4:label_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = label_4:label_4_3, 
               names_to = "Claim",
               values_to = "count__raw_labelled", 
               names_prefix = "label_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>%
  ungroup() %>% 
  complete(quarter = quarters$quarter, Claim = Claim) %>%   
  replace(is.na(.), 0)

#### CON_predicted
CON_predicted_quarter <- 
  CON_predicted %>% 
  group_by(quarter) %>% 
  mutate(total = n()) %>% 
  group_by(quarter, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(quarter = quarters$quarter, Claim = Claim) %>% 
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CON_precision_sample[1]/PR$CON_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$CON_precision_sample[2]/PR$CON_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$CON_precision_sample[3]/PR$CON_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$CON_precision_sample[4]/PR$CON_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CON_precision_boosted[1]/PR$CON_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$CON_precision_boosted[2]/PR$CON_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$CON_precision_boosted[3]/PR$CON_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$CON_precision_boosted[4]/PR$CON_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)

#### CON_combined
CON_combined_quarter <- merge(
  CON_predicted_quarter %>% 
    select(!starts_with("proportion")),
  CON_labelled_quarter, by = c("quarter", "Claim")) %>% 
  mutate(count__raw = 
           count__raw + count__raw_labelled,
         count__corr_sample = 
           count__corr_sample + count__raw_labelled,
         count__corr_boosted = 
           count__corr_boosted + count__raw_labelled,
         total = total + total_labelled) %>% 
  select(!ends_with("_labelled")) %>% 
  mutate(proportion__raw = 
           count__raw/total*100,
         proportion__corr_sample = 
           count__corr_sample/total*100,
         proportion__corr_boosted = 
           count__corr_boosted/total*100) %>% 
  relocate(total, .after = proportion__corr_boosted) %>% 
  replace(is.na(.), 0)

CON_combined_quarter_long <- CON_combined_quarter %>% 
  pivot_longer(!c(quarter, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>%
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)")))

CON_combined_quarter_boosted <- CON_combined_quarter_long %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  group_by(Claim) %>% 
  mutate(moving_avg = rollmean(count, k=2, fill=NA, align='center'),
         moving_avg = ifelse(moving_avg < 0, 0, moving_avg),
         moving_avg_per = rollmean(proportion, k=2, fill=NA, align='center'),
         moving_avg_per = ifelse(moving_avg_per < 0, 0, moving_avg_per)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(quarter = quarter)

#### FFI ----

FFI_claims_quarter <-
  FFI %>% group_by(quarter) %>% 
  mutate(total = n()) %>% 
  group_by(quarter, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(quarter = quarters$quarter, Claim = Claim) %>% 
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$FFI_precision_sample[1]/PR$FFI_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$FFI_precision_sample[2]/PR$FFI_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$FFI_precision_sample[3]/PR$FFI_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$FFI_precision_sample[4]/PR$FFI_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$FFI_precision_boosted[1]/PR$FFI_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$FFI_precision_boosted[2]/PR$FFI_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$FFI_precision_boosted[3]/PR$FFI_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$FFI_precision_boosted[4]/PR$FFI_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)

FFI_claims_quarter_long <- FFI_claims_quarter %>% 
  pivot_longer(!c(quarter, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>% 
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)"))) %>% 
  data.frame() 

FFI_claims_quarter_boosted <- FFI_claims_quarter_long %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  group_by(Claim) %>% 
  mutate(moving_avg = rollmean(count, k=2, fill=NA, align='center'),
         moving_avg = ifelse(moving_avg < 0, 0, moving_avg),
         moving_avg_per = rollmean(proportion, k=2, fill=NA, align='center'),
         moving_avg_per = ifelse(moving_avg_per < 0, 0, moving_avg_per)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(quarter = quarter)

#### CII ----

CII_claims_quarter <-
  CII %>% group_by(quarter) %>% 
  mutate(total = n()) %>% 
  group_by(quarter, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(quarter = quarters$quarter, Claim = Claim) %>% 
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CII_precision_sample[1]/PR$CII_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$CII_precision_sample[2]/PR$CII_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$CII_precision_sample[3]/PR$CII_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$CII_precision_sample[4]/PR$CII_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CII_precision_boosted[1]/PR$CII_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$CII_precision_boosted[2]/PR$CII_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$CII_precision_boosted[3]/PR$CII_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$CII_precision_boosted[4]/PR$CII_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)


CII_claims_quarter_long <- CII_claims_quarter %>% 
  pivot_longer(!c(quarter, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>% 
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)"))) %>% 
  data.frame() 

CII_claims_quarter_boosted <- CII_claims_quarter_long %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  group_by(Claim) %>% 
  mutate(moving_avg = rollmean(count, k=2, fill=NA, align='center'),
         moving_avg = ifelse(moving_avg < 0, 0, moving_avg),
         moving_avg_per = rollmean(proportion, k=2, fill=NA, align='center'),
         moving_avg_per = ifelse(moving_avg_per < 0, 0, moving_avg_per)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(quarter = quarter)

#### COMbined (boosted) ----

COM_claims_quarter_boosted <-
  rbind(FFI_claims_quarter_boosted %>% 
          mutate(type = "Fossil fuel industry (FFI)    "),
        CII_claims_quarter_boosted %>% 
          mutate(type = "Carbon-intensive industry (CII)    "),
        CON_combined_quarter_boosted %>% 
          mutate(type = "Contrarians    ")) %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  select(!c(proportion, total, moving_avg, moving_avg_per, Predictions)) %>% 
  filter(Claim == "4") %>% 
  relocate(type, .after = Claim) %>% 
  group_by(quarter, Claim) %>%
  group_modify(~ adorn_totals(.x, where = "row")) %$% 
  merge(.,
        df4 %>%
          filter(type =="witness") %>%
          group_by(quarter) %>%
          summarise(total = n()),
        by = "quarter",
        all.x = T) %>%
  replace(is.na(.), 0) %>% 
  mutate(proportion = count/total*100,
         type = factor(type, 
                       levels = c("Contrarians    ", 
                                  "Fossil fuel industry (FFI)    ", 
                                  "Carbon-intensive industry (CII)    ", 
                                  "Total"))) %>% 
  replace(is.na(.), 0) %>% 
  group_by(type) %>% 
  mutate(moving_avg = rollmean(count, k=2, fill=NA, align='center'),
         moving_avg = ifelse(moving_avg < 0, 0, moving_avg),
         moving_avg_per = rollmean(proportion, k=3, fill=NA, align='center'),
         moving_avg_per = ifelse(moving_avg_per < 0, 0, moving_avg_per))

### Yearly ----

#### CON ----

#### CON_labelled
CON_labelled_year <- CON_labelled %>% 
  group_by(year) %>% 
  mutate(total_labelled = n()) %>% 
  group_by(year, total_labelled) %>% 
  summarise_at(vars(label_4:label_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = label_4:label_4_3, 
               names_to = "Claim",
               values_to = "count__raw_labelled", 
               names_prefix = "label_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4")))

#### CON_predicted
CON_predicted_year <- 
  CON_predicted %>% 
  group_by(year) %>% 
  mutate(total = n()) %>% 
  group_by(year, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(year = unique(df$year), Claim = Claim) %>%
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CON_precision_sample[1]/PR$CON_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$CON_precision_sample[2]/PR$CON_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$CON_precision_sample[3]/PR$CON_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$CON_precision_sample[4]/PR$CON_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CON_precision_boosted[1]/PR$CON_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$CON_precision_boosted[2]/PR$CON_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$CON_precision_boosted[3]/PR$CON_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$CON_precision_boosted[4]/PR$CON_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)

# CON_predicted_year_long
CON_predicted_year_long <- CON_predicted_year %>% 
  pivot_longer(!c(year, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>% 
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)"))) %>% 
  data.frame() 

#### CON_combined
CON_combined_year <- merge(
  CON_predicted_year %>% 
    select(!starts_with("proportion")),
  CON_labelled_year, by = c("year", "Claim")) %>% 
  mutate(count__raw = 
           count__raw + count__raw_labelled,
         count__corr_sample = 
           count__corr_sample + count__raw_labelled,
         count__corr_boosted = 
           count__corr_boosted + count__raw_labelled,
         total = total + total_labelled) %>% 
  select(!ends_with("_labelled")) %>% 
  mutate(proportion__raw = 
           count__raw/total*100,
         proportion__corr_sample = 
           count__corr_sample/total*100,
         proportion__corr_boosted = 
           count__corr_boosted/total*100) %>% 
  relocate(total, .after = proportion__corr_boosted) %>% 
  replace(is.na(.), 0)

CON_combined_year_long <- CON_combined_year %>% 
  pivot_longer(!c(year, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>%
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)")))

#### FFI ----

FFI_claims_year <-
  FFI %>% group_by(year) %>% 
  mutate(total = n()) %>% 
  group_by(year, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(year = unique(df$year), Claim = Claim) %>%
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$FFI_precision_sample[1]/PR$FFI_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$FFI_precision_sample[2]/PR$FFI_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$FFI_precision_sample[3]/PR$FFI_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$FFI_precision_sample[4]/PR$FFI_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$FFI_precision_boosted[1]/PR$FFI_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$FFI_precision_boosted[2]/PR$FFI_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$FFI_precision_boosted[3]/PR$FFI_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$FFI_precision_boosted[4]/PR$FFI_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)

FFI_claims_year_long <- FFI_claims_year %>% 
  pivot_longer(!c(year, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>% 
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)"))) %>% 
  data.frame() 

#### CII ----

CII_claims_year <-
  CII %>% group_by(year) %>% 
  mutate(total = n()) %>% 
  group_by(year, total) %>% 
  summarise_at(vars(pred_4:pred_4_3), list(~sum(.))) %>% 
  pivot_longer(cols = pred_4:pred_4_3, 
               names_to = "Claim",
               values_to = "count__raw", 
               names_prefix = "pred_") %>% 
  mutate(Claim = factor(str_replace(Claim, "_", "."),
                        levels = c("4.1", "4.2", "4.3", "4"))) %>% 
  ungroup() %>% 
  complete(year = unique(df$year), Claim = Claim) %>%
  mutate(proportion__raw = count__raw/total*100,
         count__corr_sample = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CII_precision_sample[1]/PR$CII_recall_sample[1],
           Claim == "4.2" ~ 
             count__raw*PR$CII_precision_sample[2]/PR$CII_recall_sample[2],
           Claim == "4.3" ~ 
             count__raw*PR$CII_precision_sample[3]/PR$CII_recall_sample[3],
           Claim == "4" ~ 
             count__raw*PR$CII_precision_sample[4]/PR$CII_recall_sample[4]),
         count__corr_boosted = case_when(
           Claim == "4.1" ~ 
             count__raw*PR$CII_precision_boosted[1]/PR$CII_recall_boosted[1],
           Claim == "4.2" ~ 
             count__raw*PR$CII_precision_boosted[2]/PR$CII_recall_boosted[2],
           Claim == "4.3" ~ 
             count__raw*PR$CII_precision_boosted[3]/PR$CII_recall_boosted[3],
           Claim == "4" ~ 
             count__raw*PR$CII_precision_boosted[4]/PR$CII_recall_boosted[4]),
         proportion__corr_sample = count__corr_sample/total*100,
         proportion__corr_boosted = count__corr_boosted/total*100) %>% 
  replace(is.na(.), 0)


CII_claims_year_long <- CII_claims_year %>% 
  pivot_longer(!c(year, Claim, total), 
               names_to = c(".value", "Predictions"), 
               names_sep="__") %>% 
  mutate(Predictions = ifelse(Predictions == "corr_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "corr_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)"))) %>% 
  data.frame() 

COM_claims_year_boosted <- COM_claims_quarter_boosted %>% 
  mutate(year = floor_date(quarter, "year")) %>%
  group_by(year) %>%
  mutate(total = sum(total)/4) %>%
  ungroup() %>%
  group_by(year, type, total) %>% 
  summarise(count = sum(count),
            proportion = count/total*100)

## Plot 5: Predicted policy claims ----

# Setup
colors = c("#FF0000", "#E7298A", "#7570B3",  "darkgray")

Plot_COMBINED_quarterly <-
  ggarrange(
    ggplot(COM_claims_quarter_boosted, aes(x = quarter, y = moving_avg, 
                                           color = type, linetype = type)) +
      geom_line(size = .8) +
      scale_color_manual(values = colors) +
      scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "solid")) +
      scale_x_continuous(breaks = quarters_labels$quarter,
                         labels = quarters_labels$quarters_labels,
                         limits = c(min(quarters_labels$quarter),
                                    max(quarters_labels$quarter)),
                         expand = c(0.01,0)) +
      scale_y_continuous(limits = c(0, 300),
                         breaks = scales::pretty_breaks(n=6)) +
      labs(x = "   Time", y = "N",
           color = "Witness category      ", linetype = "Witness category      ") +
      theme_classic() +
      guides(linetype = guide_legend(override.aes = list(size = 11))) +
      theme(axis.title.x = element_blank()),
    ggplot(COM_claims_quarter_boosted, aes(x = quarter, y = moving_avg_per, 
                                           color = type, linetype = type)) +
      geom_line(size = .8) +
      scale_color_manual(values = colors) +
      scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "solid")) +
      scale_x_continuous(breaks = quarters_labels$quarter,
                         labels = quarters_labels$quarters_labels,
                         limits = c(min(quarters_labels$quarter),
                                    max(quarters_labels$quarter)),
                         expand = c(0.01,0)) +
      scale_y_continuous(limits = c(0, 10),
                         breaks = scales::pretty_breaks(n=8)) +
      labs(x = "   Time", 
           y = "%",
           color = "Witness category      ", linetype = "Witness category      ") +
      theme_classic() +
      guides(linetype = guide_legend(override.aes = list(size = 11))),
    common.legend = T,
    legend = "bottom",
    nrow = 2,
    align = "hv"); Plot_COMBINED_quarterly

#### Export ----

ggsave(
  filename = "Quarterly_rolling_avg_k2_COMBINED.pdf",
  plot = Plot_COMBINED_quarterly,
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


## Plot 6: Predicted policy (sub-)claims bar-chart ----

# Setup
claim_labels = c("Climate policies are harmful",
                 "Climate policies are ineffective",
                 "Climate policy opposition for other reasons", 
                 "Any solutions-contrarianism claim") # Any claim 4
claim_colors = c("#44AA99","#DDCC77","#882255", "#A9A9A9")

barchart_claim_4 <- rbind(CON_combined_quarter %>% 
                            group_by(Claim) %>% 
                            summarise(count = sum(count__corr_boosted),
                                      total = sum(total),
                                      per = count/total, 
                                      type = "Contrarians"),
                          FFI_claims_quarter %>% 
                            group_by(Claim) %>% 
                            summarise(count = sum(count__corr_boosted),
                                      total = sum(total),
                                      per = count/total,
                                      type = "FFI"),
                          CII_claims_quarter %>% 
                            group_by(Claim) %>% 
                            summarise(count = sum(count__corr_boosted),
                                      total = sum(total),
                                      per = count/total,
                                      type = "CII")) %>% 
  mutate(type = factor(type, levels = c("Contrarians", "FFI", "CII")),
         labels = percent(per, accuracy =.1)) %>% 
  ggplot(aes(x = Claim, y = count, fill = Claim, label = labels)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -0.5) +
  scale_x_discrete(labels = c(4.1, 4.2, "4.3+", 4)) +
  scale_fill_manual(labels = claim_labels,
                    values = claim_colors) +
  scale_y_continuous(name = "Claim count",
                     breaks = pretty_breaks(),
                     limits = c(0,1050)) +
  facet_wrap(vars(type)) +
  theme_minimal() +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(.1, "cm"),
        legend.spacing.y = unit(.0, "cm"),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)))

ggsave(
  filename = "Barchart_claim_4.pdf",
  plot = barchart_claim_4,
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 70,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


## Plot A3: Predicted policy (sub-)claims over time ----

# Setup
claim_labels = c("Climate policies are harmful (Sub-claim 4.1)",
                 "Climate policies are ineffective (Sub-claim 4.2)",
                 "Climate policy opposition for other reasons (Sub-claim 4.3+)", 
                 "Climate solutions won't work/aren't needed (Super-claim 4)") # Any claim 4

claim_colors = c("#44AA99","#DDCC77","#882255", "#A9A9A9")

### Counts quarterly rolling ----

#### CON ----
Plot_CON_combined_C_quarterly_roll <-
  CON_combined_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "N") +
  theme_classic()

#### FFI ----
Plot_FFI_C_quarterly_roll <-
  FFI_claims_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "N") +
  theme_classic()

#### CII ----
Plot_CII_C_quarterly_roll <-
  CII_claims_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "N") +
  theme_classic()

### Proportion quarterly rolling ----

#### CON ----
Plot_CON_combined_P_quarterly_roll <-
  CON_combined_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg_per, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "%") +
  theme_classic()

#### FFI ----
Plot_FFI_P_quarterly_roll <-
  FFI_claims_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg_per, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "%") +
  theme_classic()

#### CII ----
Plot_CII_P_quarterly_roll <-
  CII_claims_quarter_boosted %>% 
  filter(Predictions == "corrected (boosted)") %>% 
  ggplot(aes(x = quarter + 45)) +
  geom_line(aes(y = moving_avg_per, color = Claim), size = .8) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_x_continuous(breaks = quarters_labels$quarter,
                     labels = quarters_labels$quarters_labels,
                     limits = c(min(quarters_labels$quarter),
                                max(quarters_labels$quarter)),
                     expand = c(0.01,0)) +
  labs(x = "   Time", y = "%") +
  theme_classic()


#### Prepare quarterly rolling plots for export ----

legend =  as_ggplot(get_legend(Plot_CON_combined_C_quarterly_roll, 
                               position = "bottom"))

### Fix the y-axis
ggarrange(
  Quarterly_rolling_avg_k2_CON <- ggarrange(Plot_CON_combined_C_quarterly_roll +
                                              scale_y_continuous(limits = c(0,163),
                                                                 breaks = scales::breaks_pretty(n = 8)) +
                                              theme(axis.title.x = element_blank(),
                                                    plot.margin=unit(c(0,0,-.05,0), "cm")),
                                            Plot_CON_combined_P_quarterly_roll +
                                              scale_y_continuous(limits = c(0,60),
                                                                 breaks = scales::breaks_pretty(n = 6)),
                                            align = "v",
                                            nrow = 2,
                                            common.legend = TRUE,
                                            legend = F),
  
  Quarterly_rolling_avg_k2_FFI <- ggarrange(Plot_FFI_C_quarterly_roll +
                                              scale_y_continuous(limits = c(0,93),
                                                                 breaks = scales::breaks_pretty(n = 8)) +
                                              theme(axis.title.x = element_blank(),
                                                    plot.margin=unit(c(0,0,-.05,0), "cm"),
                                                    axis.title.y = element_text(margin = margin(0,8,0,0))),
                                            Plot_FFI_P_quarterly_roll +
                                              scale_y_continuous(limits = c(0,50),
                                                                 breaks = scales::breaks_pretty(n = 6)),
                                            align = "v",
                                            nrow = 2,
                                            common.legend = TRUE,
                                            legend = F),
  
  Quarterly_rolling_avg_k2_CII <- ggarrange(Plot_CII_C_quarterly_roll +
                                              scale_y_continuous(limits = c(0,34),
                                                                 breaks = scales::breaks_pretty(n = 4)) +
                                              theme(axis.title.x = element_blank(),
                                                    plot.margin=unit(c(0,0,-.05,0), "cm"),
                                                    axis.title.y = element_text(margin = margin(0,8,0,0))),
                                            Plot_CII_P_quarterly_roll +
                                              scale_y_continuous(limits = c(0, 15),
                                                                 breaks = scales::breaks_pretty(n = 3)),
                                            align = "v",
                                            nrow = 2,
                                            common.legend = TRUE,
                                            legend = F),
  ncol = 1)

#### Export ----

ggsave(
  filename = "Quarterly_rolling_avg_k2_legend.pdf",
  plot = as_ggplot(get_legend(Plot_CON_combined_C_quarterly_roll +
                                guides(color=guide_legend(nrow=4)),
                              position = "bottom")),
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 20,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Quarterly_rolling_avg_k2_CON.pdf",
  plot = Quarterly_rolling_avg_k2_CON,
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Quarterly_rolling_avg_k2_FFI.pdf",
  plot = Quarterly_rolling_avg_k2_FFI,
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Quarterly_rolling_avg_k2_CII.pdf",
  plot = Quarterly_rolling_avg_k2_CII,
  device = "pdf",
  path = filepath_plots,
  scale = 1.4,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

### Inspect the claims and sub-claims in detail ----

COM_claims_quarter_boosted %>% group_by(type) %>% 
  summarise(count = sum(count),
            proportion = count/(sum(total))*100)

CON_combined_quarter_boosted %>% group_by(Claim) %>% 
  summarise(count = sum(count),
            proportion = count/(sum(total))*100)

FFI_claims_quarter_boosted %>% group_by(Claim) %>% 
  summarise(count = sum(count),
            proportion = count/(sum(total))*100)

CII_claims_quarter_boosted %>% group_by(Claim) %>% 
  summarise(count = sum(count),
            proportion = count/(sum(total))*100)


FFI_val %>% 
  filter(label_4_3==1)


## Plot A2: Prediction corrections ----

Plot_CON_appendix <-
  CON_predicted_year_long %>% 
  mutate(corrected = ifelse(Predictions=="raw",
                            "Raw predictions",
                            "Corrected predictions"),
         corrected = factor(corrected,
                            levels = c("Raw predictions",
                                       "Corrected predictions"))) %>% 
  ggplot(aes(x = year, count)) +
  geom_line(aes(color = Claim,
                linetype = Predictions,
                alpha = Predictions),
            size = .6) +
  facet_wrap(vars(corrected)) +
  geom_ribbon(data = CON_predicted_year %>% 
                mutate(corrected = "Corrected predictions", 
                       corrected = factor(corrected,
                                          levels = c("Raw predictions",
                                                     "Corrected predictions"))),
              aes(x = year, fill = Claim,
                  ymin=count__corr_sample,
                  ymax=count__corr_boosted),
              alpha=0.3,
              inherit.aes = FALSE,
              show.legend = T) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_alpha_manual(values = c(.9,1,1)) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_fill_manual(labels = claim_labels, 
                    values = claim_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8)) + 
  scale_y_continuous(limits = c(0, 200)) +
  guides(linetype = guide_legend(override.aes = list(fill = "#FFFFFF"))) +
  labs(x = "Year", y = "Paragraphs (N)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

Plot_FFI_appendix <-
  FFI_claims_year_long %>% 
  mutate(corrected = ifelse(Predictions=="raw",
                            "Raw predictions",
                            "Corrected predictions"),
         corrected = factor(corrected,
                            levels = c("Raw predictions",
                                       "Corrected predictions"))) %>% 
  ggplot(aes(x = year, count)) +
  geom_line(aes(color = Claim,
                linetype = Predictions,
                alpha = Predictions),
            size = .6) +
  facet_wrap(vars(corrected)) +
  geom_ribbon(data = FFI_claims_year %>% 
                mutate(corrected = "Corrected predictions", 
                       corrected = factor(corrected,
                                          levels = c("Raw predictions",
                                                     "Corrected predictions"))),
              aes(x = year, fill = Claim,
                  ymin=count__corr_sample,
                  ymax=count__corr_boosted),
              alpha=0.3,
              inherit.aes = FALSE,
              show.legend = T) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_alpha_manual(values = c(.9,1,1)) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_fill_manual(labels = claim_labels, 
                    values = claim_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8)) + 
  scale_y_continuous(limits = c(0, 310)) +
  guides(linetype = guide_legend(override.aes = list(fill = "#FFFFFF"))) +
  labs(x = "Year", y = "Paragraphs (N)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


Plot_CII_appendix <-
  CII_claims_year_long %>% 
  mutate(corrected = ifelse(Predictions=="raw",
                            "Raw predictions",
                            "Corrected predictions"),
         corrected = factor(corrected,
                            levels = c("Raw predictions",
                                       "Corrected predictions"))) %>% 
  ggplot(aes(x = year, count)) +
  geom_line(aes(color = Claim,
                linetype = Predictions,
                alpha = Predictions),
            size = .6) +
  facet_wrap(vars(corrected)) +
  geom_ribbon(data = CII_claims_year %>% 
                mutate(corrected = "Corrected predictions", 
                       corrected = factor(corrected,
                                          levels = c("Raw predictions",
                                                     "Corrected predictions"))),
              aes(x = year, fill = Claim,
                  ymin=count__corr_sample,
                  ymax=count__corr_boosted),
              alpha=0.3,
              inherit.aes = FALSE,
              show.legend = T) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_alpha_manual(values = c(.9,1,1)) +
  scale_color_manual(labels = claim_labels,
                     values = claim_colors) +
  scale_fill_manual(labels = claim_labels, 
                    values = claim_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8)) + 
  scale_y_continuous(limits = c(0, 400)) +
  guides(linetype = guide_legend(override.aes = list(fill = "#FFFFFF"))) +
  labs(x = "Year", y = "Paragraphs (N)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


#### Export ----

Appendix_legend <- as_ggplot(get_legend(Plot_FFI_appendix,
                                        position = "bottom"))

Plot_FFI_appendix <- ggarrange(Plot_FFI_appendix, 
                               legend = "none"); Plot_FFI_appendix
Plot_CII_appendix <- ggarrange(Plot_CII_appendix, 
                               legend = "none"); Plot_CII_appendix
Plot_CON_appendix <- ggarrange(Plot_CON_appendix,
                               legend = "none"); Plot_CON_appendix

ggsave(
  filename = "Predictions_4_legend_appendix.pdf",
  plot = Appendix_legend,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 190,
  height = 5,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Predictions_4_FFI_year_appendix.pdf",
  plot = Plot_FFI_appendix,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Predictions_4_CII_year_appendix.pdf",
  plot = Plot_CII_appendix,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

ggsave(
  filename = "Predictions_4_CON_year_appendix.pdf",
  plot = Plot_CON_appendix,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 190,
  height = 66,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


# Part 4: Strategy examination ----

## Plot 4: Arcdiagram of sub-claim co-occurrence ----

# install.packages("devtools")
# library(devtools)
# devtools::install_github('gastonstat/arcdiagram')
library(arcdiagram)
library(igraph)
library(ggraph)

# Setup
claim_labels = c("Ice isn't melting",
                 "Heading into ice age",
                 "Weather is cold",
                 "Hiatus in warming",
                 "Sea level rise is exaggerated",
                 "Extremes not increasing",
                 "Changed the name",
                 "It's natural cycles",
                 "No evidence for Greenhouse Effect",
                 "Sensitivity is low",
                 "No species impact",
                 "CO2 is not a pollutant",
                 "Only a few degrees",
                 "No link to conflict",
                 "Only positive health impacts",
                 "Only positive economic impacts ",
                 "Climate policies are harmful",
                 "Climate policies are ineffective",
                 "Policies are unnecessary/secondary",
                 "Climate policy is too difficult",
                 "Climate-friendly alternatives won't work",
                 "We need fossil fuels",
                 "Science is unreliable",
                 "Movement is unreliable",
                 "Climate is conspiracy") %>% rev()

# Extract the sub-claim level data
data_raw <- df %>% 
  filter(C0 == 0) %>% 
  select(SubClaims_lvl2) %>% 
  select(!c(L2dummy_1_5, L2dummy_4_7)) %>% # Drop sub-claims with 0 occurrence
  rev()

# Get the weighted adjacency matrix and extract the node size
data <- crossprod(as.matrix(data_raw))
attributes <- data.frame(name=names(diag(data)),
                         n=diag(data),
                         row.names=NULL) %>% 
  mutate(grp = as.factor(substr(name, 9, 9)), 
         name = factor(name, name))
# Drop self-cooccurrence
diag(data) <- 0
# Wrangle the data into the right format
data_long <- as.data.frame(data) %>% 
  mutate(from = row.names(data)) %>%
  relocate(from) %>%
  gather(key="to", value="weights", -1) %>%
  mutate(to = gsub("\\.", " ",to)) %>%
  na.omit() %>% 
  mutate(adjacency = ifelse(weights > 0, 1, 0))
# Inspect co-occurrences
table(data_long$weights)
# Drop all edges that represent fewer than 3 co-occurences
data_plot <- data_long %>% 
  filter(weights >= 3)
# Create the plot
g <- graph_from_data_frame(data_plot,
                           vertices = attributes,
                           directed = FALSE)
# # Re-order the claims
# order <- c(SubClaims[!startsWith(SubClaims_lvl2, "L2dummy_4")],
#            SubClaims[startsWith(SubClaims_lvl2, "L2dummy_4")])
# order <- order[!order %in% c("L2dummy_1_5", "L2dummy_2_3", "L2dummy_4_7")]
# g <- permute(g, match(V(g)$name, order))

subclaim_cooccurrence <-
  g %>%
  ggraph(layout="linear", circular = F) + 
  geom_node_point(aes(y = -.15, size=n,
                      color=grp, fill=grp), 
                  alpha=0.7) +
  geom_edge_arc(aes(width = weights, 
                    alpha = weights),
                strength = .8,
                lineend = "square", 
                show.legend = NA) +
  labs(color = "Super-claim", fill = "Super-claim",
       size = "Super-claim count") +
  scale_edge_width(range = c(.5, 1), 
                   name = "Sub-claim co-occurence",
                   breaks = c(5, 20, 40, 60)) + 
  scale_edge_alpha(range = c(.05, 1.2),
                   name = "Sub-claim co-occurence",
                   breaks = c(5, 20, 40, 60)) +
  scale_size_continuous(range=c(0.01,5.5)) +
  scale_color_manual(values=superclaim_colors, 
                     labels = superclaim_labels) +
  scale_fill_manual(values=superclaim_colors, 
                    labels = superclaim_labels) +
  geom_node_text(aes(label=claim_labels),
                 angle=0,
                 hjust=1,
                 nudge_y = -.9,
                 size=2.3) +
  theme_void() +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         size = guide_legend(order = 2)) +
  theme(legend.position="right",
        plot.margin=unit(c(0,.1,0,0), "null"),
        panel.spacing=unit(c(0,0,0,0), "null"), 
        aspect.ratio=10/8,
        legend.title=element_text(size=7), 
        legend.text=element_text(size=7)) +
  expand_limits(y = c(-12, 0)) +
  coord_flip(); subclaim_cooccurrence

### Export ----
ggsave(
  filename = "Subclaim-cooccurrence.pdf",
  plot = subclaim_cooccurrence,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 110,
  height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

# Plot 5: Proportion of solutions contrarianism in contrarian testimonies ----

## Data preparation ----

strategy <- df %>% 
  mutate(
    claim_1 = select(., starts_with("L2dummy_1")) %>% rowSums(),
    claim_2 = select(., starts_with("L2dummy_2")) %>% rowSums(),
    claim_3 = select(., starts_with("L2dummy_3")) %>% rowSums(),
    claim_4 = select(., starts_with("L2dummy_4")) %>% rowSums(),
    claim_5 = select(., starts_with("L2dummy_5")) %>% rowSums()
  ) %>% group_by(hearing_id, witness, witness_denialist) %>% 
  summarise(claim_1 = sum(claim_1),
            claim_2 = sum(claim_2), 
            claim_3 = sum(claim_3),
            claim_4 = sum(claim_4),
            claim_5 = sum(claim_5),
            claim_not4 = claim_1+claim_2+claim_3+claim_5,
            paragraphs = n()) %>%
  ungroup() %>% 
  mutate(top_claim = colnames(.[, (4:8)])[max.col(.[, (4:8)])],
         total_claims = rowSums(.[, (4:8)]),
         top_claim = ifelse(total_claims == 0, NA, top_claim),
         core_strategy = case_when(
           claim_not4 == 0 & claim_4 == 0 ~ "No claim",
           claim_not4 == 0 & claim_4 != 0 ~ "Exclusively solutions contrarianism",
           claim_not4 != 0 & claim_4 == 0 ~ "Exclusively other contrarianism",
           claim_not4 != 0 & claim_4 != 0 ~ "Mixed strategy"),
         core_strategy_binary = ifelse(claim_4>claim_not4,
                                       "Solutions contrarianism",
                                       "Other contrarianism"),
         claim_ratio = claim_4/(claim_4+claim_not4)*100)


## Inspect core strategy -----
data.frame(table(strategy$core_strategy)) %>% 
  mutate(Prop = round(Freq/sum(Freq)*100, 2))

# 7 contrarian witnesses exclusively relied on other claims (~7%)
# 46 contrarian witnesses exclusively relied on claim 4 (~52%)
# 33 contrarian witnesses relied on both types of claims (~38%)
# 3 contrarian witnesses didn't put forward any claim (~3%)

# Inspect core strategy binary
data.frame(table(strategy$core_strategy_binary)) %>% 
  mutate(Prop = round(Freq/sum(Freq)*100, 2))

# Inspect both
data.frame(table(strategy$core_strategy, strategy$core_strategy_binary)) %>% 
  mutate(Prop = round(Freq/sum(Freq)*100, 2))

strategy %>% select(starts_with("claim")) %>% colSums()

strategy %>% filter(core_strategy != "No claim") %$% quantile(claim_ratio, .29)

strategy %>% filter(core_strategy == "Mixed strategy") %>% arrange(claim_ratio) %>% 
  print(n=60)


strategy_plot <- 
  strategy %>% 
  # filter(core_strategy != "No claim") %>%
  ggplot(aes(x = 0, y = claim_ratio)) +
  geom_violin() +
  geom_rug(position = position_jitter(height = .5),
           sides = "r", alpha = 1, size = .2, length = unit(0.7,"cm")) +
  scale_x_continuous(limits = c(-0.5, 0.75)) +
  scale_y_continuous(breaks = pretty_breaks(7),
                     labels = label_percent(scale = 1)) +
  labs(y = "Proportion of solutions-contrarianism sub-claims") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

strategy %>% 
  # filter(core_strategy != "No claim") %>%
  ggplot(aes(x = witness_denialist, y = claim_ratio)) +
  geom_violin() +
  geom_rug(position = position_jitter(height = .5),
           sides = "r", alpha = 1, size = .2, length = unit(0.7,"cm")) +
  scale_y_continuous(breaks = pretty_breaks(7),
                     labels = label_percent(scale = 1)) +
  labs(y = "Proportion of solutions-contrarianism sub-claims") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

### Export ----
ggsave(
  filename = "Strategy.pdf",
  plot = strategy_plot,
  device = "pdf",
  path = filepath_plots,
  scale = 1.5,
  width = 70,
  height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


