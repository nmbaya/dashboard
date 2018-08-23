#### Datasets -------
# This section lists all the datasets to be loaded in the dashboard

#### Dataset 1
# Dataset name
project_name1 <- "CMAM Caregiver"

# Read summary statistics generated
#summary_tab1 <- read.xlsx("update_files.xlsx", sheet = 2)
summary_tab1 <- paste("<strong>Title: </strong>", 
                      "Implementation Research on Linking the Community Management of Acute",
                      "Malnutrition and Integrated Community Case Management: A Stepped-Wedge",
                      "Cluster Randomized Trial in Turkana and Isiolo Counties, Kenya.",
                      '<br/>',
                      "<strong>Background: </strong>",
                      "Severely Malnourished Children have a higher risk of death from common childhood illness", 
                      "such as diarrhea, pneumonia, and malaria. Despite acute malnutrition contributing to such high", 
                      "proportion of child mortality, in many poor countries, the majority of acutely malnourished children are ", 
                      "never brought to health facilities or are brought too late, and most facility-based treatment programs do",  
                      "not reach optimal coverage. This is largely due to the distance barrier and associated costs, which limit", 
                      "reach and optimal follow-up in these areas. These challenges calls for an approach with a strong", 
                      "community component in order to reach sick and malnourished children who face barriers to accessing", 
                      "treatment. Integrated community case management (iCCM) is a strategy that utilizes community health", 
                      "volunteers (CHVs) to diagnose and treat multiple conditions, most commonly pneumonia, diarrhea and",  
                      "malaria, in children under-five years. An integrated approach to addressing the twin problem of disease", 
                      "and malnutrition would make it possible to address the presenting and underlying aspects of a child's", 
                      "illness, but new evidence on the potential impact and practical experiences on integrating community-based", 
                      "management of acute malnutrition as part of an iCCM package is not well documented.",
                      '<br/>',
                      "<strong> Objectives: </strong>", 
                      "This study therefore aims at investigating the effectiveness" ,
                      "and cost effectiveness of integrating management of acute malnutrition into iCCM.",
                      sep = "")


# Load data
caregiver <- read_dta("caregiver_labeled.dta")
caregiver <- as_factor(caregiver)
working_df1 <- setDT(caregiver)
vars <- names(working_df1)[!names(working_df1) %in% grep("access|subm|start|endt|device|subscri|simi|usern|caseid|dura",
                                                         names(working_df1), value = T)]
working_df1 <- working_df1 %>% tbl_df()
working_df1 <- working_df1[, vars]
working_df1 <- setDT(working_df1)


# Rename some key variables
setnames(working_df1, c("fi_code_new", "cg_qcounty", "cg_q1p7_auto_dup", "cg_q1p7_auto", "today"),
         c("fieldworker", "region", "duplicate_id_var", "questionnaire_id_var", "date_of_interview"))


# Dataset 2
# Dataset name
project_name2 <- "KBFI Endline"

# Read summary statistics generated
summary_tab2 <- paste("<strong>Title: </strong>", "Evidence Based Implementation of Baby-Friendly Workplace Support Initiative and Evaluation of its potential feasibility and effectiveness on improving maternal and child nutrition and health.",
                      "<br/>",
                      "<strong>Background: </strong>", "Interventions that promote breastfeeding are critical for optimal child growth, development and survival, and the wellbeing and productivity of their mothers and families, and consequently to sustainable development. ",
                      "Workplace support for breastfeeding is key to sustainable development as it has an impact on the wellbeing of the employees and their economic productivity, and the growth and development of their children through mothers practicing optimal breastfeeding and care practices. UNICEF has proposed a model workplace support for breastfeeding initiative in a tea plantation that can be scaled to other similar workplaces in Kenya and beyond. ",
                      "The initiative is aimed at building Better Business Practices for Children in order to minimize malnutrition in children following the signed commitment by Federation of Kenya Employers (FKE) and Kenya Private Sector Alliance (KEPSA) in 2010, in order to minimize malnutrition in children. The research team will conduct research in an identified tea plantation in Kericho in order to; ",
                      "<br/>",
                      "<ul>",
                      "<li>Inform the design and implementation of the model workplace support for breastfeeding initiative;</li>",
                      "<li>Determine whether the intervention works by testing its operational feasibility and effectiveness on the health and wellbeing of women working in the tea plantation and their children and the women's' productivity; and </li>",
                      "<li>assess the cost-benefit of the initiative; using a mixed methods approach.</li>",
                      "</ul>",
                      "Ultimately, it is expected that the learning from this research will guide the scaling up of the workplace support initiative in Kenya and other settings in the developing world. A key anticipated product of this project will be its contribution to the development of the guidelines for workplace support in Kenya and beyond. "
)

# Load data
kbfi <- read_dta("endline_labeled_clean.dta")
kbfi <- as_factor(kbfi)
working_df2 <- setDT(kbfi)
vars <- names(working_df2)[!names(working_df2) %in% grep("access|subm|start|endt|device|subscri|simi|usern|caseid|dura",
                                                         names(working_df2), value = T)]
working_df2 <- working_df2 %>% tbl_df()
working_df2 <- working_df2[, vars]
working_df2 <- setDT(working_df2)


# Rename some key variables
setnames(working_df2, c("fw_selection_label", "bfp_q1_3_area", "dup_mother_id", "mother_id", "today"),
         c("fieldworker", "region", "duplicate_id_var", "questionnaire_id_var", "date_of_interview"))


# Dataset 3
# Dataset name
project_name3 <- "IMLANG EGMA"

# Read summary statistics generated
summary_tab3 <- paste("<strong>Title: </strong>", "iMlango-Transitions Evaluation Project - Baseline",
                      "<br/>",
                      "<strong>Background: </strong>", "The iMlango T MEL framework defines the beneficiary group as girls enrolled in the 205 iMlango primary schools, ",
                      "which the programme estimates to be about 66,000 girls, alongside 64,000 boys also enrolled at these schools. ",
                      "The project will be seeking to ensure these children stay in school, learn more than they would otherwise without ",
                      "the intervention, and that they transition through school, both between grades and to secondary school. The project ",
                      "will also work with girls (and potentially boys) that drop out through Community Internet Hubs (CIHs).  ",
                      "<br/>",
                      "The Terms of Reference sets the overall objective of this assignment as to carry out a mixed-method, ",
                      "gender-sensitive evaluation for the iMlango Transitions Project of the Avanti Communications Limited ",
                      "over the next 4 years. The evaluation is supposed to be inclusive of persons with disabilities, assess ",
                      "the delivery, effectiveness, VfM and impact of the project and report the findings and lessons learnt ",
                      "throughout the project period.  From a thorough review of the TOR we derive three specific objectives ",
                      "of this study as follows:",
                      "<br/>",
                      "<ul>",
                      "<li>To design and implement a gender-sensitive mixed method baseline study as an integrated part of the overall MEL strategy and plan for the iMlango Transitions Project by 6th April 2018;</li>",
                      "<li>To design and conduct a gender-sensitive, mixed-methods midline evaluation that assesses effectiveness, impact and VfM of the iMlango Transition Project by 31st March 2019</li>",
                      "<li>To design and conduct a midline evaluation that will assess the effectiveness, impact and VfM of the project at endline by 31st March 2021.</li>",
                      "</ul>")

# Load data
egma <- read_dta("egmasurvey.dta")
egma <- as_factor(egma)
working_df3 <- setDT(egma)
vars <- names(working_df3)[!names(working_df3) %in% grep("access|subm|start|endt|device|subscri|simi|usern|caseid|dura",
                                                         names(working_df3), value = T)]
working_df3 <- working_df3 %>% tbl_df()
working_df3 <- working_df3[, vars]
working_df3 <- setDT(working_df3)


# Rename some key variables
setnames(working_df3, c("fieldworker", "county", "girlid_dup", "girlid", "today"),
         c("fieldworker", "region", "duplicate_id_var", "questionnaire_id_var", "date_of_interview"))



# Dataset 4
# Dataset name
project_name4 <- "PAMANECH"

# Read summary statistics generated
summary_tab4 <- paste("<strong>Title: </strong>", "Promoting sustainable health care investments in urban informal settlements: a case study of Nairobi, Kenya",
                      "<br/>",
                      "<strong>Background: </strong>", "Whereas several healthcare programs targeted at improving the livelihoods of residents of urban informal settlements in Kenya have been implemented, the extent of sustainability of such interventions is scantily documented.  In 2012 to 2016, the African Population and Health Research Center (APHRC) implemented a health systems strengthening project, Partnership for Maternal, Newborn and Child Health (PAMANECH), to determine the effect of strengthened public- private partnerships on the quality, accessibility, and affordability of maternal newborn and child services and health outcomes in informal settlements. ",
                      "Building on the public-private partnerships model and the lessons learned, this three year project aims to refine the package of interventions as the next step in the pathway to scaling up the service delivery model. ",
                      "<br/>",
                      "<strong>Objectives: </strong>", "To strengthen sustainability of healthcare investments through a public-private partnerships model, for the improvement of health care services and outcomes for mothers, neonates and children under five in Viwandani and Korogocho informal settlements",
                      "<br/>",
                      "<strong>Methods: </strong>", "Quasi-experimental design, with pre and post-intervention assessments as well as continuous process monitoring during implementation. The evaluations will assess the impact, if any, of strengthened mechanisms for sustainability on the maternal, newborn and child health services provision and population health outcomes. ",
                      "The evaluations will be conducted at baseline and at end line, employing both qualitative and quantitative methods, after at least 24 months of running the full implementation of the intervention. The findings of the surveys will be shared with key actors who include sub-County Health Management Teams (sCHMT), community leaders, the City County of Nairobi (CCN), and other local authorities with the aim of influencing policies and action around health care delivery in informal settlements in Kenya.")

# Load data
pamanech <- read_dta("WORA_labeled_Clean_24_06_2018.dta")
pamanech <- as_factor(pamanech)
working_df4 <- setDT(pamanech)
vars <- names(working_df4)[!names(working_df4) %in% grep("access|subm|start|endt|device|subscri|simi|usern|caseid|dura",
                                                         names(working_df4), value = T)]
working_df4 <- working_df4 %>% tbl_df()
working_df4 <- working_df4[, vars]
working_df4 <- setDT(working_df4)


# Rename some key variables
setnames(working_df4, c("fw_name", "p2_site", "dup_mother_id", "p2_mothers_id", "today"),
         c("fieldworker", "region", "duplicate_id_var", "questionnaire_id_var", "date_of_interview"))


# Data files to load
# List all the datasets loaded above with a bit of description as the list name
data_files <- list("CMAM Caregiver Dataset"=working_df1, "KBFI Endline Dataset"=working_df2, "Imlango - EGMA"=working_df3, "PAMANECH - Baseline"=working_df4)
project_names <- list("CMAM Caregiver Dataset"=project_name1, "KBFI Endline Dataset"=project_name2, "Imlango - EGMA"=project_name3, "PAMANECH - Baseline"=project_name4)
summary_tabs <- list("CMAM Caregiver Dataset"=summary_tab1, "KBFI Endline Dataset"=summary_tab2, "Imlango - EGMA"=summary_tab3, "PAMANECH - Baseline"=summary_tab4)



