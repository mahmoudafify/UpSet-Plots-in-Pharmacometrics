library(dplyr)
library(readxl)
library(ComplexUpset)
library(ggplot2)
library(tidyverse)

##########################

## Oncology example: The dataset is confidential but we shared its code below. 
##########################
data <- read_excel("unilateral risk features.xlsx", 
                   sheet = "IMPROTANT VAR - surger(NO)")
hr_neoadj <-  filter(data, data$`Type of enucleation`=="enucleation after neoadjuvant chemo")

hr_neo <-  select (hr_neoadj,- hr_per_protocol,- MRN,- surgery1,-`Type of enucleation`,- hr_per_protocol, - `Extraocular metastasis`)

hr_neo <- as.data.frame(hr_neo)
# hr_neo <- hr_neo %>% select(-Others)

selected_columns2 <- 
  colnames(hr_neo %>% select (-neoadj))
upsetplot2 <- upset(
  hr_neo, selected_columns2, name='Pathology Risk Features', min_size=1,
  encode_sets=FALSE,  # for annotate() to select the set by name disable encoding
  matrix=(
    intersection_matrix(
      geom=geom_point(
        shape='square',
        size=3.5
      ),
      segment=geom_segment(
        linetype='dotted'
      ),
      outline_color=list(
        active='darkorange3',
        inactive='grey70'
      )
    )
    + scale_y_discrete(
      position='right'
    )
  ),
  set_sizes=(
    upset_set_size(
      # position='right'
    )
    + scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), trans=reverse_log_trans() )
    
    + geom_text(aes(label=..count..), hjust=1.1, stat='count',size=4)
    # you can also add annotations on top of bars:
    + expand_limits(y=50)
    + theme(axis.text.x=element_text(angle=90))
    
  ),
  sort_sets="ascending",
  queries=list(
    upset_query(
      intersect=c('Bone metastatsis', 'Recieved adjuvant chemotherapy','Massive choroidal invasion', 'Others'),
      color='red',
      fill='red',
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Death', 'CNS metastasis','Sclera invasion','Optic nerve prelaminar invasion','Massive choroidal invasion', 'Others'),
      color='red',
      fill='red',
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Orbit metastatsis', 'Others', 'Sclera invasion','Massive choroidal invasion'),
      color='red',
      fill='red',
      only_components=c('intersections_matrix', 'Intersection size')),

    upset_query(
      intersect=c('Free from features', 'Second malignancy'),
      color='red',
      fill='red',
      only_components=c('intersections_matrix', 'Intersection size')),

    upset_query(
      intersect=c('Positive optic nerve margin', 'Post laminar invasion'),
      color='red',
      fill='red',
      only_components=c('intersections_matrix', 'Intersection size'))
  )
  )



upsetplot2

##############

## Diagnosis example: the dataset is attached on the Github.

##############

symptoms <- c("Anosmia", "Cough", "Fatigue", "Diarrhea", "Breath", "Fever")
names(symptoms) <- symptoms




dat <- readxl::read_xlsx("symptoms.xlsx") 
dat %>% print(n = nrow(dat))
subsets <- dat$combination



## Check if each subset mentions each symptom or not
symptom_mat <- map_dfc(subsets, str_detect, symptoms) %>%
  data.frame() %>%
  t() %>% # transpose the result, ugh
  as_tibble()



colnames(symptom_mat)  <- symptoms



symptom_mat$count <- dat$count



symptom_mat %>% print(n = nrow(symptom_mat))
indvs <- symptom_mat %>%
  uncount(count) 



indvs

# Calculate row sums
row_sums <- rowSums(indvs)


# Add row sums as a new variable to the data frame
indvs <- cbind(indvs, RowSums = row_sums) 

nrows <- indvs %>% nrow()


# adding hypothetical variable (age) based on the number of of events
age_old <- rnorm(1764, 70, 10)
age_new <- rnorm(1764, 30, 10)

indvs$age_old <- age_old
indvs$age_new <- age_new

indvs <-  indvs %>% mutate(
  age = if_else(RowSums < 3, age_new, age_old)
) %>% select(- RowSums, - age_old, -age_new)


# Display the updated data frame
print(indvs) 



library(ComplexUpset)
library(officer)





size = get_size_mode('exclusive_intersection')

isize = upset_annotate('age',geom_boxplot(na.rm=TRUE))
isize$geom = c(
  geom_boxplot(na.rm=TRUE, alpha=0.3),
  scale_y_continuous(name="Age (years)"))

plot <- upset(data = indvs, intersect = symptoms, 
              annotations = list('age'=isize),
              name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.", 
              min_size = 0,
              width_ratio = 0.2,
              height_ratio = 0.8,
              matrix=(
                intersection_matrix(
                  geom=geom_point(
                    # shape='square',
                    size=3
                  ),
                  segment=geom_segment(
                    linetype='dotted'
                    # ),
                    # outline_color=list(
                    #   active='darkorange3',
                    #   inactive='grey70'
                    # )
                  )
                  # + scale_color_manual(
                  #   values=c('TRUE'='orange', 'FALSE'='grey'),
                  #   labels=c('TRUE'='yes', 'FALSE'='no'),
                  #   breaks=c('TRUE', 'FALSE'),
                  #   name='Is intersection member?'
                )),
              themes=upset_modify_themes(
                list(
                  'intersections_matrix'=theme(text=element_text(size=15)),
                  'overall_sizes'=theme(axis.text.x=element_text(size =15),
                                        text = element_text(size=15)),
                  
                  "Intersection size"=theme(
                    # axis.line =element_text(size=10),
                    # line = element_text(size=10),
                    text = element_text(size=15))
                  # rect = element_text(size=10),
                  # axis.line.x = element_text(size=10))
                )
              ),
              set_sizes=(
                upset_set_size(
                  # position='right'
                ) + scale_y_continuous(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)), trans=reverse_log_trans() )
                + geom_text(aes(label=..count..), hjust=1.1, stat='count',size=3)
                # you can also add annotations on top of bars:
                + expand_limits(y=10000)
                + theme(axis.text.x=element_text(angle=90))),
              
              base_annotations=list(
                'Intersection size'=intersection_size(
                  text=list(
                    size=2.6
                  ),
                  text_mapping=aes(
                    label=paste0(
                      !!upset_text_percentage(),
                      '\n(', !!size, ')'
                    ),
                    colour=ifelse(!!size > 10000, 'on_bar', 'on_background'),
                    y=ifelse(!!size > 10000, !!size - 50, !!size)
                    
                  )) + ylim(c(0, 350)) +
                  labs(title = "Co-Occurence of COVID-19 Symptoms",
                       caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")
                
              ))

plot



##############

## Side Effects example: the dataset is attached on the Github.

##############
side_Data <- read.csv("side_data.csv") %>% select(-X)

side_Data2 <- na.omit(side_Data)

upsetplotSide <- upset(side_Data2, intersect = colnames(side_Data),keep_empty_groups =F,
      # annotations = list(
      #   'age' =isize),
      min_size = 0,
      name="Side Effects", 
      
      width_ratio = 0.10,
      height_ratio = 0.4,
      matrix=(
        intersection_matrix(
          geom=geom_point(
            # shape='square',
            size=3
          ),
          segment=geom_segment(
            linetype='dotted'
            # ),
            # outline_color=list(
            #   active='darkorange3',
            #   inactive='grey70'
            # )
          )
          # + scale_color_manual(
          #   values=c('TRUE'='orange', 'FALSE'='grey'),
          #   labels=c('TRUE'='yes', 'FALSE'='no'),
          #   breaks=c('TRUE', 'FALSE'),
          #   name='Is intersection member?'
        )),
      themes=upset_modify_themes(
        list(
          'intersections_matrix'=theme(text=element_text(size=12)),
          'overall_sizes'=theme(axis.text.x=element_text(size =12),
                                text = element_text(size=12)),
          
          "Intersection size"=theme(
            # axis.line =element_text(size=10),
            # line = element_text(size=10),
            text = element_text(size=12))
          # rect = element_text(size=10),
          # axis.line.x = element_text(size=10))
        )
      ),
      set_sizes=(
        upset_set_size(
          # position='right'
        ) + scale_y_continuous(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x)), trans=reverse_log_trans() )
        + geom_text(aes(label=..count..), hjust=1.1, stat='count',size=3)
        # you can also add annotations on top of bars:
        + expand_limits(y=1000)
        + theme(axis.text.x=element_text(angle=90))),
      
      base_annotations=list(
        'Intersection size'=intersection_size(
          text=list(
            size=2.5
          )) + ylim(c(0, 70)) +
          labs(title = "Co-Occurence of side effects",
               caption = "Data: https://data.mendeley.com/datasets/gjcyvs5nfx/3")
        
      ))


upsetplotSide


##############

## Missing data analysis example: the same dataset of side effects (attached on the Github).

##############
# Assuming you have a data frame named 'your_data' with missing values
# You can create a binary data frame indicating missing (1) or not missing (0) values
missing_data <- side_Data %>%
  mutate_all(~ifelse(is.na(.), 1, 0))

age_missing <- rnorm(nrow(missing_data), 70, 10)
age_not_missing <- rnorm(nrow(missing_data), 30, 10)

row_sums_age <- rowSums(missing_data)

missing_data$Nmissing <- row_sums_age

missing_data <- mutate(missing_data, age= if_else(Nmissing>3, age_missing, age_not_missing))
# Create the UpSet plot
missing_plot <- upset(missing_data, colnames(missing_data %>% select(-age, - Nmissing)),
      annotations = list(
        'age' =isize),
      min_size = 0,
      name="Missing in Side Effects Variables", 
      width_ratio = 0.3,
      height_ratio = 2,
      matrix=(
        intersection_matrix(
          geom=geom_point(
            # shape='square',
            size=3
          ),
          segment=geom_segment(
            linetype='dotted'
            # ),
            # outline_color=list(
            #   active='darkorange3',
            #   inactive='grey70'
            # )
          )
          # + scale_color_manual(
          #   values=c('TRUE'='orange', 'FALSE'='grey'),
          #   labels=c('TRUE'='yes', 'FALSE'='no'),
          #   breaks=c('TRUE', 'FALSE'),
          #   name='Is intersection member?'
        )),
      themes=upset_modify_themes(
        list(
          'intersections_matrix'=theme(text=element_text(size=15)),
          'overall_sizes'=theme(axis.text.x=element_text(size =15),
                                text = element_text(size=15)),
          
          "Intersection size"=theme(
            # axis.line =element_text(size=10),
            # line = element_text(size=10),
            text = element_text(size=15))
          # rect = element_text(size=10),
          # axis.line.x = element_text(size=10))
        )
      ),
      set_sizes=(
        upset_set_size(
          # position='right'
        ) + scale_y_continuous(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x)), trans=reverse_log_trans() )
        + geom_text(aes(label=..count..), hjust=1.1, stat='count',size=3)
        # you can also add annotations on top of bars:
        + expand_limits(y=1000)
        + theme(axis.text.x=element_text(angle=90))),
      
      base_annotations=list(
        'Intersection size'=intersection_size(
          text=list(
            size=2.6
          ),
          text_mapping=aes(
            label=paste0(
              !!upset_text_percentage(),
              '\n(', !!size, ')'
            ),
            colour=ifelse(!!size > 10000, 'on_bar', 'on_background'),
            y=ifelse(!!size > 10000, !!size - 50, !!size)
            
          )) + ylim(c(0, 120)) +
          labs(title = "Co-Occurence of Missing Side Effects",
               caption = "Data: https://data.mendeley.com/datasets/gjcyvs5nfx/3")
        
      ))
missing_plot


##############

## Case Study (5): Medication Usage Profiling: the dataset is attached on the Github.

##############
# Set the number of patients and drugs
num_patients <- 1000
num_drugs <- 5

# Create a matrix to represent drug interactions (0 or 1)
drug_interactions <- matrix(sample(0:1, num_patients * num_drugs, replace = TRUE), nrow = num_patients) %>%
  as.data.frame()

# Print the matrix
print(drug_interactions)

colnames(drug_interactions) <- c("Aspirin", "Ibuprofen", "Lisinopril", "Simvastatin", "Metformin")

interaction_plot <- upset(drug_interactions, colnames(drug_interactions),
      min_size = 0,
      width_ratio = 0.13,
      name = "Drugs",
      height_ratio = 0.4,
      matrix=(
        intersection_matrix(
          geom=geom_point(
            # shape='square',
            size=3
          ),
          segment=geom_segment(
            linetype='dotted'
            # ),
            # outline_color=list(
            #   active='darkorange3',
            #   inactive='grey70'
            # )
          )
          # + scale_color_manual(
          #   values=c('TRUE'='orange', 'FALSE'='grey'),
          #   labels=c('TRUE'='yes', 'FALSE'='no'),
          #   breaks=c('TRUE', 'FALSE'),
          #   name='Is intersection member?'
        )),
      themes=upset_modify_themes(
        list(
          'intersections_matrix'=theme(text=element_text(size=15)),
          'overall_sizes'=theme(axis.text.x=element_text(size =15),
                                text = element_text(size=15)),
          
          "Intersection size"=theme(
            # axis.line =element_text(size=10),
            # line = element_text(size=10),
            text = element_text(size=15))
          # rect = element_text(size=10),
          # axis.line.x = element_text(size=10))
        )
      ),
      set_sizes=(
        upset_set_size(
          # position='right'
        ) + scale_y_continuous(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x)), trans=reverse_log_trans() )
        + geom_text(aes(label=..count..), hjust=1.1, stat='count',size=3)
        # you can also add annotations on top of bars:
        + expand_limits(y=2000)
        + theme(axis.text.x=element_text(angle=90))),
      
      base_annotations=list(
        'Intersection size'=intersection_size(
          text=list(
            size=3
          )) + ylim(c(0, 50)) +
          labs(title = "Screening of Concomitant Medications ",
               caption = "Data: Simulated Data")
        
      ))

##############

