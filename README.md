Project Phases
================
Last Update: 4/25/2022

<img src="./Images/deliveryPhases.JPG" width="130%" />

# Discovery

As a graduate level course offering at North Carolina State University,
*ST542 - Statistical Practice* provides a discussion-based introduction
to statistical practice that is geared towards students in the final
semester of their Master of Statistics degree. Students practice writing
and presenting throughout the semester, gaining soft skills that aren’t
necessarily covered in other statistical courses. Students are paired
and assigned to consulting engagements provided by the instructor that
are selected from a list of projects submitted by researchers on campus.
Covered topics of the course include negotiating expectations with
clients, technical writing and editing, data cleaning and visualization,
and finding research ideas from consulting projects.

## Participants

**Client:** Kim Heagy *(NCSU Horticulture Grad Student)*  
**Consultants:** Brian Sugg, Chengxi Zou *(NCSU Statistics Grad
Students)*

## Project Background

Pumpkins are an emerging specialty crop in North Carolina. In 2019 the
state produced 713,000 cwt of pumpkins on 3,000 acres, ranking 6th in
the United States where overall national production is steadily
increasing. With the rise in pumpkin production there is an increasing
need for providing clear guidelines to North Carolina farmers on how
plant density impacts overall production. In the absence of clear
guidelines, North Carolina Extension Agents have noticed a variety of
methods in how farmers grow pumpkins. There is a challenge in not only
determining how to optimize land usage, but also in how to optimize the
capacity of transportation bins for moving pumpkins to market. Another
challenge facing farmers is the rise in commodity prices for fertilizer,
the primary means for applying nitrogen to crops.

Taking into consideration these challenges, it has become necessary to
provide advice to farmers on how growing conditions affect the yield of
pumpkin crops. As part of her thesis for her graduate program at NC
State, Kim Heagy has focused her research in two areas on how to grow
pumpkins most efficiently:

1.  Researching plant density by conducting a plant spacing study, where
    the hypothesis is pumpkins grown closer together will have a higher
    overall yield.  
2.  Researching fertilizer application levels by conducting a separate
    nitrogen study, where the hypothesis is higher leaf nitrogen
    composition will indicate higher yields.

The two studies were conducted at the NC State Extension Upper Mountain
Research Station under the care of North Carolina Extension Agents.

## Expectations

This project will be expected to deliver:

1.  A response to the proposed hyptheses:
      - *Pumpkins grown closer together will have a higher overall
        yield.*  
      - *Higher leaf nitrogen composition will indicate higher
        yields.*  
2.  Documentation of how the analysis was conducted  
3.  Clean data files and visuals

Following the below delivery timeline for each phase of work:

<img src="./Images/deliveryTimeline.JPG" width="130%" />

# Data Processing

Raw data from the research conducted in 2020 and 2021 was provided in
multiple Excel files. An inventory is outlined below of the source files
that were received:

  - `2020 Pumpkin Results for Joy.xlsx`
      - 2020 Spacing Study  
  - `2020 stand count.xlsx`
      - 2020 Spacing Study Stand Counts  
  - `2021 Pumpkin Trial Data.xlsx`
      - 2021 Spacing Study  
      - 2021 Nitrogen Study  
      - 2021 Leaf Composition Study  
  - `2021 Bin Pumpkin Sizes.xlsx`
      - 2021 Bin Capacity Study

## Data Formatting

Each of the provided files required formatting in Excel prior to
importing into R. A full step-by-step overview of the changes made
within each Excel workbook is outlined further below. Generally
speaking, the formatting effort focused on getting the worksheets into a
table form. This included tasks such as removing unnecessary blank rows
and columns, ensuring categorical values were consistently spelled and
spaced (“10 x 1” rather than “10x 1”), along with other minor changes to
assist with a clean import to R.

### 2020 Pumpkin Results for Joy.xlsx

This file contained the 2020 spacing study data, consisting of three
worksheets total, with one worksheet for each of the three metrics that
were recorded during the harvest (weight, length, diameter). The
worksheet inventory included:

  - `Spacing Weight`  
  - `Spacing Length`  
  - `Spacing Diameter`

The below formatting steps were followed for each of the three
worksheets:

1.  Removed empty rows and empty columns from the top and the left of
    the main data frame
      - Resulted in all column names moving up to row 1  
2.  Removed empty rows between records that were separating `reps`  
3.  Inserted 1 column at column D position as `Spacing` to hold a
    concatenation of the `Row Distance` and `In Row Distance` columns,
    to match the same presentation of the `Spacing` variable in the 2021
    data
      - Formula = `CONCATENATE($E2," x ",$F2)` and apply down entire
        column  
4.  Copied the new `Spacing` column D and pasted back as values only to
    remove formula dependence on `Row Distance` and `In Row Distance`  
5.  Deleted the `Row Distance` and `In Row Distance` columns after
    concatenation  
6.  Checked `Green Fruit` column E to ensure all values were evenly
    spaced by “,” (“comma space”) for parsing during the import
    process  
7.  Repeated the above steps 1 - 6 for the other two worksheets

**Note:** The `Diameter` value for `Plot` 301 `Fruit` 11 in the `Spacing
Diameter` worksheet was listed as `17` which looked large upon initial
evaluation, compared to others of the similar weight and diameter.
Confirmed with client this was an input error due to messy handwriting,
and updated to reflect the intended value of `14`.

Finally, the file was renamed to `2020pumpkinData.xlsx` after formatting
for easy future reference.

#### Preview Before Formatting

<div class="figure">

<img src="./Images/Files/2020spacingBefore.JPG" alt="2020 Pumpkin Results for Joy.xlsx" width="85%" />

<p class="caption">

2020 Pumpkin Results for Joy.xlsx

</p>

</div>

#### Preview After Formatting

<div class="figure">

<img src="./Images/Files/2020spacingAfter.JPG" alt="2020pumpkinData.xlsx" width="85%" />

<p class="caption">

2020pumpkinData.xlsx

</p>

</div>

### 2020 stand count.xlsx

This file contained the stand count data from the 2020 spacing study,
which was not part of the original file `2020 Pumpkin Results for
Joy.xlsx`. The worksheet inventory included:

  - `Sheet1`

The below formatting steps were followed:

1.  Removed empty rows and empty columns from the top and the left of
    the main data frame
      - Resulted in all column names moving up to row 1

Finally, the file was renamed to `2020standCount.xlsx` after formatting
for easy future reference.

### 2021 Pumpkin Trial Data.xlsx

This file contained the 2021 spacing study, nitrogen study, and leaf
composition study data, consisting of nine worksheets total, with one
worksheet for each of the three metrics that were recorded during the
harvest (weight, length, diameter) for each of the three studies. The
worksheet inventory included:

  - `Spacing Weights (lbs)`  
  - `Spacing Lengths (inches)`  
  - `Spacing Diameters (inches)`  
  - `Nitrogen Weights (lbs)`  
  - `Nitrogen Lengths (inches)`  
  - `Nitrogen Diameters (inches)`  
  - `Leaf Analysis Nitrogen (%)`  
  - `Leaf Analysis Phosphorus (%)`  
  - `Leaf Analysis Potassium (%)`

#### Spacing Study

The below formatting steps were followed for each of the three
worksheets for the spacing study:

1.  Removed empty rows and empty columns from the top and the left of
    the main data frame
      - Resulted in all column names moving up to row 1  
2.  Removed empty rows between records that were separating `reps`  
3.  Checked `Spacing` column F to ensure all values were evenly spaced
    for consistent categorical values
      - Example: Adjusted `5x 8` to `5 x 8`  
4.  Checked `Green Fruit` column G to ensure all values were evenly
    spaced by “,” (“comma space”) for parsing during the import
    process  
5.  Repeated the above steps 1 - 4 for the other two spacing study
    worksheets

#### Nitrogen Study

The below formatting steps were followed for the `Nitrogen Weights
(lbs)` worksheet:

1.  Removed empty column A, so the table starts in column A  
2.  Removed empty rows between records that were separating `reps`  
3.  Checked column B - `Nitrogen Rate` to ensure all values were spaced
    with 1 space between the number and unit of measure, and no spaces
    were within the unit of measure
      - Example: Adjusted `lbs/ acre` to `lbs/acre` for consistency  
4.  Renamed worksheet to `Nitrogen Weights (lbs) Orange`  
5.  Created a copy of the worksheet, and renamed the copy as `Nitrogen
    Weights (lbs) Green`  
6.  In the `Nitrogen Weights (lbs) Orange` worksheet:
      - Deleted columns V through AT to remove all green pumpkin data  
      - Deleted the top row so all column headers move up to row 1  
7.  In the `Nitrogen Weights (lbs) Green` worksheet:
      - Deleted columns G through V to remove all orange pumpkin data  
      - Deleted the top row so all column headers move up to row 1

The below formatting steps were followed for the `Nitrogen Lengths
(inches)` worksheet:

1.  Removed empty column A, so the table starts in column A  
2.  Removed empty rows between records that were separating `reps`  
3.  Checked column B - `Nitrogen Rate` to ensure all values were spaced
    with 1 space between the number and unit of measure, no spaces were
    within the unit of measure, and no extra forward slashes were
    present
      - Example: Adjusted `lbs/ acre` to `lbs/acre` for consistency  
      - Example: Adjusted `200 /lbs/acre` to `200 lbs/acre` for
        consistency  
4.  Renamed worksheet to `Nitrogen Lengths (inches) O`
      - Note: Spelling out “Orange” was too many characters for a
        worksheet name  
5.  Created a copy of the worksheet, and renamed the copy as `Nitrogen
    Lengths (inches) G`  
6.  In the `Nitrogen Lengths (inches) O` worksheet:
      - Deleted columns L through P to remove all green pumpkin data  
      - Deleted the top row so all column headers move up to row 1  
7.  In the `Nitrogen Lengths (inches) G` worksheet:
      - Deleted columns G through L to remove all orange pumpkin data  
      - Deleted the top row so all column headers move up to row 1

The below formatting steps were followed for the `Nitrogen Diameters
(inches)` worksheet:

1.  Removed empty column A, so the table starts in column A  
2.  Removed empty rows between records that were separating `reps`  
3.  Checked column B - `Nitrogen Rate` to ensure all values were spaced
    with 1 space between the number and unit of measure, and no spaces
    were within the unit of measure
      - Example: Adjusted `lbs/ acre` to `lbs/acre` for consistency  
4.  Renamed worksheet to `Nitrogen Diameters (inches) O`
      - Note: Spelling out “Orange” was too many characters for a
        worksheet name  
5.  Created a copy of the worksheet, and renamed the copy as `Nitrogen
    Diameters (inches) G`  
6.  In the `Nitrogen Diameters (inches) O` worksheet:
      - Deleted columns L through P to remove all green pumpkin data  
      - Deleted the top row so all column headers move up to row 1  
7.  In the `Nitrogen Diameters (inches) G` worksheet:
      - Deleted columns G through L to remove all orange pumpkin data  
      - Deleted the top row so all column headers move up to row 1

#### Leaf Composition Study

The below formatting steps were followed for each of the three
worksheets for the leaf composition study:

1.  Removed empty rows and empty columns from the top and the left of
    the main data frame
      - Resulted in all column names moving up to row 1  
2.  Removed empty rows between records that were separating `reps`  
3.  Removed empty column E  
4.  Checked `Nitrogen Rate` column B to ensure all values were evenly
    spaced for consistent categorical values
      - Example: Adjusted `lbs/ acre` to `lbs/acre` for consistency  
5.  Repeated the above steps 1 - 4 for the other two leaf composition
    study worksheets

**Note:** The `Diameter` value for `Plot` 301 `Orange Fruit` 1 in the
`Nitrogen Diameters (inches) O` worksheet was listed as `15.2` which
looked large upon initial evaluation, compared to others of the similar
weight and diameter. Confirmed with client this was an input error due
to messy handwriting, and updated to reflect the intended value of
`13.2`.

**Note:** The `Ideal Stand Count` value for `Plot` 301 in all three
`Spacing ...` worksheets was listed as `10`. Updated to reflect the
correct ideal stand count value of `20` that was associated with this
plot in all three worksheets.

**Note:** The `Nitrogen Rate` value for `Plot` 302 in the `Leaf Analysis
Nitrogen (%)` worksheet was blank. Updated to reflect the value of `200
lbs/acre` that was associated with this plot in the `Leaf Analysis
Phosphorus (%)` and `Leaf Analysis Potassium (%)` worksheets.

**Note:** The `Treatment` value for `Plot` 406 in the `Leaf Analysis
Nitrogen (%)` worksheet was listed as `4`. Updated to reflect the
treatment value of `2` that was associated with this plot in the `Leaf
Analysis Phosphorus (%)` and `Leaf Analysis Potassium (%)` worksheets.

Finally, the file was renamed to `2021pumpkinData.xlsx` after formatting
for easy future reference.

#### Preview Before Formatting

<div class="figure">

<img src="./Images/Files/2021spacingBefore.JPG" alt="2021 Pumpkin Trial Data.xlsx" width="85%" />

<p class="caption">

2021 Pumpkin Trial Data.xlsx

</p>

</div>

#### Preview After Formatting

<div class="figure">

<img src="./Images/Files/2021spacingAfter.JPG" alt="2021pumpkinData.xlsx" width="85%" />

<p class="caption">

2021pumpkinData.xlsx

</p>

</div>

### 2021 Bin Pumpkin Sizes.xlsx

To be processed…

## Data Import

### Spacing Study

#### 2020 Data

Check workbook names and worksheet names for each of the 3 read\_excel
functions.  
Check columns to gather and ensure in Excel the columns with metrics are
6 to 53.  
Adjust both the year and pumpkin ID variables to reflect year 2020.  
Remove or correct data errors/outliers.

``` r
# Read sheet with WEIGHTS, transform from wide to tall, remove NA
# records
spacingWeight <- read_excel(path = "./ReadData/2020pumpkinData.xlsx", sheet = "Spacing Weight")
spacingWeight <- gather(spacingWeight, key = "Pumpkin", value = "Weight", 
    6:53)
spacingWeight <- subset(spacingWeight, !is.na(Weight))
# Rename columns
names(spacingWeight) <- c("plot", "treatment", "rep", "spacingDim", "greenPumpkins", 
    "pumpkinNum", "weight")
# Create unique identifier for each pumpkin
spacingWeight <- mutate(spacingWeight, year = "2020")
spacingWeight <- mutate(spacingWeight, pumpkinID = paste0(year, "-", "S", 
    "-", plot, "-", pumpkinNum))

########################## 

# Read sheet with LENGTHS, transform from wide to tall, remove NA
# records
spacingLength <- read_excel(path = "./ReadData/2020pumpkinData.xlsx", sheet = "Spacing Length")
spacingLength <- gather(spacingLength, key = "Pumpkin", value = "Length", 
    6:52)
spacingLength <- subset(spacingLength, !is.na(Length))
# Rename columns
names(spacingLength) <- c("plot", "treatment", "rep", "spacingDim", "greenPumpkins", 
    "pumpkinNum", "length")
# Create unique identifier for each pumpkin and assign crop year
spacingLength <- mutate(spacingLength, year = "2020")
spacingLength <- mutate(spacingLength, pumpkinID = paste0(year, "-", "S", 
    "-", plot, "-", pumpkinNum))

########################## 

# Read sheet with DIAMETERS, transform from wide to tall, remove NA
# records
spacingDiameter <- read_excel(path = "./ReadData/2020pumpkinData.xlsx", 
    sheet = "Spacing Diameter")
spacingDiameter <- gather(spacingDiameter, key = "Pumpkin", value = "Diameter", 
    6:52)
spacingDiameter <- subset(spacingDiameter, !is.na(Diameter))
# Rename columns
names(spacingDiameter) <- c("plot", "treatment", "rep", "spacingDim", "greenPumpkins", 
    "pumpkinNum", "diameter")
# Create unique identifier for each pumpkin and assign crop year
spacingDiameter <- mutate(spacingDiameter, year = "2020")
spacingDiameter <- mutate(spacingDiameter, pumpkinID = paste0(year, "-", 
    "S", "-", plot, "-", pumpkinNum))

########################## 

# Join all spacing metrics in one raw table for quality checks
spacingDataRaw2020 <- inner_join(spacingWeight, spacingLength)
spacingDataRaw2020 <- inner_join(spacingDataRaw2020, spacingDiameter)

# Read sheet with stand count data
spacingStand <- read_excel(path = "./ReadData/2020standCount.xlsx", sheet = "Sheet1")
# Rename columns
names(spacingStand) <- c("treatment", "spacingDim", "plot", "standCountIdeal", 
    "standCount")
# Remove measurement abbreviation for feet (')
spacingStand$spacingDim <- str_remove_all(spacingStand$spacingDim, "'")

# Join stand counts to spacing metrics
spacingDataRaw2020 <- left_join(spacingDataRaw2020, spacingStand)

# Arrange columns for presentation of final table for spacing data
spacingDataRaw2020 <- select(spacingDataRaw2020, c(1, 2, 3, 13, 12, 4, 
    5, 6, 7, 8, 9, 10, 11))

# Create clean table for further transformations
spacingData2020 <- as_tibble(spacingDataRaw2020)

# Remove or correct errors/outliers that still remain after inner join
# 2020 spacing study, plot 102, delete fruit 28 - no weight data taken
# since it was rotten
spacingData2020 <- subset(spacingData2020, weight != "rotten")
# 2020 spacing study, plot 201, correct fruit 17 - weight is 33.2 not
# 3.2
if (sum(spacingData2020$pumpkinID == "2020-S-201-17") == 1) {
    w <- which(spacingData2020$pumpkinID == "2020-S-201-17")
    spacingData2020$weight[w] <- "33.2"
} else {
    stop("Weight outlier from 2020 spacing study, plot 201, pumpkin 17 could not be found and corrected.")
}
```

#### 2021 Data

Check workbook names and worksheet names for each of the 3 read\_excel
functions.

``` r
# Read sheet with WEIGHTS, transform from wide to tall, remove NA
# records
spacingWeight <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", sheet = "Spacing Weights (lbs)")
spacingWeight <- gather(spacingWeight, key = "Pumpkin", value = "Weight", 
    8:55)
spacingWeight <- subset(spacingWeight, !is.na(Weight))
# Rename columns
names(spacingWeight) <- c("plot", "treatment", "rep", "standCount", "standCountIdeal", 
    "spacingDim", "greenPumpkins", "pumpkinNum", "weight")
# Create unique identifier for each pumpkin
spacingWeight <- mutate(spacingWeight, year = "2021")
spacingWeight <- mutate(spacingWeight, pumpkinID = paste0(year, "-", "S", 
    "-", plot, "-", pumpkinNum))

########################## 

# Read sheet with LENGTHS, transform from wide to tall, remove NA
# records
spacingLength <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", sheet = "Spacing Lengths (inches)")
spacingLength <- gather(spacingLength, key = "Pumpkin", value = "Length", 
    8:55)
spacingLength <- subset(spacingLength, !is.na(Length))
# Rename columns
names(spacingLength) <- c("plot", "treatment", "rep", "standCount", "standCountIdeal", 
    "spacingDim", "greenPumpkins", "pumpkinNum", "length")
# Create unique identifier for each pumpkin and assign crop year
spacingLength <- mutate(spacingLength, year = "2021")
spacingLength <- mutate(spacingLength, pumpkinID = paste0(year, "-", "S", 
    "-", plot, "-", pumpkinNum))

########################## 

# Read sheet with DIAMETERS, transform from wide to tall, remove NA
# records
spacingDiameter <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Spacing Diameters (inches)")
spacingDiameter <- gather(spacingDiameter, key = "Pumpkin", value = "Diameter", 
    8:55)
spacingDiameter <- subset(spacingDiameter, !is.na(Diameter))
# Rename columns
names(spacingDiameter) <- c("plot", "treatment", "rep", "standCount", "standCountIdeal", 
    "spacingDim", "greenPumpkins", "pumpkinNum", "diameter")
# Create unique identifier for each pumpkin and assign crop year
spacingDiameter <- mutate(spacingDiameter, year = "2021")
spacingDiameter <- mutate(spacingDiameter, pumpkinID = paste0(year, "-", 
    "S", "-", plot, "-", pumpkinNum))

########################## 

# Join all spacing metrics in one raw table for quality checks
spacingDataRaw2021 <- inner_join(spacingWeight, spacingLength)
spacingDataRaw2021 <- inner_join(spacingDataRaw2021, spacingDiameter)

# Create clean table for further transformations
spacingData2021 <- as_tibble(spacingDataRaw2021)
```

### Nitrogen Study

#### 2021 Data

Check workbook names and worksheet names for each of the 3 read\_excel
functions.

``` r
# Read sheet with WEIGHTS for orange pumpkins, transform from wide to
# tall, remove NA records
nitrogenWeightOrange <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Weights (lbs) Orange")
nitrogenWeightOrange <- gather(nitrogenWeightOrange, key = "Pumpkin", value = "Weight", 
    7:21)
nitrogenWeightOrange <- subset(nitrogenWeightOrange, !is.na(Weight))
# Rename columns
names(nitrogenWeightOrange) <- c("plot", "nitrogenRate", "treatment", "rep", 
    "standCount", "standCountIdeal", "pumpkinNum", "weight")
# Create variable for color
nitrogenWeightOrange <- mutate(nitrogenWeightOrange, color = "Orange")
# Create unique identifier for each pumpkin
nitrogenWeightOrange <- mutate(nitrogenWeightOrange, year = "2021")
nitrogenWeightOrange <- mutate(nitrogenWeightOrange, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Read sheet with WEIGHTS for green pumpkins, transform from wide to
# tall, remove NA records
nitrogenWeightGreen <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Weights (lbs) Green")
nitrogenWeightGreen <- gather(nitrogenWeightGreen, key = "Pumpkin", value = "Weight", 
    7:30)
nitrogenWeightGreen <- subset(nitrogenWeightGreen, !is.na(Weight))
# Rename columns
names(nitrogenWeightGreen) <- c("plot", "nitrogenRate", "treatment", "rep", 
    "standCount", "standCountIdeal", "pumpkinNum", "weight")
# Create variable for color
nitrogenWeightGreen <- mutate(nitrogenWeightGreen, color = "Green")
# Create unique identifier for each pumpkin
nitrogenWeightGreen <- mutate(nitrogenWeightGreen, year = "2021")
nitrogenWeightGreen <- mutate(nitrogenWeightGreen, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Combine all weights into one tibble
nitrogenWeight <- rbind(nitrogenWeightOrange, nitrogenWeightGreen)

########################## 

# Read sheet with LENGTHS for orange pumpkins, transform from wide to
# tall, remove NA records
nitrogenLengthOrange <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Lengths (inches) O")
nitrogenLengthOrange <- gather(nitrogenLengthOrange, key = "Pumpkin", value = "Length", 
    7:11)
nitrogenLengthOrange <- subset(nitrogenLengthOrange, !is.na(Length))
# Rename columns
names(nitrogenLengthOrange) <- c("plot", "nitrogenRate", "treatment", "rep", 
    "standCount", "standCountIdeal", "pumpkinNum", "length")
# Create variable for color
nitrogenLengthOrange <- mutate(nitrogenLengthOrange, color = "Orange")
# Create unique identifier for each pumpkin
nitrogenLengthOrange <- mutate(nitrogenLengthOrange, year = "2021")
nitrogenLengthOrange <- mutate(nitrogenLengthOrange, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Read sheet with LENGTHS for green pumpkins, transform from wide to
# tall, remove NA records
nitrogenLengthGreen <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Lengths (inches) G")
nitrogenLengthGreen <- gather(nitrogenLengthGreen, key = "Pumpkin", value = "Length", 
    7:10)
nitrogenLengthGreen <- subset(nitrogenLengthGreen, !is.na(Length))
# Rename columns
names(nitrogenLengthGreen) <- c("plot", "nitrogenRate", "treatment", "rep", 
    "standCount", "standCountIdeal", "pumpkinNum", "length")
# Create variable for color
nitrogenLengthGreen <- mutate(nitrogenLengthGreen, color = "Green")
# Create unique identifier for each pumpkin
nitrogenLengthGreen <- mutate(nitrogenLengthGreen, year = "2021")
nitrogenLengthGreen <- mutate(nitrogenLengthGreen, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Combine all lengths into one tibble
nitrogenLength <- rbind(nitrogenLengthOrange, nitrogenLengthGreen)

########################## 

# Read sheet with DIAMETERS for orange pumpkins, transform from wide to
# tall, remove NA records
nitrogenDiameterOrange <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Diameters (inches) O")
nitrogenDiameterOrange <- gather(nitrogenDiameterOrange, key = "Pumpkin", 
    value = "Diameter", 7:11)
nitrogenDiameterOrange <- subset(nitrogenDiameterOrange, !is.na(Diameter))
# Rename columns
names(nitrogenDiameterOrange) <- c("plot", "nitrogenRate", "treatment", 
    "rep", "standCount", "standCountIdeal", "pumpkinNum", "diameter")
# Create variable for color
nitrogenDiameterOrange <- mutate(nitrogenDiameterOrange, color = "Orange")
# Create unique identifier for each pumpkin
nitrogenDiameterOrange <- mutate(nitrogenDiameterOrange, year = "2021")
nitrogenDiameterOrange <- mutate(nitrogenDiameterOrange, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Read sheet with DIAMETERS for green pumpkins, transform from wide to
# tall, remove NA records
nitrogenDiameterGreen <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Nitrogen Diameters (inches) G")
nitrogenDiameterGreen <- gather(nitrogenDiameterGreen, key = "Pumpkin", 
    value = "Diameter", 7:10)
nitrogenDiameterGreen <- subset(nitrogenDiameterGreen, !is.na(Diameter))
# Rename columns
names(nitrogenDiameterGreen) <- c("plot", "nitrogenRate", "treatment", 
    "rep", "standCount", "standCountIdeal", "pumpkinNum", "diameter")
# Create variable for color
nitrogenDiameterGreen <- mutate(nitrogenDiameterGreen, color = "Green")
# Create unique identifier for each pumpkin
nitrogenDiameterGreen <- mutate(nitrogenDiameterGreen, year = "2021")
nitrogenDiameterGreen <- mutate(nitrogenDiameterGreen, pumpkinID = paste0(year, 
    "-", "N", "-", plot, "-", color, "-", pumpkinNum))

# Combine all diameters into one tibble
nitrogenDiameter <- rbind(nitrogenDiameterOrange, nitrogenDiameterGreen)
```

### Leaf Composition Study

#### 2021 Data

Check worksheet names for each of the 3 read\_excel functions.  
Check columns used in gather() function.

``` r
# Read sheet with NITROGEN %, transform from wide to tall, remove NA
# records
leafNitrogen <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", sheet = "Leaf Analysis Nitrogen (%)")
leafNitrogen <- gather(leafNitrogen, key = "Date", value = "Nitrogen", 
    5:12)
leafNitrogen <- subset(leafNitrogen, !is.na(Nitrogen))
# Rename columns
names(leafNitrogen) <- c("plot", "nitrogenRate", "treatment", "rep", "date", 
    "nitrogenPct")

########################## 

# Read sheet with PHOSPHORUS %, transform from wide to tall, remove NA
# records
leafPhosphorus <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", 
    sheet = "Leaf Analysis Phosphorus (%)")
leafPhosphorus <- gather(leafPhosphorus, key = "Date", value = "Phosphorus", 
    5:12)
leafPhosphorus <- subset(leafPhosphorus, !is.na(Phosphorus))
# Rename columns
names(leafPhosphorus) <- c("plot", "nitrogenRate", "treatment", "rep", 
    "date", "phosphorusPct")

########################## 

# Read sheet with POTASSIUM %, transform from wide to tall, remove NA
# records
leafPotassium <- read_excel(path = "./ReadData/2021pumpkinData.xlsx", sheet = "Leaf Analysis Potassium (%)")
leafPotassium <- gather(leafPotassium, key = "Date", value = "Potassium", 
    5:12)
leafPotassium <- subset(leafPotassium, !is.na(Potassium))
# Rename columns
names(leafPotassium) <- c("plot", "nitrogenRate", "treatment", "rep", "date", 
    "potassiumPct")

########################## 

# Join all leaf metrics into one table
leafData <- inner_join(leafNitrogen, leafPhosphorus)
leafData <- inner_join(leafData, leafPotassium)

# Create variable for year
leafData <- mutate(leafData, year = "2021")
```

## Data Transformation

### Spacing Study

``` r
# Stack the 2020 and 2021 data sets into one tibble
spacingData <- rbind(spacingData2020, spacingData2021)

# Format variables
spacingData$plot <- as.factor(paste0(spacingData$year, "-", spacingData$plot))
spacingData$treatment <- as.factor(spacingData$treatment)
spacingData$rep <- as.factor(paste0(spacingData$year, "-", spacingData$rep))
spacingData$pumpkinNum <- as.numeric(spacingData$pumpkinNum)
spacingData$year <- as.factor(spacingData$year)

# Create variable for pumpkin color, populate based on conditional
# statement, and format
spacingData <- mutate(spacingData, color = "unknown")
for (i in 1:nrow(spacingData)) {
    spacingData[i, 14] <- if_else(spacingData[i, 8] %in% sapply(strsplit(as.character(spacingData[i, 
        7]), split = ", "), function(x) as.numeric(x)), "Green", "Orange")
}
spacingData$color <- as.factor(spacingData$color)

# Update unique pumpkin ID to include color (to match nitrogen data
# format)
spacingData$pumpkinID <- paste0("S", "-", spacingData$plot, "-", spacingData$color, 
    "-", spacingData$pumpkinNum)

# Create variable for plant area and transform spacing variables
# factors
spacingData <- mutate(spacingData, plantArea = sapply(strsplit(as.character(spacingDim), 
    split = " x "), function(x) prod(as.numeric(x))))
spacingData$plantArea <- as.factor(spacingData$plantArea)
spacingData$spacingDim <- as.character(spacingData$spacingDim)

# Ensure metrics are all numerical
spacingData$weight <- as.numeric(spacingData$weight)
spacingData$length <- as.numeric(spacingData$length)
spacingData$diameter <- as.numeric(spacingData$diameter)

# Create variable for realized ideal stand count percentage
spacingData <- mutate(spacingData, standCountIdealPct = standCount/standCountIdeal)

# Create variable for volume spacingData <-
# mutate(spacingData,volumeSphere=(4/3)*pi*(diameter/2)^3)
spacingData <- mutate(spacingData, volumeEllipsoid = round((4/3) * pi * 
    (diameter/2) * (diameter/2) * (length/2), 1))

# Create rounded variables for diameter, both down and up
spacingData <- mutate(spacingData, diameterRoundDown = floor(diameter))
spacingData$diameterRoundDown <- as.factor(spacingData$diameterRoundDown)
spacingData <- mutate(spacingData, diameterRoundUp = ceiling(diameter))
spacingData$diameterRoundUp <- as.factor(spacingData$diameterRoundUp)

# Arrange columns for presentation of final table for spacing data
spacingData <- select(spacingData, c(11, 10, 1, 3, 2, 8, 15, 6, 4, 5, 16, 
    14, 9, 12, 13, 17, 18, 19))

# Split spacingDim for betweenRow and inRow columns
spacingData <- separate(spacingData, spacingDim, into = c("betweenRow", 
    "inRow"), sep = " x ", remove = FALSE)
spacingData$spacingDim <- as.factor(spacingData$spacingDim)
spacingData$betweenRow <- as.factor(spacingData$betweenRow)
spacingData$inRow <- as.factor(spacingData$inRow)

# Create variables for plot area and harvest area (square feet)
spacingData <- mutate(spacingData, plotArea = ifelse(betweenRow == "10", 
    1600, ifelse(betweenRow == "5", 800, NA)))
spacingData$plotArea <- as.numeric(spacingData$plotArea)
spacingData <- mutate(spacingData, harvestArea = ifelse(betweenRow == "10", 
    400, ifelse(betweenRow == "5", 200, NA)))
spacingData$harvestArea <- as.numeric(spacingData$harvestArea)

# Provide structure of transformed variables with data preview
str(spacingData)
```

    ## tibble [1,259 x 22] (S3: tbl_df/tbl/data.frame)
    ##  $ pumpkinID         : chr [1:1259] "S-2020-101-Green-1" "S-2020-102-Green-1" "S-2020-103-Orange-1" "S-2020-104-Green-1" ...
    ##  $ year              : Factor w/ 2 levels "2020","2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot              : Factor w/ 56 levels "2020-101","2020-102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep               : Factor w/ 7 levels "2020-1","2020-2",..: 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ treatment         : Factor w/ 8 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 3 1 ...
    ##  $ pumpkinNum        : num [1:1259] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plantArea         : Factor w/ 4 levels "10","20","30",..: 1 2 3 4 1 2 3 4 3 1 ...
    ##  $ spacingDim        : Factor w/ 8 levels "10 x 1","10 x 2",..: 1 2 3 4 5 6 7 8 3 1 ...
    ##  $ betweenRow        : Factor w/ 2 levels "10","5": 1 1 1 1 2 2 2 2 1 1 ...
    ##  $ inRow             : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 2 4 5 6 3 1 ...
    ##  $ standCount        : num [1:1259] 79 40 28 20 40 20 14 10 28 80 ...
    ##  $ standCountIdeal   : num [1:1259] 80 40 28 20 40 20 14 10 28 80 ...
    ##  $ standCountIdealPct: num [1:1259] 0.988 1 1 1 1 ...
    ##  $ color             : Factor w/ 2 levels "Green","Orange": 1 1 2 1 1 2 2 2 2 2 ...
    ##  $ weight            : num [1:1259] 17 9.8 40.7 37 22.9 19.8 25.7 27.6 22.8 12.6 ...
    ##  $ length            : num [1:1259] 11.1 9.3 16.1 13.5 11.5 11.4 12.7 12.9 12.3 10.1 ...
    ##  $ diameter          : num [1:1259] 11.3 10.1 15.8 15.2 12.3 12.6 13.5 14.3 13.1 10.1 ...
    ##  $ volumeEllipsoid   : num [1:1259] 742 497 2104 1633 911 ...
    ##  $ diameterRoundDown : Factor w/ 11 levels "7","8","9","10",..: 5 4 9 9 6 6 7 8 7 4 ...
    ##  $ diameterRoundUp   : Factor w/ 11 levels "7","8","9","10",..: 6 5 10 10 7 7 8 9 8 5 ...
    ##  $ plotArea          : num [1:1259] 1600 1600 1600 1600 800 800 800 800 1600 1600 ...
    ##  $ harvestArea       : num [1:1259] 400 400 400 400 200 200 200 200 400 400 ...

``` r
spacingData
```

    ## # A tibble: 1,259 x 22
    ##    pumpkinID year  plot  rep   treatment pumpkinNum plantArea spacingDim
    ##    <chr>     <fct> <fct> <fct> <fct>          <dbl> <fct>     <fct>     
    ##  1 S-2020-1~ 2020  2020~ 2020~ 1                  1 10        10 x 1    
    ##  2 S-2020-1~ 2020  2020~ 2020~ 2                  1 20        10 x 2    
    ##  3 S-2020-1~ 2020  2020~ 2020~ 3                  1 30        10 x 3    
    ##  4 S-2020-1~ 2020  2020~ 2020~ 4                  1 40        10 x 4    
    ##  5 S-2020-1~ 2020  2020~ 2020~ 5                  1 10        5 x 2     
    ##  6 S-2020-1~ 2020  2020~ 2020~ 6                  1 20        5 x 4     
    ##  7 S-2020-1~ 2020  2020~ 2020~ 7                  1 30        5 x 6     
    ##  8 S-2020-1~ 2020  2020~ 2020~ 8                  1 40        5 x 8     
    ##  9 S-2020-2~ 2020  2020~ 2020~ 3                  1 30        10 x 3    
    ## 10 S-2020-2~ 2020  2020~ 2020~ 1                  1 10        10 x 1    
    ## # ... with 1,249 more rows, and 14 more variables: betweenRow <fct>,
    ## #   inRow <fct>, standCount <dbl>, standCountIdeal <dbl>,
    ## #   standCountIdealPct <dbl>, color <fct>, weight <dbl>, length <dbl>,
    ## #   diameter <dbl>, volumeEllipsoid <dbl>, diameterRoundDown <fct>,
    ## #   diameterRoundUp <fct>, plotArea <dbl>, harvestArea <dbl>

``` r
# Create subset with only orange pumpkins
spacingDataOrange <- subset(spacingData, color == "Orange")
```

### Nitrogen Study

#### Weight

``` r
# Format variables
nitrogenWeight$nitrogenRate <- as.factor(nitrogenWeight$nitrogenRate)
nitrogenWeight$treatment <- as.factor(nitrogenWeight$treatment)
nitrogenWeight$pumpkinNum <- as.numeric(nitrogenWeight$pumpkinNum)
nitrogenWeight$color <- as.factor(nitrogenWeight$color)
nitrogenWeight$year <- as.factor(nitrogenWeight$year)
nitrogenWeight$plot <- as.factor(paste0(nitrogenWeight$year, "-", nitrogenWeight$plot))
nitrogenWeight$rep <- as.factor(paste0(nitrogenWeight$year, "-", nitrogenWeight$rep))

# Ensure metrics are all numerical
nitrogenWeight$weight <- as.numeric(nitrogenWeight$weight)

# Create variable for realized ideal stand count percentage
nitrogenWeight <- mutate(nitrogenWeight, standCountIdealPct = standCount/standCountIdeal)

# Arrange columns for presentation of final table for spacing data
nitrogenWeight <- select(nitrogenWeight, c(11, 10, 1, 4, 3, 7, 2, 5, 6, 
    12, 9, 8))

# Provide structure of transformed variables with data preview
str(nitrogenWeight)
```

    ## tibble [618 x 12] (S3: tbl_df/tbl/data.frame)
    ##  $ pumpkinID         : chr [1:618] "2021-N-101-Orange-1" "2021-N-102-Orange-1" "2021-N-103-Orange-1" "2021-N-104-Orange-1" ...
    ##  $ year              : Factor w/ 1 level "2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot              : Factor w/ 24 levels "2021-101","2021-102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep               : Factor w/ 4 levels "2021-1","2021-2",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ treatment         : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 2 4 5 1 ...
    ##  $ pumpkinNum        : num [1:618] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ nitrogenRate      : Factor w/ 6 levels "0 lbs/acre","120 lbs/acre",..: 1 5 6 2 3 4 5 2 3 1 ...
    ##  $ standCount        : num [1:618] 7 6 7 6 7 3 7 7 8 8 ...
    ##  $ standCountIdeal   : num [1:618] 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ standCountIdealPct: num [1:618] 0.875 0.75 0.875 0.75 0.875 0.375 0.875 0.875 1 1 ...
    ##  $ color             : Factor w/ 2 levels "Green","Orange": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ weight            : num [1:618] 30.9 24.2 31.4 24.5 16.2 29.4 23.4 20.6 31.4 26.2 ...

``` r
nitrogenWeight
```

    ## # A tibble: 618 x 12
    ##    pumpkinID year  plot  rep   treatment pumpkinNum nitrogenRate standCount
    ##    <chr>     <fct> <fct> <fct> <fct>          <dbl> <fct>             <dbl>
    ##  1 2021-N-1~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            7
    ##  2 2021-N-1~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           6
    ##  3 2021-N-1~ 2021  2021~ 2021~ 3                  1 80 lbs/acre           7
    ##  4 2021-N-1~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          6
    ##  5 2021-N-1~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          7
    ##  6 2021-N-1~ 2021  2021~ 2021~ 6                  1 200 lbs/acre          3
    ##  7 2021-N-2~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           7
    ##  8 2021-N-2~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          7
    ##  9 2021-N-2~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          8
    ## 10 2021-N-2~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            8
    ## # ... with 608 more rows, and 4 more variables: standCountIdeal <dbl>,
    ## #   standCountIdealPct <dbl>, color <fct>, weight <dbl>

#### Length

``` r
# Format variables
nitrogenLength$nitrogenRate <- as.factor(nitrogenLength$nitrogenRate)
nitrogenLength$treatment <- as.factor(nitrogenLength$treatment)
nitrogenLength$pumpkinNum <- as.numeric(nitrogenLength$pumpkinNum)
nitrogenLength$color <- as.factor(nitrogenLength$color)
nitrogenLength$year <- as.factor(nitrogenLength$year)
nitrogenLength$plot <- as.factor(paste0(nitrogenLength$year, "-", nitrogenLength$plot))
nitrogenLength$rep <- as.factor(paste0(nitrogenLength$year, "-", nitrogenLength$rep))

# Ensure metrics are all numerical
nitrogenLength$length <- as.numeric(nitrogenLength$length)

# Create variable for realized ideal stand count percentage
nitrogenLength <- mutate(nitrogenLength, standCountIdealPct = standCount/standCountIdeal)

# Arrange columns for presentation of final table for spacing data
nitrogenLength <- select(nitrogenLength, c(11, 10, 1, 4, 3, 7, 2, 5, 6, 
    12, 9, 8))

# Provide structure of transformed variables with data preview
str(nitrogenLength)
```

    ## tibble [120 x 12] (S3: tbl_df/tbl/data.frame)
    ##  $ pumpkinID         : chr [1:120] "2021-N-101-Orange-1" "2021-N-102-Orange-1" "2021-N-103-Orange-1" "2021-N-104-Orange-1" ...
    ##  $ year              : Factor w/ 1 level "2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot              : Factor w/ 24 levels "2021-101","2021-102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep               : Factor w/ 4 levels "2021-1","2021-2",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ treatment         : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 2 4 5 1 ...
    ##  $ pumpkinNum        : num [1:120] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ nitrogenRate      : Factor w/ 6 levels "0 lbs/acre","120 lbs/acre",..: 1 5 6 2 3 4 5 2 3 1 ...
    ##  $ standCount        : num [1:120] 7 6 7 6 7 3 7 7 8 8 ...
    ##  $ standCountIdeal   : num [1:120] 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ standCountIdealPct: num [1:120] 0.875 0.75 0.875 0.75 0.875 0.375 0.875 0.875 1 1 ...
    ##  $ color             : Factor w/ 2 levels "Green","Orange": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ length            : num [1:120] 13.6 12.5 13.9 12.5 10.1 12.3 12.5 12 13.7 13 ...

``` r
nitrogenLength
```

    ## # A tibble: 120 x 12
    ##    pumpkinID year  plot  rep   treatment pumpkinNum nitrogenRate standCount
    ##    <chr>     <fct> <fct> <fct> <fct>          <dbl> <fct>             <dbl>
    ##  1 2021-N-1~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            7
    ##  2 2021-N-1~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           6
    ##  3 2021-N-1~ 2021  2021~ 2021~ 3                  1 80 lbs/acre           7
    ##  4 2021-N-1~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          6
    ##  5 2021-N-1~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          7
    ##  6 2021-N-1~ 2021  2021~ 2021~ 6                  1 200 lbs/acre          3
    ##  7 2021-N-2~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           7
    ##  8 2021-N-2~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          7
    ##  9 2021-N-2~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          8
    ## 10 2021-N-2~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            8
    ## # ... with 110 more rows, and 4 more variables: standCountIdeal <dbl>,
    ## #   standCountIdealPct <dbl>, color <fct>, length <dbl>

#### Diameter

``` r
# Format variables
nitrogenDiameter$nitrogenRate <- as.factor(nitrogenDiameter$nitrogenRate)
nitrogenDiameter$treatment <- as.factor(nitrogenDiameter$treatment)
nitrogenDiameter$pumpkinNum <- as.numeric(nitrogenDiameter$pumpkinNum)
nitrogenDiameter$color <- as.factor(nitrogenDiameter$color)
nitrogenDiameter$year <- as.factor(nitrogenDiameter$year)
nitrogenDiameter$plot <- as.factor(paste0(nitrogenDiameter$year, "-", nitrogenDiameter$plot))
nitrogenDiameter$rep <- as.factor(paste0(nitrogenDiameter$year, "-", nitrogenDiameter$rep))

# Ensure metrics are all numerical
nitrogenDiameter$diameter <- as.numeric(nitrogenDiameter$diameter)

# Create variable for realized ideal stand count percentage
nitrogenDiameter <- mutate(nitrogenDiameter, standCountIdealPct = standCount/standCountIdeal)

# Arrange columns for presentation of final table for spacing data
nitrogenDiameter <- select(nitrogenDiameter, c(11, 10, 1, 4, 3, 7, 2, 5, 
    6, 12, 9, 8))

# Provide structure of transformed variables with data preview
str(nitrogenDiameter)
```

    ## tibble [120 x 12] (S3: tbl_df/tbl/data.frame)
    ##  $ pumpkinID         : chr [1:120] "2021-N-101-Orange-1" "2021-N-102-Orange-1" "2021-N-103-Orange-1" "2021-N-104-Orange-1" ...
    ##  $ year              : Factor w/ 1 level "2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot              : Factor w/ 24 levels "2021-101","2021-102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep               : Factor w/ 4 levels "2021-1","2021-2",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ treatment         : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 2 4 5 1 ...
    ##  $ pumpkinNum        : num [1:120] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ nitrogenRate      : Factor w/ 6 levels "0 lbs/acre","120 lbs/acre",..: 1 5 6 2 3 4 5 2 3 1 ...
    ##  $ standCount        : num [1:120] 7 6 7 6 7 3 7 7 8 8 ...
    ##  $ standCountIdeal   : num [1:120] 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ standCountIdealPct: num [1:120] 0.875 0.75 0.875 0.75 0.875 0.375 0.875 0.875 1 1 ...
    ##  $ color             : Factor w/ 2 levels "Green","Orange": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ diameter          : num [1:120] 15.1 13.7 14.9 13.5 11.7 14.6 12.7 12.3 14.6 13.3 ...

``` r
nitrogenDiameter
```

    ## # A tibble: 120 x 12
    ##    pumpkinID year  plot  rep   treatment pumpkinNum nitrogenRate standCount
    ##    <chr>     <fct> <fct> <fct> <fct>          <dbl> <fct>             <dbl>
    ##  1 2021-N-1~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            7
    ##  2 2021-N-1~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           6
    ##  3 2021-N-1~ 2021  2021~ 2021~ 3                  1 80 lbs/acre           7
    ##  4 2021-N-1~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          6
    ##  5 2021-N-1~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          7
    ##  6 2021-N-1~ 2021  2021~ 2021~ 6                  1 200 lbs/acre          3
    ##  7 2021-N-2~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           7
    ##  8 2021-N-2~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          7
    ##  9 2021-N-2~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          8
    ## 10 2021-N-2~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            8
    ## # ... with 110 more rows, and 4 more variables: standCountIdeal <dbl>,
    ## #   standCountIdealPct <dbl>, color <fct>, diameter <dbl>

#### All Measures

``` r
# Join all nitrogen metrics into one table Left join used with weight
# as base as limited length and diameter metrics were collected
nitrogenData <- left_join(nitrogenWeight, nitrogenLength)
nitrogenData <- left_join(nitrogenData, nitrogenDiameter)

# Create variable for volume nitrogenData <-
# mutate(nitrogenData,volumeSphere=(4/3)*pi*(diameter/2)^3)
nitrogenData <- mutate(nitrogenData, volumeEllipsoid = round((4/3) * pi * 
    (diameter/2) * (diameter/2) * (length/2), 1))

# Create rounded variables for diameter, both down and up
nitrogenData <- mutate(nitrogenData, diameterRoundDown = floor(diameter))
nitrogenData$diameterRoundDown <- as.factor(nitrogenData$diameterRoundDown)
nitrogenData <- mutate(nitrogenData, diameterRoundUp = ceiling(diameter))
nitrogenData$diameterRoundUp <- as.factor(nitrogenData$diameterRoundUp)

# Provide structure of transformed variables with data preview
str(nitrogenData)
```

    ## tibble [618 x 17] (S3: tbl_df/tbl/data.frame)
    ##  $ pumpkinID         : chr [1:618] "2021-N-101-Orange-1" "2021-N-102-Orange-1" "2021-N-103-Orange-1" "2021-N-104-Orange-1" ...
    ##  $ year              : Factor w/ 1 level "2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot              : Factor w/ 24 levels "2021-101","2021-102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep               : Factor w/ 4 levels "2021-1","2021-2",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ treatment         : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 2 4 5 1 ...
    ##  $ pumpkinNum        : num [1:618] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ nitrogenRate      : Factor w/ 6 levels "0 lbs/acre","120 lbs/acre",..: 1 5 6 2 3 4 5 2 3 1 ...
    ##  $ standCount        : num [1:618] 7 6 7 6 7 3 7 7 8 8 ...
    ##  $ standCountIdeal   : num [1:618] 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ standCountIdealPct: num [1:618] 0.875 0.75 0.875 0.75 0.875 0.375 0.875 0.875 1 1 ...
    ##  $ color             : Factor w/ 2 levels "Green","Orange": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ weight            : num [1:618] 30.9 24.2 31.4 24.5 16.2 29.4 23.4 20.6 31.4 26.2 ...
    ##  $ length            : num [1:618] 13.6 12.5 13.9 12.5 10.1 12.3 12.5 12 13.7 13 ...
    ##  $ diameter          : num [1:618] 15.1 13.7 14.9 13.5 11.7 14.6 12.7 12.3 14.6 13.3 ...
    ##  $ volumeEllipsoid   : num [1:618] 1624 1228 1616 1193 724 ...
    ##  $ diameterRoundDown : Factor w/ 6 levels "11","12","13",..: 5 3 4 3 1 4 2 2 4 3 ...
    ##  $ diameterRoundUp   : Factor w/ 7 levels "11","12","13",..: 6 4 5 4 2 5 3 3 5 4 ...

``` r
nitrogenData
```

    ## # A tibble: 618 x 17
    ##    pumpkinID year  plot  rep   treatment pumpkinNum nitrogenRate standCount
    ##    <chr>     <fct> <fct> <fct> <fct>          <dbl> <fct>             <dbl>
    ##  1 2021-N-1~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            7
    ##  2 2021-N-1~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           6
    ##  3 2021-N-1~ 2021  2021~ 2021~ 3                  1 80 lbs/acre           7
    ##  4 2021-N-1~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          6
    ##  5 2021-N-1~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          7
    ##  6 2021-N-1~ 2021  2021~ 2021~ 6                  1 200 lbs/acre          3
    ##  7 2021-N-2~ 2021  2021~ 2021~ 2                  1 40 lbs/acre           7
    ##  8 2021-N-2~ 2021  2021~ 2021~ 4                  1 120 lbs/acre          7
    ##  9 2021-N-2~ 2021  2021~ 2021~ 5                  1 160 lbs/acre          8
    ## 10 2021-N-2~ 2021  2021~ 2021~ 1                  1 0 lbs/acre            8
    ## # ... with 608 more rows, and 9 more variables: standCountIdeal <dbl>,
    ## #   standCountIdealPct <dbl>, color <fct>, weight <dbl>, length <dbl>,
    ## #   diameter <dbl>, volumeEllipsoid <dbl>, diameterRoundDown <fct>,
    ## #   diameterRoundUp <fct>

``` r
# Create subset with only orange pumpkins
nitrogenDataOrange <- subset(nitrogenData, color == "Orange")
```

### Leaf Composition Study

``` r
# Format variables
leafData$plot <- as.factor(leafData$plot)
leafData$nitrogenRate <- as.factor(leafData$nitrogenRate)
leafData$treatment <- as.factor(leafData$treatment)
leafData$rep <- as.factor(leafData$rep)
leafData$year <- as.factor(leafData$year)

# Ensure metrics are all numerical AND convert percentage to decimal
leafData$nitrogenPct <- as.numeric(leafData$nitrogenPct/100)
leafData$phosphorusPct <- as.numeric(leafData$phosphorusPct/100)
leafData$potassiumPct <- as.numeric(leafData$potassiumPct/100)

# Convert Excel date into a real date and set correct year for 2021
# data
leafData$date <- as.Date(as.numeric(leafData$date), origin = "1899-12-30")
year(leafData$date) <- 2021

# Arrange columns for presentation of final table for spacing data
leafData <- select(leafData, c(9, 1, 4, 3, 2, 5, 6, 7, 8))

# Provide structure of transformed variables with data preview
str(leafData)
```

    ## tibble [192 x 9] (S3: tbl_df/tbl/data.frame)
    ##  $ year         : Factor w/ 1 level "2021": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ plot         : Factor w/ 24 levels "101","102","103",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rep          : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ treatment    : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 2 4 5 1 ...
    ##  $ nitrogenRate : Factor w/ 6 levels "0 lbs/acre","120 lbs/acre",..: 1 5 6 2 3 4 5 2 3 1 ...
    ##  $ date         : Date[1:192], format: "2021-07-23" "2021-07-23" ...
    ##  $ nitrogenPct  : num [1:192] 0.0538 0.0584 0.0534 0.0576 0.0523 0.0429 0.0548 0.0544 0.0505 0.056 ...
    ##  $ phosphorusPct: num [1:192] 0.0029 0.0033 0.0028 0.0029 0.0027 0.0017 0.0031 0.003 0.0029 0.003 ...
    ##  $ potassiumPct : num [1:192] 0.0412 0.0433 0.0416 0.0368 0.037 0.0304 0.0464 0.0435 0.0428 0.0421 ...

``` r
leafData
```

    ## # A tibble: 192 x 9
    ##    year  plot  rep   treatment nitrogenRate date       nitrogenPct phosphorusPct
    ##    <fct> <fct> <fct> <fct>     <fct>        <date>           <dbl>         <dbl>
    ##  1 2021  101   1     1         0 lbs/acre   2021-07-23      0.0538       0.00290
    ##  2 2021  102   1     2         40 lbs/acre  2021-07-23      0.0584       0.0033 
    ##  3 2021  103   1     3         80 lbs/acre  2021-07-23      0.0534       0.0028 
    ##  4 2021  104   1     4         120 lbs/acre 2021-07-23      0.0576       0.00290
    ##  5 2021  105   1     5         160 lbs/acre 2021-07-23      0.0523       0.0027 
    ##  6 2021  106   1     6         200 lbs/acre 2021-07-23      0.0429       0.0017 
    ##  7 2021  201   2     2         40 lbs/acre  2021-07-23      0.0548       0.0031 
    ##  8 2021  202   2     4         120 lbs/acre 2021-07-23      0.0544       0.003  
    ##  9 2021  203   2     5         160 lbs/acre 2021-07-23      0.0505       0.00290
    ## 10 2021  204   2     1         0 lbs/acre   2021-07-23      0.0560       0.003  
    ## # ... with 182 more rows, and 1 more variable: potassiumPct <dbl>

## Save Clean Data

This project has produced several clean data sets\! An overview of their
properties and links to download them in different file formats is
outlined further below.

### Data Inventory

  - spacingData
      - 1,259 total records containing **all metrics** collected from
        the 2020 and 2021 Spacing Study  
  - nitrogenData
      - 618 total records collected from the 2021 Nitrogen Study, of
        which:
          - 618 records contain the **weight metric**  
          - 120 records contain **all metrics**
          - *Note: Since length and diameter were only recorded for a
            limited number of pumpkins, there are limited records with
            all three metrics available*  
  - leafData
      - 192 total records containing **all metrics** collected from the
        2021 Leaf Composition Study

All clean data files can be found and browsed in the [GitHub
repository](https://github.com/bsugg/PumpkinProject/tree/main/CleanData)
for this project.

Specific files of desired format (R, SAS, or Excel) can be downloaded by
clicking the appropriate links in the subsequent sections.

### R

  - [pumpkinData.RData](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/R/pumpkinData.RData)
      - Can be loaded into other R projects by setting the correct
        working directory and using the following code:
          - `load("pumpkinData.RData")`  
      - This single file contains the following data sets:
          - spacingData  
          - nitrogenData  
          - leafData

### SAS

  - [spacingdata.sas7bdat](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/SAS/spacingdata.sas7bdat)  
  - [nitrogendata.sas7bdat](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/SAS/nitrogendata.sas7bdat)  
  - [leafdata.sas7bdat](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/SAS/leafdata.sas7bdat)

### Excel

  - [spacingData.xlsx](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/Excel/spacingData.xlsx)  
  - [nitrogenData.xlsx](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/Excel/nitrogenData.xlsx)  
  - [leafData.xlsx](https://github.com/bsugg/PumpkinProject/raw/main/CleanData/Excel/leafData.xlsx)

#### Variable Descriptions

To be added…

  - spacingData  
  - nitrogenData  
  - leafData

# Data Exploration

## Explore Spacing Study

``` r
# Scatter plot of weight vs length
ggplot(spacingData, aes(x = length, y = weight, color = color)) + geom_point() + 
    scale_color_manual(values = c("dark green", "dark orange")) + labs(x = "Length (Inches)", 
    y = "Weight (Pounds)", color = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreSpacingData-1.png)<!-- -->

``` r
# Scatter plot of weight vs diameter
ggplot(spacingData, aes(x = diameter, y = weight, color = color)) + geom_point() + 
    scale_color_manual(values = c("dark green", "dark orange")) + labs(x = "Diameter (Inches)", 
    y = "Weight (Pounds)", color = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreSpacingData-2.png)<!-- -->

``` r
# Scatter plot of diameter vs length
ggplot(spacingData, aes(x = length, y = diameter, color = color)) + geom_point() + 
    scale_color_manual(values = c("dark green", "dark orange")) + labs(x = "Length (Inches)", 
    y = "Diameter (Inches)", color = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreSpacingData-3.png)<!-- -->

``` r
# Boxplot of weight by spacing dimension for orange pumpkins
ggplot(spacingDataOrange, aes(x = spacingDim, y = weight, fill = spacingDim)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Spacing Dimension (Feet)", y = "Weight (Pounds)")
```

![](README_files/figure-gfm/exploreSpacingData-4.png)<!-- -->

``` r
# Boxplot of weight by plant area for orange pumpkins
ggplot(spacingDataOrange, aes(x = plantArea, y = weight, fill = plantArea)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Plant Area (Square Feet)", y = "Weight (Pounds)")
```

![](README_files/figure-gfm/exploreSpacingData-5.png)<!-- -->

``` r
# Boxplot of volume by spacing dimension for orange pumpkins
ggplot(spacingDataOrange, aes(x = spacingDim, y = volumeEllipsoid, fill = spacingDim)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Spacing Dimension (Feet)", y = "Ellipsoid Volume (Cubic Inches)")
```

![](README_files/figure-gfm/exploreSpacingData-6.png)<!-- -->

``` r
# Boxplot of volume by plant area for orange pumpkins
ggplot(spacingDataOrange, aes(x = plantArea, y = volumeEllipsoid, fill = plantArea)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Plant Area (Square Feet)", y = "Ellipsoid Volume (Cubic Inches)")
```

![](README_files/figure-gfm/exploreSpacingData-7.png)<!-- -->

``` r
# Bar chart of count by spacing dimension for orange pumpkins
ggplot(spacingDataOrange, aes(x = spacingDim, fill = spacingDim)) + geom_bar(stat = "count") + 
    theme(legend.position = "none") + labs(x = "Spacing Dimension (Feet)", 
    y = "Orange Pumpkin Count")
```

![](README_files/figure-gfm/exploreSpacingData-8.png)<!-- -->

``` r
# Bar chart of count by plant area for orange pumpkins
ggplot(spacingDataOrange, aes(x = plantArea, fill = plantArea)) + geom_bar(stat = "count") + 
    theme(legend.position = "none") + labs(x = "Plant Area (Square Feet)", 
    y = "Orange Pumpkin Count")
```

![](README_files/figure-gfm/exploreSpacingData-9.png)<!-- -->

``` r
# Bar chart of color count by spacing dimension
ggplot(spacingData, aes(x = spacingDim, fill = color)) + geom_bar(stat = "count", 
    position = "fill") + scale_fill_manual(values = c("dark green", "dark orange")) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Spacing Dimension (Feet)", y = "Pumpkin Count %", fill = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreSpacingData-10.png)<!-- -->

``` r
# Bar chart of color count by plant area
ggplot(spacingData, aes(x = plantArea, fill = color)) + geom_bar(stat = "count", 
    position = "fill") + scale_fill_manual(values = c("dark green", "dark orange")) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Plant Area (Square Feet)", y = "Pumpkin Count %", fill = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreSpacingData-11.png)<!-- -->

## Explore Nitrogen Study

``` r
# Scatter plot of weight vs length
ggplot(nitrogenData, aes(x = length, y = weight, color = color)) + geom_point() + 
    scale_color_manual(values = c("dark green", "dark orange")) + labs(x = "Length (Inches)", 
    y = "Weight (Pounds)", color = "Pumpkin Color")
```

    ## Warning: Removed 498 rows containing missing values (geom_point).

![](README_files/figure-gfm/exploreNitrogenData-1.png)<!-- -->

``` r
# Scatter plot of weight vs diameter
ggplot(nitrogenData, aes(x = diameter, y = weight, color = color)) + geom_point() + 
    scale_color_manual(values = c("dark green", "dark orange")) + labs(x = "Diameter (Inches)", 
    y = "Weight (Pounds)", color = "Pumpkin Color")
```

    ## Warning: Removed 498 rows containing missing values (geom_point).

![](README_files/figure-gfm/exploreNitrogenData-2.png)<!-- -->

``` r
# Box plot of weight vs treatment level for orange pumpkins
ggplot(nitrogenDataOrange, aes(x = treatment, y = weight, fill = treatment)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Treatment", y = "Weight (Pounds)")
```

![](README_files/figure-gfm/exploreNitrogenData-3.png)<!-- -->

``` r
# Box plot of volume vs treatment level for orange pumpkins
ggplot(nitrogenDataOrange, aes(x = treatment, y = volumeEllipsoid, fill = treatment)) + 
    geom_boxplot(varwidth = TRUE, alpha = 0.2) + theme(legend.position = "none") + 
    labs(x = "Treatment", y = "Volume Ellipsoid (Cubic Inches)")
```

    ## Warning: Removed 98 rows containing non-finite values (stat_boxplot).

![](README_files/figure-gfm/exploreNitrogenData-4.png)<!-- -->

``` r
# Bar chart of count by spacing dimension for orange pumpkins
ggplot(nitrogenDataOrange, aes(x = treatment, fill = treatment)) + geom_bar(stat = "count") + 
    theme(legend.position = "none") + labs(x = "Treatment", y = "Orange Pumpkin Count")
```

![](README_files/figure-gfm/exploreNitrogenData-5.png)<!-- -->

``` r
# Bar chart of color count by nitrogen treatment level
ggplot(nitrogenData, aes(x = treatment, fill = color)) + geom_bar(stat = "count", 
    position = "fill") + scale_fill_manual(values = c("dark green", "dark orange")) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Treatment", y = "Pumpkin Count %", fill = "Pumpkin Color")
```

![](README_files/figure-gfm/exploreNitrogenData-6.png)<!-- -->

## Explore Leaf Study

Associated fertilizer application levels for each treatment:

  - Treatment 1: 0 lbs/acre  
  - Treatment 2: 40 lbs/acre  
  - Treatment 3: 80 lbs/acre  
  - Treatment 4: 120 lbs/acre  
  - Treatment 5: 160 lbs/acre  
  - Treatment 6: 200 lbs/acre

<!-- end list -->

``` r
# Line chart by treatment level over time for Nitrogen
ggplot(data = leafData, aes(x = date, y = nitrogenPct, color = treatment)) + 
    geom_line() + scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Collection Date", y = "Nitrogen %", col = "Treatment")
```

![](README_files/figure-gfm/exploreLeafData-1.png)<!-- -->

``` r
# Line chart by treatment level over time for Phosphorus
ggplot(data = leafData, aes(x = date, y = phosphorusPct, color = treatment)) + 
    geom_line() + scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Collection Date", y = "Phosphorus %", col = "Treatment")
```

![](README_files/figure-gfm/exploreLeafData-2.png)<!-- -->

``` r
# Line chart by treatment level over time for Potassium
ggplot(data = leafData, aes(x = date, y = potassiumPct, color = treatment)) + 
    geom_line() + scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(x = "Collection Date", y = "Potassium %", col = "Treatment")
```

![](README_files/figure-gfm/exploreLeafData-3.png)<!-- -->

# Analysis

The analysis portion is covered in the corresponding [R Shiny
App](https://bsugg.shinyapps.io/ShinyPumpkinProject/) for this project
under the *Analysis* menu item. Other graphs and visuals are also
available in the *Data Exploration* section of this app.

![](README_files/figure-gfm/spacingANOVA-1.png)<!-- -->![](README_files/figure-gfm/spacingANOVA-2.png)<!-- -->![](README_files/figure-gfm/spacingANOVA-3.png)<!-- -->![](README_files/figure-gfm/spacingANOVA-4.png)<!-- -->

![](README_files/figure-gfm/nitrogenANOVA-1.png)<!-- -->![](README_files/figure-gfm/nitrogenANOVA-2.png)<!-- -->![](README_files/figure-gfm/nitrogenANOVA-3.png)<!-- -->![](README_files/figure-gfm/nitrogenANOVA-4.png)<!-- -->

# Conclusions

From the comparison of means using Tukey-Kramer HSD and the Student’s
t-test, we found when applying different plant spacing areas and
between-row distances on pumpkin plants, the resulting volume and
quantity of harvested pumpkins would be significantly different,
although the quantity didn’t differ much between spacing areas of 30
sqft and 40 sqft.

In terms of our model assumptions, the provided qq-plots showed the
entire population follows an approximately normal distribution. The
equal variance assumption was not violated since the ratios of the
largest variance to the smallest variance were all less than 3. The only
exception occurred when analyzing the quantity where the treatment
combination of spacing area 10 sqft and between-row distance 5 ft had a
variance that was much larger than other groups. This single large
variance might be caused by some unknown factors in the experiment that
should be investigated further.

The variances of all the other groups were similar, so we accepted the
model assumptions and proceeded with our analysis using a two-way ANOVA
with interaction. The analysis indicated the interaction effect was not
a significant factor on the means of pumpkin volume or quantity. After
finding the best polynomial model, we discovered both models were
similar in terms, but with different corresponding coefficient values.

Both of the fitted models had a large F ratio which means the variation
among group means is pretty large, as seen in the graphs. The low
adjusted R-square value in the volume model indicated that the
independent variables do not explain much of the variation in volume,
even though there is a significant effect from the two factors. We
suspect there may exist some lurking variables from the research study
influencing our model fit, which can be investigated further with our
client.

When the plant spacing area increases, pumpkin quantity decreases and
pumpkin volume increases. If we choose a between-row distance of 5 ft
instead of 10 ft, the volume and quantity of pumpkins would increase in
size and amount across the four different plant spacing areas. Overall,
we conclude both the plant spacing area and between-row distance are
statistically significant in controlling the volume and quantity of
pumpkins. Plants grown closer together will produce more pumpkins and
have a higher overall yield, confirming our client’s hypothesis.
