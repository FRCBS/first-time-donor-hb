## How to run the code

### Parameter settings

Some parameters must be set for the code to run correctly. The parameters 
are saved in the **param** list. The parameters are set in the **parameters** block
below. The parameters are:

- wd: The working directory to be used. This is set by Rstudio to either the **src**
or the root folder of the project, e.g., "C:/git_repot/first-time-donor-hb".
The correct setting is the project root. The code in the block checks if the 
**src** folder has been selected, and changes the folder to project root if yes.
- result.file: where the anonymised results will be exported. By default this
is in the **results** subfolder in file named **exported-data.xlsx**. This is computed
by the code and needs not be changed.
- country: two-letter identifier for the country, e.g., FI. **Should be changed.**
- age.minimum, age.maximum: The minimum and maximum ages, respectively, at the
time of donation that are included in the exported data.
- hb.minimum, hb.maximum: Similarly as above, but for Hb values. The default
values are -Inf and Inf, respectively, meaning that all Hb non-NA/NULL Hb values 
are included. 
- include.na: Should donations with Hb value unavailable (NA) included or not.
Default is yes (**TRUE**).
- minimum.group.size: The smallest group size (*n*) that is exported.
- donation.type.keys: Which values in **donationdata$donation$BloodDonationTypeKey**
are included in the analysis. The idea is to consider full blood donations, 
either successful or deferred cases.
- hb.decimals: The number of decimals in Hb values that should be used.
- donationdata: Path to the .Rdata file containing the data to be used. This is
hard-coded by default to **donationdata.Rdata** in the working directory.
- units: HB units to be used: Shold be one of: mmol/L, or g/L or g/dL.
- cutoff.male, cutoff.female: The required mimimum values to donate for males 
and females, respectively. These **must be set** and expressed in **units**.
- data.set: Which data sets should be summarised and exported. The default is
c('donation0','donation.r','simple'), which correspond to (1) first time donations 
(first mention of releaseID in the **donation** data set); (2) repeat donations, 
i.e., other than the first mention; and (3) all donations. The primary interest 
lies in the first donations, but other data sets are included by default for 
completeness and to enable additional analyses.
- extractHour: A function to extract the hour from **donationdata**. The default 
provided works when the time information is included in **DonationTimeDTTM** 
column in **donationdata$donation** in POSIXct format. The function
can be adjusted accordingnly.
- donation.cols, donor.cols: Some columns maybe have multiple sources in
donationdata. These settings are vectors that specify the columns that are to be
copied from donationdata$donation and donationdata$donor, respectively. 
The default values are **c()** (no columns) and **c('DateOfBirth','Sex','BloodGroup'),**
again, respectively. These values correspond to the **data-description.xlsx** file.
That is, by default the date of birth, sex and blood group
are copied from the donor data to the working data set. The values can be copied
from donor.cols to donation.cols to alter the behaviour. In addition, column(s) 
that is used to compute the hour of the donation with the day, should be added
to **donation.cols**, as is done country-specifically under **NL** and **FI**.

## Exporting the data

To export the data, code should be run from the start of this file 
(** main-with-export.Rmd**) up to and including the chunk **export-data**. 
This includes setting the parameters (described) above, loading and processing 
the data, extracting anonymous statistics and writing these to the 
**exported-data.xlsx** file as described above.

The statistics are anonymised by removing any groups with size smaller then 
the parameter **minimum.group.size** (default 5). For Hb, small and large Hb
values with fewer observations than the limit are pooled together and inxluded
in the exported data using specific placeholders (-1000000 and 1000000). 
