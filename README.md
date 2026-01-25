## How to run the code

### File to run

The code necessary for extracting and exporting anonymised data can be found in
[src/export-data.r](src/export-data.r).

### Parameter settings

Some parameters must be set for the code to run correctly. The parameters 
are saved in the **param** list. Parameters that typically change by country 
are organised in blocks, see. e.g. under `if (param$country == 'NL')`: parameters
for NL and FI are provided for reference. You can edit the block under 
`if (param$country == 'xx')` locally.

The parameters are:

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
are included in the hemoglobin analysis. The idea is to consider full blood donations, 
either successful or deferred cases. The default is `c('Whole Blood (K)','No Donation (E)','VisitNoDonation')`
- donation.type.keys.survival: Which values in **donationdata$donation$BloodDonationTypeKey**
are included in the **survival analysis**. **NB! Only successful full blood donations should be
included here.** The default is "Whole Blood (K)".
- hb.decimals: The number of decimals in Hb values that should be used.
- donationdata: Path to the .Rdata file containing the data to be used. This is
hard-coded by default to **donationdata.Rdata** in the working directory. If the 
donationdata object is already in the memory, this value can be left undefined and
the line *load(param$donationdata)* be commented out.
- units: HB units to be used: Shold be one of: mmol/L, or g/L or g/dL.
- cutoff.male, cutoff.female: The required mimimum values to donate for males 
and females, respectively. These **must be set** and expressed in **units**.
- data.set: Which data sets should be summarised and exported. The default is
c('donation0','donation.r','simple'), which correspond to (1) first time donations 
(first mention of releaseID in the **donation** data set); (2) repeat donations, 
i.e., other than the first mention; and (3) all donations. The primary interest 
lies in the first donations, but other data sets are included by default for 
completeness and to enable additional analyses.
- extractHour: A function to extract the hour from **donationdata**. By default,
the function always returns 0 and no hourly data is exported. Under the country-specific
paramters two options are included: the function defined under `param$country == 'FI'`, 
works when the time information is included in **DonationTimeDTTM** 
column in **donationdata$donation** in POSIXct format. The function
can be adjusted accordingly.
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
- max.ord.group.number: the maximum number (ord) of donation that are considered as a separate group
in survival analysis; the rest are estimated as a single group. The default is 15.
- max.sample.size: the maximum number of sample (rows) used when estimated the Cox regressions
in survival analysis. Default is 10e7. Can be decreased to improve performance.
- omit.data: a list (of key-value pairs) containing description of donations that should be omitted.
The *key* identifies the column in donationdata and *value* the value that should be omitted. For an
example, see under the parameters specific to Finland.

The hemoglobin statistics are anonymised by removing any groups with size smaller then 
the parameter **minimum.group.size** (default 5). For Hb, small and large Hb
values with fewer observations than the limit are pooled together and inxluded
in the exported data using specific placeholders (-1000000 and 1000000). 

## Exporting the data

To export the data, the file **src/export-data.r** should be run. 
This includes setting the parameters (described) above. When the code is run, 
it loads and processes the data, and extracts anonymous statistics and writes these to the 
**exported-data-hb.xlsx** and **exported-data-survival.xlsx** files. The latter file may be 
split into parts of maximum 100,000 lines of survival curves, with the second etc. file 
identified by a running number before the extension, eg. **exported-data-survival-2.xlsx**. This
is done to make individual files small enough to be sent as an email attachment. Please be sure to
send all the .xlsx-files.

## Post-export plotting

The file **src/post-export-plotting.r** can be run (with the objects from exporting the data still in memory)
to produce some visualisations of the data. The visualisations are saved as PDF files in the **results** directory, ie.
the same directory in which the .xlsx-files are written.
