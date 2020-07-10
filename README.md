# NYS-Chemical-Spills

This is demonstration of the use of R Shiny for visualization and analysis of data on [chemical storage](https://data.ny.gov/Energy-Environment/Bulk-Storage-Facilities-in-New-York-State/pteg-c78n) 
and [spill incidents](https://data.ny.gov/Energy-Environment/Spill-Incidents/u44d-k5fk) in NYS State.

The interactive app can be viewed [here](https://datasnuz.shinyapps.io/NYS_Chemical_Spills/).

![pic](https://github.com/snuzbrokh/NYS-Chemical-Spills/blob/master/nyc_allspills.png)

The source and active maintainer of both datasets is the New York State Department of Environmental Conservation (DEC). 

## App

### Interactive Map
The features a custom UI that encourages user exploration and investigation of chemical spills that have occurred in NY State. Each spill is drawn with a circle whose circle grows logarithmically with the size of the spill. Spills and linked facilities can be selected by the material chemical involved and stored respectively. Spills can also be filtered by size and year of occurence. Hovering over a facility or a spill will bring up useful summary information. In addition, facilities can be grouped and toggled based on their status: closed, inactive, or active.

### Analysis
The analysis tab showcases a summary per DEC region, County, and Material Family of total chemical spills by volume. Significant for policymakers - the "Spill Sources" tab breaks down the total spills for the particular set of counties by source. Certain area in NY state with a lot of industrial activity will have a higher percentage of their spills coming from Industrial and Storage sources compared to a rural region - where most spills are from Vehicles or Municipal sources. 

### DEC Responsiveness
This tab shows the "Case Lag" per DEC Region and County. Case Lag is the amount of time from when a spill is reported to the DEC Regional office to the time a case is closed. Closing a case entails processing the spill, organizing a cleanup, and administration of repopening of the contaminated site. 
The other chart under "Worst Offenders by Spills" lists out each set of counties worst spillers. Worst in this case measured in the total volume of chemical spilled. 


## Spill Data:
The dataset contains records of spills of petroleum, commodity chemicals, petrochemicals, and other hazardous materials. 
Under State law and regulations, spills with the potential of pollution of public lands or waters must be reported by the spiller
or anyone who has knowledge of the incident. Such incidents occur frequently in New York State. 


Every year the DEC receives about 16,000 reports of confirmed and suspected chemical releases into the environment.
Approximately 90% of those involve petroleum products. The rest involve hazardous substances, unknown materials, or
other materials such as untreated sewage and - in a few cases from a Tonawanda funeral home - human remains. 

Environmental damage from such releases depends on the material spilled and the extent of contamination. Many of these
reports are release of small quantities, typically a few gallons, that are contained and cleaned up quickly. In other instances
material releases from tanks - aboveground and underground - can seep through soil, eventually to groudwater and can make
water supplied unsafe to drink. Vapors from spilled materials can collect in homes and workplaces, creating fire and explosion hazards.
Uncontained spills, especially those impacting surface water, can kill or injure plants, fish, and wildlife, and damage their habitats. 


## Bulk Storage Data
Given the magnitude of impact spills of large quantities of petroleum or hazardous chemicals can have on our shared environment,
I decided to combine this dataset with data on public bulk storage facilities in NY State.

