# The impact of H5N1 on US domestic and international dairy markets (23Oct2025)

## Abstract

The outbreak of Highly Pathogenic Avian Influenza H5N1 in U.S. dairy cattle poses substantial risks to public health, economic sustainability of farming, and global food systems. Using a Computable General Equilibrium model, we simulate its short- to medium-term impacts on Gross Domestic Product and other macro-economic outcomes for the US and its main trading partners. We simulate impacts under the current situation and realistic and reasonable worst-case scenarios. We estimate domestic economic losses ranging between 0.06% and 0.9% of US GDP, with losses to the dairy sector ranging between 3.4% and 20.6%. Trading partners increase dairy production to compensate for the loss. Current government subsidies are about 1.2% (95% HDI: 1% to 1.4%) of output losses, and likely insufficient to incentivise farmers to step up surveillance and biosecurity for mitigating the possible emergence of H5N1 strains with pandemic potential into human populations.


## GTAP Model

We use the Global Trade Analysis Project (GTAP) model for this analysis. For more information, see GTAP website (https://www.gtap.agecon.purdue.edu/). This is a multi-region and multi-sector Computable General Equilibrium (CGE) model. We selected the GTAP model because it provides a more detailed description of the agricultural sector compared to other CGE models.

CGE models are economic tools that use real economic data to estimate how an economy might respond to changes in policy, technology, or external events, such as infectious disease outbreaks. They are also widely used to analyze the economic impacts of policy changes, including trade policies, tax reforms, and environmental regulations. This is a static comparative approach we aim to understand the imapct of H5N1 without describing the adjustement process.


## Replication process

To achieve full replication, please refer to the zip file, which contains all the necessary information to completely replicate the paper.

Below are the steps to replicate our results. Only a basic understanding of GTAP is required. You can refer to Burfisher (2021) book "Introduction to Computable General Equilibrium Models" for a great introduction to RunGTAP. After downloading and setting up RunGTAP and related software, you can implement our replication package (replicationH5N1_pub.zip) using the GTAP model and RunGTAP. Please follow these steps:

1-Use GTAPAgg2 to create an aggregation similar to the one displayed at Aggregation file, section named "agg9x10". You can input "agg9x10" directly via "Read aggregation scheme from file" and then click "Create aggregated database".

2-Update the version of RunGTAP to reflect this new aggregation by clicking on "Version" and selecting "New". Our modules are the same as Burfisher (2021) (GTAPUV7, etc.) and can be found in Aggregation-> module and global settings. Now a test simulation will run. 

3-Copy, paste, or import (via Load) each scenario file (found in the scenario directory and GTAPRUN.xlsx) into the Shock tab. Don't forget to implement the "swap" in the closure, which can be found in the .tex file in the scenario directory. Finally, run each scenario one by one. Make sure to select the correct solution method, Gragg, in the Solve tab, then click Solve.

4-Explore the results of each scenario individually using the Results tab, and compare them with our .xlsx files in the Data directory, which contain all our results.

5-To run a sensitivity analysis (SA), go to Tools → Sensitivity → wrt to shock or parameter (depending on the goal). Enter the scenarios displayed in GTAPRUN.xlsx or the SA files in the SA directory. When reproducing the results, remember that for implementing the HDI, we use two times the standard deviation on each side of the mean value.


## Post processing of GTAP model output

The output of the GTAP model is post-processed using R and the functions in `R\parse_GTAP_output.R`.  The model output data are display in the `Data_Results` folder. 


## Figures
[View Figure2.pdf](https://github.com/user-attachments/files/23093060/Figure2.pdf)
**Figure 2. Potential Impact of H5N1 outbreak on GDP and Output Quantities Across Countries** Note: Figure 2 shows GDP and output quantities in response to H5N1, expressed as \% deviation from pre-outbreak values (Supplementary Tables 4, and 7); shown in Panel B are point estimates, interquartile and total ranges of estimates for S2 and S3.

[View Figure3.pdf](https://github.com/user-attachments/files/23093058/Figure3.pdf) 
**Figure 3. Potential Aggregate Economic Impacts of H5N1 outbreak on Prices, Consumption, Investment, and Exports** Note: Figure 3 shows aggregate shocks on prices, consumption, investment, and exports in response to H5N1, expressed as \% deviation from pre-outbreak values (Supplementary Table 4).

[View Figure4.pdf](https://github.com/user-attachments/files/23093059/Figure4.pdf)
**Figure 4. Potential Sectoral Economic Impacts of H5N1 outbreak on Prices, Exported Quantities, Employment, and Investments** Note: Figure 4 shows sectoral shocks on prices, exported quantities, employment, and investments in response to H5N1, expressed as \% deviation from pre-outbreak values (Supplementary Tables 5, 6, 8, and 9); shown are point estimates, interquartile and total ranges of estimates for S2 and S3.
