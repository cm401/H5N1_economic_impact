# The impact of H5N1 on US domestic and international dairy markets

## Abstract

The recent outbreak of H5N1 avian influenza in U.S. dairy cattle poses significant risks to public health, economic sustainability of farming,  and global food systems. Using a Computable General Equilibrium model, we assess short-to-medium-term impacts on GDP, trade, and employment under varying scenarios. We project economic losses ranging from $14bn to $164bn. Current government subsidies are about 1% of output losses, and likely insufficient to incentivise farmers to step up surveillance and biosecurity for mitigating the potential emergence of H5N1 strains with pandemic potential into human populations. 


## GTAP Model



## Post processing of GTAP model output

The output of the GTAP model is post-processed using R and the functions in `R\parse_GTAP_output.R`. We provide the model output in the `data` folder to reproduce Figure 2.

![**Figure 2: Projections of the economic impact of the H5N1 outbreak, % deviation from pre-outbreak values.** Panel A shows assumed shocks by sector and scenario (S1 ‘current situation’ green, S2 ‘realistic scenario’ blue, S3’reasonable worst case’ red), see table A4; Panel B shows aggregate shocks on prices, consumption, GDP, and exports (table A5); Panel C shows sectoral shocks on prices, trade balance (positive: increase in export demand, negative: decrease in export demand), employment, investments and sectoral outputs (tables A6-A10); shown are point estimates for S1 and S2, and interquartile and total ranges of estimates for S3.](Figure_2.png)
