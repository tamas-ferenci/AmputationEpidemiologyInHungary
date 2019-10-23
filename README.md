# AmputationEpidemiologyInHungary
Epidemiology of amputations due to peripheral vascular disease in Hungary

Scripts included:
- Parse raw input: [AmputationEpidemiology_Parser.R](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/AmputationEpidemiology_Parser.R)
- Spatial analysis: [AmputationEpidemiology_Spatial.R](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/AmputationEpidemiology_Spatial.R)
- Automated epidemiology report at county level: [AmputationEpidemiologyCountyLevel.R](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/AmputationEpidemiologyCountyLevel.R) and [AmputationEpidemiologyCountyLevel_Report.Rmd](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/AmputationEpidemiologyCountyLevel_Report.Rmd)

Necessary supplementary material includes the Hungarian population pyramid ([NUTS3](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/PopPyramid_5YR_NUTS3_20042014.csv)
and [LAU1](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/PopPyramid_5YR_LAU1_20152017.csv)), obtained with the [KSHStatinfoScraper](https://github.com/tamas-ferenci/KSHStatinfoScraper) package,
the ESP2013 standard population ([ESP2013.csv](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/ESP2013.csv)), the Hungarian NUTS/LAU associations ([LAU1NUTS3NUTS2NUTS1.csv](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/LAU1NUTS3NUTS2NUTS1.csv))
and the Hungarian ZIP-code/settlement associations ([IrszHnk.csv](https://github.com/tamas-ferenci/AmputationEpidemiologyInHungary/blob/master/IrszHnk.csv),
as documented in [https://github.com/tamas-ferenci/IrszHnk](https://github.com/tamas-ferenci/IrszHnk)).