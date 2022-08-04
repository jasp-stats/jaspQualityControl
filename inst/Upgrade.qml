// option renaming for syntax

// Type 1 Gauge Study

Upgrade
	{
		functionName:		"msaType1Gauge"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "measurements";								to: "measurement"}
		ChangeRename {from: "biasReferenceValue";						to: "referenceValue"}
		ChangeRename {from: "biasTolerance";							to: "toleranceRange"}
		ChangeRename {from: "biasPercentCG";							to: "percentToleranceForCg"}
		ChangeRename {from: "BiasStudyVarMultiplier";					to: "studyVarianceMultiplier"}
		ChangeRename {from: "biasTtest";								to: "tTest"}
		ChangeRename {from: "biasTtestConfidenceIntervalPercent";		to: "tTestCiLevel"}

		// plots
		ChangeRename {from: "biasRun";									to: "runChart"}
		ChangeRename {from: "biasRunDots";								to: "runChartIndividualMeasurementDots"}
		ChangeRename {from: "biasRunTolLims";							to: "runChartToleranceLimitLines"}
		ChangeRename {from: "biasHistogram";							to: "histogram"}
		ChangeRename {from: "biasBinWidthType";							to: "histogramBinWidthType"}

		ChangeJS
		{
			name:		"histogramBinWidthType"
			jsFunction:	function(options)
			{
				switch(options["histogramBinWidthType"])
				{
					case "fd":						return "freedman-diaconis";
				}
			}
		}

		ChangeRename {from: "biasNumberOfBins";							to: "histogramManualNumberOfBins"}
		ChangeRename {from: "biasHistMean";								to: "histogramMeanLine"}
		ChangeRename {from: "biasHistMeanConfidenceInterval";			to: "histogramMeanCi"}
		ChangeRename {from: "biasHistMeanConfidenceIntervalPercent";	to: "histogramMeanCiLevel"}
		ChangeRename {from: "biasHistRef";								to: "histogramReferenceValueLine"}
	}

// Linearity Study

Upgrade
	{
		functionName:		"msaGaugeLinearity"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "parts";								to: "part"}
		ChangeRename {from: "measurements";							to: "measurement"}
		ChangeRename {from: "LBtableLinearity";						to: "linearityTable"}
		ChangeRename {from: "LBtableBias";							to: "biasTable"}

		// plots
		ChangeRename {from: "LBgraph";								to: "linearityAndBiasPlot"}
		ChangeRename {from: "LBpercentGraph";						to: "percentageOfProcessVariationPlot"}

		// optional
		ChangeRename {from: "EnablePV";								to: "manualProcessVariation"}
		ChangeRename {from: "linearityProcessVariation";			to: "manualProcessVariationValue"}
	}
