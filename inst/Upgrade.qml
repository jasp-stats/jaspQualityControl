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

// Type 2 and 3 Gauge r&R

Upgrade
	{
		functionName:		"msaGaugeRR"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "gaugeRRdataFormat";								to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "gaugeRRlongFormat":						return "longFormat";
					case "gaugeRRwideFormat":						return "wideFormat";
				}
			}
		}

		ChangeRename {from: "operators";								to: "operator"}
		ChangeRename {from: "parts";									to: "part"}
		ChangeRename {from: "measurementsLong";							to: "measurementLongFormat"}
		ChangeRename {from: "measurements";								to: "measurementsWideFormat"}
		ChangeRename {from: "tolerance";								to: "toleranceValue"}
		ChangeRename {from: "gaugeToleranceEnabled";					to: "tolerance"}
		ChangeRename {from: "gaugeANOVA";								to: "anova"}
		ChangeRename {from: "TypeForFstat";								to: "anovaModelType"}

		ChangeJS
		{
			name:		"anovaModelType"
			jsFunction:	function(options)
			{
				switch(options["anovaModelType"])
				{
					case "FixedEffects":							return "fixedEffect";
					case "RandomEffects":							return "randomEffect";
				}
			}
		}
		
		ChangeRename {from: "alphaForANOVA";							to: "anovaAlphaForInteractionRemoval"}
		ChangeRename {from: "studyVarMultiplierType";					to: "studyVarianceMultiplierType"}

		ChangeJS
		{
			name:		"studyVarMultiplierType"
			jsFunction:	function(options)
			{
				switch(options["studyVarMultiplierType"])
				{
					case "svmSD":									return "sd";
					case "svmPercent":								return "percent";
				}
			}
		}

		ChangeRename {from: "studyVarMultiplier";					to: "studyVarianceMultiplierValue"}



		
		
		


		
		
		
		

		// plots

		// report
	}
