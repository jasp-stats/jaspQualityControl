import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"doeFull"
		newFunctionName: "doeResponseSurfaceMethodology"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

	}

	Upgrade
	{
		functionName:	"doeAnalysis"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

		ChangeRename { from: "FAallVariables";										to: "allVariables"					}
		ChangeRename { from: "FAresponse";											to: "dependent"						}
		ChangeRename { from: "FAassignedFactors";									to: "fixedFactors"					}
		ChangeRename { from: "FAblocks";											to: "blocks"						}
		ChangeRename { from: "enabledIntOrder ";									to: "highestOrder"					}
		ChangeRename { from: "intOrder";											to: "order"							}
	}

	Upgrade
	{
		functionName:	"variablesChartsIndividuals"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

		ChangeRename { from: "ncol";												to: "movingRangeLength"				}
	}
}

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
					case "fd":											return "freedmanDiaconis";
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
		ChangeRename {from: "parts";									to: "part"}
		ChangeRename {from: "measurements";								to: "measurement"}
		ChangeRename {from: "LBtableLinearity";							to: "linearityTable"}
		ChangeRename {from: "LBtableBias";								to: "biasTable"}

		// plots
		ChangeRename {from: "LBgraph";									to: "linearityAndBiasPlot"}
		ChangeRename {from: "LBpercentGraph";							to: "percentProcessVariationPlot"}

		// optional
		ChangeRename {from: "EnablePV";									to: "manualProcessVariation"}
		ChangeRename {from: "linearityProcessVariation";				to: "manualProcessVariationValue"}
	}

// Type 2 and 3 Gauge r&R

Upgrade
	{
		functionName:		"msaGaugeRR"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "gaugeRRdataFormat";						to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "gaugeRRlongFormat":							return "longFormat";
					case "gaugeRRwideFormat":							return "wideFormat";
				}
			}
		}

		ChangeRename {from: "operators";								to: "operator"}
		ChangeRename {from: "parts";									to: "part"}
		ChangeRename {from: "measurementsLong";							to: "measurementLongFormat"}
		ChangeRename {from: "measurements";								to: "measurementsWideFormat"}
		ChangeRename {from: "Type3";									to: "type3"}
		ChangeRename {from: "standardDeviationReference";				to: "processVariationReference"}

		ChangeJS
		{
			name:		"processVariationReference"
			jsFunction:	function(options)
			{
				switch(options["processVariationReference"])
				{
					case "studyStandardDeviation":						return "studySd";
					case "historicalStandardDeviation":					return "historicalSd";
				}
			}
		}
		
		ChangeRename {from: "historicalStandardDeviationValue";			to: "historicalSdValue"}
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
					case "FixedEffects":								return "fixedEffect";
					case "RandomEffects":								return "randomEffect";
				}
			}
		}
		
		ChangeRename {from: "alphaForANOVA";							to: "anovaAlphaForInteractionRemoval"}
		ChangeRename {from: "studyVarMultiplierType";					to: "studyVarianceMultiplierType"}

		ChangeJS
		{
			name:		"studyVarianceMultiplierType"
			jsFunction:	function(options)
			{
				switch(options["studyVarianceMultiplierType"])
				{
					case "svmSD":										return "sd";
					case "svmPercent":									return "percent";
				}
			}
		}

		ChangeRename {from: "studyVarMultiplier";						to: "studyVarianceMultiplierValue"}

		// plots
		ChangeRename {from: "gaugeVarCompGraph";						to: "varianceComponentsGraph"}
		ChangeRename {from: "gaugeRchart";								to: "rChart"}
		ChangeRename {from: "gaugeXbarChart";							to: "xBarChart"}
		ChangeRename {from: "gaugeScatterPlotOperators";				to: "scatterPlot"}
		ChangeRename {from: "gaugeScatterPlotFitLine";					to: "scatterPlotFitLine"}
		ChangeRename {from: "gaugeScatterPlotOriginLine";				to: "scatterPlotOriginLine"}
		ChangeRename {from: "gaugeByPart";								to: "partMeasurementPlot"}
		ChangeRename {from: "gaugeByPartAll";							to: "partMeasurementPlotAllValues"}
		ChangeRename {from: "gaugeByOperator";							to: "operatorMeasurementPlot"}
		ChangeRename {from: "gaugeByInteraction";						to: "partByOperatorMeasurementPlot"}
		ChangeRename {from: "trafficPlot";								to: "trafficLightChart"}

		// report
		ChangeRename {from: "anovaGaugeReport";							to: "report"}
		ChangeRename {from: "anovaGaugeTitle";							to: "reportTitle"}
		ChangeRename {from: "anovaGaugeName";							to: "reportGaugeName"}
		ChangeRename {from: "anovaGaugeDate";							to: "reportDate"}
		ChangeRename {from: "anovaGaugeReportedBy";						to: "reportReportedBy"}
		ChangeRename {from: "anovaGaugeMisc";							to: "reportMiscellaneous"}
	}

	// Gauge r&R (non-replicable)

	Upgrade
	{
		functionName:		"msaGaugeRRnonrep"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "gaugeRRNonRepDataFormat";					to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "gaugeRRNonRepLongFormat":						return "longFormat";
					case "gaugeRRNonRepWideFormat":						return "wideFormat";
				}
			}
		}

		ChangeRename {from: "operators";								to: "operator"}
		ChangeRename {from: "parts";									to: "part"}
		ChangeRename {from: "measurements";								to: "measurementLongFormat"}
		ChangeRename {from: "measurementsWide";							to: "measurementsWideFormat"}
		ChangeRename {from: "NRstandardDeviationReference";				to: "processVariationReference"}
		ChangeJS
		{
			name:		"processVariationReference"
			jsFunction:	function(options)
			{
				switch(options["processVariationReference"])
				{
					case "studyStandardDeviation":						return "studySd";
					case "historicalStandardDeviation":					return "historicalSd";
				}
			}
		}
		
		ChangeRename {from: "NRhistoricalStandardDeviationValue";		to: "historicalSdValue"}
		ChangeRename {from: "gaugeNRToleranceEnabled";					to: "tolerance"}
		ChangeRename {from: "NRtolerance";								to: "toleranceValue"}
		ChangeRename {from: "NRgaugeRR";								to: "anova"}
		ChangeRename {from: "NRstudyVarMultiplierType";					to: "studyVarianceMultiplierType"}

		ChangeJS
		{
			name:		"studyVarianceMultiplierType"
			jsFunction:	function(options)
			{
				switch(options["studyVarianceMultiplierType"])
				{
					case "svmSD":										return "sd";
					case "svmPercent":									return "percent";
				}
			}
		}

		ChangeRename {from: "NRstudyVarMultiplier";						to: "studyVarianceMultiplierValue"}

		// plots
		ChangeRename {from: "NRgaugeVarCompGraph";						to: "varianceComponentsGraph"}
		ChangeRename {from: "NRrCharts";								to: "rChart"}
		ChangeRename {from: "NRxbarCharts";								to: "xBarChart"}
		ChangeRename {from: "NRpartOperatorGraph";						to: "partMeasurementPlot"}
		ChangeRename {from: "NRpartOperatorGraphAll";					to: "partMeasurementPlotAllValues"}
		ChangeRename {from: "NRoperatorGraph";							to: "operatorMeasurementPlot"}

		// report
		ChangeRename {from: "anovaGaugeNestedReport";					to: "report"}
		ChangeRename {from: "anovaGaugeNestedTitle";					to: "reportTitle"}
		ChangeRename {from: "anovaGaugeNestedName";						to: "reportName"}
		ChangeRename {from: "anovaGaugeNestedDate";						to: "reportDate"}
		ChangeRename {from: "anovaGaugeNestedReportedBy";				to: "reportReportedBy"}
		ChangeRename {from: "anovaGaugeNestedMisc";						to: "reportMiscellaneous"}
	}

	// Linearity Study

	Upgrade
	{
		functionName:		"msaAttribute"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "AAAdataFormat";							to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "AAAlongFormat":								return "longFormat";
					case "AAAwideFormat":								return "wideFormat";
				}
			}
		}

		ChangeRename {from: "operators";							to: "operator"}
		ChangeRename {from: "parts";								to: "part"}
		ChangeRename {from: "measurements";							to: "measurementsWideFormat"}
		ChangeRename {from: "measurementsLong";						to: "measurementLongFormat"}
		ChangeRename {from: "PositiveRef";							to: "positiveReference"}
		ChangeRename {from: "AAAcohensKappa";						to: "cohensKappa"}
		ChangeRename {from: "AAAfleissKappa";						to: "fleissKappa"}
		ChangeRename {from: "AAAkendallTau";						to: "kendallsTau"}

		

		
	}
}
