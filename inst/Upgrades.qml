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

	// Attribute Agreement Analysis

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

	// Test Retest

	Upgrade
	{
		functionName:		"msaTestRetest"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "testRetestDataFormat";					to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "testRetestLongFormat":								return "longFormat";
					case "testRetestWideFormat":								return "wideFormat";
				}
			}
		}

		ChangeRename {from: "operators";										to: "operator"}
		ChangeRename {from: "parts";											to: "part"}
		ChangeRename {from: "measurementsLong";									to: "measurementLongFormat"}
		ChangeRename {from: "measurements";										to: "measurementsWideFormat"}
		ChangeRename {from: "EnableRangePSD";									to: "manualProcessSd"}
		ChangeRename {from: "rangePSD";											to: "manualProcessSdValue"}
		ChangeRename {from: "EnableRangeTolerance";								to: "tolerance"}
		ChangeRename {from: "rangeTolerance";									to: "toleranceValue"}
		ChangeRename {from: "rangeRr";											to: "repeatabilityAndReproducibilityTable"}

		// plots
		ChangeRename {from: "rangeScatterPlotOperatorParts";					to: "runChartPart"}
		ChangeRename {from: "rangeScatterPlotOperators";						to: "scatterPlotMeasurement"}
		ChangeRename {from: "rangeScatterPlotFitLine";							to: "scatterPlotMeasurementFitLine"}
		ChangeRename {from: "jitter";											to: "scatterPlotMeasurementAllValues"}
		ChangeRename {from: "rangeRchart";										to: "rChart"}
		ChangeRename {from: "trafficPlot";										to: "trafficLightChart"}
	}

	// Variables Charts Subgroups

	Upgrade
	{
		functionName:		"variablesChartsSubgroups"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "CCDataFormat";										to: "dataFormat"}
		
		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "CClongFormat":									return "longFormat";
					case "CCwideFormat":									return "wideFormat";
				}
			}
		}

		ChangeRename {from: "variablesLong";								to: "measurementLongFormat"}
		ChangeRename {from: "variables";									to: "measurementsWideFormat"}
		ChangeRename {from: "subgroups";									to: "subgroup"}
		ChangeRename {from: "CCSubgroupSize";								to: "manualSubgroupSizeValue"}
		ChangeRename {from: "TypeChart";									to: "chartType"}

		ChangeJS
		{
			name:		"chartType"
			jsFunction:	function(options)
			{
				switch(options["chartType"])
				{
					case "Xbarchart":										return "xBarAndR";
					case "Schart":											return "xBarAndS";
				}
			}
		}

		ChangeRename {from: "Wlimits";										to: "warningLimits"}
		ChangeRename {from: "Phase2";										to: "knownParameters"}
		ChangeRename {from: "mean";											to: "knownParametersMean"}
		ChangeRename {from: "SD";											to: "knownParametersSd"}
		ChangeRename {from: "manualTicks";									to: "manualTicksXAxis"}
		ChangeRename {from: "nTicks";										to: "manualTicksXAxisValue"}
		
		// report
		ChangeRename {from: "CCReport";									to: "report"}
		ChangeRename {from: "ccTitle";									to: "reportTitle"}
		ChangeRename {from: "ccName";									to: "reportMeasurementName"}
		ChangeRename {from: "ccDate";									to: "reportDate"}
		ChangeRename {from: "ccReportedBy";								to: "reportReportedBy"}
		ChangeRename {from: "ccMisc";									to: "reportMiscellaneous"}
		ChangeRename {from: "ccSubTitle";								to: "reportSubtitle"}
		ChangeRename {from: "ccChartName";								to: "reportChartName"}
	}

	// Variables Charts Individuals

	Upgrade
	{
		functionName:		"variablesChartsIndividuals"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "manualTicks";									to: "manualTicksXAxis"}
		ChangeRename {from: "nTicks";										to: "manualTicksXAxisValue"}
<<<<<<< HEAD
=======
		ChangeRename {from: "CorPlot";										to: "autocorrelationPlot"}
		ChangeRename {from: "nLag";											to: "autocorrelationPlotLagsNumber"}
		ChangeRename {from: "CI";											to: "autocorrelationPlotCiLevel"}
		

		// report
		ChangeRename {from: "CCReport";										to: "report"}
		ChangeRename {from: "ccTitle";										to: "reportTitle"}
		ChangeRename {from: "ccName";										to: "reportMeasurementName"}
		ChangeRename {from: "ccDate";										to: "reportDate"}
		ChangeRename {from: "ccReportedBy";									to: "reportReportedBy"}
		ChangeRename {from: "ccMisc";										to: "reportMiscellaneous"}
		ChangeRename {from: "ccSubTitle";									to: "reportSubtitle"}
		ChangeRename {from: "ccChartName";									to: "reportChartName"}
	}
>>>>>>> f416c4d (Renaming Control Charts for Attributes)

	// Control Charts for Attributes

	Upgrade
	{
		functionName:		"attributesCharts"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "D";											to: "defectiveOrDefect"}
		ChangeRename {from: "Attributes";									to: "attributesChart"}
		
		ChangeJS
		{
			name:		"attributesChart"
			jsFunction:	function(options)
			{
				switch(options["attributesChart"])
				{
					case "Defectives":										return "defectives";
					case "Defects":											return "defects";
					case "ImR":												return "xmr";
				}
			}
		}

		ChangeRename {from: "TypeDefectives";								to: "attributesChartDefectivesChartType"}

		ChangeJS
		{
			name:		"attributesChartDefectivesChartType"
			jsFunction:	function(options)
			{
				switch(options["attributesChartDefectivesChartType"])
				{
					case "npchart":											return "npChart";
					case "pchart":											return "pChart";
					case "Laneyprimechart":									return "laneyPPrimeChart";
				}
			}
		}

		ChangeRename {from: "TypeDefects";									to: "attributesChartDefectsChartType"}

		ChangeJS
		{
			name:		"attributesChartDefectsChartType"
			jsFunction:	function(options)
			{
				switch(options["attributesChartDefectsChartType"])
				{
					case "cchart":											return "cChart";
					case "uchart":											return "uChart";
					case "Laneychart":										return "laneyUPrimeChart";
				}
			}
		}
	
		// report
		ChangeRename {from: "AReport";											to: "report"}
		ChangeRename {from: "ATitle";											to: "reportTitle"}
		ChangeRename {from: "AName";											to: "reportMeasurementName"}
		ChangeRename {from: "AOperator";										to: "reportReportedBy"}
		ChangeRename {from: "AMisc";											to: "reportMiscellaneous"}
		ChangeRename {from: "AID";												to: "reportId"}
		ChangeRename {from: "AAppraiser";										to: "reportAppraiser"}
		ChangeRename {from: "AMeasurement";										to: "reportMeasusrementSystemName"}
		ChangeRename {from: "ASize";											to: "reportSubgroupSize"}
		ChangeRename {from: "ATime";											to: "reportTime"}
		ChangeRename {from: "AFrequency";										to: "reportFrequency"}
	}

	// Process Capability Studies
	Upgrade
	{
		functionName:		"processCapabilityStudies"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"
		
		// main analysis
		ChangeRename {from: "subgroups";									to: "subgroup"}
		ChangeRename {from: "manualTicks";									to: "manualTicksXAxis"}
		ChangeRename {from: "nTicks";										to: "manualTicksXAxisValue"
	}
}
