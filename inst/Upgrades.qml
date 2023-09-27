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

		Upgrade
	{
		functionName:	"variablesChartsIndividuals"
		fromVersion:	"0.17.3"
		toVersion:		"0.18.0"

		ChangeRename { from: "CCReport";											to: "variableChartIndividualsReport" }
	}


// option renaming for syntax

// Type 1 Gauge Study

	Upgrade
	{
		functionName:		"msaType1Gauge"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		ChangeRename {from: "anovaGaugeNestedName";						to: "reportGaugeName"}
		ChangeRename {from: "anovaGaugeNestedDate";						to: "reportDate"}
		ChangeRename {from: "anovaGaugeNestedReportedBy";				to: "reportReportedBy"}
		ChangeRename {from: "anovaGaugeNestedMisc";						to: "reportMiscellaneous"}
	}

	// Attribute Agreement Analysis

	Upgrade
	{
		functionName:		"msaAttribute"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
		// main analysis
		ChangeRename {from: "variables";									to: "measurement"}
		ChangeRename {from: "subgroups";									to: "axisLabels"}
		ChangeRename {from: "split";										to: "stage"}
		ChangeRename {from: "ImRchart";										to: "xmrChart"}
		ChangeRename {from: "movingRangeLength";							to: "xmrChartMovingRangeLength"}
		ChangeRename {from: "manualTicks";									to: "manualTicksXAxis"}
		ChangeRename {from: "nTicks";										to: "manualTicksXAxisValue"}
		ChangeRename {from: "CorPlot";										to: "autocorrelationPlot"}
		ChangeRename {from: "nLag";											to: "autocorrelationPlotLagsNumber"}
		ChangeRename {from: "CI";											to: "autocorrelationPlotCiLevel"}
		

		// report
		ChangeRename {from: "variableChartIndividualsReport";				to: "report"}
		ChangeRename {from: "ccTitle";										to: "reportTitle"}
		ChangeRename {from: "ccName";										to: "reportMeasurementName"}
		ChangeRename {from: "ccDate";										to: "reportDate"}
		ChangeRename {from: "ccReportedBy";									to: "reportReportedBy"}
		ChangeRename {from: "ccMisc";										to: "reportMiscellaneous"}
	}

	// Control Charts for Attributes

	Upgrade
	{
		functionName:		"attributesCharts"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
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

	// Time Weighted Charts
	Upgrade
	{
		functionName:		"timeWeightedCharts"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
		ChangeRename {from: "variables";										to: "measurements"}
		ChangeRename {from: "Cumulativechart";									to: "cumulativeSumChart"}
		ChangeRename {from: "h";												to: "cumulativeSumChartNumberSd"}
		ChangeRename {from: "k";												to: "cumulativeSumChartShiftSize"}
		ChangeRename {from: "Exponentialchart";									to: "exponentiallyWeightedMovingAverageChart"}
		ChangeRename {from: "EWMAlambda";										to: "exponentiallyWeightedMovingAverageChartLambda"}
		ChangeRename {from: "EWMAcenter";										to: "exponentiallyWeightedMovingAverageChartCenter"}
		ChangeRename {from: "EWMAStd";											to: "exponentiallyWeightedMovingAverageChartSd"}
		ChangeRename {from: "EWMANsigma";										to: "exponentiallyWeightedMovingAverageChartSigmaControlLimits"}
		ChangeRename {from: "gchart";											to: "gChart"}
		ChangeRename {from: "tchart";											to: "tChart"}

	}


	// Process Capability Studies
	Upgrade
	{
		functionName:		"processCapabilityStudies"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
		// main analysis
		ChangeRename {from: "pcDataFormat";									to: "dataFormat"}

		ChangeJS
		{
			name:		"dataFormat"
			jsFunction:	function(options)
			{
				switch(options["dataFormat"])
				{
					case "PClongFormat":									return "longFormat";
					case "PCwideFormat":									return "wideFormat";
				}
			}
		}

		ChangeRename {from: "variablesLong";								to: "measurementLongFormat"}
		ChangeRename {from: "variables";									to: "measurementsWideFormat"}
		ChangeRename {from: "subgroups";									to: "subgroup"}
		ChangeRename {from: "pcSubgroupSize";								to: "manualSubgroupSizeValue"}

		ChangeJS
		{
			name:		"capabilityStudyType"
			jsFunction:	function(options)
			{
				switch(options["capabilityStudyType"])
				{
					case "nonnormalCapabilityAnalysis":						return "nonNormalCapabilityAnalysis";
				}
			}
		}

		ChangeRename {from: "nonNormalDist";								to: "nonNormalDistribution"}

		ChangeJS
		{
			name:		"nonNormalDistribution"
			jsFunction:	function(options)
			{
				switch(options["nonNormalDistribution"])
				{
					case "Weibull":											return "weibull";
					case "Lognormal":										return "lognormal";
					case "3lognormal":										return "3ParameterLognormal";
					case "3weibull":										return "3ParameterWeibull";
				}
			}
		}

		ChangeJS
		{
			name:		"nonNormalMethod"
			jsFunction:	function(options)
			{
				switch(options["nonNormalMethod"])
				{
					case "nonconformance":									return "nonConformance";
				}
			}
		}

		ChangeRename {from: "lowerSpecificationField";						to: "lowerSpecificationLimit"}
		ChangeRename {from: "lowerSpecification";							to: "lowerSpecificationLimitValue"}
		ChangeRename {from: "targetValueField";								to: "target"}
		ChangeRename {from: "upperSpecificationField";						to: "upperSpecificationLimit"}
		ChangeRename {from: "upperSpecification";							to: "upperSpecificationLimitValue"}
		ChangeRename {from: "CapabilityStudyPlot";							to: "processCapabilityPlot"}
		ChangeRename {from: "csNumberOfBins";								to: "processCapabilityPlotBinNumber"}
		ChangeRename {from: "CapabilityStudyTables";						to: "processCapabilityTable"}
		ChangeRename {from: "csConfidenceInterval";							to: "processCapabilityTableCi"}
		ChangeRename {from: "csConfidenceIntervalPercent";					to: "processCapabilityTableCiLevel"}
		ChangeRename {from: "xbarR";										to: "xBarAndRChart"}
		ChangeRename {from: "IMR";											to: "xmrChart"}
		ChangeRename {from: "movingRangeLength";							to: "xmrChartMovingRangeLength"}
		ChangeRename {from: "displayDensity";								to: "histogramDensityLine"}
		ChangeRename {from: "pcNumberOfBins";								to: "histogramBinNumber"}
		ChangeRename {from: "addGridlines";									to: "probabilityPlotGridLines"}

		// report
		ChangeRename {from: "pcReportDisplay";								to: "report"}
		ChangeRename {from: "pcReportTitle";								to: "reportTitle"}
		ChangeRename {from: "pcReportName";									to: "reportProcessName"}
		ChangeRename {from: "pcReportDate";									to: "reportDate"}
		ChangeRename {from: "pcReportReportedBy";							to: "reportReportedBy"}
		ChangeRename {from: "pcReportMisc";									to: "reportMiscellaneous"}

		// advanced options
		ChangeRename {from: "rank";											to: "probabilityPlotRankMethod"}

		ChangeJS
		{
			name:		"probabilityPlotRankMethod"
			jsFunction:	function(options)
			{
				switch(options["probabilityPlotRankMethod"])
				{
					case "Bernard":											return "bernard";
					case "Herd-Johnson":									return "herdJohnson";
					case "Kaplan-Meier":									return "kaplanMeier";
					case "Hazen":											return "hazen";
				}
			}
		}
		
		ChangeRename {from: "manualTicks";									to: "manualTicksXAxis"}
		ChangeRename {from: "nTicks";										to: "manualTicksXAxisValue"}
		ChangeRename {from: "nTicksProbabilityPlot";						to: "manualTicksProbabilityPlotValue"}
		
		ChangeJS
		{
			name:		"nullDistribution"
			jsFunction:	function(options)
			{
				switch(options["nullDistribution"])
				{
					case "Normal":											return "normal";
					case "Weibull":											return "weibull";
					case "Lognormal":										return "lognormal";
				}
			}
		}
	}

	// Two-level Factorial Design
	Upgrade
	{
		functionName:		"doeFactorial"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"
		
		ChangeRename {from: "dataCoding";									to: "unitDisplay"}

		ChangeJS
		{
			name:		"unitDisplay"
			jsFunction:	function(options)
			{
				switch(options["unitDisplay"])
				{
					case "dataCoded":										return "coded";
					case "dataUncoded":										return "uncoded";
				}
			}
		}

		ChangeJS
		{
			name:		"runOrder"
			jsFunction:	function(options)
			{
				switch(options["runOrder"])
				{
					case "runOrderRandom":										return "random";
					case "runOrderStandard":									return "standard";
				}
			}
		}

		ChangeRename {from: "factorialType";									to: "factorialDesignType"}


		ChangeJS
		{
			name:		"factorialDesignType"
			jsFunction:	function(options)
			{
				switch(options["factorialDesignType"])
				{
					case "factorialTypeDefault":								return "defaultGenerator";
					case "factorialTypeSpecify":								return "customGenerator";
					case "factorialTypeSplit":									return "splitPlot";	
				}
			}
		}
		
		ChangeRename {from: "factorialTypeSpecifyGenerators";					to: "factorialDesignTypeCustomGeneratorSpecification"}
		ChangeRename {from: "numberHTCFactors";									to: "factorialDesignTypeSplitPlotNumberHardToChangeFactors"}
		ChangeRename {from: "designBy";											to: "designOptionsType"}

		ChangeJS
		{
			name:		"designOptionsType"
			jsFunction:	function(options)
			{
				switch(options["designOptionsType"])
				{
					case "designByRuns":										return "numberOfRuns";
					case "designByResolution":									return "resolution";
					case "designByFraction":									return "fraction";	
				}
			}
		}

		ChangeRename {from: "factorialRuns";									to: "designOptionsTypeNumberOfRunsValue"}
		ChangeRename {from: "factorialResolution";								to: "designOptionsTypeFactorialResolutionValue"}
		ChangeRename {from: "factorialFraction";								to: "designOptionsTypeFractionValue"}
		ChangeRename {from: "factorialCenterPoints";							to: "numberCenterPoints"}
		ChangeRename {from: "factorialCornerReplicates";						to: "numberReplicationsCornerPoints"}
		ChangeRename {from: "factorialRepeats";									to: "numberReplicationsCornerPointsRepetitionsOnly"}
		ChangeRename {from: "factorialBlocks";									to: "numberOfBlocks"}
		ChangeRename {from: "repeatRuns";										to: "randomRunsNumberRepetitions"}
		ChangeRename {from: "showAvailableDesigns";								to: "availableDesignsTable"}
		ChangeRename {from: "displayDesign";									to: "designPreviewTable"}
		ChangeRename {from: "showAliasStructure";								to: "aliasStructureTable"}
		ChangeRename {from: "FAresponse";										to: "designAnalysisResponseVariable"}
		ChangeRename {from: "FAassignedFactors";								to: "designAnalysisAssignedFactors"}
		ChangeRename {from: "FArunOrder";										to: "designAnalysisRunOrder"}
		ChangeRename {from: "enabledIntOrder";									to: "highestOrderInteractionTerm"}
		ChangeRename {from: "intOrder";											to: "highestOrderInteractionTermValue"}
		ChangeRename {from: "showAliasStructure2";								to: "aliasStructurePlots"}
		ChangeRename {from: "resNorm";											to: "normalProbabilityResidualPlot"}
		ChangeRename {from: "resHist";											to: "residualsHistogram"}
		ChangeRename {from: "resFitted";										to: "residualsAgainstFittedValuesPlot"}
		ChangeRename {from: "resOrder";											to: "residualsAgainstOrderPlot"}
		ChangeRename {from: "runOrderPlot";										to: "residualsAgainstOrderPlotType"}

		ChangeJS
		{
			name:		"residualsAgainstOrderPlotType"
			jsFunction:	function(options)
			{
				switch(options["residualsAgainstOrderPlotType"])
				{
					case "runOrderStandardPlot":								return "standard";
					case "runOrderRandomPlot":									return "run";
				}
			}
		}

		ChangeRename {from: "fourInOne";										to: "matrixResidualsPlot"}
	}

	// General Full Factorial Design
	Upgrade
	{
		functionName:		"doeFull"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"

		ChangeRename {from: "runOrderFull";										to: "runOrder"}

		ChangeJS
		{
			name:		"runOrder"
			jsFunction:	function(options)
			{
				switch(options["runOrder"])
				{
					case "runOrderRandom":										return "random";
					case "runOrderStandard":									return "standard";
				}
			}
		}

		ChangeRename {from: "fullCornerReplicates";								to: "numberReplicationsCornerPoints"}
		ChangeRename {from: "fullRepeatRuns";									to: "randomRunsNumberRepetitions"}
		ChangeRename {from: "fullRepeats";										to: "numberReplicationsCornerPointsRepetitionsOnly"}
		ChangeRename {from: "displayFullDesign";								to: "designPreviewTable"}
		ChangeRename {from: "fileFull";											to: "file"}
	}

	// Response Surface Design
	Upgrade
	{
		functionName:		"doeResponseSurfaceMethodology"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"

		ChangeJS
		{
			name:		"designType"
			jsFunction:	function(options)
			{
				switch(options["designType"])
				{
					case "cube":										return "cubePoints";
					case "star":										return "axialPoints";
				}
			}
		}

		ChangeRename {from: "numberOfCubes";							to: "cubePointsNumber"}
		ChangeRename {from: "numberOfStars";							to: "axialPointsNumber"}
		ChangeRename {from: "inscribed";								to: "inscribedDesign"}
		ChangeRename {from: "coded_out";								to: "codedOutput"}
		ChangeRename {from: "alpha";									to: "alphaType"}

		ChangeJS
		{
			name:		"alphaType"
			jsFunction:	function(options)
			{
				switch(options["alphaType"])
				{
					case "Orthogonal":										return "orthogonal";
					case "Rotatable":										return "rotatable";
					case "Spherical":										return "spherical";
					case "Faces":											return "faces";
				}
			}
		}

		ChangeJS
		{
			name:		"runOrder"
			jsFunction:	function(options)
			{
				switch(options["runOrder"])
				{
					case "runOrderRandom":										return "random";
					case "runOrderStandard":									return "standard";
				}
			}
		}

		ChangeRename {from: "designBlock";										to: "blockDesign"}
		ChangeRename {from: "coef";												to: "coefficientTable"}
		ChangeRename {from: "res";												to: "residualHistogram"}
		ChangeRename {from: "resNorm";											to: "normalProbabilityResidualPlot"}
		ChangeRename {from: "normalPlot";										to: "standardizedEffectNormalPlot"}
		ChangeRename {from: "addGridlines";										to: "standardizedEffectNormalPlotGridLines"}
		ChangeRename {from: "ResFitted";										to: "residualsAgainstFittedValuesPlot"}
		ChangeRename {from: "pareto";											to: "standardizedEffectParetoPlot"}
		ChangeRename {from: "fourInOne";										to: "matrixResidualsPlot"}
		ChangeRename {from: "contour";											to: "contourSurfacePlot"}
		ChangeRename {from: "cplot";											to: "contourSurfacePlotTwoDimensional"}
		ChangeRename {from: "coded";											to: "contourSurfacePlotCoded"}
		ChangeRename {from: "legend";											to: "contourSurfacePlotLegend"}
		ChangeRename {from: "divide";											to: "contourSurfacePlotNumberDivisions"}
		ChangeRename {from: "phi";												to: "contourSurfacePlotVerticalRotationAngle"}
		ChangeRename {from: "theta";											to: "contourSurfacePlotHorizontalRotationAngle"}
	}

	// Define Custom Design
	Upgrade
	{
		functionName:		"doeModifyDesign"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"

		ChangeRename {from: "MDresponse";										to: "responseVariable"}
		ChangeRename {from: "MDassignedFactors";								to: "assignedFactors"}
		ChangeRename {from: "MDrunOrder";										to: "runOrder"}
		ChangeRename {from: "dataCoding";										to: "unitDisplay"}

		ChangeJS
		{
			name:		"unitDisplay"
			jsFunction:	function(options)
			{
				switch(options["unitDisplay"])
				{
					case "dataCoded":										return "coded";
					case "dataUncoded":										return "uncoded";
				}
			}
		}

		ChangeRename {from: "displayRunOrder";								to: "displayedRunOrder"}

		ChangeJS
		{
			name:		"displayedRunOrder"
			jsFunction:	function(options)
			{
				switch(options["displayedRunOrder"])
				{
					case "runOrderRandom":										return "random";
					case "runOrderStandard":									return "standard";
				}
			}
		}

		ChangeRename {from: "designBy";											to: "designOptionsType"}

		ChangeJS
		{
			name:		"designOptionsType"
			jsFunction:	function(options)
			{
				switch(options["designOptionsType"])
				{
					case "byRuns":											return "numberOfRuns";
					case "byResolution":									return "resolution";
					case "byFraction":										return "fraction";	
				}
			}
		}

		ChangeRename {from: "MDresolution";									to: "designOptionsTypeResolutionValue"}
		ChangeRename {from: "MDfraction";									to: "designOptionsTypeFractionValue"}
		ChangeRename {from: "MDruns";										to: "designOptionsTypeNumberOfRunsValue"}
		ChangeRename {from: "MDcenterPoints";								to: "numberCenterPoints"}
		ChangeRename {from: "repeatRuns";									to: "randomRunsNumberRepetitions"}
		ChangeRename {from: "showDesiredDesign";							to: "desiredDesignTable"}
	}

	// Probability of Detection
	Upgrade
	{
		functionName:		"probabilityOfDetection"
		fromVersion:		"0.18.0"
		toVersion:			"0.18.1"

		ChangeRename {from: "covariates";									to: "covariate"}
		ChangeRename {from: "wantsModelFitTable";							to: "modelFitTable"}
		ChangeRename {from: "showData";										to: "detectionPlotDataDisplay"}
		ChangeRename {from: "showDataGeom";									to: "detectionPlotDataDisplayType"}
		ChangeRename {from: "addJitter";									to: "detectionPlotDataDisplayTypePointsJitter"}
		ChangeRename {from: "showDensity";									to: "detectionPlotDensityDisplay"}
		ChangeRename {from: "wantsConfidenceInterval";						to: "detectionPlotCi"}
		ChangeRename {from: "confidenceIntervalValue";						to: "detectionPlotCiLevel"}
		ChangeRename {from: "xTicks";										to: "xAxisTicksType"}

		ChangeJS
		{
			name:		"xAxisTicksType"
			jsFunction:	function(options)
			{
				switch(options["xAxisTicksType"])
				{
					case "data-based":										return "dataBased";
					case "data + model-based":								return "dataAndModelBased";
				}
			}
		}

		ChangeRename {from: "logTransform";									to: "logTransformedCovariate"}




	}
}
