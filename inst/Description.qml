import QtQuick
import JASP.Module

Description
{
	title:				qsTr("Quality Control")
	description:		qsTr("Investigate if a manufactured product adheres to a defined set of quality criteria")
	icon:				"qualityControl-module.svg"
	hasWrapper:			false
	
	GroupTitle
	{
		title:			qsTr("Measurement Systems Analysis")
		icon:			"qualityControl-measurement.svg"
	}

	Analysis
	{
		title:			qsTr("Type 1 Instrument Capability Study")
		func:			"msaType1Gauge"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Type 2 and 3 Gauge r&R Study")
		func:			"msaGaugeRR"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Gauge r&R Study (Non-replicable Measurements)")
		func:			"msaGaugeRRnonrep"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Type 4 Linearity Study")
		func:			"msaGaugeLinearity"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Attributes Agreement Analysis")
		func:			"msaAttribute"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Test-retest (Range method)")
		func:			"msaTestRetest"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Probability of Detection")
		func:			"probabilityOfDetection"
		preloadData: 	false
	}

	GroupTitle
	{
		title:			qsTr("Control Charts")
		icon:			"qualityControl-control.svg"
	}

	Analysis
	{
		title:			qsTr("Variables Charts for Subgroups")
		func:			"variablesChartsSubgroups"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Variables Charts for Individuals")
		func:			"variablesChartsIndividuals"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Control Charts for Attributes")
		func:			"attributesCharts"
		preloadData: 	false
	}
	Analysis
	{
		title:			qsTr("Time Weighted Charts")
		func:			"timeWeightedCharts"
		preloadData: 	false
	}

	Analysis
	{
		title:			qsTr("Rare Event Charts")
		func:			"rareEventCharts"
		preloadData: 	false
	}

	GroupTitle
	{
		title:			qsTr("Capability Analysis")
		icon:			"qualityControl-capability.svg"
	}
	Analysis
	{
		title:			qsTr("Process Capability Study")
		func:			"processCapabilityStudies"
		preloadData: 	false
	}

	GroupTitle
	{
		title:			qsTr("Design of Experiments")
		icon:			"qualityControl-design.svg"
	}

	Analysis
	{
		title:			qsTr("Create Factorial Worksheet")
		func:			"doeFactorial"
		requiresData:	false
	}
	Analysis
	{
		title:			qsTr("Create Response Surface Worksheet")
		func:			"doeResponseSurfaceMethodology"
		requiresData:	false
	}
	Analysis
	{
		title:			qsTr("Analyse Design")
		func:			"doeAnalysis"
		requiresData:	true
		preloadData: 	false
	}
}
