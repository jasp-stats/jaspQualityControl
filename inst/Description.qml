import QtQuick			2.12
import JASP.Module		1.0

Description
{
	name:				"jaspProcessControl"
	title:				qsTr("Quality Control")
	description:		qsTr("Investigate if a manufactured product adheres to a defined set of quality criteria")
	version:			"0.16"
	author:				"JASP Team"
	maintainer:			"JASP <info@jasp-stats.org>"
	website:			"https://github.com/jasp-stats/jaspProcessControl"
	license:			"GPL (>= 2)"
	icon:				"processControl-module.svg"

	GroupTitle
	{
		title:			qsTr("Measurement Systems Analysis")
		icon:			"processControl-measurement.svg"
	}
	Analysis
	{
		title:			qsTr("Type 1 Instrument Capability")
		func:			"msaType1Gauge"
	}
	Analysis
	{
		title:			qsTr("Linearity Study")
		func:			"msaGaugeLinearity"
	}
	Analysis
	{
		title:			qsTr("Type 2 and 3 Gauge r&R (manual/automatic equipment)")
		func:			"msaGaugeRR"
	}
	Analysis
	{
		title:			qsTr("Gauge r&R (non-replicable measurements)")
		func:			"msaGaugeRRnonrep"
	}
	Analysis
	{
		title:			qsTr("Attributes Agreement Analysis")
		func:			"msaAttribute"
	}
	Analysis
	{
		title:			qsTr("Test-retest (Range method)")
		func:			"msaTestRetest"
	}

	GroupTitle
	{
		title:			qsTr("Control Charts")
		icon:			"processControl-control.svg"
	}
	Analysis
	{
		title:			qsTr("Variables Charts for Subgroups")
		func:			"variablesChartsSubgroups"
	}
	Analysis
	{
		title:			qsTr("Variables Charts for Individuals")
		func:			"variablesChartsIndividuals"
	}
	Analysis
	{
		title:			qsTr("Control Charts for Attributes")
		func:			"attributesCharts"
	}
	Analysis
	{
		title:			qsTr("Time Weighted Charts")
		func:			"timeWeightedCharts"
	}

	GroupTitle
	{
		title:			qsTr("Capability Study")
		icon:			"processControl-capability.svg"
	}
	Analysis
	{
		title:			qsTr("Process Capability Study")
		func:			"processCapabilityStudies"
	}

	GroupTitle
	{
		title:			qsTr("DOE")
		icon:			"processControl-design.svg"
	}
	Analysis
	{
        title:			qsTr("Screening Design")
		func:			"doeScreening"
		requiresData:	false
	}
	Analysis
	{
		title:			qsTr("Two-level Factorial Design")
		func:			"doeFactorial"
		requiresData:	false
	}
    Analysis
    {
        title:          qsTr("General Full Factorial Design")
        func:           "doeFull"
        requiresData:   false
    }
    Analysis
    {
        title:          qsTr("Modify Existing Design")
        func:           "doeModifyDesign"
    }
	Analysis
	{
		title:			qsTr("Response Surface")
		func:			"doeResponseSurfaceMethodology"
		requiresData:	false
	}
	Analysis
	{
		title:			qsTr("Two-level Factorial Analysis")
		func:			"factorialAnalysis"
	}
    Analysis
    {
        title:			qsTr("Definitive Screening Analysis")
        func:			"definitiveScreeningAnalysis"
    }

	GroupTitle
	{
		title:			qsTr("Probability of Detection")
//		icon:			"processControl-measurement.svg"
	}
	Analysis
	{
		title:			qsTr("Probability of Detection")
		func:			"probabilityOfDetection"
	}
}
