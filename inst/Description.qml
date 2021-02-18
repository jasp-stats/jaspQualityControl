import QtQuick 			2.12
import JASP.Module 		1.0

Description
{
	name				: "jaspProcessControl"
	title				: qsTr("Quality Control")
	description			: qsTr("A JASP module for quality control analyses.")
	version				: "0.0.1"
	author				: "JASP Team"
	maintainer			: "JASP <info@jasp-stats.org>"
	website				: "https://github.com/jasp-stats/jaspProcessControl"
	license				: "GPL (>= 2)"
	icon				: "processControl-module.svg"

	 GroupTitle
	 {
	 	title:			qsTr("Measurement Systems Analysis")
	 	icon:			"processControl-measurement.svg"
	 }
	 
	 Analysis
	{
		title:			qsTr("Type 1 Gauge")
		func:			"msaType1Gauge"
	}
		 Analysis
	{
		title:			qsTr("Gauge Linearity and Bias")
		func:			"msaGaugeLinearity"
	}
	Analysis
	{
		title:			qsTr("Gauge r&R")
		func:			"msaGaugeRR"
	}
	Analysis
	{
		title:			qsTr("Attributes Agreement Analysis")
		func:			"msaAttribute"
	}

	 GroupTitle
	 {
	 	title:			qsTr("Control Charts")
	 	icon:			"processControl-control.svg"
	 }
	Analysis
	{
		title:			qsTr("Control Charts")
		func:			"controlCharts"
	}	

	 GroupTitle
	 {
	 	title:			qsTr("Capability Studies")
	 	icon:			"processControl-capability.svg"
	 }
	Analysis
	{
		title:			qsTr("Process Capability Studies")
		func:			"processCapabilityStudies"
	}

	 GroupTitle
	 {
	 	title:			qsTr("Design")
	 	icon:			"processControl-design.svg"
	 }
	Analysis
	{
		title:			qsTr("Design of Experiments")
		func:			"designOfExperiments"
		requiresData:	false
	}
}
