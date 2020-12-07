import QtQuick 			2.12
import JASP.Module 		1.0

Description
{
	name				: "jaspProcessControl"
	title				: qsTr("Quality Control")
	description			: qsTr("A JASP module for quality control analysis.")
	version				: "0.0.1"
	author				: "JASP Team"
	maintainer			: "JASP <info@jasp-stats.org>"
	website				: "https://github.com/jasp-stats/jaspProcessControl"
	license				: "GPL (>= 2)"
	icon				: "processControl-module.svg"

	// GroupTitle
	// {
	// 	title:			qsTr("Measurements")
	// 	icon:			"processControl-measurement.svg"
	// }
	Analysis
	{
		title:			qsTr("Measurement Systems Analysis")
		func:			"measurementSystemAnalysis"
	}

	// GroupTitle
	// {
	// 	title:			qsTr("Control")
	// 	icon:			"processControl-control.svg"
	// }
	Analysis
	{
		title:			qsTr("Control Charts")
		func:			"processControl"
	}	

	// GroupTitle
	// {
	// 	title:			qsTr("Capability")
	// 	icon:			"processControl-capability.svg"
	// }
	Analysis
	{
		title:			qsTr("Process Capability Studies")
		func:			"processCapability"
	}

	// GroupTitle
	// {
	// 	title:			qsTr("Design")
	// 	icon:			"processControl-design.svg"
	// }
	Analysis
	{
		title:			qsTr("Design of Experiments")
		func:			"designOfExperiments"
		requiresData:	false
	}
}
