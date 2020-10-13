import QtQuick 			2.12
import JASP.Module 		1.0

Description
{
	name				: "jaspProcessControl"
	title				: qsTr("Process Control")
	description			: qsTr("Module for process control")
	version				: "0.1"
	author				: "JASP team"
	maintainer			: "JASP <info@jasp-stats.org>"
	website				: "https://github.com/jasp-stats"
	license				: "GPL (>= 2)"
	icon				: "analysis-descriptives.svg"

	Analysis
	{
		title:			qsTr("Process capability")
		func:			"processCapability"
	}

	Analysis
	{
		title:			qsTr("Process control")
		func:			"processControl"
	}

	Analysis
	{
		title:			qsTr("Measurement system analysis")
		func:			"measurementSystemAnalysis"
	}

	Analysis
	{
		title:			qsTr("Design of experiments")
		func:			"designOfExperiments"
	}
}
