
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									1


	DropDown
	{
		name:									"dataFormat"
		label:									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue:						0
		values: [
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat" }
		]
	}

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"operator"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			name:								"part"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
			visible:							dataFormat.currentValue == "longFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			singleVariable:						false
			allowedColumns:						["scale"]
			visible:							dataFormat.currentValue == "wideFormat"
		}
	}

	Section
	{
		title: 									qsTr("Gauge r&R Options")

		Group
		{
			title: 								qsTr("Analysis Options")

			DropDown
			{
				name: 							"processVariationReference"
				label: 							qsTr("Std. Deviation reference")
				id: 							variationReference
				indexDefaultValue: 				0
				values: [
					{ label: qsTr("Study Std. Deviation"), value: "studySd"},
					{ label: qsTr("Historical process Std. Deviation"), value: "historicalSd"}
				]
			}

			DoubleField
			{
				name:							"historicalSdValue"
				label:							qsTr("Std. Deviation value")
				defaultValue:					3
				enabled:						variationReference.currentValue == "historicalSd"
			}

			CheckBox
			{
				name:							"tolerance"
				label:							qsTr("Tolerance")
				childrenOnSameRow:				true

				DoubleField
				{
					name: 						"toleranceValue"
					defaultValue:				1
					min:						0.000000001
					decimals:					9
				}
			}

			CheckBox
			{
				name: 							"anova"
				label: 							qsTr("r&R table ANOVA method")
				checked:						true

				DropDown
				{
					name:						"studyVarianceMultiplierType"
					label:						qsTr("Study Var. multiplier type")
					id: 						studyVarMultiplierType
					indexDefaultValue:			0
					values: 
					[
						{ label: qsTr("Std. Deviation"), value: "sd"},
						{ label: qsTr("Percent"), value: "percent"}
					]
				}

				DoubleField
				{
					name: 						"studyVarianceMultiplierValue"
					label: 						qsTr("Study Var. multiplier value")
					fieldWidth: 				60
					defaultValue:				6
					min:						0.001
					max:						99.999
					decimals:					3
				}
			}
		}

		Group
		{
			title:								qsTr("Plots")

			CheckBox
			{
				name:						"varianceComponentsGraph"
				label:						qsTr("Graph variation components")
				checked:					true
			}

			CheckBox
			{
				name: 							"rChart"
				label: 							qsTr("R charts by operator")
			}

			CheckBox
			{
				name: 							"xBarChart"
				label: 							qsTr("Average charts by operator")
			}

			CheckBox
			{
				name: 							"partMeasurementPlot"
				label: 							qsTr("Measurements by part plot")

				CheckBox
				{
					name: 						"partMeasurementPlotAllValues"
					label: 						qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name: 							"operatorMeasurementPlot"
				label: 							qsTr("Measurements by operator plot")
			}
		}
	}

	Section
	{
		title: 									qsTr("Gauge r&R Report")
		
		CheckBox
		{

			name: 								"report"
			label: 								qsTr("Show report")
			id:		anovaGaugeReport
			columns: 2
			
			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 1

				TextField
				{
					name: 								"reportTitle"
					label: 								qsTr("Title")
					id:									reportTitle
					placeholderText:					qsTr("Measurement")
					fieldWidth:							100
				}

				TextField
				{
					name: 								"reportGaugeName"
					label: 								qsTr("Gauge name")
					id:									reportGaugeName
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					name: 								"reportDate"
					label: 								qsTr("Date")
					id:									reportDate
					placeholderText:					qsTr("Date")
					fieldWidth:							100
				}

				TextField
				{
					name: 								"reportReportedBy"
					label: 								qsTr("Reported by")
					id:									reportReportedBy
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					name: 								"reportMiscellaneous"
					label: 								qsTr("Misc.")
					id:									reportMiscellaneous
					placeholderText:					qsTr("Miscellaneous")
					fieldWidth:							100
				}
			}
		
			Group
			{
				title:			qsTr("Select Report Components")
			
				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show R chart by operator")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByOperatorPlot"
					label:		qsTr("Show measurements by operator")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportAverageChartByOperator"
					label:		qsTr("Show average chart by operator")
					checked:	true
				}
			}
		}
	}
}
