
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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common" as Common

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
		id:										variablesFormLongFormat
		visible:								dataFormat.currentValue == "longFormat"

		AvailableVariablesList
		{
			name:								"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"operatorLongFormat"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

		VariablesForm
	{
		id:										variablesFormWideFormat
		visible:								dataFormat.currentValue == "wideFormat"

		AvailableVariablesList
		{
			name:								"variablesFormWideFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			singleVariable:						false
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"operatorWideFormat"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	Group
	{
		title: 								qsTr("Analysis options")

		DropDown
		{
			name: 							"processVariationReference"
			label: 							qsTr("Std. dev. reference")
			id: 							variationReference
			indexDefaultValue: 				0
			values: [
				{ label: qsTr("Study std. dev."), value: "studySd"},
				{ label: qsTr("Historical process std. dev."), value: "historicalSd"}
			]
		}

		DoubleField
		{
			name:							"historicalSdValue"
			label:							qsTr("Std. dev. value")
			defaultValue:					3
			enabled:						variationReference.currentValue == "historicalSd"
		}

		CheckBox
		{
			name:							"tolerance"
			label:							qsTr("Tolerance width")
			childrenOnSameRow:				true

			DoubleField
			{
				name: 						"toleranceValue"
				defaultValue:				1
				min:						0
				inclusive:					JASP.MaxOnly
				decimals:					9
			}
		}

		CheckBox
		{
			name: 							"anova"
			label: 							qsTr("r&R ANOVA table")
			checked:						true

			DropDown
			{
				name:						"studyVarianceMultiplierType"
				label:						qsTr("Study var. multiplier type")
				id: 						studyVarMultiplierType
				indexDefaultValue:			0
				values: 
				[
					{ label: qsTr("Std. dev."), value: "sd"},
					{ label: qsTr("Percent"), value: "percent"}
				]
			}

			DoubleField
			{
				name: 						"studyVarianceMultiplierValue"
				label: 						qsTr("Study var. multiplier value")
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
			label: 							qsTr("Range charts by operator")
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

		CheckBox
		{
			name: 							"trafficLightChart"
			label: 							qsTr("Traffic light chart")
		}
	}

	Section
	{
		title: 									qsTr("Report options")
		
		CheckBox
		{

			name: 								"report"
			label: 								qsTr("Show report")
			id:		anovaGaugeReport
			columns: 1
			
			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 2

				CheckBox
				{
					name:					"reportTitle"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportTitleText"
						label: 								qsTr("Title")
						id:									reportTitleText
						placeholderText:					qsTr("Gauge r&R report")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPartName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPartNameText"
						label: 								qsTr("Part name")
						id:									reportPartNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}


				CheckBox
				{
					name:					"reportGaugeName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNameText"
						label: 								qsTr("Gauge name")
						id:									reportGaugeNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportCharacteristic"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportCharacteristicText"
						label: 								qsTr("Characteristic")
						id:									reportCharacteristicText
						placeholderText:					qsTr("Characteristic")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportGaugeNumber"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNumberText"
						label: 								qsTr("Gauge number")
						id:									reportGaugeNumberText
						placeholderText:					qsTr("Number")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportTolerance"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportToleranceText"
						label: 								qsTr("Tolerance")
						id:									reportToleranceText
						placeholderText:					qsTr("Tolerance")
						fieldWidth:							100
					}
				}
				
				CheckBox
				{
					name:					"reportLocation"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportLocationText"
						label: 								qsTr("Location")
						id:									reportLocationText
						placeholderText:					qsTr("Location")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPerformedBy"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPerformedByText"
						label: 								qsTr("Performed by")
						id:									reportPerformedByText
						placeholderText:					qsTr("Analyst")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportDate"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportDateText"
						label: 								qsTr("Date")
						id:									reportDate
						placeholderText:					qsTr("Date")
						fieldWidth:							100
					}
				}
			}
		
			Group
			{
				title:			qsTr("Select Report Components")
			
				CheckBox
				{
					name:		"reportGaugeTable"
					label:		qsTr("Show gauge evaluation table")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show range chart by operator")
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

				CheckBox
				{
					name:		"reportTrafficLightChart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("Advanced Options")
		columns:								1

		Common.ControlChartTests {}
	}
}
