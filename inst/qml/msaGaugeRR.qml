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
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue: 						0
		values: 
		[
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"},
		]
		onValueChanged:
		{
			measurementsWideFormat.itemDoubleClicked(0)
			measurementLongFormat.itemDoubleClicked(0)
		}
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
			title:								qsTr("Operator")
			id:									operator
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible: 							!type3.checked
		}

		AssignedVariablesList
		{
			name:								"part"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			singleVariable:						true
			visible:							dataFormat.currentValue == "longFormat"
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			id:									measurementsWideFormat
			singleVariable:						false
			visible:							dataFormat.currentValue == "wideFormat"
			allowedColumns:						["scale"]
		}

		CheckBox
		{
			name:								"type3"
			id:									type3
			label:								qsTr("Type 3 study (automatic equipment)")
			onCheckedChanged:
			{
				operator.itemDoubleClicked(0)
			}
		}
	}

	Group
	{
		title: 									qsTr("ANOVA Method Options")
		columns: 								2

		Group
		{

			DropDown
			{
				name: 							"processVariationReference"
				label: 							qsTr("Process variation based on")
				id: 							variationReference
				indexDefaultValue: 				0
				values: 
				[
					{ label: qsTr("Study variation"), value: "studySd" },
					{ label: qsTr("Historical standard deviation"), value: "historicalSd" }
				]
			}

			DoubleField
			{
				name:							"historicalSdValue"
				label:							qsTr("Historical standard deviation:")
				defaultValue:					3
				min: 						0.000000001
				decimals: 					9
				enabled:						variationReference.currentValue == "historicalSd"
			}

			CheckBox
			{
				name: 							"tolerance"
				label: 							qsTr("Tolerance width")
				childrenOnSameRow: 				true

				DoubleField
				{
					name: 						"toleranceValue"
					defaultValue: 				10
					min: 						0.000000001
					decimals: 					9
				}
			}

			CheckBox
			{
				name: 							"anova"
				label: 							qsTr("r&R table ANOVA method")
				checked: 						true

				DropDown
				{
					name: 						"anovaModelType"
					label: 						qsTr("Type of model for F-statistic")
					values: 
					[
        				{ label: qsTr("Fixed effects"), value: "fixedEffect"},
            			{ label: qsTr("Random effects"), value: "randomEffect"}
        			]
					indexDefaultValue: (type3.checked) ? 1 : 2
					enabled:      !type3.checked
				}

				DoubleField
				{
					name: 						"anovaAlphaForInteractionRemoval"
					label: 						qsTr("Alpha interaction removal")
					fieldWidth: 				60
					defaultValue: 				0.05
					max: 						1
					decimals: 					3
				}

				DropDown
				{
					name: 						"studyVarianceMultiplierType"
					label: 						qsTr("Study Var. multiplier type")
					id: 						studyVarianceMultiplierType
					indexDefaultValue: 			0
					values: 
					[
						{ label: qsTr("Std. Deviation"), value: "sd" },
						{ label: qsTr("Percent"), value: "percent" }
					]
				}

				DoubleField
				{
					name: 						"studyVarianceMultiplierValue"
					label: 						qsTr("Study variation multiplier")
					fieldWidth: 				60
					defaultValue: 				6
					min:						0.001
					max:						99.999
					decimals: 					3
				}
			}
		}

		Group
		{
			title: 								qsTr("Plots")

			CheckBox
			{
				name: 						"varianceComponentsGraph"
				label: 						qsTr("Components of variation")
				checked: 					true
			}

			CheckBox
			{
				name: 							"rChart"
				label: 							qsTr("R charts by operator")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name:							"xBarChart"
				label:							qsTr("Average charts by operator")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"scatterPlot"
				label:							qsTr("Scatter plots operators")
				enabled:						!type3.checked

				CheckBox
				{
					name:						"scatterPlotFitLine"
					label:						qsTr("Fit line")
				}

				CheckBox
				{
					name:						"scatterPlotOriginLine"
					label:						qsTr("Show origin line")
				}
			}

			CheckBox
			{
				name:							"partMeasurementPlot"
				label:							qsTr("Measurements by part plot")

				CheckBox
				{
					name:						"partMeasurementPlotAllValues"
					label:						qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name:							"operatorMeasurementPlot"
				label:							qsTr("Measurements by operator plot")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"partByOperatorMeasurementPlot"
				label: 							qsTr("Part × operator interaction plot")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"trafficLightChart"
				label: 							qsTr("Traffic light chart")
			}
		}
	}

	Section
	{
		title:	qsTr("ANOVA Method Report")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
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
					name:		"reportMeasurementsByPartPlot"
					label:		qsTr("Show measurements by part")
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
				
				CheckBox
				{
					name:		"reportPartByOperatorPlot"
					label:		qsTr("Show part × operator interaction")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportTrafficLightCHart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}
}
