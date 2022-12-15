import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									2

	DropDown
	{
		id: 									pcDataFormat
		name:									"CCDataFormat"
		label:									qsTr("Data format")
		indexDefaultValue:						0
		values: [
			{ label: qsTr("Single column"), value: "CClongFormat"},
			{ label: qsTr("Across rows"), value: "CCwideFormat"}
		]
		onValueChanged:
		{
			variables.itemDoubleClicked(0)
			variablesLong.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									variablesLong
			name:								"variablesLong"
			title:								qsTr("Measurements")
			allowedColumns:						["scale"]
			singleVariable:						true
			visible:							pcDataFormat.currentValue == "CClongFormat"
		}

		AssignedVariablesList
		{
			id:									subgroups
			name:								"subgroups"
			title:								qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible: 							pcDataFormat.currentValue == "CClongFormat" & subgroupSizeType.value == "groupingVariable"
		}

		AssignedVariablesList
		{
			id:									variables
			name:								"variables"
			title:								qsTr("Measurements")
			allowedColumns:						["scale"]
			visible:							pcDataFormat.currentValue == "CCwideFormat"
		}



		AssignedVariablesList
		{
			id:									axisLabels
			name:								"axisLabels"
			title:								qsTr("Axis labels")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible: 							pcDataFormat.currentValue == "CCwideFormat"
		}
	}

	Group
	{

		RadioButtonGroup
		{
			name:								"subgroupSizeType"
			title: 								qsTr("Specify subgroups")
			id:									subgroupSizeType
			visible:							pcDataFormat.currentValue == "CClongFormat"							

			RadioButton
				{
					value: 							"groupingVariable"
					label: 							qsTr("Through grouping variable")
					checked:		 				true
				}

				RadioButton
				{
					value: 							"manual"
					label: 							qsTr("Manual subgroup size")
					childrenOnSameRow:				true
					
					DoubleField
					{
						name: 									"CCSubgroupSize"
						min: 									2
						defaultValue:							5
					}
				}

		}



		RadioButtonGroup
		{
			name:								"subgroupSizeUnequal"
			title: 								qsTr("For unequal subgroup sizes")
			id:									subgroupSizeUnequal
			
			RadioButton
			{
				value: 							"assumeEqualSize"
				label: 							qsTr("Assume equal subgroup sizes (largest subgroup)")
				checked:		 				true
			}

			RadioButton
			{
				value: 							"actualSizes"
				label: 							qsTr("Calculate with actual sizes")
			}
		}
	}

	Group
	{
		title: 									qsTr("Control Charts")
		columns: 								1

		RadioButtonGroup
		{
			name:								"TypeChart"
			id:                  				typechart

			RadioButton
			{
				value: 							"Xbarchart"
				label: 							qsTr("X-bar & R")
				checked:		 				true
			}

			RadioButton
			{
				value: 							"Schart"
				label: 							qsTr("X-bar & s")
			}
		}

		CheckBox
		{
			name: 								"Wlimits"
			label: 								qsTr("Warning limits")
		}

		CheckBox
		{
			name: 								"Phase2"
			label: 								qsTr("Known parameters")

			Group
			{

			columns: 2

  			DoubleField
  			{
  				name:							"mean"
  				label:							qsTr("Mean")
  				defaultValue:					0
  				negativeValues: 				true
  				fieldWidth:					  	70
  				decimals:            			10
  			}

  			DoubleField
  			{
  				name:							"SD"
  				label:							qsTr("Standard deviation")
  				defaultValue:					3
  				fieldWidth:					  	70
  				decimals:            			10
  			}

			}

		}

		CheckBox
		{
			name:                   			"manualTicks"
			label: 								qsTr("Number of ticks on x-axis:")
			childrenOnSameRow: 					true

			DoubleField
			{
				name: 							"nTicks"
				defaultValue:					5
			}
		}
	}

	Section
	{
		title: 									qsTr("Variable Charts for Subgroups Report")

		TextField
		{
			id:									ccTitle
			label: 								qsTr("Title")
			name: 								"ccTitle"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccName
			label: 								qsTr("Name")
			name: 								"ccName"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccDate
			label: 								qsTr("Date")
			name: 								"ccDate"
			placeholderText:					qsTr("Date")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccReportedBy
			label: 								qsTr("Reported by")
			name: 								"ccReportedBy"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccMisc
			label: 								qsTr("Misc")
			name: 								"ccMisc"
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Sub-title:")
			name: 								"ccSubTitle"
			placeholderText:					qsTr("Sub-title")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Chart name:")
			name: 								"ccChartName"
			placeholderText:					qsTr("Name of the chart")
			fieldWidth:							100
		}

		CheckBox
		{
			name: 								"CCReport"
			label: 								qsTr("Show Report")
		}
	}
}
