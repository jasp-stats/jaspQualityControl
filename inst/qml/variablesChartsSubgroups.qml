import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									2

	DropDown
	{
		name:									"dataFormat"
		label:									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue:						0
		values: [
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"}
		]
		onValueChanged:
		{
			measurementsWideFormat.itemDoubleClicked(0)
			measurementLongFormat.itemDoubleClicked(0)
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
			name:								"measurementLongFormat"
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			allowedColumns:						["scale"]
			singleVariable:						true
			visible:							dataFormat.currentValue == "longFormat"
		}

		AssignedVariablesList
		{
			id:									subgroup
			name:								"subgroup"
			title:								qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible: 							dataFormat.currentValue == "longFormat" & subgroupSizeType.value == "groupingVariable"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			id:									measurementsWideFormat
			allowedColumns:						["scale"]
			visible:							dataFormat.currentValue == "wideFormat"
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
			visible:							dataFormat.currentValue == "longFormat"							

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
					name: 									"manualSubgroupSizeValue"
					label: 									qsTr("Subgroup size")
					id:										manualSubgroupSizeValue
					min: 									2
					defaultValue:							5
					visible:								dataFormat.currentValue == "longFormat"
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
			name:								"chartType"
			id:									chartType

			RadioButton
			{
				value: 							"xBarAndR"
				label: 							qsTr("X-bar & R")
				checked:		 				true
			}

			RadioButton
			{
				value: 							"xBarAndS"
				label: 							qsTr("X-bar & s")
			}
		}

		CheckBox
		{
			name: 								"warningLimits"
			label: 								qsTr("Warning limits")
		}

		CheckBox
		{
			name: 								"knownParameters"
			label: 								qsTr("Known parameters")

			Group
			{

			columns: 2

  			DoubleField
  			{
  				name:							"knownParametersMean"
  				label:							qsTr("Mean")
  				defaultValue:					0
  				negativeValues: 				true
  				fieldWidth:					  	70
  				decimals:						10
  			}

  			DoubleField
  			{
  				name:							"knownParametersSd"
  				label:							qsTr("Standard deviation")
  				defaultValue:					3
  				fieldWidth:					  	70
  				decimals:						10
  			}

			}

		}

		CheckBox
		{
			name:								"manualTicksXAxis"
			label: 								qsTr("Number of ticks on x-axis:")
			childrenOnSameRow: 					true

			DoubleField
			{
				name: 							"manualTicksXAxisValue"
				defaultValue:					5
			}
		}
	}

	Section
	{
		title: 									qsTr("Variable Charts for Subgroups Report")

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
			name: 								"reportMeasurementName"
			label: 								qsTr("Name")
			id:									reportMeasurementName
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
			label: 								qsTr("Misc")
			id:									reportMiscellaneous
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		TextField
		{
			name: 								"reportSubtitle"
			label: 								qsTr("Sub-title:")
			placeholderText:					qsTr("Sub-title")
			fieldWidth:							100
		}

		TextField
		{
			name: 								"reportChartName"
			label: 								qsTr("Chart name:")
			placeholderText:					qsTr("Name of the chart")
			fieldWidth:							100
		}

		CheckBox
		{
			name: 								"report"
			label: 								qsTr("Show Report")
		}
	}
}
