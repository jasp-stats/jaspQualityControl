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
			visible: 							dataFormat.currentValue == "wideFormat"
		}

		AssignedVariablesList
		{
			id:									stages
			name:								"stages"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
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
					value: 							"manual"
					label: 							qsTr("Subgroup size")
					checked:		 				true
					childrenOnSameRow:				true
					
					DoubleField
					{
						name: 									"manualSubgroupSizeValue"
						min: 									2
						defaultValue:							5
					}
				}
			
			RadioButton
				{
					value: 							"groupingVariable"
					label: 							qsTr("Through grouping variable")
				}

				

		}



		RadioButtonGroup
		{
			name:								"subgroupSizeUnequal"
			title: 								qsTr("Unequal subgroup sizes")
			id:									subgroupSizeUnequal

			RadioButton
			{
				value: 								"actualSizes"
				label: 								qsTr("Use actual sizes")
				checked: 							true
			}
			
			RadioButton
			{
				value: 								"fixedSubgroupSize"
				label: 								qsTr("Use fixed subgroup size")
				childrenOnSameRow:		 			true

				IntegerField 
				{
					name: 								"fixedSubgroupSizeValue"
					fieldWidth: 						30
					defaultValue: 						5
					min:								2
				}
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

  			DoubleField
  			{
  				name:							"knownParametersMean"
  				label:							qsTr("Mean")
  				defaultValue:					0
  				negativeValues: 				true
  				fieldWidth:						30
  				decimals:						10
  			}

  			DoubleField
  			{
  				name:							"knownParametersSd"
  				label:							qsTr("Standard deviation")
  				defaultValue:					3
  				fieldWidth:					  	30
  				decimals:						10
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

	Section
	{
		title: 									qsTr("Advanced Options")
		
		CheckBox
		{
			name: 								"xBarAndSUnbiasingConstant"
			label: 								qsTr("Use unbiasing constant for X-bar & s chart")
			checked:							true
		}
	}
}
