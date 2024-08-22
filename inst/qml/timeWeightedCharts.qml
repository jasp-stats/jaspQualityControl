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
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			allowedColumns:						["scale"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			id:									subgroup
			name:								"subgroup"
			title:								subgroupSizeType.value == "individual" ? qsTr("Timestamp (optional)") : qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							subgroupSizeType.value == "groupingVariable" | subgroupSizeType.value == "individual"
		}

		AssignedVariablesList
		{
			name:								"stagesLongFormat"
			id:									stagesLongFormat
			title:								qsTr("Stages")
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
			id:									measurementsWideFormat
			allowedColumns:						["scale"]
		}



		AssignedVariablesList
		{
			id:									axisLabels
			name:								"axisLabels"
			title:								qsTr("Timestamp (optional)")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"stagesWideFormat"
			id:									stagesWideFormat
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
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
				value: 							"individual"
				label: 							qsTr("No subgroups (n = 1)")
				checked:		 				true
			}

			RadioButton
			{
				value: 							"manual"
				label: 							qsTr("Subgroup size")
				childrenOnSameRow:				true
				
				IntegerField
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

				DropDown
				{
					name: 					"groupingVariableMethod"
					id: 					groupingVariable
					label: 					"Grouping method"
					values: 
					[
						{ label: qsTr("Subgroup value change"),			value: "newLabel"},
						{ label: qsTr("Same subgroup value"),			value: "sameLabel"}
					]
					indexDefaultValue: 0
				}
			}

		}
	}



	Group
	{

		CheckBox
		{
			name: 								"cumulativeSumChart"
			label: 								qsTr("Cumulative sum chart")
			checked:							true

			DoubleField
			{
				name:							"cumulativeSumChartNumberSd"
				label:							qsTr("Number of std. dev. for control limits")
				defaultValue:					4
			}

			DoubleField
			{
				name:							"cumulativeSumChartShiftSize"
				label:							qsTr("Shift size")
				defaultValue:					0.5
			}

			DoubleField
			{
				name:							"cumulativeSumChartTarget"
				label:							qsTr("Target")
				defaultValue:					0
			}

			Group 
			{
				DropDown
				{
					name:									"cumulativeSumChartSdSource"
					label:									qsTr("Std. dev.")
					id: 									cumulativeSumChartSdSource
					indexDefaultValue:						0
					values: [
						{ label: qsTr("Estimated from data"), value: "data"},
						{ label: qsTr("Historical"), value: "historical"}
					]
				}

				DropDown
				{
					name:									"cumulativeSumChartSdMethod"
					visible:								cumulativeSumChartSdSource.currentValue == "data"
					label:									qsTr("Std. dev. estimation method")
					id: 									cumulativeSumChartSdMethod
					values: subgroupSizeType.value == "individual" ?
					[
						{ label: qsTr("X-mR"), value: "averageMovingRange"}
					] :
					[
						{ label: qsTr("S-bar"), value: "s"},
						{ label: qsTr("R-bar"), value: "r"}
					]
					indexDefaultValue: subgroupSizeType.value == "individual" ? 0 : 1
				}

				DoubleField
				{
					name:							"cumulativeSumChartSdValue"
					label:							qsTr("Std. dev. value")
					visible:						cumulativeSumChartSdSource.currentValue == "historical"
					defaultValue:					3
					fieldWidth: 					50
				}

				IntegerField
				{
					name: 									"cumulativeSumChartAverageMovingRangeLength"
					label:									qsTr("Moving range length")
					visible:								exponentiallyWeightedMovingAverageChartSdSource.currentValue == "data" & cumulativeSumChartSdMethod.currentValue == "averageMovingRange"
					min: 									2
					defaultValue:							2
				}
			}
		}

		CheckBox
		{
			name: 								"exponentiallyWeightedMovingAverageChart"
			label: 								qsTr("Exponentially weighted moving average chart")

			
			DoubleField
			{
				name:							"exponentiallyWeightedMovingAverageChartSigmaControlLimits"
				label:							qsTr("Number of std. dev. for control limits")
				defaultValue:					3
			}

			DoubleField
			{
				name:							"exponentiallyWeightedMovingAverageChartLambda"
				label:							qsTr("Lambda (smoothing parameter)")
				defaultValue:					0.3
			}

			Group 
			{
				DropDown
				{
					name:									"exponentiallyWeightedMovingAverageChartSdSource"
					label:									qsTr("In-control std. dev.")
					id: 									exponentiallyWeightedMovingAverageChartSdSource
					indexDefaultValue:						0
					values: [
						{ label: qsTr("Estimated from data"), value: "data"},
						{ label: qsTr("Historical"), value: "historical"}
					]
				}
				
				DropDown
				{
					name:									"exponentiallyWeightedMovingAverageChartSdMethod"
					visible:								exponentiallyWeightedMovingAverageChartSdSource.currentValue == "data"
					label:									qsTr("Std. dev. estimation method")
					id: 									exponentiallyWeightedMovingAverageChartSdMethod
					values: subgroupSizeType.value == "individual" ?
					[
						{ label: qsTr("X-mR"), value: "averageMovingRange"}
					] :
					[
						{ label: qsTr("S-bar"), value: "s"},
						{ label: qsTr("R-bar"), value: "r"}
					]
					indexDefaultValue: subgroupSizeType.value == "individual" ? 0 : 1
				}

				DoubleField
				{
					name:							"exponentiallyWeightedMovingAverageChartSdValue"
					label:							qsTr("Std. dev. value")
					visible:						exponentiallyWeightedMovingAverageChartSdSource.currentValue == "historical"
					defaultValue:					3
					fieldWidth: 					50
				}
				
				IntegerField
				{
					name: 							"exponentiallyWeightedMovingAverageChartMovingRangeLength"
					label:							qsTr("Moving range length")
					visible:						exponentiallyWeightedMovingAverageChartSdSource.currentValue == "data" & exponentiallyWeightedMovingAverageChartSdMethod.currentValue == "averageMovingRange"
					min: 							2
					defaultValue:					2
				}


			}
		}
	}

	Section
	{
		title:									qsTr("Time Weighted Charts Report")

		CheckBox
		{
			name: 								"report"
			label: 								qsTr("Show Report")
			columns:							1

			CheckBox
			{
				name:								"reportMetaData"
				label:								qsTr("Show report metadata")
				checked:							true
				columns:							2

				CheckBox
				{
					name:								"reportTitle"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportTitleText"
						label: 								qsTr("Title")
						id:									reportTitleText
						placeholderText:					qsTr("Variable Charts for Subgroups Report")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportChartName"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportChartNameText"
						label: 								qsTr("Chart name")
						placeholderText:					qsTr("Name of the chart")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportSubtitle"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportSubtitleText"
						label: 								qsTr("Sub-title")
						placeholderText:					qsTr("Sub-title")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportMeasurementName"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportMeasurementNameText"
						label: 								qsTr("Measurement name")
						id:									reportMeasurementNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportFootnote"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportFootnoteText"
						label: 								qsTr("Footnote")
						id:									reportFootnoteText
						placeholderText:					qsTr("Comment")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportLocation"
					checked:							true
					childrenOnSameRow:					true

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
					name:								"reportDate"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportDateText"
						label: 								qsTr("Date")
						id:									reportDateText
						placeholderText:					qsTr("Date")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportPerformedBy"
					checked:							true
					childrenOnSameRow:					true

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
					name:								"reportPrintDate"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						name: 								"reportPrintDateText"
						label: 								qsTr("Date printed")
						id:									reportPrintDateText
						placeholderText:					qsTr("Today")
						fieldWidth:							100
					}
				}
			}
		}
	}
}
