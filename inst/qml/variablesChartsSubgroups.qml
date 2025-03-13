import QtQuick
import QtQuick.Layouts
import JASP.Controls

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
			title:								qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							subgroupSizeType.value == "groupingVariable"
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
		title: 									qsTr("Control charts")
		columns: 								1

		RadioButtonGroup
		{
			name:								"chartType"
			id:									chartType

			RadioButton
			{
				value: 							"xBarAndS"
				label: 							qsTr("X-bar & s")
				checked:		 				true
			}

			RadioButton
			{
				value: 							"xBarAndR"
				label: 							qsTr("X-bar & R")
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
  				fieldWidth:						70
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

	Section
	{
		title: 									qsTr("Variable Charts for Subgroups Report")

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

	Section
	{
		title: 									qsTr("Advanced Options")
		columns:								1
		
		CheckBox
		{
			name: 								"xBarAndSUnbiasingConstant"
			label: 								qsTr("Use unbiasing constant for X-bar & s control chart")
			checked:							true
		}

		DoubleField
		{
			name: 								"controlLimitsNumberOfSigmas"
			label: 								qsTr("Number of std. dev. for calculation of control limits")
			fieldWidth: 						30
			defaultValue: 						3
			min:								1
		}

		Group
		{
			title:		qsTr("Tests for control charts")

			DropDown
			{
				name:									"testSet"
				label:									qsTr("Test set")
				id: 									testSet
				indexDefaultValue:						0
				values: [
					{ label: qsTr("JASP"), value: "jaspDefault"},
					{ label: qsTr("Nelson laws"), value: "nelsonLaws"},
					{ label: qsTr("Western Electric rules"), value: "westernElectric"},
					{ label: qsTr("Custom selection"), value: "custom"}
				]
			}


			CheckBox
			{
				name: 								"rule1"
				label: 								qsTr("Points outside of control limits")
				checked:							true
				enabled:							testSet.currentValue == "custom"
			}

			CheckBox
			{
				name: 								"rule2"
				label: 								""
				checked:							true
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule2Value"
					afterLabel:							qsTr("points in a row, on the same side of center line")
					fieldWidth: 						25
					defaultValue: 						testSet.currentValue == "nelsonLaws" ? 9 : testSet.currentValue == "westernElectric" ? 8 : 7
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule3"
				label: 								""
				checked:							testSet.currentValue != "westernElectric"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule3Value"
					afterLabel:							qsTr("points in a row, all increasing or decreasing")
					fieldWidth: 						25
					defaultValue: 						testSet.currentValue == "nelsonLaws" ? 6 : 7
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule4"
				label: 								""
				checked:							testSet.currentValue != "jaspDefault"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule4Value"
					afterLabel:							qsTr("out of k+1 points > 2 std. dev. from center line (same side)")
					fieldWidth: 						25
					defaultValue: 						2
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule5"
				label: 								""
				checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule5Value"
					afterLabel:							qsTr("points in a row < 1 std. dev from center line (either side)")
					fieldWidth: 						25
					defaultValue: 						15
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule6"
				label: 								""
				checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule6Value"
					afterLabel:							qsTr("points in a row > 1 std. dev from center line (either side)")
					fieldWidth: 						25
					defaultValue: 						8
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule7"
				label: 								""
				checked:							testSet.currentValue != "jaspDefault"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule7Value"
					afterLabel:							qsTr("out of k+1 points > 1 std. dev. from center line (same side)")
					fieldWidth: 						25
					defaultValue: 						4
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule8"
				label: 								""
				checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom"
				enabled:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule8Value"
					afterLabel:							qsTr("points in a row, alternating increase and decrease")
					fieldWidth: 						25
					defaultValue: 						14
					min:								2
				}
			}
		}
	}
}
