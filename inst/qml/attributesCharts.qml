import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									1
	
	VariablesForm
	{
		preferredHeight:						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"defectiveOrDefect"
			title:								qsTr("Defectives/Defects")
			allowedColumns:						["scale"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			name:								"total"
			title:								qsTr("Total")
			allowedColumns:						["scale"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			name:								"timeStamp"
			title:								qsTr("Timestamp (optional)")
			id:									timeStamp
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	Group
	{
		
		RadioButtonGroup
		{
			name:								"attributesChart"
			title:								qsTr("Charts for Attributes")
			columns:							3

			RadioButton
			{
				name:							"defectives"
				label:							qsTr("Defectives")
				checked:						true

				RadioButtonGroup
				{
					name:						"attributesChartDefectivesChartType"

					RadioButton
					{
						name:					"npChart"
						label:					qsTr("np chart")
						checked:				true
					}

					RadioButton
					{
						name:					"pChart"
						label:					qsTr("p chart")
					}

					RadioButton
					{
						name:					"laneyPPrimeChart"
						label:					qsTr("Laney p'(p-prime) chart")
					}
				}
			}

			RadioButton
			{
				value:							"defects"
				label:							qsTr("Defects")

				RadioButtonGroup
				{
					name:						"attributesChartDefectsChartType"
	
					RadioButton
					{
						value:					"cChart"
						label:					qsTr("c chart")
						checked:				true
					}

					RadioButton
					{
						value:					"uChart"
						label:					qsTr("u chart")
					}

					RadioButton
					{
						value:					"laneyUPrimeChart"
						label:					qsTr("Laney u'(u-prime) chart")
					}
				}
			}

			RadioButton
			{
				value:					"xmr"
				label:					qsTr("X-mR chart")
			}
		}
	}

	Section
	{
		title:									qsTr("Control Charts for Attributes Report")

		CheckBox
		{
			name:								"report"
			label:								qsTr("Show Report")
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
						label:								qsTr("Title")
						name:								"reportTitleText"
						placeholderText:					qsTr("Report for Attribute Control Charts")
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
						label:								qsTr("Name")
						name:								"reportMeasurementNameText"
						placeholderText:					qsTr("Name")
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
						label:								qsTr("Performed by")
						name:								"reportPerformedByText"
						placeholderText:					qsTr("Analyst")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportId"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("ID")
						name:								"reportIdText"
						placeholderText:					qsTr("ID")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportAppraiser"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("Appraiser")
						name:								"reportAppraiserText"
						placeholderText:					qsTr("Appraiser")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportMeasusrementSystemName"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("Measurement system")
						name:								"reportMeasusrementSystemNameText"
						placeholderText:					qsTr("Measurement")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportSubgroupSize"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("Subgroups size")
						name:								"reportSubgroupSizeText"
						placeholderText:					qsTr("Size")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportTime"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("Time")
						name:								"reportTimeText"
						placeholderText:					qsTr("Time")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:								"reportFrequency"
					checked:							true
					childrenOnSameRow:					true

					TextField
					{
						label:								qsTr("Frequency")
						name:								"reportFrequencyText"
						placeholderText:					qsTr("Frequency")
						fieldWidth:							100
					}
				}
			}
		}
	}

	// Section
	// {
	// 	title: 									qsTr("Advanced Options")
	// 	columns:								1

	// 	Group
	// 	{
	// 		title:		qsTr("Tests for control charts")

	// 		DropDown
	// 		{
	// 			name:									"testSet"
	// 			label:									qsTr("Test set")
	// 			id: 									testSet
	// 			indexDefaultValue:						0
	// 			values: [
	// 				{ label: qsTr("JASP"), value: "jaspDefault"},
	// 				{ label: qsTr("Custom selection"), value: "custom"}
	// 			]
	// 		}


	// 		CheckBox
	// 		{
	// 			name: 								"rule1"
	// 			label: 								qsTr("Points outside of control limits")
	// 			checked:							true
	// 			enabled:							testSet.currentValue == "custom"
	// 		}

	// 		CheckBox
	// 		{
	// 			name: 								"rule2"
	// 			label: 								""
	// 			checked:							false
	// 			enabled:							testSet.currentValue == "custom"
	// 			childrenOnSameRow:					true

	// 			IntegerField
	// 			{
	// 				name: 								"rule2Value"
	// 				afterLabel:							qsTr("points in a row, on the same side of center line")
	// 				fieldWidth: 						25
	// 				defaultValue: 						testSet.currentValue == "nelsonLaws" ? 9 : testSet.currentValue == "westernElectric" ? 8 : 7
	// 				min:								2
	// 			}
	// 		}

	// 		CheckBox
	// 		{
	// 			name: 								"rule3"
	// 			label: 								""
	// 			checked:							false
	// 			enabled:							testSet.currentValue == "custom"
	// 			childrenOnSameRow:					true

	// 			IntegerField
	// 			{
	// 				name: 								"rule3Value"
	// 				afterLabel:							qsTr("points in a row, all increasing or decreasing")
	// 				fieldWidth: 						25
	// 				defaultValue: 						testSet.currentValue == "nelsonLaws" ? 6 : 7
	// 				min:								2
	// 			}
	// 		}

	// 		CheckBox
	// 		{
	// 			name: 								"rule8"
	// 			label: 								""
	// 			checked:							false
	// 			enabled:							testSet.currentValue == "custom"
	// 			childrenOnSameRow:					true

	// 			IntegerField
	// 			{
	// 				name: 								"rule8Value"
	// 				afterLabel:							qsTr("points in a row, alternating increase and decrease")
	// 				fieldWidth: 						25
	// 				defaultValue: 						14
	// 				min:								2
	// 			}
	// 		}
	// 	}
	// }
}
