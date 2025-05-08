import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									2

	VariablesForm
	{
		id:										variablesForm
		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"variable"
			title:								qsTr("Variable")
			id:									variable
			allowedColumns:						dataType.value == "dataTypeInterval" & dataTypeIntervalType.value != "time" ? ["scale"] : ["nominal"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			name:								"stage"
			title:								qsTr("Stage")
			id:									stages
			allowedColumns:						["nominal"]
			singleVariable:						true
		}
	}

	RadioButtonGroup
	{
		name:								"dataType"
		title: 								qsTr("Data type")
		id:									dataType

		RadioButton
		{
			value: 							"dataTypeDates"
			label: 							qsTr("Date/time")
			checked:		 				true

			DropDown
			{
				name: 					"dataTypeDatesStructure"
				id: 					dataTypeDatesStructure
				label: 					qsTr("Structure")
				values: 
				[
					{ label: qsTr("Date + Time"),						value: "dateTime"},
					{ label: qsTr("Time + Date"),						value: "timeDate"},
					{ label: qsTr("Date only"),							value: "dateOnly"},
					{ label: qsTr("Time only"),							value: "timeOnly"}				
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"dataTypeDatesFormatDate"
				id: 					dataTypeDatesFormatDate
				label: 					qsTr("Date format")
				visible:				dataTypeDatesStructure.value != "timeOnly"
				values: 
				[
					{ label: qsTr("DMY"),							value: "dmy"},
					{ label: qsTr("MDY"),							value: "mdy"},
					{ label: qsTr("YMD"),							value: "ymd"},
					{ label: qsTr("DM"),							value: "dm"},
					{ label: qsTr("MD"),							value: "md"}
				]
				indexDefaultValue: 3
			}

			DropDown
			{
				name: 					"dataTypeDatesFormatTime"
				id: 					dataTypeDatesFormatTime
				label: 					qsTr("Time format")
				visible:				dataTypeDatesStructure.value != "dateOnly"
				values: 
				[
					{ label: qsTr("H"),								value: "H"},
					{ label: qsTr("HM"),							value: "HM"},
					{ label: qsTr("HMS"),							value: "HMS"},
					{ label: qsTr("Ip"),							value: "Ip"},
					{ label: qsTr("IMp"),							value: "IMp"}				
				]
				indexDefaultValue: 1
			}
		}

		RadioButton
		{
			value: 							"dataTypeInterval"
			label: 							qsTr("Interval between events")
			childrenOnSameRow:				false

			DropDown
			{
				name: 					"dataTypeIntervalType"
				id: 					dataTypeIntervalType
				label: 					qsTr("Interval type")
				values: 
				[
					{ label: qsTr("Opportunities"),								value: "opportunities"},
					{ label: qsTr("Time"),										value: "time"},
					{ label: qsTr("Hours (decimal)"),							value: "hours"},
					{ label: qsTr("Days (decimal)"),							value: "days"}				
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"dataTypeIntervalTimeFormat"
				id: 					dataTypeIntervalTimeFormat
				label: 					qsTr("Time format")
				visible:				dataTypeIntervalType.value == "time"
				values: 
				[
					{ label: qsTr("H"),								value: "H"},
					{ label: qsTr("HM"),							value: "HM"},
					{ label: qsTr("HMS"),							value: "HMS"},
					{ label: qsTr("Ip"),							value: "Ip"},
					{ label: qsTr("IMp"),							value: "IMp"}				
				]
				indexDefaultValue: 1
			}
		}

	}

	Group
	{
		CheckBox
		{
			name: 								"gChart"
			label: 								qsTr("G chart")
			checked:							true

			DropDown
			{
				name: 					"gChartProportionSource"
				id: 					gChartProportionSource
				label:					qsTr("Proportion")
				values: 
				[
					{ label: qsTr("Estimated from data"),			value: "data"},
					{ label: qsTr("Historical"),					value: "historical"}
				]
				indexDefaultValue: 0
			}

			DoubleField
			{
				name: 									"gChartHistoricalProportion"
				id:										gChartHistoricalProportion
				label:									qsTr("Proportion value")
				min: 									0
				max:									1
				visible:								gChartProportionSource.value == "historical"
				defaultValue:							0.5
			}
		}

		CheckBox
		{
			name: 								"tChart"
			label: 								qsTr("T chart")
			checked:							false
		
			DropDown
			{
				name: 					"tChartDistribution"
				id: 					tChartDistribution
				label:					qsTr("Based on")
				values: 
				[
					{ label: qsTr("Weibull distribution"),			value: "weibull"},
					{ label: qsTr("Exponential distribution"),		value: "exponential"}
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"tChartDistributionParameterSource"
				id: 					tChartDistributionParameterSource
				label:					qsTr("Distribution parameters")
				values: 
				[
					{ label: qsTr("Estimated from data"),			value: "data"},
					{ label: qsTr("Historical"),					value: "historical"}
				]
				indexDefaultValue: 0
			}

			DoubleField
			{
				name: 									"tChartHistoricalParametersWeibullShape"
				id:										tChartHistoricalParametersWeibullShape
				label:									qsTr("Shape")
				min: 									0
				inclusive:								JASP.None
				visible:								tChartDistributionParameterSource.value == "historical" & tChartDistribution.value == "weibull"
				defaultValue:							2
			}

			
			DoubleField
			{
				name: 									"tChartHistoricalParametersScale"
				id:										tChartHistoricalParametersScale
				label:									qsTr("Scale")
				min: 									0
				inclusive:								JASP.None
				visible:								tChartDistributionParameterSource.value == "historical"
				defaultValue:							2
			}
		}
	}


	Section
	{
		title:									qsTr("Rare Event Charts Report")

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
					{ label: qsTr("Custom selection"), value: "custom"}
				]
			}


			CheckBox
			{
				name: 								"rule1"
				label: 								qsTr("One point outside of control limits - Test 1: Beyond limit")
				checked:							true
				visible:							testSet.currentValue == "custom"
			}

			CheckBox
			{
				name: 								"rule2"
				label: 								""
				checked:							testSet.currentValue == "custom"
				visible:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule2Value"
					afterLabel:							qsTr("points in a row, on the same side of center line - Test 2: Shift")
					fieldWidth: 						25
					defaultValue: 						9
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule3"
				label: 								""
				checked:							testSet.currentValue == "custom"
				visible:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule3Value"
					afterLabel:							qsTr("points in a row, all increasing or decreasing - Test 3: Trend")
					fieldWidth: 						25
					defaultValue: 						6
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule8"
				label: 								""
				checked:							testSet.currentValue == "custom"
				visible:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule8Value"
					afterLabel:							qsTr("points in a row, alternating increase and decrease - Test 8: Oscillation")
					fieldWidth: 						25
					defaultValue: 						14
					min:								2
				}
			}

			CheckBox
			{
				name: 								"rule9"
				label: 								""
				checked:							testSet.currentValue == "custom"
				visible:							testSet.currentValue == "custom"
				childrenOnSameRow:					true

				IntegerField
				{
					name: 								"rule9Value"
					afterLabel:							qsTr("points in a row, equal to 0 - Test 9: Benneyan test")
					fieldWidth: 						25
					defaultValue: 						3
					min:								2
				}
			}
		}
	}
}
