import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									2

	info:										qsTr("Rare event control charts (g chart and t chart) are alternatives to the Shewhart chart for monitoring events that occur infrequently, where the number of opportunities or the time between events is more meaningful than tracking continuous data. The g chart monitors the number of opportunities between rare events; the t chart monitors the time between rare events.")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("G chart: the number of time units or opportunities between rare events, highlighting deviations from expected intervals.") + "\n"
		+ "- " + qsTr("T chart: the time intervals between rare events, for identifying process shifts.") + "\n"
		+ "- " + qsTr("Out-of-control signals are flagged using the tests selected under Advanced Options.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Montgomery, D. C. (2009). Introduction to statistical quality control. John Wiley & Sons.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n"

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
			info:								qsTr("Either the timepoint at which each event took place or the interval between events.")
		}

		AssignedVariablesList
		{
			name:								"stage"
			title:								qsTr("Stage")
			id:									stages
			allowedColumns:						["nominal"]
			singleVariable:						true
			info:								qsTr("A column that splits the analysis into multiple stages.")
		}
	}

	RadioButtonGroup
	{
		name:								"dataType"
		title: 								qsTr("Data type")
		id:									dataType
		info:								qsTr("Whether the variable contains the timepoint at which each event occurred or the interval between events.")

		RadioButton
		{
			value: 							"dataTypeDates"
			label: 							qsTr("Date/time")
			checked:		 				true
			info:							qsTr("The data specify a timepoint at which each event took place.")

			DropDown
			{
				name: 					"dataTypeDatesStructure"
				id: 					dataTypeDatesStructure
				label: 					qsTr("Structure")
				info:					qsTr("The structure of the timepoints: date only, time only, date and time, or time and date.")
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
				info:					qsTr("The date format in the data, where D = day, M = month, Y = year (e.g. DMY = 30/12/2024). The separator symbol does not matter.")
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
				info:					qsTr("The time format in the data, where H = hour, M = minute, S = second (e.g. HMS = 01:02:03). Ip and IMp refer to integer hours with am/pm (e.g. 12pm, 12:30pm). The separator symbol does not matter.")
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
			info:							qsTr("The data specify the time or number of opportunities between events.")

			DropDown
			{
				name: 					"dataTypeIntervalType"
				id: 					dataTypeIntervalType
				label: 					qsTr("Interval type")
				info:					qsTr("The unit in which the interval is expressed. Opportunities, hours, and days are read as is (times treated as decimal, e.g. 1.25 hours = 1 hour 15 minutes). Select Time to specify a time format.")
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
				info:					qsTr("The time format of the interval, where H = hour, M = minute, S = second.")
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
			info:								qsTr("Display the g chart, monitoring the number of opportunities between rare events.")

			DropDown
			{
				name: 					"gChartProportionSource"
				id: 					gChartProportionSource
				label:					qsTr("Proportion")
				info:					qsTr("Source of the proportion used in the g chart calculations: estimated from the data or a historical value.")
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
				info:									qsTr("The historical proportion value. Only used when the proportion source is Historical.")
			}
		}

		CheckBox
		{
			name: 								"tChart"
			label: 								qsTr("T chart")
			checked:							false
			info:								qsTr("Display the t chart, monitoring the time between rare events.")

			DropDown
			{
				name: 					"tChartDistribution"
				id: 					tChartDistribution
				label:					qsTr("Based on")
				info:					qsTr("Distribution used to calculate the t chart control limits.")
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
				info:					qsTr("Source of the distribution parameters: best-fit estimate from the data or historical values.")
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
				info:									qsTr("Historical Weibull shape parameter. Only used with a Weibull distribution and historical parameters.")
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
				info:									qsTr("Historical scale parameter of the distribution. Only used with historical parameters.")
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
			info:								qsTr("Display a formatted report of the rare event charts combining the selected metadata and charts.")

			CheckBox
			{
				name:								"reportMetaData"
				label:								qsTr("Show report metadata")
				checked:							true
				columns:							2
				info:								qsTr("Include a metadata header (title, chart name, measurement, date, etc.) in the report.")

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
			info:		qsTr("Out-of-control tests applied to the rare event charts.")

			DropDown
			{
				name:									"testSet"
				label:									qsTr("Test set")
				id: 									testSet
				indexDefaultValue:						0
				info:									qsTr("Predefined JASP test set, or a custom selection of tests that can be individually modified.")
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
				info:								qsTr("Test 1 (beyond limit): flag one point beyond the control limits.")
			}

			CheckBox
			{
				name: 								"rule2"
				label: 								""
				checked:							testSet.currentValue == "custom"
				visible:							testSet.currentValue == "custom"
				childrenOnSameRow:					true
				info:								qsTr("Test 2 (shift): flag N consecutive points on the same side of the central line.")

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
				info:								qsTr("Test 3 (trend): flag N consecutive points all increasing or all decreasing.")

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
				info:								qsTr("Test 8 (oscillation): flag N points in a row alternating up and down.")

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
				info:								qsTr("Test 9 (Benneyan test): flag N points in a row equal to 0.")

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
