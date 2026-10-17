import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									2

	info:										qsTr("Time-weighted control charts are alternatives to the Shewhart chart for detecting small process shifts in Phase II monitoring. The cumulative sum (CUSUM) chart plots a running total of deviations from a target, while the exponentially weighted moving average (EWMA) chart weights recent observations more heavily. Both monitor the process mean using previous values at each point.")

	infoBottom: 								"## " + qsTr("Assumptions") + "\n"
		+ "- " + qsTr("Sequential (time-ordered) measurements, independent of one another, and approximately normally distributed (or large enough subgroups for the average to be approximately normal).") + "\n"
		+ "\n---\n## " + qsTr("Output") + "\n"
		+ "- " + qsTr("CUSUM chart: cumulative deviations from the target, plotting the upper and lower CUSUM statistics versus the sample number.") + "\n"
		+ "- " + qsTr("EWMA chart: the exponentially weighted moving average of the process mean over time.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Page, E. S. (1954). Continuous inspection schemes. Biometrika, 41(1-2), 100-115.") + "\n"
		+ "- " + qsTr("Roberts, S. W. (1959). Control chart tests based on geometric moving averages. Technometrics, 1(3), 239-250.") + "\n"
		+ "- " + qsTr("Montgomery, D. C. (2013). Introduction to statistical quality control (7th ed.). John Wiley & Sons.") + "\n"
		+ "- " + qsTr("International Organization for Standardization (2021). Control charts – Part 4: Cumulative sum charts. ISO 7870-4:2021.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n"

	DropDown
	{
		name:									"dataFormat"
		label:									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue:						0
		info:									qsTr("Layout of the data: all observations in one column (\"Single column\") or one subgroup per row (\"Across rows\").")
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
			info:								qsTr("The observations collected from the process.")
		}

		AssignedVariablesList
		{
			id:									subgroup
			name:								"subgroup"
			title:								subgroupSizeType.value == "individual" ? qsTr("Timestamp (optional)") : qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							subgroupSizeType.value == "groupingVariable" | subgroupSizeType.value == "individual"
			info:								qsTr("The subgroup each observation is assigned to, or (for individual observations) an optional timestamp used as x-axis labels.")
		}

		AssignedVariablesList
		{
			name:								"stagesLongFormat"
			id:									stagesLongFormat
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup.")
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
			info:								qsTr("The measurement columns, with one subgroup per row.")
		}



		AssignedVariablesList
		{
			id:									axisLabels
			name:								"axisLabels"
			title:								qsTr("Timestamp (optional)")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("Optional subgroup names for each row, used as x-axis labels.")
		}

		AssignedVariablesList
		{
			name:								"stagesWideFormat"
			id:									stagesWideFormat
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup row.")
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
			info:								qsTr("How subgroups are formed from a single column of observations.")

			RadioButton
			{
				value: 							"individual"
				label: 							qsTr("No subgroups (n = 1)")
				checked:		 				true
				info:							qsTr("Treat each observation as an individual measurement (subgroup size of one).")
			}

			RadioButton
			{
				value: 							"manual"
				label: 							qsTr("Subgroup size")
				childrenOnSameRow:				true
				info:							qsTr("Assign observations in order of appearance to subgroups of the specified size.")

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
				info:							qsTr("Use a single-column subgroup variable that assigns each observation to a subgroup.")

				DropDown
				{
					name: 					"groupingVariableMethod"
					id: 					groupingVariable
					label: 					"Grouping method"
					info:					qsTr("How to group when identical subgroup values are not adjacent. \"Subgroup value change\" groups only adjacent identical values; \"Same subgroup value\" groups all identical values regardless of adjacency.")
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
			info:								qsTr("Display the cumulative sum (CUSUM) chart, sensitive to small shifts in the process mean.")

			DoubleField
			{
				name:							"cumulativeSumChartNumberSd"
				label:							qsTr("Number of std. dev. for control limits")
				defaultValue:					4
				info:							qsTr("Standardised decision interval (h), the number of standard deviations defining the control limits. Typically 4 or 5.")
			}

			DoubleField
			{
				name:							"cumulativeSumChartShiftSize"
				label:							qsTr("Shift size")
				defaultValue:					0.5
				info:							qsTr("Reference value (k, or allowable slack), usually about halfway between the target and the shift to detect. Typically 0.5.")
			}

			DoubleField
			{
				name:							"cumulativeSumChartTarget"
				label:							qsTr("Target")
				defaultValue:					0
				info:							qsTr("Value used as the target (nominal) from which cumulative deviations are computed.")
			}

			Group
			{
				DropDown
				{
					name:									"cumulativeSumChartSdSource"
					label:									qsTr("Std. dev.")
					id: 									cumulativeSumChartSdSource
					indexDefaultValue:						0
					info:									qsTr("Source of the standard deviation used to form the CUSUM: estimated from the data or a historical value.")
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
					info:									qsTr("Method used to estimate the standard deviation from the data.")
					values: (subgroupSizeType.value == "individual" & dataFormat.currentValue == "longFormat") ?
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
					info:							qsTr("The historical standard deviation value. Only used when the std. dev. source is Historical.")
				}

				IntegerField
				{
					name: 									"cumulativeSumChartAverageMovingRangeLength"
					label:									qsTr("Moving range length")
					visible:								exponentiallyWeightedMovingAverageChartSdSource.currentValue == "data" & cumulativeSumChartSdMethod.currentValue == "averageMovingRange"
					min: 									2
					defaultValue:							2
					info:									qsTr("Number of consecutive observations spanned by each moving range when estimating the standard deviation.")
				}
			}
		}

		CheckBox
		{
			name: 								"exponentiallyWeightedMovingAverageChart"
			label: 								qsTr("Exponentially weighted moving average chart")
			info:								qsTr("Display the exponentially weighted moving average (EWMA) chart, which weights recent observations more heavily to detect small shifts.")


			DoubleField
			{
				name:							"exponentiallyWeightedMovingAverageChartSigmaControlLimits"
				label:							qsTr("Number of std. dev. for control limits")
				defaultValue:					3
				info:							qsTr("Number of standard deviations from the central line used to compute the EWMA control limits.")
			}

			DoubleField
			{
				name:							"exponentiallyWeightedMovingAverageChartLambda"
				label:							qsTr("Lambda (smoothing parameter)")
				defaultValue:					0.3
				info:							qsTr("Smoothing parameter (lambda) between 0 and 1; smaller values give more weight to past observations.")
			}

			Group
			{
				DropDown
				{
					name:									"exponentiallyWeightedMovingAverageChartSdSource"
					label:									qsTr("In-control std. dev.")
					id: 									exponentiallyWeightedMovingAverageChartSdSource
					indexDefaultValue:						0
					info:									qsTr("Source of the in-control standard deviation: estimated from the data or a historical value.")
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
					info:									qsTr("Method used to estimate the standard deviation from the data.")
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
					info:							qsTr("The historical standard deviation value. Only used when the std. dev. source is Historical.")
				}

				IntegerField
				{
					name: 							"exponentiallyWeightedMovingAverageChartMovingRangeLength"
					label:							qsTr("Moving range length")
					visible:						exponentiallyWeightedMovingAverageChartSdSource.currentValue == "data" & exponentiallyWeightedMovingAverageChartSdMethod.currentValue == "averageMovingRange"
					min: 							2
					defaultValue:					2
					info:							qsTr("Number of consecutive observations spanned by each moving range when estimating the standard deviation.")
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
			info:								qsTr("Display a formatted report of the time-weighted charts combining the selected metadata and charts.")

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
			info:		qsTr("Out-of-control tests applied to the time-weighted charts.")

			CheckBox
			{
				name: 								"rule1"
				label: 								qsTr("Points outside of control limits")
				checked:							true
				enabled:							true
				info:								qsTr("Flag any point beyond the control limits.")
			}
		}
	}
}
