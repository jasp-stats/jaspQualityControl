import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									2

	info:										qsTr("Two very effective alternatives to the Shewhart control chart may be used when small process shifts are of interest: the cumulative sum (CUSUM) control chart and the exponentially weighted moving average (EWMA) control chart. CUSUM and EWMA control charts are excellent alternatives to the Shewhart control chart for Phase II process monitoring situations. Collectively, they are sometimes called time-weighted control charts. Like X-bar charts, they monitor the mean of the process, but, unlike X-bar charts, they take the previous values into account at each point. Moreover, these charts are considered a reliable estimate when the correct standard deviation is available.")

	infoBottom: 								"## " + qsTr("CUSUM control chart") + "\n"
		+ "### " + qsTr("Features") + "\n"
		+ qsTr("A CUSUM control chart is essentially a running total of deviations from some preselected reference value. The mean of any group of consecutive values is represented visually by the current slope of the graph. The basic advantage of a CUSUM chart is that it is more sensitive to small shifts of the process mean compared to the Shewhart control charts.") + "\n"
		+ "### " + qsTr("When to use") + "\n"
		+ "- " + qsTr("The false alarm rate of a Shewhart control chart is too high.") + "\n"
		+ "- " + qsTr("The process rate is so high that false alarms are hardly acceptable.") + "\n"
		+ "- " + qsTr("Small process changes (less than one sigma) must be detected quickly.") + "\n"
		+ "- " + qsTr("Automation is available to update the chart in real time.") + "\n\n"
		+ qsTr("In general, the underlying distribution is supposed to be normal, but CUSUM control charts can also be used for binomial and Poisson distributions for modelling nonconformities and fraction nonconforming.") + "\n"
		+ "### " + qsTr("Recommendations for CUSUM design") + "\n"
		+ qsTr("The tabular CUSUM is designed by choosing values for the reference value k and the decision interval h. It is usually recommended that these parameters be selected to provide good average run length (ARL) performance. The ARL is the number of plotted points until special cause variation is signalled; it is a function of the process mean shift. Based on many analytical studies of CUSUM ARL performance, general recommendations for selecting h and k can be given. Define H = h\u03C3 and K = k\u03C3, where \u03C3 is the standard deviation of the sample variable used in forming the CUSUM. Using h = 4 or h = 5 and k = \u00BD will generally provide a CUSUM that has good ARL properties against a shift of about 1\u03C3 in the process mean.") + "\n"
		+ "\n---\n## " + qsTr("Assumptions") + "\n"
		+ qsTr("The basic assumptions for a CUSUM control chart are:") + "\n"
		+ "1. " + qsTr("the measurements must be sequential or time sequenced;") + "\n"
		+ "2. " + qsTr("the measurements are independent of each other: one data point does not determine or impact the next data point;") + "\n"
		+ "3. " + qsTr("the measurements are approximately normally distributed, or the sample size of each plotted point is large enough so that the average yields an approximate normal distribution.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Page, E. S. (1954). Continuous inspection schemes. Biometrika, 41(1-2), 100-115.") + "\n"
		+ "- " + qsTr("Roberts, S. W. (1959). Control chart tests based on geometric moving averages. Technometrics, 1(3), 239-250.") + "\n"
		+ "- " + qsTr("Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
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
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup. Only the first specified stage of each subgroup is considered.")
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
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup row. Only one stage per subgroup is possible.")
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
				info:							qsTr("Assign observations in order of appearance (from top to bottom) to subgroups of the specified size. If the number of observations cannot be equally divided into subgroups of the specified size, the last subgroup has the size of the remaining observations.")

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
					info:					qsTr("How to group when identical subgroup values are not adjacent in the dataset (top to bottom). \"Subgroup value change\" groups only series of identical and adjacent values; \"Same subgroup value\" groups all identical values regardless of adjacency. For instance, for the subgroup values [A, A, B, B, A], \"Subgroup value change\" yields three groups, [A, A], [B, B] and [A], whereas \"Same subgroup value\" yields two groups, [A, A, A] and [B, B].")
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
				info:							qsTr("Standardised decision interval (h), the number of standard deviations defining the control limits. The choice of h and k has a substantial impact on the performance of the CUSUM. The decision interval is generally set to 4 (sometimes 5) standard deviations.")
			}

			DoubleField
			{
				name:							"cumulativeSumChartShiftSize"
				label:							qsTr("Shift size")
				defaultValue:					0.5
				info:							qsTr("Reference value (k, also called the allowance or slack value), often chosen about halfway between the target (\u03BC0) and the out-of-control value of the mean (\u03BC1) that should be detected quickly. Since the CUSUM chart is used to monitor small shifts, generally 0.5 to 1 standard deviation is used (k = 0.5).")
			}

			DoubleField
			{
				name:							"cumulativeSumChartTarget"
				label:							qsTr("Target")
				defaultValue:					0
				info:							qsTr("Value used as the target (nominal) from which cumulative deviations are computed. CUSUM statistics are cumulative deviations from the target.")
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
