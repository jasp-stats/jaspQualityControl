import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common" as Common

Form
{
	columns:									2

	info:										qsTr("Control charts model the variation of a process, indicating its stability and establishing a state of statistical control. Variable charts for subgroups monitor the behaviour of a dimensional (continuous) variable, using either historical data (Phase 2) or new data (Phase 1).")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("X-bar & R chart: plots the process mean (X-bar) and process range (R) over time.") + "\n"
		+ "- " + qsTr("X-bar & s chart: plots the process mean (X-bar) and process standard deviation (s) over time.") + "\n"
		+ "- " + qsTr("Out-of-control signals are flagged using the tests selected under Advanced Options.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n- tidyr\n- tibble\n"

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
			title:								qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							subgroupSizeType.value == "groupingVariable"
			info:								qsTr("The subgroup each observation is assigned to, when all observations are in a single column.")
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
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup (one stage per subgroup row).")
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
				value: 							"manual"
				label: 							qsTr("Subgroup size")
				checked:		 				true
				childrenOnSameRow:				true
				info:							qsTr("Assign observations in order of appearance to subgroups of the specified size. If the count is not divisible, the last subgroup holds the remaining observations.")

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



		RadioButtonGroup
		{
			name:								"subgroupSizeUnequal"
			title: 								qsTr("Unequal subgroup sizes")
			id:									subgroupSizeUnequal
			info:								qsTr("How to handle subgroups of differing sizes when computing the process variance and control limits.")

			RadioButton
			{
				value: 								"actualSizes"
				label: 								qsTr("Use actual sizes")
				checked: 							true
				info:								qsTr("Compute control limits per subgroup using the actual subgroup sizes.")
			}

			RadioButton
			{
				value: 								"fixedSubgroupSize"
				label: 								qsTr("Use fixed subgroup size")
				childrenOnSameRow:		 			true
				info:								qsTr("Assume a single fixed subgroup size, producing the same control limits for all groups.")

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
			info:								qsTr("Which pair of control charts to display.")

			RadioButton
			{
				value: 							"xBarAndS"
				label: 							qsTr("X-bar & s")
				checked:		 				true
				info:							qsTr("Plot the process mean (X-bar) and the process standard deviation (s).")
			}

			RadioButton
			{
				value: 							"xBarAndR"
				label: 							qsTr("X-bar & R")
				info:							qsTr("Plot the process mean (X-bar) and the process range (R).")
			}
		}

		CheckBox
		{
			name: 								"warningLimits"
			label: 								qsTr("Warning limits")
			info:								qsTr("Plot limits one and two standard deviations from the central line.")
		}

		CheckBox
		{
			name: 								"knownParameters"
			label: 								qsTr("Known parameters")
			info:								qsTr("Use known historical parameter values (Phase 2) for the mean and standard deviation.")

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
			info:								qsTr("Display a formatted report of the control charts combining the selected metadata and charts.")

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
		
		CheckBox
		{
			name: 								"xBarAndSUnbiasingConstant"
			label: 								qsTr("Use unbiasing constant for X-bar & s control chart")
			checked:							true
			info:								qsTr("Apply the unbiasing constant when estimating the standard deviation for the X-bar & s control chart.")
		}

		DoubleField
		{
			name: 								"controlLimitsNumberOfSigmas"
			label: 								qsTr("Number of std. dev. for calculation of control limits")
			fieldWidth: 						30
			defaultValue: 						3
			min:								1
			info:								qsTr("Number of standard deviations from the central line used to compute the control limits.")
		}

		Common.ControlChartTests {}
	}
}
