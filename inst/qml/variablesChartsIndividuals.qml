import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common" as Common

Form
{
	columns:									1

	info:										qsTr("Variable charts for individuals (Shewhart control charts) study how a process changes over time using individual measurements rather than subgroups. The individual moving range (X-mR) chart is the preferred alternative to X-bar & R / X-bar & s charts for low-volume production or when there is no practical rational subgroup. An autocorrelation chart models the pairwise correlation of values at successive lags.")

	infoBottom: 								"## " + qsTr("Assumptions") + "\n"
		+ "- " + qsTr("X-mR chart: sequential (time-ordered) measurements, independent data points, and approximately normally distributed data.") + "\n"
		+ "- " + qsTr("Autocorrelation chart: data points are dependent on one another (each related to the next).") + "\n"
		+ "\n---\n## " + qsTr("Output") + "\n"
		+ "- " + qsTr("X-mR chart: the process value and moving range (MR) over time.") + "\n"
		+ "- " + qsTr("Autocorrelation: the autocorrelation across the lags.") + "\n"
		+ "- " + qsTr("Out-of-control signals are flagged using the tests selected under Advanced Options. Only tests 1, 2, 3 and 8 are applied to the moving range chart.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.") + "\n"
		+ "- " + qsTr("Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual (2nd ed.). AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n- stats\n"

	VariablesForm
	{
		preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"measurement"
			title:								qsTr("Measurement")
			singleVariable:						true
			allowedColumns:						["scale"]
			info:								qsTr("The observations collected from the process.")
		}

		AssignedVariablesList
		{
			name:								"axisLabels"
			title:								qsTr("Timestamp (optional)")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("Optional labels for each observation, used as x-axis labels.")
		}

		AssignedVariablesList
		{
			name:								"stage"
			title:								qsTr("Stage")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("A column that splits the analysis into multiple stages.")
		}
	}

	Group
	{
		title: 									qsTr("Variables Chart for Individuals")

		CheckBox
		{
			name: 								"xmrChart"
			label: 								qsTr("X-mR chart")
			checked: 							true
			info:								qsTr("Display the individual moving range (X-mR) chart of the process value and moving range over time.")

			DoubleField
			{
				name:							"xmrChartMovingRangeLength"
				label:							qsTr("Moving range length")
				defaultValue:					2
				min: 							2
				max: 							(dataSetInfo.rowCount < 2)? 2 : dataSetInfo.rowCount
				info:							qsTr("Number of consecutive observations spanned by each moving range.")
			}
		}

		CheckBox
		{
			name: 								"autocorrelationPlot"
			label: 								qsTr("Autocorrelation")
			checked: 							false
			info:								qsTr("Display the autocorrelation chart of the measurements across lags.")

			DoubleField
			{
				name:						  	"autocorrelationPlotLagsNumber"
				label:						  	qsTr("Number of lags")
				defaultValue:					25
				min:			           	 	1
				info:							qsTr("Number of lags shown on the autocorrelation chart.")
			}

			DoubleField
			{
				name:							"autocorrelationPlotCiLevel"
				label:							qsTr("Confidence interval size")
				defaultValue:					0.95
				min:							0.0001
				info:							qsTr("Size of the confidence interval used to calculate the autocorrelation limits.")
			}
		}
	}

	Section
	{
		title: 									qsTr("Variable Charts for Individuals Report")

		CheckBox
		{
			name: "report"
			label: qsTr("Show report")
			id:		variableChartIndividualsReport
			columns: 1
			info:	qsTr("Display a formatted report of the control charts combining the selected metadata and charts.")

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

			Group
			{
				title:			qsTr("Select Report Components")
				info:			qsTr("Choose which charts are included in the report.")

				CheckBox
				{
				name: "reportIMRChart"
				label: qsTr("Show X-mR chart")
				checked: true
				}

				CheckBox
				{
				name: "reportAutocorrelationChart"
				label: qsTr("Show autocorrelation chart")
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("Advanced Options")
		columns:								1

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
