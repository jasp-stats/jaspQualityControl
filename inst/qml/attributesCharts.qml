import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									1

	info:										qsTr("Control charts for attributes monitor count-based data obtained by noting the presence or absence of a characteristic in items of a subgroup. p and np charts are based on the binomial distribution (defectives); u and c charts on the Poisson distribution (defects). Laney p' and u' charts adjust the limits for overdispersion when sample sizes are variable and large. The X-mR chart uses empirical limits and makes no distributional assumption.")

	infoBottom: 								"## " + qsTr("Assumptions") + "\n"
		+ "- " + qsTr("p/np charts: data approximately binomial; u/c charts: data approximately Poisson.") + "\n"
		+ "- " + qsTr("np/c charts require a constant subgroup size; p/u charts allow variable or constant sizes.") + "\n"
		+ "- " + qsTr("Laney p'/u': data not modelled by a binomial or Poisson distribution and a variable subgroup size (overdispersion).") + "\n"
		+ "\n---\n## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Defectives charts (binary defective/not defective): p, np, and Laney p' charts.") + "\n"
		+ "- " + qsTr("Defects charts (multiple defects per unit): u, c, and Laney u' charts.") + "\n"
		+ "- " + qsTr("X-mR chart: the process values (individuals) and moving range (mR) over time.") + "\n"
		+ "- " + qsTr("Out-of-control signals flag special-cause variation; the first three signals always apply, and only signal 1 applies to the X-mR chart when its assumptions are violated.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.") + "\n"
		+ "- " + qsTr("Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual (2nd ed.). AIAG.") + "\n"
		+ "- " + qsTr("International Organization for Standardization (2023). Control charts – Part 2: Shewhart control charts. ISO 7870-2:2023.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n"

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
			info:								qsTr("The number of items that do or do not possess the attribute (defectives), or the number of defects per subgroup.")
		}

		AssignedVariablesList
		{
			name:								"total"
			title:								qsTr("Total")
			allowedColumns:						["scale"]
			singleVariable:						true
			info:								qsTr("The sample (subgroup) size of each observation.")
		}

		AssignedVariablesList
		{
			name:								"timeStamp"
			title:								qsTr("Timestamp (optional)")
			id:									timeStamp
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("Optional column used as x-axis labels.")
		}
	}

	Group
	{

		RadioButtonGroup
		{
			name:								"attributesChart"
			title:								qsTr("Charts for Attributes")
			columns:							3
			info:								qsTr("Type of attributes chart to display.")

			RadioButton
			{
				name:							"defectives"
				label:							qsTr("Defectives")
				checked:						true
				info:							qsTr("Charts for products that are either defective or not defective (binary).")

				RadioButtonGroup
				{
					name:						"attributesChartDefectivesChartType"

					RadioButton
					{
						name:					"npChart"
						label:					qsTr("np chart")
						checked:				true
						info:					qsTr("Charts the number of non-conforming (defective) items per subgroup; subgroup size is constant.")
					}

					RadioButton
					{
						name:					"pChart"
						label:					qsTr("p chart")
						info:					qsTr("Charts the proportion of non-conforming (defective) items per subgroup; subgroup size may be variable.")
					}

					RadioButton
					{
						name:					"laneyPPrimeChart"
						label:					qsTr("Laney p'(p-prime) chart")
						info:					qsTr("Charts the proportion of defectives per subgroup with control limits adjusted for overdispersion.")
					}
				}
			}

			RadioButton
			{
				value:							"defects"
				label:							qsTr("Defects")
				info:							qsTr("Charts for products that can have multiple defects per unit.")

				RadioButtonGroup
				{
					name:						"attributesChartDefectsChartType"

					RadioButton
					{
						value:					"cChart"
						label:					qsTr("c chart")
						checked:				true
						info:					qsTr("Charts the number of defects per subgroup; subgroup size is constant.")
					}

					RadioButton
					{
						value:					"uChart"
						label:					qsTr("u chart")
						info:					qsTr("Charts the proportion of defects per subgroup; subgroup size may be variable.")
					}

					RadioButton
					{
						value:					"laneyUPrimeChart"
						label:					qsTr("Laney u'(u-prime) chart")
						info:					qsTr("Charts the proportion of defects per subgroup with control limits adjusted for overdispersion.")
					}
				}
			}

			RadioButton
			{
				value:					"xmr"
				label:					qsTr("X-mR chart")
				info:					qsTr("Charts the process values (individuals) and moving range (mR) over time using empirical limits.")
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
			info:								qsTr("Display a formatted report of the attribute control charts combining the selected metadata.")

			CheckBox
			{
				name:								"reportMetaData"
				label:								qsTr("Show report metadata")
				checked:							true
				columns:							2
				info:								qsTr("Include a metadata header (title, name, appraiser, ID, etc.) in the report.")

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
}
