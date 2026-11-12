import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									1

	info:										qsTr("A Shewhart control chart is a graph used to study how a process changes over time. The obtained measurements can be either variables or attributes data, and subgroups are taken from the process at regular intervals. From each subgroup one or more statistical measures (such as average, dispersion, or proportion) are plotted in time order. Each chart always has a central line, usually the average of the subgroup statistic (or based on past experience, if the process is in a state of statistical control). The control chart has two statistically determined boundaries, one on either side of the central line, called the upper control limit (UCL) and the lower control limit (LCL). They are typically placed at a distance of three times the standard deviation of the statistic being plotted. By comparing current data to these lines, conclusions can be drawn about whether the process variation is consistent (i.e., in control) or is unpredictable (i.e., out of control, and affected by special causes of variation).") + "\n\n"
		+ qsTr("Control charts for variables data are always used in pairs: the top chart monitors the averages (the centring of the data from the process) and the bottom chart monitors the ranges (the spread of the distribution). Control charts for attributes data consist of a single chart. Attributes data represent observations obtained by noting the presence or absence of some characteristic (attribute) in each of the items in the subgroup under consideration, then counting how many items do or do not possess the attribute, or how many such events occur in the item, group or area. Attributes data are generally rapid and inexpensive to obtain and often do not require specialised collection skills.") + "\n\n"
		+ qsTr("The assumed distribution for attributes data has only one independent parameter, the average level. The p and np control charts are based on the binomial distribution (for defectives), while the u and c control charts are based on the Poisson distribution (for defects). There are many types of count-based data that cannot be characterised by either a binomial (p and np) or a Poisson (u and c) distribution. When such data are placed on p, np, u, or c control charts the limits based on these models would be wrong. The Laney p' (p prime) and u' (u prime) charts are used to monitor attributes data if the sample sizes are variable and often very large. In such cases, it is common for the average proportion or rate to vary over the sampling period, a phenomenon known as overdispersion. Overdispersion means the sample-to-sample variation (i.e., the between-sample variation) is much larger than the variation within the sample. Underdispersion is the opposite.") + "\n\n"
		+ qsTr("Unlike the attributes control charts, the X-mR control chart makes no assumptions about the relationship between the location and the dispersion parameter. It directly measures the location with the average and the dispersion with the moving ranges. Thus, while the p, np, u, and c control charts use \"theoretical\" limits, the X-mR control chart uses \"empirical\" limits. X-mR control charts are used if the sample sizes are reasonably constant. Laney control charts are used if sample sizes vary; if the sample size is constant the Laney control chart is exactly the same as the X-mR control chart.")

	infoBottom: 								"## " + qsTr("Assumptions") + "\n"
		+ qsTr("The assumptions for attributes control charts are:") + "\n"
		+ "- " + qsTr("p/np control chart: the observed data are approximately binomially distributed;") + "\n"
		+ "- " + qsTr("u/c control chart: the observed data are approximately Poisson distributed;") + "\n"
		+ "- " + qsTr("np/c control chart: the subgroup size used for data collection is constant;") + "\n"
		+ "- " + qsTr("p/u control chart: the subgroup size used for data collection is either variable or constant;") + "\n"
		+ "- " + qsTr("Laney p'/u': the observed data cannot be modelled by either a binomial or a Poisson distribution and the subgroup size is variable.") + "\n"
		+ "\n---\n## " + qsTr("Out-of-control signals") + "\n"
		+ qsTr("To indicate special cause variation the following unnatural patterns are used:") + "\n"
		+ "- " + qsTr("Signal 1: one point beyond the control limits (sporadic issue). One point more than three standard deviations from the central line, equivalent to a violation of the UCL or LCL.") + "\n"
		+ "- " + qsTr("Signal 2: seven consecutive points above or below the central line (mean shift). Seven points in a row on the same side of the central line, equivalent to a run above or below the central line.") + "\n"
		+ "- " + qsTr("Signal 3: a run up or down of seven consecutive points (trend). Seven points in a row all increasing or all decreasing, equivalent to a run downward or upward.") + "\n"
		+ "- " + qsTr("Signal 4: two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).") + "\n"
		+ "- " + qsTr("Signal 5: hugging the central line, indicated when 15 consecutive points are within the one sigma limits (reducing variation).") + "\n"
		+ "- " + qsTr("Signal 6: eight consecutive points beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).") + "\n\n"
		+ qsTr("The lack of control in both average and variation charts is generally found in the initial stages of setting up control charts, sometimes called Phase 1 control charts. Using all six signals is only applicable for the average chart during Phase 1, when the control chart is used as an analysis tool. The first three signals are always used, for variables as well as attributes control charts.") + "\n\n"
		+ qsTr("In case the data of the individual and moving range chart (X-mR chart) violate the assumptions (normality and independently distributed), only signal 1 is applied. Autocorrelation is an indication of that.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.\n"
		+ "- Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual (2nd ed.). AIAG.\n"
		+ "- International Organization for Standardization (2023). Control charts – Part 2: Shewhart control charts. ISO 7870-2:2023.\n"
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
