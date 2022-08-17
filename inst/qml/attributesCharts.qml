import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0

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
			title:								qsTr("Time stamp (optional)")
			id:									timeStamp
			singleVariable:						true
			allowedColumns:						["nominal","nominalText","ordinal"]
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

		TextField
		{
			label:								qsTr("Title")
			name:								"reportTitle"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Name")
			name:								"reportMeasurementName"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Operator")
			name:								"reportReportedBy"
			placeholderText:					qsTr("Operator")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("ID")
			name:								"reportId"
			placeholderText:					qsTr("ID")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Misc")
			name:								"reportMiscellaneous"
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Appraiser")
			name:								"reportAppraiser"
			placeholderText:					qsTr("Appraiser")
			fieldWidth:							100
		}
	
		TextField
		{
			label:								qsTr("Measurement system")
			name:								"reportMeasusrementSystemName"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Subgroups size")
			name:								"reportSubgroupSize"
			placeholderText:					qsTr("Size")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Time")
			name:								"reportTime"
			placeholderText:					qsTr("Time")
			fieldWidth:							100
		}

		TextField
		{
			label:								qsTr("Frequency")
			name:								"reportFrequency"
			placeholderText:					qsTr("Frequency")
			fieldWidth:							100
		}

		CheckBox
		{
			name:								"report"
			label:								qsTr("Show Report")
		}
	}
}
