import QtQuick 								    2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									1

	VariablesForm
	{
		preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"variables"
			title:								qsTr("Variables")
			allowedColumns:						["scale"]
		}

	  AssignedVariablesList
		{
			name:								"subgroups"
			title:								qsTr("Subgroups")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
	}

	Group
	{
		title: 									qsTr("Variables Chart for Individuals")

		CheckBox
		{
			name: 								"ImRchart"
			label: 								qsTr("X-mR chart")
			checked: 							true

			DoubleField
			{
				name:							"ncol"
				label:							qsTr("Moving range length")
				defaultValue:					2
				min: 							2
			}
		CheckBox
		{
			name:                   			"manualTicks"
			label: 								qsTr("Number of ticks on x-axis:")
			childrenOnSameRow: true

			DoubleField
			{
				name: 							"nTicks"
				defaultValue:					5
			}
		}
		}

		CheckBox
		{
			name: 								"CorPlot"
			label: 								qsTr("Autocorrelation")
			checked: 							false

			DoubleField
			{
				name:						  	"nLag"
				label:						  	qsTr("Number of lags")
				defaultValue:					25
				min:			           	 	1
			}

			DoubleField
			{
				name:							"CI"
				label:							qsTr("Confidence interval size")
				defaultValue:					0.95
				min:			            	0.0001
			}
		}
	}

	Section
	{
		title: 									qsTr("Variable Charts for Individuals Report")

		TextField
		{
			id:									ccTitle
			label: 								qsTr("Title")
			name: 								"ccTitle"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccName
			label: 								qsTr("Name")
			name: 								"ccName"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccDate
			label: 								qsTr("Date")
			name: 								"ccDate"
			placeholderText:					qsTr("Date")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccReportedBy
			label: 								qsTr("Reported by")
			name: 								"ccReportedBy"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									ccMisc
			label: 								qsTr("Misc")
			name: 								"ccMisc"
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Sub-title:")
			name: 								"ccSubTitle"
			placeholderText:					qsTr("Sub-title")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Chart name:")
			name: 								"ccChartName"
			placeholderText:					qsTr("Name of the chart")
			fieldWidth:							100
		}

		CheckBox
		{
			name: 								"CCReport"
			label: 								qsTr("Show Report")
		}
	}
}
