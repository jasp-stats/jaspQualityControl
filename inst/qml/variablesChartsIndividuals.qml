import QtQuick									2.8
import QtQuick.Layouts							1.3
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
			id:									variable
			name:								"variable"
			title:								qsTr("Variable")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"subgroups"
			title:								qsTr("Labels (Optional)")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
	}

	DropDown
	{
		name: 									"palette"
		indexDefaultValue: 						0
		label:									qsTr("Color Palette")
		values: 
		[
			{ value: "iso", label: qsTr("ISO 7870-1") },
			{ value: "jasp", label: qsTr("JASP") },
			{ value: "colorblind", label: qsTr("Colorblind") }
		]
	}

	Group
	{
		title: 									qsTr("Plots")

		CheckBox
		{
			name: 								"xmrchart"
			label: 								qsTr("X-mR: Average and moving range")

			DoubleField
			{
				name:							"ncol"
				label:							qsTr("Moving range length")
				defaultValue:					2
				min: 							2
			}
		}

		CheckBox
		{
			name: 								"autocorrelation"
			label: 								qsTr("Autocorrelation plot")

			DoubleField
			{
				name:						  	"nLag"
				label:						  	qsTr("Number of lags")
				defaultValue:					25
				min:			           	 	1
			}

			CIField
			{
				name:							"CI"
				label:							qsTr("Confidence interval")
			}
		}
	}

	CheckBox
	{
		id:										report
		name:									"report"
		label: 									qsTr("Generate report")
		enabled:								variable.count > 0

		Group
		{
			columns:							2
			visible:							report.checked

			TextField
			{
				id:								ccTitle
				label: 							qsTr("Title")
				name: 							"ccTitle"
				placeholderText:				qsTr("Measurement")
				fieldWidth:						100
			}

			TextField
			{
				id:								ccName
				label: 							qsTr("Name")
				name: 							"ccName"
				placeholderText:				qsTr("Name")
				fieldWidth:						100
			}

			TextField
			{
				id:								ccDate
				label: 							qsTr("Date")
				name: 							"ccDate"
				placeholderText:				qsTr("Date")
				fieldWidth:						100
			}

			TextField
			{
				id:								ccReportedBy
				label: 							qsTr("Reported by")
				name: 							"ccReportedBy"
				placeholderText:				qsTr("Name")
				fieldWidth:						100
			}

			TextField
			{
				id:								ccMisc
				label: 							qsTr("Misc")
				name: 							"ccMisc"
				placeholderText:				qsTr("Miscellaneous")
				fieldWidth:						100
			}

			TextField
			{
				label: 							qsTr("Sub-title:")
				name: 							"ccSubTitle"
				placeholderText:				qsTr("Sub-title")
				fieldWidth:						100
			}

			TextField
			{
				label: 							qsTr("Chart name:")
				name: 							"ccChartName"
				placeholderText:				qsTr("Name of the chart")
				fieldWidth:						100
			}
		}
	}
}
