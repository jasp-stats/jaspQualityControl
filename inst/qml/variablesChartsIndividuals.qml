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
			name:								"measurement"
			title:								qsTr("Measurement")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"axisLabels"
			title:								qsTr("Axis labels")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
		
		AssignedVariablesList
		{
			name:								"stage"
			title:								qsTr("Stage")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
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

			DoubleField
			{
				name:							"xmrChartMovingRangeLength"
				label:							qsTr("Moving range length")
				defaultValue:					2
				min: 							2
				max: 							dataSetModel.rowCount()
			}
		CheckBox
		{
			name:								"manualTicksXAxis"
			label: 								qsTr("Number of ticks on x-axis:")
			childrenOnSameRow: true

			DoubleField
			{
				name: 							"manualTicksXAxisValue"
				defaultValue:					5
			}
		}
		}

		CheckBox
		{
			name: 								"autocorrelationPlot"
			label: 								qsTr("Autocorrelation")
			checked: 							false

			DoubleField
			{
				name:						  	"autocorrelationPlotLagsNumber"
				label:						  	qsTr("Number of lags")
				defaultValue:					25
				min:			           	 	1
			}

			DoubleField
			{
				name:							"autocorrelationPlotCiLevel"
				label:							qsTr("Confidence interval size")
				defaultValue:					0.95
				min:							0.0001
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
			columns: 2

			CheckBox
			{
			name: "reportMetaData"
			label: qsTr("Show report metadata")
			checked: true

				TextField
				{
					id:									reportTitle
					label: 								qsTr("Title")
					name: 								"reportTitle"
					placeholderText:					qsTr("Measurement")
					fieldWidth:							100
				}

				TextField
				{
					id:									reportMeasurementName
					label: 								qsTr("Name")
					name: 								"reportMeasurementName"
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					id:									reportDate
					label: 								qsTr("Date")
					name: 								"reportDate"
					placeholderText:					qsTr("Date")
					fieldWidth:							100
				}

				TextField
				{
					id:									reportReportedBy
					label: 								qsTr("Reported by")
					name: 								"reportReportedBy"
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					id:									reportMiscellaneous
					label: 								qsTr("Misc")
					name: 								"reportMiscellaneous"
					placeholderText:					qsTr("Miscellaneous")
					fieldWidth:							100
				}
			}

			Group
			{
				title:			qsTr("Select Report Components")
			
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
}
