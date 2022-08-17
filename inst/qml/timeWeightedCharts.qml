import QtQuick 									2.8
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
			name:								"measurements"
			title:								qsTr("Measurements")
			allowedColumns:						["scale"]
		}
	}

	Group
	{
		columns: 								1

		CheckBox
		{
			name: 								"cumulativeSumChart"
			label: 								qsTr("Cumulative sum chart")
			checked:							true

			DoubleField
			{
				name:							"cumulativeSumChartNumberSd"
				label:							qsTr("Number of standard deviations")
				defaultValue:					4
				enabled:						variationReference.currentValue != "studyVariation"
			}

			DoubleField
			{
				name:							"cumulativeSumChartShiftSize"
				label:							qsTr("Shift size")
				defaultValue:					0.5
				enabled:						variationReference.currentValue != "studyVariation"
			}
		}

		CheckBox
		{
			name: 								"ExponentiallyWeightedMovingAverageChart"
			label: 								qsTr("Exponentially weighted moving average chart")

			DoubleField
			{
				name:							"ExponentiallyWeightedMovingAverageChartLambda"
				label:							qsTr("Lambda")
				defaultValue:					0.3
			}

			DoubleField
			{
				name:							"ExponentiallyWeightedMovingAverageChartCenter"
				label:							qsTr("Center")
			}

			DoubleField
			{
				name:							"ExponentiallyWeightedMovingAverageChartSd"
				label:							qsTr("Within-group standard deviation")
				defaultValue:					3
				fieldWidth: 					50
			}

			DoubleField
			{
				name:							"ExponentiallyWeightedMovingAverageChartSigmaControlLimits"
				label:							qsTr("Sigmas for computing control limits")
				defaultValue:					3
			}
		}

		CheckBox
		{
			name: 								"gChart"
			label: 								qsTr("g chart")
		}

		// CheckBox
		// {
		// 	name: 								"tchart"
		// 	label: 								qsTr("t chart")
		// }
	}
}
