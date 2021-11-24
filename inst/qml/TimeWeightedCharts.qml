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
			name:								"variables"
			title:								qsTr("Measurements")
			allowedColumns:						["scale"]
		}
	}

	Group
	{
		columns: 								1

		CheckBox
		{
			name: 								"Cumulativechart"
			label: 								qsTr("Cumulative sum chart")

			DoubleField
			{
				name:							"h"
				label:							qsTr("Number of standard deviations")
				defaultValue:					4
				enabled:						variationReference.currentValue != "studyVariation"
			}

			DoubleField
			{
				name:							"k"
				label:							qsTr("Shift size")
				defaultValue:					0.5
				enabled:						variationReference.currentValue != "studyVariation"
			}
		}

		CheckBox
		{
			name: 								"Exponentialchart"
			label: 								qsTr("Exponentially weighted moving average chart")

			DoubleField
			{
				name:							"EWMAlambda"
				label:							qsTr("Lambda")
				defaultValue:					0.3
			}

			DoubleField
			{
				name:							"EWMAcenter"
				label:							qsTr("Center")
			}

			DoubleField
			{
				name:							"EWMAStd"
				label:							qsTr("Within-group standard deviation")
				defaultValue:					3
				fieldWidth: 					50
			}

			DoubleField
			{
				name:							"EWMANsigma"
				label:							qsTr("Sigmas for computing control limits")
				defaultValue:					3
			}
		}

		CheckBox
		{
			name: 								"gchart"
			label: 								qsTr("g chart")
		}

		CheckBox
		{
			name: 								"tchart"
			label: 								qsTr("t chart")
		}
	}
}
