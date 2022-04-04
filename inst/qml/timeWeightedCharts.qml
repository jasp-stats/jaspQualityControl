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
		title:									qsTr("Plots")
		columns: 								1

		CheckBox
		{
			name: 								"csc"
			label: 								qsTr("CSC - Cumulative sum")

			DoubleField
			{
				name:							"numsigma"
				label:							qsTr("Std. error decision interval")
				defaultValue:					4
			}

			DoubleField
			{
				name:							"shift"
				label:							qsTr("Std. error shift detection")
				defaultValue:					0.5
			}
		}

		CheckBox
		{
			name: 								"ewma"
			label: 								qsTr("EWMA - Exponentially weighted moving average")

			DoubleField
			{
				name:							"lambda"
				label:							qsTr("Smoothing parameter")
				defaultValue:					0.3
			}

			DoubleField
			{
				name:							"center"
				label:							qsTr("Center")
			}

			DoubleField
			{
				name:							"sigma"
				label:							qsTr("Within-group standard deviation")
				defaultValue:					3
				fieldWidth: 					50
			}

			DoubleField
			{
				name:							"nsigma"
				label:							qsTr("Std. error control limits")
				defaultValue:					3
			}
		}

		CheckBox
		{
			name: 								"g"
			label: 								qsTr("G - Time between events")
		}

		CheckBox
		{
			name: 								"t"
			label: 								qsTr("T - Number between events")
		}
	}
}
