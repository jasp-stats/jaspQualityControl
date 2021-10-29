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
			name:								"D"
			title:								qsTr("Defectives/Defects")
			allowedColumns:						["scale"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			name:								"total"
			title:								qsTr("Sample")
			allowedColumns:						["scale"]
			singleVariable:						true
		}
	}

	Group
	{
		RadioButtonGroup
		{
			name:								"Attributes"
			title: 								qsTr("Charts for Attributes")
			columns: 							3

			RadioButton
			{
				name: 							"Defectives"
				label: 							qsTr("Defectives")
				checked: 						true

				RadioButtonGroup
				{
					name:						"TypeDefectives"

					RadioButton
					{
						name: 					"npchart"
						label: 					qsTr("np chart")
						checked:		 		true
					}

					RadioButton
					{
						name: 					"pchart"
						label: 					qsTr("p chart")
					}

					RadioButton
					{
						name: 					"Laneyprimechart"
						label: 					qsTr("Laney p' (p-prime) chart")
					}
				}
			}

			RadioButton
			{
				value: 							"Defects"
				label: 							qsTr("Defects")

				RadioButtonGroup
				{
					name:						"TypeDefects"

					RadioButton
					{
						value: 					"cchart"
						label: 					qsTr("c chart")
						checked: 				true
					}

					RadioButton
					{
						value:					"uchart"
						label:					qsTr("u chart")
					}

					RadioButton
					{
						value:					"Laneychart"
						label:					qsTr("Laney u' (u-prime) chart")
					}
				}
			}

			RadioButton
			{
				value:					"ImR"
				label:					qsTr("X-mR chart")
			}
		}
	}
}
