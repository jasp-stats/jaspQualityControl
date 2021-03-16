import QtQuick 								    2.8
import QtQuick.Layouts 						    1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
  usesJaspResults:							true
	columns:									1

  VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		id:										variablesForm3

		AvailableVariablesList
		{
			name:								"variablesForm3"
		}

		AssignedVariablesList
		{
			id:									variables5
			name:								"variables"
			title:								qsTr("Measurements")
			allowedColumns:						["scale"]
		}
	}

  Group{
		columns: 									1
		CheckBox {	name: "Cumulativechart";				label: qsTr("Cumulative sum chart")
			DoubleField
			{
				name:			"h"
				label:			qsTr("Number of standard deviations:")
				defaultValue:	4
				enabled:		variationReference.currentValue != "studyVariation"
			}
			DoubleField
			{
				name:			"k"
				label:			qsTr("Shift size:")
				defaultValue:	0.5
				enabled:		variationReference.currentValue != "studyVariation"
			}
		}
		CheckBox {	name: "Exponentialchart";				label: qsTr("Exponentially weighted moving average chart")
			DoubleField
			{
				name:			"sigma"
				label:			qsTr("Number of standard deviations:")
				defaultValue:	3
				enabled:		variationReference.currentValue != "studyVariation"
			}
			DoubleField
			{
				name:			"lambda"
				label:			qsTr("Lambda:")
				defaultValue:	0.2
				enabled:		variationReference.currentValue != "studyVariation"
			}

		}
		CheckBox {	name: "gchart";							label: qsTr("g chart")												}
		CheckBox {	name: "tchart";							label: qsTr("t chart")												}
	}
}


