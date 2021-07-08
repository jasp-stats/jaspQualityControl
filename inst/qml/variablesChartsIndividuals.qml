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
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									variables
			name:								"variables"
			title:								qsTr("Variables")
			allowedColumns:						["scale"]
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
				  name:			"ncol"
				  label:			qsTr("Moving range length:")
				  defaultValue:	2
			  }
		}

		CheckBox
		{
			name: 								"CorPlot"
			label: 								qsTr("Autocorrelation")
			checked: 							false

			  DoubleField
			  {
				  name:			"nLag"
				  label:			qsTr("Number of lags:")
				  defaultValue:	24
			  }
			  DoubleField
			  {
				  name:			"CI"
				  label:			qsTr("Confidence interval size:")
				  defaultValue:	0.95
			  }
		}
	}
}
