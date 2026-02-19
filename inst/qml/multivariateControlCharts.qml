import QtQuick
import QtQuick.Layouts
import JASP.Controls

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
			name:								"variables"
			title:								qsTr("Variables")
			allowedColumns:						["scale"]
			info:								qsTr("Two or more continuous quality characteristics to monitor jointly using a Hotelling T² chart.")
		}
	}


	Group
	{

		title:									qsTr("Control Chart")

		CheckBox
		{
			name:								"confidenceLevelAutomatic"
			id:									confidenceLevelAutomatic
			label:								qsTr("Automatic confidence level: (1 - 0.0027)ᵖ")
			checked:							true
			info:								qsTr("When checked, confidence level is automatically set to (1 - 0.0027)^p, where p is the number of variables. This is the standard Bonferroni-adjusted level for multivariate charts.")
		}

		CIField
		{
			name:								"confidenceLevel"
			label:								qsTr("Confidence level")
			enabled:							!confidenceLevelAutomatic.checked
			info:								qsTr("Custom confidence level for the control limits. Only active when automatic confidence level is unchecked.")
		}
	}



	Section
	{
		title:												qsTr("Output")

		Group
		{
			CheckBox
			{
				name:											"centerTable"
				label:											qsTr("Variable centers")
				checked:										false
				info:											qsTr("Show the mean of each variable.")
			}

			CheckBox
			{
				name:											"covarianceMatrixTable"
				label:											qsTr("Covariance matrix")
				checked:										false
				info:											qsTr("Show the sample covariance matrix of the selected variables.")
			}

			CheckBox
			{
				name:											"tSquaredValuesTable"
				label:											qsTr("T\u00B2 values table")
				checked:										false
				info:											qsTr("Show a table with the T\u00B2 statistic for each observation, along with the UCL and in/out-of-control status.")
			}
		}

		CheckBox
		{
			id:														addTsqToData
			name:													"addTsqToData"
			label:													qsTr("Add T\u00B2 values to data")
			info:													qsTr("Adds the computed Hotelling T\u00B2 values as a new column in the dataset.")

			ComputedColumnField
			{
				name:												"tsqColumn"
				text:												qsTr("Column name")
				placeholderText:								qsTr("e.g., tsq")
				fieldWidth:											120
				enabled:											addTsqToData.checked
			}
		}
		
	}
}
