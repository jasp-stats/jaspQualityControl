import QtQuick
import JASP
import JASP.Controls
import JASP.Widgets

Form
{

	VariablesForm
	{
		AvailableVariablesList
		{
			name:                               "allVariables"
			label:                              qsTr("Available factors")
		}

		AssignedVariablesList
		{
			name:                               "dependent"
			allowedColumns:                     ["scale", "ordinal", "nominal"]
			singleVariable:                     true
			label:                              qsTr("Response")
		}

		AssignedVariablesList
		{
			id:									factors
			name:                               "fixedFactors"
			allowedColumns:                     ["scale", "ordinal", "nominal", "nominalText"]
			label:                              qsTr("Factors")
			height:								250 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:                               "blocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
		}

		AssignedVariablesList
		{
			visible:							false
			name:                               "covariates"
		}

		AssignedVariablesList
		{
			visible:                            false
			name:                               "wlsWeights"
		}
	}

	CheckBox
	{
		name:                               "tableAlias"
		label:                              "Show alias structure"
	}

	Section
	{
		title: qsTr("Model")

		CheckBox
		{
			id:										highestOrder
			name:                                   "highestOrder"
			label:                              	qsTr("Define by highest order interaction term")

			IntegerField
			{
				name:                               "order"
				defaultValue:                        1
				min:                                 1
				max:                                 factors.count > 0 ? factors.count : 999
				label:								  qsTr("Highest order interaction term")
			}
		}

		VariablesForm
		{
			enabled: !highestOrder.checked
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors"]}
			AssignedVariablesList {  name: "modelTerms"; id: modelTerms; title: qsTr("Model Terms"); listViewType: JASP.Interaction}
		}

	}

	Group
	{
		title:                                  qsTr("Residuals Plots")

		CheckBox
		{
			name:                               "plotNorm"
			label:                              qsTr("Normal probability plot")
		}

		CheckBox
		{
			name:                               "plotHist"
			label:                              qsTr("Histogram")
		}

		CheckBox
		{
			name:                               "plotFitted"
			label:                              qsTr("Residuals vs. fitted values")
		}

		CheckBox
		{
			name:                               "plotRunOrder"
			label:                              qsTr("Residuals vs. run order")
		}

		// CheckBox
		// {
		// 	name:                               "fourInOne"
		// 	label:                              qsTr("Matrix residuals plot")
		// 	enabled:							runOrder.count > 0
		// }
	}

	Group
	{
		title: qsTr("Other Plots")

		CheckBox
		{
			name:                                   "plotPareto"
			label:                                  qsTr("Pareto Plot of Standardized Effects")
		}
	}
}
