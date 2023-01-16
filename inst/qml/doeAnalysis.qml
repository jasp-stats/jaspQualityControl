import QtQuick 2.0

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

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
		}

		AssignedVariablesList
		{
			name:                               "blocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
		}

		AssignedVariablesList
		{
			id:                                 runOrder
			name:                               "runOrder"
			allowedColumns:                     ["scale", "ordinal"]
			singleVariable:                     true
			label:                              qsTr("Run Order")
		}

		AssignedVariablesList
		{
			name:                               "covariates"
			allowedColumns:                     ["scale"]
			label:                              qsTr("Covariates")
		}

		AssignedVariablesList
		{
			visible:                            false
			name:                               "wlsWeights"
			singleVariable:                     true
		}
	}

	CheckBox
	{
		name:                                   "enabledIntOrder"
		childrenOnSameRow: 						true

		IntegerField
		{
			name:                               "intOrder"
			label:                              qsTr("Highest order interaction term")
			defaultValue:                           1
			min:                                    1
			max:                                    factors.count > 0 ? factors.count : 999
		}
	}

	Section
	{
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors"]}
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model Terms"); listViewType: JASP.Interaction; enabled: !enabledIntOrder.checked }
		}

	}

	Group
	{
		title: qsTr("Design")

		CheckBox
		{
			name:                               "tableAlias"
			label:                              "Show alias structure"
			enabled:							runOrder.count > 0
		}

		CheckBox
		{
			name:                                   "plotPareto"
			label:                                  qsTr("Pareto Plot of Standardized Effects")
		}
	}

	Group
	{
		title:                                  qsTr("Residuals")

		CheckBox
		{
			name:                               "plotNorm"
			label:                              qsTr("Normal probability plot of residuals")
		}

		CheckBox
		{
			name:                               "plotHist"
			label:                              qsTr("Histogram of residuals")
		}

		CheckBox
		{
			name:                               "plotFitted"
			label:                              qsTr("Residuals vs fitted value")
		}

		CheckBox
		{
			name:                               "plotRunOrder"
			label:                              qsTr("Residuals vs run/standard order")
			enabled:							runOrder.count > 0

			RadioButtonGroup
			{
				name:                                   "plotOrder"


				RadioButton
				{
					name:                              "plotOrderStandard"
					label:                              qsTr("Standard")
					checked:                            true
				}

				RadioButton
				{
					name:                               "plotOrderRun"
					label:                              qsTr("Run")
				}
			}
		}

		// CheckBox
		// {
		// 	name:                               "fourInOne"
		// 	label:                              qsTr("Matrix residuals plot")
		// 	enabled:							runOrder.count > 0
		// }
	}
}
