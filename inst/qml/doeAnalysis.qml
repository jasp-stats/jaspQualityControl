import QtQuick
import JASP
import JASP.Controls
import JASP.Widgets


Form
{
	DropDown
	{
		id: 									designType
		name:									"designType"
		label:									qsTr("Design type")
		indexDefaultValue:						0
		values: [
				{ label: qsTr("Factorial design"), value: "factorialDesign"},
				{ label: qsTr("Response surface design"), value: "responseSurfaceDesign"}
				]
	}

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
			allowedColumns:                     ["ordinal", "nominal", "nominalText"]
			label:                              qsTr("Categorical Factors")
			height:								125 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									continuousFactors
			name:                               "continuousFactors"
			allowedColumns:                     ["scale"]
			label:                              qsTr("Continuous Factors")
			height:								125 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:                               "blocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
			allowedColumns:                     ["ordinal", "nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			visible:							false
			name:                               "covariates"
		}
	}

	Group
	{
		CheckBox
		{
			name:                               "tableAlias"
			label:                              "Show alias structure"
		}

		CheckBox
		{
			name:                               "tableEquation"
			label:                              "Show regression equation"
		}

		CheckBox
		{
			name:                               "codeFactors"
			label:                              "Automatically code factors"
		}
	}

	Section
	{
		title: qsTr("Model")

		CheckBox
		{
			id:										highestOrder
			name:                                   "highestOrder"
			label:                              	qsTr("Define by highest order interaction term")
			visible:								designType.currentValue == "factorialDesign"

			IntegerField
			{
				name:                               "order"
				defaultValue:                        1
				min:                                 1
				max:                                 factors.count > 0 ? factors.count : 999
				label:								  qsTr("Highest order interaction term")
			}
		}

		CheckBox
		{
			id:										rsmPredefinedModel
			name:                                   "rsmPredefinedModel"
			label:                              	qsTr("Select predefined model")
			visible:								designType.currentValue == "responseSurfaceDesign"
			checked: 								designType.currentValue == "responseSurfaceDesign"								

			DropDown
					{
					id: 									rsmPredefinedTerms
					name:									"rsmPredefinedTerms"
					label:									qsTr("Include following terms")
					indexDefaultValue:						3
					values: [
							{ label: qsTr("Linear"), value: "linear"},
							{ label: qsTr("Linear and interaction terms"), value: "linearAndInteractions"},
							{ label: qsTr("Linear and squared terms"), value: "linearAndSquared"},
							{ label: qsTr("Full quadratic"), value: "fullQuadratic"}
							]
					}
		}

		VariablesForm
		{
			enabled: !highestOrder.checked & !rsmPredefinedModel.checked
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "continuousFactors"]}
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
