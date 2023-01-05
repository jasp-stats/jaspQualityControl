import QtQuick 2.0

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:                                    1

	VariablesForm
	{
		AvailableVariablesList
		{
			name:                               "FAallVariables"
			label:                              qsTr("Available factors")
		}

		AssignedVariablesList
		{
			name:                               "FAresponse"
			allowedColumns:                     ["scale", "ordinal", "nominal"]
			singleVariable:                     true
			label:                              qsTr("Response variable")
		}

		AssignedVariablesList
		{
			name:                               "FAassignedFactors"
			allowedColumns:                     ["scale", "ordinal", "nominal", "nominalText"]
			label:                              qsTr("Assigned factors")
		}

		AssignedVariablesList
		{
			debug:                              true
			name:                               "FAblocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
		}

		AssignedVariablesList
		{
			id:                                 runOrder
			name:                               "FArunOrder"
			allowedColumns:                     ["scale", "ordinal", "nominal"]
			singleVariable:                     true
			label:                              qsTr("Run order")
		}
	}

	CheckBox
	{
		name:                                   "enabledIntOrder"
		id :                                    "enabledIntOrder"
		childrenOnSameRow: true

		IntegerField
		{
			name:                                   "intOrder"
			label:                                  qsTr("Highest order interaction term:")
			defaultValue:                           1
			min:                                    1
			max:                                    5 // change this to number of items in FAassignedFactors
		}
	}

	Group
	{
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["FAassignedFactors"]}
			AssignedVariablesList {  name: "modelTerms"; id: modelTerms; title: qsTr("Model Terms"); listViewType: JASP.Interaction;
				enabled: !enabledIntOrder.checked }
		}

	}

	Group
	{
		columns: 2
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Design plots")

			CheckBox
			{
				name:                               "showAliasStructure2"
				label:                              "Show alias structure"
				enabled:							runOrder.count > 0
			}

			CheckBox
			{
				name:                                   "paretoPlot"
				label:                                  qsTr("Pareto Plot of Standardized Effects")
			}
		}

		Group
		{
			name:                                   "resPlots"
			title:                                  qsTr("Residuals plots")

			CheckBox
			{
				name:                               "resNorm"
				label:                              qsTr("Normal probability plot of residuals")
			}

			CheckBox
			{
				name:                               "resHist"
				label:                              qsTr("Histogram of residuals")
			}

			CheckBox
			{
				name:                               "resFitted"
				label:                              qsTr("Residuals vs fitted value")
			}

			CheckBox
			{
				name:                               "resOrder"
				label:                              qsTr("Residuals vs run/standard order")
				enabled:							runOrder.count > 0

				RadioButtonGroup
				{
					name:                                   "runOrderPlot"


					RadioButton
					{
						name:                              "runOrderStandardPlot"
						label:                              qsTr("Standard")
						checked:                            true
					}

					RadioButton
					{
						name:                               "runOrderRandomPlot"
						label:                              qsTr("Run")
					}
				}
			}

			CheckBox
			{
				name:                               "fourInOne"
				label:                              qsTr("Matrix residuals plot")
				enabled:							runOrder.count > 0
			}
		}
	}

}
