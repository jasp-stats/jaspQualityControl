import QtQuick
import JASP
import JASP.Controls
import JASP.Widgets
import QtQuick.Layouts


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
		id:									variablesFormFactorial
		visible:							designType.currentValue == "factorialDesign"
		AvailableVariablesList
		{
			name:								"allVariablesFactorial"
			label:								qsTr("Available variables")
		}

		AssignedVariablesList
		{
			name:								"dependentFactorial"
			allowedColumns:						["scale", "ordinal"]
			singleVariable:						true
			label:								qsTr("Response")
		}

		AssignedVariablesList
		{
			id:									fixedFactorsFactorial
			name:								"fixedFactorsFactorial"
			allowedColumns:						["ordinal", "nominal", "nominalText"]
			label:								qsTr("Discrete predictors")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									continuousFactorsFactorial
			name:								"continuousFactorsFactorial"
			allowedColumns:						["scale", "ordinal"]
			label:								qsTr("Continuous predictors")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"covariates"
			id:									covariates
			label:								qsTr("Covariates")
			allowedColumns:						["ordinal", "scale"]
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"blocksFactorial"
			singleVariable:						true
			label:								qsTr("Blocks")
			allowedColumns:						["ordinal", "scale", "nominal", "nominalText"]
		}
	}

	VariablesForm
	{
		id:									variablesFormResponseSurface
		visible:							designType.currentValue == "responseSurfaceDesign"
		AvailableVariablesList
		{
			name:								"allVariablesResponseSurface"
			label:								qsTr("Available variables")
		}

		AssignedVariablesList
		{
			name:								"dependentResponseSurface"
			allowedColumns:						["scale", "ordinal"]
			singleVariable:						true
			label:								qsTr("Response")
		}

		AssignedVariablesList
		{
			id:									continuousFactorsResponseSurface
			name:								"continuousFactorsResponseSurface"
			allowedColumns:						["scale", "ordinal"]
			label:								qsTr("Continuous predictors")
			height:								125 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									fixedFactorsResponseSurface
			name:								"fixedFactorsResponseSurface"
			allowedColumns:						["ordinal", "nominal", "nominalText"]
			label:								qsTr("Discrete predictors")
			height:								125 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"blocksResponseSurface"
			singleVariable:						true
			label:								qsTr("Blocks")
			allowedColumns:						["ordinal", "scale", "nominal", "nominalText"]
		}
	}

	Group
	{
		columns:				2


		Group 
		{
			RadioButtonGroup
			{
				name: 					"runOrderSource"
				id: 					runOrderSource
				title:					qsTr("Run order")

				RadioButton
				{
					name: 				"runOrderSourceRowNumber"
					id : 				runOrderSourceRowNumber
					label: 				qsTr("Equal to row number")
					checked: 			true
				}

				// RadioButton
				// {
				// 	name: 				"runOrderSourceVariable"
				// 	id : 				runOrderSourceVariable
				// 	label: 				qsTr("Specified as variable")
				// }
			}

			RadioButtonGroup
			{
				name: 					"codeFactorsMethod"
				id: 					codeFactorsMethod
				title:					qsTr("Predictor levels")

				RadioButton
				{
					name: 				"automatic"
					label: 				qsTr("Automatically detect low/high")
					checked: 			true
				}

				RadioButton
				{
					name: 				"manual"
					label: 				qsTr("Manually specify low/high")
				}


				VariablesList
				{
					id					: codeFactorsManualTable
					name				: "codeFactorsManualTable"
					label				: qsTr("Predictor")
					visible				: codeFactorsMethod.value == "manual"
					optionKey			: "predictors"
					source				: designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial", "fixedFactorsFactorial", "blocksFactorial"] : ["continuousFactorsResponseSurface", "fixedFactorsResponseSurface", "blocksResponseSurface"]
					listViewType		: JASP.AssignedVariables
					draggable			: false
					preferredHeight		: jaspTheme.smallDefaultVariablesFormHeight
					rowComponentTitle	: qsTr("Low        High   ")

					rowComponent: RowLayout
					{
						Row
						{
							spacing:				customPriorLayout.space
							Layout.preferredWidth:	customPriorLayout.prefWidth
							TextField
							{
								name:			"lowValue"
								fieldWidth:		40
								defaultValue:	-1
							}
						}
						Row
						{
							spacing:				customPriorLayout.space
							Layout.preferredWidth:	customPriorLayout.prefWidth
							TextField
							{
								name:			"highValue"
								fieldWidth:		40
								defaultValue:	1
							}
						}
					}
				}
			}
		}

		Group
		{
			CheckBox
			{
				name:								"tableAlias"
				label:								"Use alias names"
				checked:							true
			}

			CheckBox
			{
				name:								"tableEquation"
				label:								qsTr("Show regression equation")
			}

			CheckBox
			{
				name:								"codeFactors"
				label:								qsTr("Display results in coded units")
			}
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
			enabled: (!highestOrder.checked && designType.currentValue == "factorialDesign") || (!rsmPredefinedModel.checked && designType.currentValue == "responseSurfaceDesign")
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial", "fixedFactorsFactorial"] : ["continuousFactorsResponseSurface", "fixedFactorsResponseSurface"]}
			AssignedVariablesList {  name: "modelTerms"; id: modelTerms; title: qsTr("Model Terms"); listViewType: JASP.Interaction}
		}

	}

		Section
	{
		title: qsTr("Plots")
		columns: 	1

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

			CheckBox
			{
				name: 								"fourInOneResidualPlot"
				label: 								qsTr("Residuals matrix plot")
			}
		}

		Group
		{
			title: qsTr("Other Plots")

			CheckBox
			{
				name:                                   "plotPareto"
				label:                                  qsTr("Pareto Plot of Effects")
			}
		}

		CheckBox
		{
			name:	"contourSurfacePlot"	
			label:	qsTr("Contour/surface plots")

			RadioButtonGroup
			{
				name: 					"contourSurfacePlotType"
				id: 					contourSurfacePlotType
				title:                 	qsTr("Plot type")

				RadioButton
				{
					name: 				"contourPlot"
					id : 				contourPlot
					label: 				qsTr("Contour plot")
					checked: 			true
				}

				RadioButton
				{
					name: 				"surfacePlot"
					id : 				surfacePlot
					label: 				qsTr("Surface plot")
				}
			}

			VariablesForm
			{
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				
				AvailableVariablesList
				{ 
					name: "continuousPredictorsPlots"	
					source: designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial"] : ["continuousFactorsResponseSurface"] 
					title: qsTr("Available continuous predictors")
				}

				AssignedVariablesList
				{ 
					name: "contourSurfacePlotVariables" 
					suggestedColumns: ["scale", "ordinal"] 
					title: qsTr("Plotting variables")
				}
			}

			Group
			{
				title: 		qsTr("Contour/surface plot options")
				columns:	1

				CheckBox
				{
					name:						"contourSurfacePlotLegend"
					label:						qsTr("Show legend next to graph")
					checked:					true
				}

				IntegerField
				{
					name:						"contourSurfacePlotResponseDivision"
					label:						qsTr("Divide response surface into N parts")
					value:						5
					min:						2
					max:						10
				}

				Group
				{
					columns:	2

					Slider
					{
						name:						"surfacePlotVerticalRotation"
						label:						qsTr("Rotating angle (vertical plane)")
						value:						20
						min:						0
						max:						360
						decimals:					0
						visible:					contourSurfacePlotType.value == "surfacePlot"
					}

					Slider
					{
						name:						"surfacePlotHorizontalRotation"
						label:						qsTr("Rotating angle (horizontal plane)")
						value:						330
						min:						0
						max:						360
						decimals:					0
						vertical:					false
						visible:					contourSurfacePlotType.value == "surfacePlot"
					}
				}
			}
		}
	}

		Section
	{
		title: qsTr("Advanced options")

		Group
		{
			DropDown
			{
				name:							"histogramBinWidthType"
				label:							qsTr("Histogram bin width type")
				id: 							binWidthType
				indexDefaultValue:				0
				values: [
					{ label: qsTr("Sturges"), value: "sturges"},
					{ label: qsTr("Scott"), value: "scott"},
					{ label: qsTr("Doane"), value: "doane"},
					{ label: qsTr("Freedman-Diaconis"), value: "freedmanDiaconis"},
					{ label: qsTr("Manual"), value: "manual"}
				]
			}

			DoubleField
			{
				name:							"histogramManualNumberOfBins"
				label:							qsTr("Number of bins")
				defaultValue:					30
				min:							3
				max:							10000
				enabled:						binWidthType.currentValue === "manual"
			}
		}
	}
}
