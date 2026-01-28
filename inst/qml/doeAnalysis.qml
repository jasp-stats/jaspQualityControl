import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP


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
			allowedColumns:						["scale"]
			label:								qsTr("Responses")
			height:								50 * preferencesModel.uiScale
		}
		
		DropDown
		{
			name: "stepwiseMethodFactorial"
			label: qsTr("Method")
			info: qsTr("Specify the order in which the predictors are entered into the model. A block of one or more predictors represents one step in the hierarchy. Note that the present release does not allow for more than one block. The backward and forward method are based on model AIC.")
			values: [
				{ label: qsTr("Enter"),	info: qsTr("All predictors are entered into the model simultaneously.")	,value: "enter"},
				{ label: qsTr("Backward"), info: qsTr("Starting with the full model, predictors are removed sequentially based on AIC."), value: "backward"},
				{ label: qsTr("Forward"), info: qsTr("Starting with the intercept-only model, predictors are entered sequentially based on AIC.")	, value: "forward"}	
			]
		}

		AssignedVariablesList
		{
			id:									fixedFactorsFactorial
			name:								"fixedFactorsFactorial"
			allowedColumns:						["nominal"]
			label:								qsTr("Discrete factors")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									continuousFactorsFactorial
			name:								"continuousFactorsFactorial"
			allowedColumns:						["scale"]
			label:								qsTr("Continuous factors")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"covariates"
			id:									covariates
			label:								qsTr("Covariates")
			allowedColumns:						["scale"]
			height:								50 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"blocksFactorial"
			singleVariable:						true
			label:								qsTr("Blocks")
			allowedColumns:						["nominal"]
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
			allowedColumns:						["scale"]
			label:								qsTr("Responses")
			height:								50 * preferencesModel.uiScale
		}
		
		DropDown
		{
			name: "stepwiseMethodResponseSurface"
			label: qsTr("Method")
			info: qsTr("Specify the order in which the predictors are entered into the model. A block of one or more predictors represents one step in the hierarchy. Note that the present release does not allow for more than one block.")
			values: [
				{ label: qsTr("Enter"),	info: qsTr("All predictors are entered into the model simultaneously.")	,value: "enter"},
				{ label: qsTr("Backward"), info: qsTr("Starting with the full model, predictors are removed sequentially based on AIC."), value: "backward"},
				{ label: qsTr("Forward"), info: qsTr("Starting with the intercept-only model, predictors are entered sequentially based on AIC.")	, value: "forward"}	
			]
		}

		AssignedVariablesList
		{
			id:									continuousFactorsResponseSurface
			name:								"continuousFactorsResponseSurface"
			allowedColumns:						["scale"]
			label:								qsTr("Continuous factors")
			height:								100 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									fixedFactorsResponseSurface
			name:								"fixedFactorsResponseSurface"
			allowedColumns:						["nominal"]
			label:								qsTr("Discrete factors")
			height:								100 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"blocksResponseSurface"
			singleVariable:						true
			label:								qsTr("Blocks")
			allowedColumns:						["nominal"]
		}
	}

	Group
	{
		columns:				2
    	Layout.topMargin: 16

		Group 
		{
			RadioButtonGroup
			{
				name: 					"codeFactorsMethod"
				id: 					codeFactorsMethod
				title:					qsTr("Factor levels")

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
					label				: qsTr("Factor")
					visible				: codeFactorsMethod.value == "manual"
					optionKey			: "predictors"
					source				: designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial", "fixedFactorsFactorial", "blocksFactorial"] : ["continuousFactorsResponseSurface", "fixedFactorsResponseSurface", "blocksResponseSurface"]
					listViewType		: JASP.AssignedVariables
					draggable			: false
					preferredHeight		: jaspTheme.smallDefaultVariablesFormHeight

					RowLayout
					{
						id: rowTitle
						anchors.right: 	parent.right
						spacing: 2

						Text { text: qsTr("Low"); 	horizontalAlignment: Text.AlignHCenter;		Layout.preferredWidth: 50; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
						Text { text: qsTr("High"); 	horizontalAlignment: Text.AlignHCenter;		Layout.preferredWidth: 50; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					}

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
				checked:							true
				label:								qsTr("Show regression equation")
			}

			CheckBox
			{
				name:								"codeFactors"
				checked:							true
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
			name:									"highestOrder"
			label:									qsTr("Define by highest order interaction term")
			visible:								designType.currentValue == "factorialDesign"
			checked:								true

			IntegerField
			{
				name:								"order"
				defaultValue:						2
				min:								1
				max:								(fixedFactorsFactorial.count + continuousFactorsFactorial.count) > 1 ? (fixedFactorsFactorial.count + continuousFactorsFactorial.count) : 999
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
			enabled: 				(!highestOrder.checked && designType.currentValue == "factorialDesign") || (!rsmPredefinedModel.checked && designType.currentValue == "responseSurfaceDesign")
			preferredHeight: 		jaspTheme.smallDefaultVariablesFormHeight
			
			AvailableVariablesList 
			{ 
				name: 					"components" 
				title: 					qsTr("Components") 
				source: 				designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial", "fixedFactorsFactorial"] : ["continuousFactorsResponseSurface", "fixedFactorsResponseSurface"]
			}
			
			AssignedVariablesList 
			{  
				name: 					"modelTerms" 
				id: 					modelTerms 
				title: 					designType.currentValue == "factorialDesign" ? qsTr("Model terms") : qsTr("Main and interaction terms") 
				listViewType: 			JASP.Interaction
				addInteractionsByDefault: false
				//rowComponentTitle: 		designType.currentValue == "factorialDesign" ? "" : qsTr("Add squared term")
				//rowComponent:  			CheckBox { name: "squaredTerm"; checked: false; visible: designType.currentValue == "responseSurfaceDesign"}
			}
		}

		VariablesForm
		{
			enabled: 				(!rsmPredefinedModel.checked && designType.currentValue == "responseSurfaceDesign")
			visible: 				designType.currentValue == "responseSurfaceDesign"
			preferredHeight: 		jaspTheme.smallDefaultVariablesFormHeight * .5
			
			AvailableVariablesList 
			{ 
				name: 					"squaredComponents" 
				title: 					qsTr("Continuous factors") 
				source: 				"continuousFactorsResponseSurface"
			}
			
			AssignedVariablesList 
			{  
				name: 					"squaredTerms" 
				id: 					squaredTerms 
				title: 					qsTr("Squared terms") 
			}

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
				label: 								qsTr("Residuals four-in-one plot")
			}
		}

		Group
		{
			title: qsTr("Other Plots")

			CheckBox
			{
				name:									"plotPareto"
				label:									qsTr("Pareto plot of effects")
			}

			CheckBox
			{
				name:									"normalEffectsPlot"
				label:									qsTr("Normal plot of effects")
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
					title: qsTr("Available continuous factors")
				}

				AssignedVariablesList
				{ 
					name: "contourSurfacePlotVariables" 
					allowedColumns: ["scale"] 
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
		title: qsTr("Response optimizer")
		columns: 2

		VariablesForm
		{
			id:									variablesFormResponseOptimizer
			preferredHeight: 					jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name:								"allResponseVariables"
				label:								qsTr("Available responses")
				source:								designType.currentValue == "factorialDesign" ? "dependentFactorial" : "dependentResponseSurface"
				width:								100
			}

			AssignedVariablesList
			{
				name:								"responsesResponseOptimizer"
				id:									responsesResponseOptimizer
				allowedColumns:						["scale"]
				label:								qsTr("Included responses")
				width:								450 * preferencesModel.uiScale

				property int rowWidth: 		Math.round(width / 12)

				RowLayout 
				{
					id:						responseHeader
					anchors.right:			parent.right
					spacing: 				6

					Text { text: qsTr("Goal");			Layout.preferredWidth: responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					Text { text: qsTr("Lower");			Layout.preferredWidth: responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					Text { text: qsTr("Target");		Layout.preferredWidth: responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					Text { text: qsTr("Upper");			Layout.preferredWidth: responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					Text { text: qsTr("Weight");		Layout.preferredWidth: responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
					Text { text: qsTr("Importance");	Layout.preferredWidth: 2 * responsesResponseOptimizer.rowWidth; wrapMode: Text.WordWrap; elide: Text.ElideNone; }
				}

				rowComponent: Row
				{
					spacing: 6
					DropDown
					{
						name:							"responseOptimizerGoal"
						id: 							responseOptimizerGoal
						indexDefaultValue:				0
						values: [
							{ label: qsTr("Maximize"), value: "maximize"},
							{ label: qsTr("Minimize"), value: "minimize"},
							{ label: qsTr("Target"), value: "target"}
						]
					}
					DoubleField
					{
						name:							"responseOptimizerLowerBound"
						defaultValue:					0
						fieldWidth:						1.2 * responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerGoal.currentValue != "minimize" & responseOptimizerManualBounds.checked
					}
					DoubleField
					{
						name:							"responseOptimizerTarget"
						defaultValue:					0.5
						fieldWidth:						responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerManualTarget.checked | responseOptimizerGoal.currentValue == "target"
					}
					DoubleField
					{
						name:							"responseOptimizerUpperBound"
						defaultValue:					1
						fieldWidth:						responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerGoal.currentValue != "maximize" & responseOptimizerManualBounds.checked
					}
					DoubleField
					{
						name:							"responseOptimizerWeight"
						defaultValue:					1
						min:							0.1
						max:							10
						fieldWidth:						responsesResponseOptimizer.rowWidth
					}
					DoubleField
					{
						name:							"responseOptimizerImportance"
						defaultValue:					1
						min:							0.1
						max:							10
						fieldWidth:					    1.7 * responsesResponseOptimizer.rowWidth
						enabled:						responsesResponseOptimizer.count > 1
					}
				}
			}
		}
		
		Group
		{
			columns:					1
			
			CheckBox
			{
				name:						"optimizationSolutionTable"
				label:						qsTr("Show optimal solution")
				checked:					true
			}

			CheckBox
			{
				name:						"optimizationPlot"
				label:						qsTr("Show optimization plot")
				checked:					true

				DropDown
				{
					name:							"optimizationPlotPredictionType"
					label:							qsTr("Predict")
					id: 							optimizationPlotPredictionType
					indexDefaultValue:				0
					values: [
						{ label: qsTr("Response"), value: "response"},
						{ label: qsTr("Individual desirability"), value: "individualDesirability"}
					]
				}

				CheckBox
				{
					name:						"optimizationPlotCustomParameters"
					id:							optimizationPlotCustomParameters
					label:						qsTr("Set input parameters manually")
					checked:					false
				}

				VariablesList
				{
					id: 				optimizationPlotCustomParameterValues
					name:				"optimizationPlotCustomParameterValues"
					label:				qsTr("Factor")
					visible:			optimizationPlotCustomParameters.checked
					source: 			designType.currentValue == "factorialDesign" ? ["continuousFactorsFactorial", "fixedFactorsFactorial"] : ["continuousFactorsResponseSurface", "fixedFactorsResponseSurface"]
					listViewType:		JASP.AssignedVariables
					draggable:			false
					preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight
					rowComponentTitle:	qsTr("Value")
					rowComponent: 		TextField { name: "value"; fieldWidth: 40; defaultValue: ""}
				}
			}
		}

		Group
		{
			columns:					1
			
			CheckBox
			{
				name:						"responseOptimizerManualBounds"
				id:							responseOptimizerManualBounds
				label:						qsTr("Set all bounds manually")
				checked:					false
			}

			CheckBox
			{
				name:						"responseOptimizerManualTarget"
				id:							responseOptimizerManualTarget
				label:						qsTr("Set all targets manually")
				checked:					false
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
					{ label: qsTr("Freedman-Diaconis"), value: "fd"},
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

		DropDown
		{
			name:							"sumOfSquaresType"
			label:							qsTr("Sum of squares type")
			id: 							sumOfSquaresType
			indexDefaultValue:				(designType.currentValue == "factorialDesign" && continuousFactorsFactorial.count > 0) ? 1 : 2
			values: [
				{ label: qsTr("Type I"), value: "type1"},
				{ label: qsTr("Type II"), value: "type2"},
				{ label: qsTr("Type III"), value: "type3"},
			]
		}

			CheckBox
			{
				name:						"squaredTermsCoded"
				label:						qsTr("Use coded data to calculate squared terms")
				visible:					designType.currentValue == "responseSurfaceDesign"
			}
	}
}
