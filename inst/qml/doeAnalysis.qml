import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP


Form
{
	info: qsTr("The purpose of analysing factorial and response surface designs is to identify influential factors and optimal response conditions. The analysis shows which factors significantly affect the response and how they interact. This allows to systematically optimize processes and improve product quality.")
	infoBottom: "## " + qsTr("Analysis of variance table") + "\n"
				+ qsTr("The analysis of variance (ANOVA) table splits the variation in the response into the parts explained by the terms of the model and the part the model does not explain (Error).") + "\n\n"
				+ "### " + qsTr("Columns") + "\n"
				+ "- **" + qsTr("Sum of squares") + "**: " + qsTr("Variation in the response attributed to the source.") + "\n"
				+ "- **df**: " + qsTr("Degrees of freedom. For a term, the number of model coefficients it contains (fewer when some of its contrasts are aliased).") + "\n"
				+ "- **" + qsTr("Mean square") + "**: " + qsTr("Sum of squares divided by df.") + "\n"
				+ "- **F**: " + qsTr("Mean square of the source divided by the mean square of Error.") + "\n"
				+ "- **p**: " + qsTr("Probability of an F-value at least this large if all coefficients of the source are zero.") + "\n\n"
				+ "### " + qsTr("Rows") + "\n"
				+ "- **" + qsTr("Model") + "**: " + qsTr("Tests whether the model explains any variation, i.e. whether at least one coefficient besides the intercept differs from zero. Its sum of squares equals Total minus Error and is the same for all sums of squares types.") + "\n"
				+ "- **" + qsTr("Linear terms, Squared terms, Interaction terms, Covariates") + "**: " + qsTr("Joint test of all terms in the group, e.g. whether all linear effects are zero. The group is treated as one term and adjusted according to the selected sums of squares type.") + "\n"
				+ "- **" + qsTr("Single terms") + "**: " + qsTr("Test of one term (listed below its group), adjusted according to the selected sums of squares type.") + "\n"
				+ "- **" + qsTr("Block") + "**: " + qsTr("Test of the differences between blocks.") + "\n"
				+ "- **" + qsTr("Error") + "**: " + qsTr("Variation the model does not explain. Its mean square estimates the noise variance and is the denominator of all F-tests.") + "\n"
				+ "- **" + qsTr("Lack of fit, Pure error") + "**: " + qsTr("Shown below Error when the design has replicated runs, e.g. center points. Pure error is the variation between replicates. Lack of fit tests whether the model misses systematic variation such as curvature; a small p-value indicates a poor fit.") + "\n"
				+ "- **" + qsTr("Total") + "**: " + qsTr("Total variation of the response around its mean, with df equal to the number of runs minus 1. Model plus Error equals Total.") + "\n\n"
				+ qsTr("With type I sums of squares, and in balanced (orthogonal) designs, the term rows add up to their group row, and the group rows and Block add up to Model. With type II or III sums of squares in unbalanced designs this is generally not the case: when terms are correlated, part of the variation can be explained by several terms, and this shared part is not attributed to any single term.") + "\n\n"
				+ "---\n"
				+ "## " + qsTr("References") + "\n"
				+ "- Fox, J., & Weisberg, S. (2019). *An R companion to applied regression* (3rd ed.). Sage." + "\n"
				+ "- Langsrud, Ø. (2003). ANOVA for unbalanced data: Use Type II instead of Type III sums of squares. *Statistics and Computing, 13*(2), 163-167." + "\n"
				+ "- Montgomery, D. C. (2017). *Design and analysis of experiments* (9th ed.). Wiley." + "\n"
				+ "\n---\n"
				+ "## " + qsTr("R Packages") + "\n"
				+ "- car\n"
				+ "- ggplot2\n"
				+ "- jaspGraphs\n"
				+ "- rsm\n"
				+ "- stats\n"

	DropDown
	{
		id: 									designType
		name:									"designType"
		label:									qsTr("Design type")
		info:									qsTr("Select the type of experimental design you want to analyse.") + "\n"
												+ "- **" + qsTr("Factorial design") + "**: " + qsTr("A factorial design is an experimental setup used to study the effects of multiple factors by varying them simultaneously. Each factor is tested at different levels and in different combinations with other factors. This design allows for the examination of both the main effects of each factor and the interactions between factors.") + "\n"
												+ "- **" + qsTr("Response surface design") + "**: " + qsTr("A response surface design is an experimental framework used to optimize and understand the relationships between several explanatory variables and one or more response variables. Besides main effects and interactions, it allows to estimate squared terms, which describe the curvature of the response. This gives a detailed map of the response landscape, which helps to find optimal conditions.")
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
			info:								qsTr("The measured outcome variables of the process under investigation. Each response is analysed separately.")
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
				{ label: qsTr("Forward"), info: qsTr("Starting with the intercept-only model, predictors are entered sequentially based on AIC.")	, value: "forward"},	
				{ label: qsTr("Stepwise"), info: qsTr("Starting with the intercept-only model, predictors are entered or removed sequentially based on AIC, combining both forward addition and backward elimination at each step.")	, value: "both"}		
			]
		}

		AssignedVariablesList
		{
			id:									fixedFactorsFactorial
			name:								"fixedFactorsFactorial"
			allowedColumns:						["nominal"]
			label:								qsTr("Discrete factors")
			info:								qsTr("The columns corresponding to the discrete factors in the design.")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									continuousFactorsFactorial
			name:								"continuousFactorsFactorial"
			allowedColumns:						["scale"]
			label:								qsTr("Continuous factors")
			info:								qsTr("The columns corresponding to the continuous factors in the design.")
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"covariates"
			id:									covariates
			label:								qsTr("Covariates")
			info:								qsTr("The columns corresponding to the covariates in the design. The difference between a covariate and a continuous factor is that the covariate will not be analysed for interaction effects and is excluded from effect plots.")
			allowedColumns:						["scale"]
			height:								50 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									blocksFactorial
			name:								"blocksFactorial"
			singleVariable:						true
			label:								qsTr("Blocks")
			info:								qsTr("The column corresponding to the blocks in the design. The difference between a block variable and a discrete factor is that the blocks will not be analysed for interaction effects and are excluded from effect plots.")
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
			info:								qsTr("The measured outcome variables of the process under investigation. Each response is analysed separately.")
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
				{ label: qsTr("Forward"), info: qsTr("Starting with the intercept-only model, predictors are entered sequentially based on AIC.")	, value: "forward"},
				{ label: qsTr("Stepwise"), info: qsTr("Starting with the intercept-only model, predictors are entered or removed sequentially based on AIC, combining both forward addition and backward elimination at each step.")	, value: "both"}		
			]
		}

		AssignedVariablesList
		{
			id:									continuousFactorsResponseSurface
			name:								"continuousFactorsResponseSurface"
			allowedColumns:						["scale"]
			label:								qsTr("Continuous factors")
			info:								qsTr("The columns corresponding to the continuous factors in the design.")
			height:								100 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									fixedFactorsResponseSurface
			name:								"fixedFactorsResponseSurface"
			allowedColumns:						["nominal"]
			label:								qsTr("Discrete factors")
			info:								qsTr("The columns corresponding to the discrete factors in the design.")
			height:								100 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			id:									blocksResponseSurface
			name:								"blocksResponseSurface"
			singleVariable:						true
			label:								qsTr("Blocks")
			info:								qsTr("The column corresponding to the blocks in the design. The difference between a block variable and a discrete factor is that the blocks will not be analysed for interaction effects and are excluded from effect plots.")
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
				info:					qsTr("For the ordering of contrasts and to identify the alpha points in a response surface design, the low and high levels of all factors need to be specified. There are two options to handle this.")

				RadioButton
				{
					name: 				"automatic"
					label: 				qsTr("Automatically detect low/high")
					info:				qsTr("Attempts to automatically detect the low and high levels by taking the minimum and maximum for continuous factors, and by ordering discrete factors alphabetically. For response surface designs, this might recognize alpha values as low/high levels. In this case, manual specification is needed.")
					checked: 			true
				}

				RadioButton
				{
					name: 				"manual"
					label: 				qsTr("Manually specify low/high")
					info:				qsTr("Shows all factors in the analysis to allow for manual specification of the low and high levels.")
				}


				VariablesList
				{
					id					: codeFactorsManualTable
					name				: "codeFactorsManualTable"
					label				: qsTr("Factor")
					info				: qsTr("Enter the low and high level of each factor. Blocks and discrete factors with more than two levels are not rescaled to -1/1, so they have no low and high level (\"Not available\").")
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
						// Blocks and discrete factors with more than two levels are not rescaled to -1/1 (the latter keep their
						// sum-to-zero contrasts), so they have no low/high level; the R code ignores their lowValue/highValue
						property var	blocks:		designType.currentValue == "factorialDesign" ? blocksFactorial : blocksResponseSurface
						property bool	hasLowHigh:	rowType === "scale" || (!blocks.columnsNames.includes(rowValue) && rowVariableLevels.levels.length <= 2)

						// Hidden helper to read the levels of this row's variable; isBound: false keeps it out of the options
						DropDown { id: rowVariableLevels; name: "rowVariableLevels"; isBound: false; visible: false; values: [rowValue] }

						Row
						{
							visible:				hasLowHigh
							TextField
							{
								name:			"lowValue"
								fieldWidth:		40
								defaultValue:	-1
							}
						}
						Row
						{
							visible:				hasLowHigh
							TextField
							{
								name:			"highValue"
								fieldWidth:		40
								defaultValue:	1
							}
						}
						Text
						{
							visible:				!hasLowHigh
							text:					qsTr("Not available")
							color:					jaspTheme.grayDarker
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
				label:								qsTr("Use alias names")
				info:								qsTr("Assign alias names to the factors. Useful when factors have long names.")
				checked:							true
			}

			CheckBox
			{
				name:								"tableEquation"
				checked:							true
				label:								qsTr("Show regression equation")
				info:								qsTr("Display the regression coefficients as an equation predicting the response.")
			}

			CheckBox
			{
				name:								"codeFactors"
				checked:							true
				label:								qsTr("Display results in coded units")
				info:								qsTr("Display the results in coded units. This means that the levels of continuous factors and of discrete factors with two levels are rescaled to lie between -1 and 1. Discrete factors with more than two levels are not rescaled: a factor with k levels enters the model with k-1 terms (sum-to-zero contrasts), as in uncoded units, and their low/high levels are not used. For factorial designs, the ANOVA table is calculated in coded units, so it does not change with this option, unless coding changes the model (an interaction or squared term without its main effects).")
			}

			CheckBox
			{
				name:								"tableCoefficientsCi"
				label:								qsTr("Confidence intervals")
				info:								qsTr("Add confidence intervals for the effects and the coefficients to the coefficients table, based on the t-distribution with the error degrees of freedom. The interval for an effect is twice the interval for its coefficient (in coded units).")
				childrenOnSameRow:					true

				CIField
				{
					name:							"tableCoefficientsCiLevel"
					info:							qsTr("Confidence level of the intervals.")
				}
			}
		}
	}

	Section
	{
		title: qsTr("Model")
		info: qsTr("There are different options available to determine the terms that should be added to the model.")

		CheckBox
		{
			id:										highestOrder
			name:									"highestOrder"
			label:									qsTr("Define by highest order interaction term")
			info:									qsTr("Include all main effects and all interactions up to the specified order. Uncheck to define the model terms manually.")
			visible:								designType.currentValue == "factorialDesign"
			checked:								true

			IntegerField
			{
				name:								"order"
				defaultValue:						2
				min:								1
				max:								(fixedFactorsFactorial.count + continuousFactorsFactorial.count) > 1 ? (fixedFactorsFactorial.count + continuousFactorsFactorial.count) : 999
				label:								  qsTr("Highest order interaction term")
				info:								qsTr("The highest order of the interactions to include. For example, 2 includes all two-way interactions.")
			}
		}

		CheckBox
		{
			id:										rsmPredefinedModel
			name:                                   "rsmPredefinedModel"
			label:                              	qsTr("Select predefined model")
			info:									qsTr("Use one of the predefined models offered for analysing response surface designs. Uncheck to define the model terms manually.")
			visible:								designType.currentValue == "responseSurfaceDesign"
			checked: 								designType.currentValue == "responseSurfaceDesign"

			DropDown
					{
					id: 									rsmPredefinedTerms
					name:									"rsmPredefinedTerms"
					label:									qsTr("Include following terms")
					info:									qsTr("The terms of the predefined model.") + "\n"
															+ "- **" + qsTr("Linear") + "**: " + qsTr("Main effects only.") + "\n"
															+ "- **" + qsTr("Linear and interaction terms") + "**: " + qsTr("Main effects and all two-way interactions.") + "\n"
															+ "- **" + qsTr("Linear and squared terms") + "**: " + qsTr("Main effects and the squared terms of all continuous factors.") + "\n"
															+ "- **" + qsTr("Full quadratic") + "**: " + qsTr("Main effects, all two-way interactions and the squared terms of all continuous factors.")
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
				info: 					qsTr("Allows manually defining the main effects and interactions in the model.")
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
				info: 					qsTr("When analysing response surface designs, the continuous factors whose squared terms are included in the manually defined model.")
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
				info:								qsTr("Show a normal probability plot of the residuals.")
			}

			CheckBox
			{
				name:                               "plotHist"
				label:                              qsTr("Histogram")
				info:								qsTr("Show a histogram of the residuals.")
			}

			CheckBox
			{
				name:                               "plotFitted"
				label:                              qsTr("Residuals vs. fitted values")
				info:								qsTr("Show a plot of the residuals per fitted value.")
			}

			CheckBox
			{
				name:                               "plotRunOrder"
				label:                              qsTr("Residuals vs. run order")
				info:								qsTr("Show a plot of the residuals per run order value.")
			}

			CheckBox
			{
				name: 								"fourInOneResidualPlot"
				label: 								qsTr("Residuals four-in-one plot")
				info:								qsTr("Show all of the above plots in a 2x2 matrix.")
			}
		}

		Group
		{
			title: qsTr("Other Plots")

			CheckBox
			{
				name:									"plotPareto"
				label:									qsTr("Pareto plot of effects")
				info:									qsTr("Show a Pareto plot of the standardized effects. For a saturated model (no error degrees of freedom), the plot shows the absolute effects with the margin of error based on Lenth's pseudo standard error.")
			}

			CheckBox
			{
				name:									"normalEffectsPlot"
				label:									qsTr("Normal plot of effects")
				info:									qsTr("Show a normal probability plot of the standardized effects. For a saturated model (no error degrees of freedom), the plot shows the effects with a reference line based on Lenth's pseudo standard error, and effects larger than its margin of error are marked significant.")
			}
		}

		CheckBox
		{
			name:	"contourSurfacePlot"
			label:	qsTr("Contour/surface plots")
			info:	qsTr("Show contour (2D) or surface (3D) plots of the predicted response.")

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
					info:				qsTr("Two-dimensional plot with the predicted response shown as colored regions.")
					checked: 			true
				}

				RadioButton
				{
					name: 				"surfacePlot"
					id : 				surfacePlot
					label: 				qsTr("Surface plot")
					info:				qsTr("Three-dimensional plot of the predicted response.")
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
					info: qsTr("The continuous factors to plot. At least two are needed. A plot is shown for each pair of factors.")
				}
			}

			Group
			{
				title: 		qsTr("Contour/surface plot options")
				columns:	1

				IntegerField
				{
					name:						"contourSurfacePlotResponseDivision"
					label:						qsTr("Divide response surface into N parts")
					info:						qsTr("Select the number of divisions of the response in the contour/surface plot.")
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
						info:						qsTr("Rotation of the surface plot in the vertical plane, in degrees.")
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
						info:						qsTr("Rotation of the surface plot in the horizontal plane, in degrees.")
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
		info: qsTr("Finds the factor settings that maximize the composite desirability of the included responses, based on the fitted model of each response. Continuous factors are searched within their observed range; all combinations of discrete factor levels are tried.")

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
				info:								qsTr("The responses to optimize. For each response, the individual desirability (between 0 and 1) is set by the goal, bounds, target and weight. The composite desirability is the weighted geometric mean of the individual desirabilities, with the importances (normalized to sum to 1) as weights.")
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
						infoLabel:						qsTr("Goal")
						info:							qsTr("Whether to maximize or minimize the response, or to hit a target value.")
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
						infoLabel:						qsTr("Lower")
						info:							qsTr("Response value below which the individual desirability is 0. Used for the goals maximize and target. Only editable with 'Set all bounds manually'; otherwise the minimum of the observed response is used.")
						defaultValue:					0
						negativeValues:					true
						fieldWidth:						1.2 * responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerGoal.currentValue != "minimize" & responseOptimizerManualBounds.checked
					}
					DoubleField
					{
						name:							"responseOptimizerTarget"
						infoLabel:						qsTr("Target")
						info:							qsTr("Response value at which the individual desirability is 1. Always editable for the goal target. For maximize and minimize only editable with 'Set all targets manually'; otherwise the maximum or minimum of the observed response is used.")
						defaultValue:					0.5
						negativeValues:					true
						fieldWidth:						responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerManualTarget.checked | responseOptimizerGoal.currentValue == "target"
					}
					DoubleField
					{
						name:							"responseOptimizerUpperBound"
						infoLabel:						qsTr("Upper")
						info:							qsTr("Response value above which the individual desirability is 0. Used for the goals minimize and target. Only editable with 'Set all bounds manually'; otherwise the maximum of the observed response is used.")
						defaultValue:					1
						negativeValues:					true
						fieldWidth:						responsesResponseOptimizer.rowWidth
						enabled:						responseOptimizerGoal.currentValue != "maximize" & responseOptimizerManualBounds.checked
					}
					DoubleField
					{
						name:							"responseOptimizerWeight"
						infoLabel:						qsTr("Weight")
						info:							qsTr("Shape of the individual desirability between bound and target (0.1 to 10). With 1 it increases linearly; larger values require the response to be closer to the target for a high desirability, smaller values are more lenient.")
						defaultValue:					1
						min:							0.1
						max:							10
						fieldWidth:						responsesResponseOptimizer.rowWidth
					}
					DoubleField
					{
						name:							"responseOptimizerImportance"
						infoLabel:						qsTr("Importance")
						info:							qsTr("Relative importance of the response in the composite desirability (0.1 to 10). Only enabled with more than one response.")
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
				info:						qsTr("Show the optimizer settings per response and the optimal solution: the factor levels with the highest composite desirability, that desirability, and the predicted (fitted) responses at these levels.")
				checked:					true
			}

			CheckBox
			{
				name:						"optimizationIntervals"
				label:						qsTr("Confidence and prediction intervals")
				info:						qsTr("Predicted response at the optimal (or manually set) factor levels, with the confidence interval for the mean response and the prediction interval for a single new run. Blocks are averaged out; covariates are held at their mean.")
				childrenOnSameRow:			true

				CIField
				{
					name:					"optimizationIntervalsLevel"
					info:					qsTr("Confidence level of the intervals.")
				}
			}

			CheckBox
			{
				name:						"optimizationPlot"
				label:						qsTr("Show optimization plot")
				info:						qsTr("Show a matrix of plots with one column per factor. The top row shows the composite desirability, the other rows one per response. Each plot varies one factor while the other factors are held at the optimal (or manually set) levels, which are marked in red.")
				checked:					true

				DropDown
				{
					name:							"optimizationPlotPredictionType"
					label:							qsTr("Predict")
					info:							qsTr("What the response rows of the optimization plot show: the predicted response or its individual desirability.")
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
					info:						qsTr("Draw the optimization plot at manually entered factor levels instead of the optimal solution. Enter a value for every factor; discrete factors need one of their observed levels.")
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
				info:						qsTr("Use the entered lower and upper bounds instead of the minimum and maximum of the observed responses.")
				checked:					false
			}

			CheckBox
			{
				name:						"responseOptimizerManualTarget"
				id:							responseOptimizerManualTarget
				label:						qsTr("Set all targets manually")
				info:						qsTr("Use the entered targets for the goals maximize and minimize instead of the maximum or minimum of the observed responses.")
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
				info:							qsTr("Select the method used to calculate the bin widths of the residual histogram, or specify a manual number of bins.")
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
				info:							qsTr("Number of bins when the bin width type is set to manual.")
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
			info:							qsTr("How the sum of squares of each term in the ANOVA table is calculated. The types differ in which other terms a term is adjusted for. In balanced designs with orthogonal (coded) terms, such as complete two-level factorial designs, all types give the same result. In unbalanced designs, e.g. with missing runs, covariates or squared terms, they can differ. The Model, Error and Total rows are the same for all types. See the description of the ANOVA table below.") + "\n"
											+ "- **" + qsTr("Type I") + "**: " + qsTr("Sequential sums of squares: each term is adjusted only for the terms entered before it, so the results depend on the order of the terms. The term rows add up to the Model row.") + "\n"
											+ "- **" + qsTr("Type II") + "**: " + qsTr("Each term is adjusted for all other terms, except for higher-order terms that contain it. For example, A is adjusted for B, but not for A✻B.") + "\n"
											+ "- **" + qsTr("Type III") + "**: " + qsTr("Each term is adjusted for all other terms in the model, i.e. it measures what the term adds when it is entered last. Results for lower-order terms depend on how the factors are coded. Not available when some coefficients are aliased.")
			id: 							sumOfSquaresType
			indexDefaultValue:				2
			values: [
				{ label: qsTr("Type I"), value: "type1"},
				{ label: qsTr("Type II"), value: "type2"},
				{ label: qsTr("Type III"), value: "type3"}
			]
		}

			CheckBox
			{
				name:						"squaredTermsCoded"
				label:						qsTr("Use coded data to calculate squared terms")
				info:						qsTr("Calculate the ANOVA table from the factors in coded units (-1 to 1), so that squared terms are calculated from coded values. Only has an effect when the results are not displayed in coded units. It can change the type II and III sums of squares of the linear terms.")
				visible:					designType.currentValue == "responseSurfaceDesign"
			}
	}
}
