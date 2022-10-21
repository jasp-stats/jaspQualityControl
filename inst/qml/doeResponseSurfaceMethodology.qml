
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form 
{
	columns: 2

	GroupBox
	{
		title: 									qsTr("Design Space")
		name:									"designInfo"
		columns:								1

		IntegerField
		{
			id:									numberOfFactors
			name:								"numberOfFactors"
			label:								qsTr("Number of factors")
			defaultValue:						2
			min:								2
			max:								256
		}

		RadioButtonGroup
		{
			columns: 							1
			name:								"designType"
			title:								qsTr("Define levels")

			RadioButton
			{
				id:								cube
				name:							"cube"
				label:							qsTr("Cube points")
				checked:						true
				childrenOnSameRow: 				true

				IntegerField
				{

					id:							numberOfCubes
					name:						"numberOfCubes"
					defaultValue:				3
					min:						0
					max:						256
				}

			}

			RadioButton
			{
				id:								star
				name:							"star"
				label:							qsTr("Axial points")
				childrenOnSameRow: 				true
				

				IntegerField
				{

					id:							numberOfStars
					name:						"numberOfStars"
					defaultValue:				numberOfFactors.value * 2
					min:						0
					max:						256
				}
			}
			
		}

		//	IntegerField
		//	{
		//		id:									numberOfGenerators
		//		name:								"numberOfGenerators"
		//		label:								qsTr("Number of generators")
		//		defaultValue:						0
		//		min:								0
		//		max:								256
		//		visible: 							cube.checked
		//	}

		Group
		{
			title:								qsTr("Design options")

			CheckBox
			{
				id:								inscribed
				name:							"inscribed"
				label:							qsTr("Inscribed design")
				enabled:						cube.checked
				checked: 						(cube.checked) ? false : false

			}
			
			CheckBox
			{
				id:								oneBlock
				name:							"oneBlock"
				label:							qsTr("Force one block")
				visible:						false
			}
			
			CheckBox
			{
				id:								noModel
				name:							"noModel"
				label:							qsTr("Use # of variables instead of model")
				enabled:						cube.checked
				checked: 						true
				visible: 						false
			}
			
			CheckBox
			{
				id:								block
				name:							"block"
				label:							qsTr("Introduce blocking")
				enabled:						cube.checked
				checked: 						(cube.checked) ? false : false
			}
			
			CheckBox
			{
				id:								coded_out
				name:							"coded_out"
				label:							qsTr("Coded output")
			}
			
			
			DropDown
			{
				name: 							"alpha"
				indexDefaultValue: 				0
				label:							qsTr("Alpha type")
				values: 						["Orthogonal", "Rotatable", "Spherical", "Faces"]
				enabled:						star.checked
			}

			IntegerField
			{
				visible:						false
				id:								numberOfFactorsForTable
				name:							"numberOfFactorsForTable"
				defaultValue:					numberOfFactors.value
			}
		}


		RadioButtonGroup
		{
			name:								"runOrder"
			title:								qsTr("Run Order")

			RadioButton
			{
				SetSeed{}
				name:							"runOrderRandom"
				label:							qsTr("Random")
				checked:						true
			}

			RadioButton
			{
				name:							"runOrderStandard"
				label:							qsTr("Standard")
			}
		}

		//		DropDown
		//		{
		//			debug:								true
		//			id:									numberOfLevels
		//			name:								"numberOfLevels"
		//			label:								qsTr("Number of factor levels")
		//			indexDefaultValue:					0
		//			values:
		//			[
		//				{ value: "2",		label: qsTr("2")}
		//				{ value: "3",		label: qsTr("3")},
		//				{ value: "Mixed",	label: qsTr("Mixed")}
		//			]
		//		}
	}

	ColumnLayout
	{
		spacing:								0
		Layout.preferredWidth:					parent.width
		Layout.columnSpan:						2

		RowLayout
		{
			Label { text: qsTr("Factor");		Layout.leftMargin: 5 * preferencesModel.uiScale;		Layout.preferredWidth: 42 * preferencesModel.uiScale}
			Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale}
			Label { text: qsTr("Low");			Layout.preferredWidth: 100 * preferencesModel.uiScale}
			Label { text: qsTr("High");			Layout.preferredWidth: 100 * preferencesModel.uiScale}
		}

		ComponentsList
		{
			name:								"factors"
			addItemManually:					false
			values:								numberOfFactorsForTable.value // update only when numberOfFactors.value gets "entered"

			rowComponent: 						RowLayout
			{
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		40 * preferencesModel.uiScale
					Label
					{
						text: 					qsTr("x") + (rowIndex + 1)
					}
				}
				Row //Factor
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale

					TextField
					{
						id:						factorName
						label: 					""
						name: 					"factorName"
						placeholderText:		qsTr("Factor name ") + (rowIndex + 1)
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true

					}
				}
				Row //Level1
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					DoubleField
					{
						label:					""
						name:					"centre"
						fieldWidth:				100 * preferencesModel.uiScale
						negativeValues:			true
					}

				}
				Row //Level2
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					DoubleField
					{
						label:					""
						name:					"distance"
						fieldWidth:				100 * preferencesModel.uiScale
						defaultValue:			1
					}
				}
				//				Row //Level3
				//				{
				//					visible:					[1].includes(numberOfLevels.currentIndex)
				//					spacing:					5 * preferencesModel.uiScale
				//					Layout.preferredWidth:		100 * preferencesModel.uiScale
				//					TextField
				//					{
				//						label: 					""
				//						name: 					"high2"
				//						placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 3")
				//						fieldWidth:				100 * preferencesModel.uiScale
				//						useExternalBorder:		false
				//						showBorder:				true
				//					}
				//				}
			}
		}
	}

	
	TextArea
	{
		id:										designModel
		name:									"designModel"
		title:									qsTr("Specify model for CCD")
		height:									100 * preferencesModel.uiScale
		width:									250 * preferencesModel.uiScale
		visible:								cube.checked && !noModel.checked
	}

	IntegerField
	{
		visible:								false
		id:										numberOfGeneratorsForTable
		name:									"numberOfGeneratorsForTable"
		defaultValue:							numberOfGenerators.value
	}

	ColumnLayout
	{
		spacing:								0
		Layout.preferredWidth:					parent.width
		Layout.columnSpan:						2

		//	RowLayout
		//	{

		//		Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale; visible: cube.checked }
		//		Label { text: qsTr("Formula");		Layout.preferredWidth: 100 * preferencesModel.uiScale; visible: cube.checked }

		//	}

		ComponentsList
		{
			name:								"generators"
			addItemManually:					false
			values:								numberOfGeneratorsForTable.value //
			visible:							cube.checked

			rowComponent: 						RowLayout
			{



				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale

					TextField
					{
						id:						generatorName
						label: 					""
						placeholderText:		qsTr("Generator name")
						name: 					"generatorName"
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
				Row //Level1
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"generatorFormula"
						placeholderText:		qsTr("Generator formula")
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}

				//				Row //Level3
				//				{
				//					visible:					[1].includes(numberOfLevels.currentIndex)
				//					spacing:					5 * preferencesModel.uiScale
				//					Layout.preferredWidth:		100 * preferencesModel.uiScale
				//					TextField
				//					{
				//						label: 					""
				//						name: 					"high2"
				//						placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 3")
				//						fieldWidth:				100 * preferencesModel.uiScale
				//						useExternalBorder:		false
				//						showBorder:				true
				//					}
				//				}
			}
		}
	}

	TextArea
	{
		id:									designBlock
		name:								"designBlock"
		title:								qsTr("Specify blocks for CCD")
		height:								100 * preferencesModel.uiScale
		width:								250 * preferencesModel.uiScale
		visible:							cube.checked && block.checked
	}

	Group
	{

		Button
		{
			id: 								buildDesign
			anchors.right:						parent.right
			anchors.bottom:						parent.bottom
			text: 								qsTr("<b>Build Design</b>")
			onClicked: 							buildDesignInv.click()
		}
		
		CheckBox
		{
			id:									buildDesignInv
			name:								"buildDesignInv"
			visible:							false
		}
		
	}

	Section
	{
		title: qsTr("Design Analysis")

		VariablesForm
		{
			AvailableVariablesList { name: "rsmVariablesList" }
			AssignedVariablesList
			{
				name: "rsmVariables"
				title: qsTr("Predictors [Location in coded format]")
				suggestedColumns: ["scale", "ordinal"]

				rowComponent: Row
				{
					DoubleField {name: "Point_P"; negativeValues: true}
				}


			}
			AssignedVariablesList  { name: "rsmResponseVariables";	title: qsTr("Response");			suggestedColumns: ["scale", "ordinal"]}
			AssignedVariablesList  { name: "rsmBlocks";				title: qsTr("Blocks (optional)");	suggestedColumns: ["ordinal", "nominal", "scale", "nominalText"];	singleVariable: true}
		}


		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: "rsmVariables" }
			ModelTermsList
			{

				listViewType			: JASP.Interaction
				rowComponentTitle		: qsTr("Term Type")
				rowComponent			: DropDown
				{
					name: "TermType"
					label: ""
					values: [
						{ label: qsTr("FO + PQ"),	value: "fopq"		},
						{ label: "",				value: "nothing"	},
						{ label: qsTr("FO"),		value: "fo"			},
					]
				}
			}
		}

		Group
		{
			title: qsTr("Response surface analysis")
			columns: 2

			CheckBox {		name: "coef";			label: qsTr("Coefficients table")	}
			CheckBox {		name: "res";			label: qsTr("Residual histogram")	}
			CheckBox {		name: "anova";			label: qsTr("ANOVA table")			}
			CheckBox {		name: "resNorm";		label: qsTr("Normal residual plot")	}

			CheckBox
			{
							name: "normalPlot";		label: qsTr("Normal plot of standardized effects")
				CheckBox {	name: "addGridlines";	label: qsTr("Add grid lines") }
			}

			CheckBox {		name: "ResFitted";		label: qsTr("Residual vs. fitted plot")				}
			CheckBox {		name: "pareto";			label: qsTr("Pareto plot of standardized effects")	}
			CheckBox {		name: "fourInOne";		label: qsTr("Matrix residuals plot")				}
		}
	}
	
	Section
	{
		title: qsTr("Contour plots")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList		{ name: "rsmVariables2";		source:"rsmVariables"								}
			AssignedPairsVariablesList	{ name: "pairs";				suggestedColumns: ["scale", "ordinal", "nominal"]	}
		}

		Group
		{
			title: qsTr("Contour plot options")
			
			CheckBox
			{
				name:							"contour"
				label:							qsTr("Contour surface")
				columns: 2
				CheckBox
				{
					name:						"cplot"
					label:						qsTr("Only show 2D plot")
					id:							cplot
				}

				CheckBox
				{
					name:						"coded"
					label:						qsTr("Show analysis and graphs in coded form")
					enabled:					cplot.checked
				}

				CheckBox
				{
					name:						"legend"
					label:						qsTr("Show legend next to graph")
					enabled:					!cplot.checked
				}
				DropDown
				{
					name:						"divide"
					label:						qsTr("Divide response surface into N parts")
					values:						[2,3,4,5,6,7]
					enabled:					!cplot.checked
				}

				Slider
				{
					name:						"phi"
					label:						qsTr("Rotating angle (vertical plane)")
					value:						0
					enabled:					!cplot.checked
				}

				Slider
				{
					name:						"theta"
					label:						qsTr("Rotating angle (horizontal plane)")
					value:						0.5
					vertical:					false
					enabled:					!cplot.checked
				}
			}
		}
	}
	
	
	Section
	{
		title: qsTr("Desirability")
		CheckBox { name: "desirability";	label: qsTr("Calculate desirability") }
		VariablesForm
		{
			AvailableVariablesList	{ name: "rsmDesirability";		label: qsTr("Response variable list");	source: "rsmResponseVariables" }
			AssignedVariablesList	{ name: "rsmMin";				title: qsTr("Minimum [Min/Max]");		suggestedColumns: ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min";		negativeValues: true}
					DoubleField {name: "Point_Max";		negativeValues: true; defaultValue: 1}
				}
			}

			AssignedVariablesList
			{
				name: "rsmMax";		title: qsTr("Maximum [Min/Max]");	suggestedColumns:	["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_1";	negativeValues: true}
					DoubleField {name: "Point_Max_1";	negativeValues: true; defaultValue: 1}
				}
			}

			AssignedVariablesList
			{
				name: "rsmTar";		title: qsTr("Target [Min/Target/Max]");	suggestedColumns: ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_2";	negativeValues: true					}
					DoubleField {name: "Point_Tar_2";	negativeValues: true;	defaultValue: 1	}
					DoubleField {name: "Point_Max_2";	negativeValues: true;	defaultValue: 2	}
				}
			}
		}
	}
}
