
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
	columns:									2

	VariablesForm
	{
		AvailableVariablesList { name: "rsmVariablesList" }
		AssignedVariablesList
		{
			name: "rsmVariables"
			title: qsTr("Predictors [Location in coded format]")
			suggestedColumns:   ["scale", "ordinal", "nominal"]

			rowComponent: Row
			{
				DoubleField {name: "Point_P"; negativeValues: true}
			}


		}
		AssignedVariablesList  { name: "rsmResponseVariables";	title: qsTr("Response");  suggestedColumns:   ["scale", "ordinal", "nominal"]}
		AssignedVariablesList  { name: "rsmBlocks";	            title: qsTr("Blocks (optional)");    suggestedColumns:   ["scale", "ordinal", "nominal"]; singleVariable: true}
	}


	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "components"; title: qsTr("Components"); source: "rsmVariables" }
		ModelTermsList
		{

			name					: "modelSpec"
			listViewType			: JASP.Interaction
			rowComponentTitle		: qsTr("Term Type")
			rowComponent			: DropDown
			{
				name: "TermType"
				label: ""
				values: [
					{ label: qsTr("FO + PQ"), value: "fopq"},
					{ label: qsTr(""), value: "nothing"},
					{ label: qsTr("FO"), value: "fo"},
					
					
				]
			}
		}
	}
	
	Group 
	{
		title: qsTr("Response Surface Summaries")
		columns: 3
		
		CheckBox 
		{
			name:                       "coef"; label:                  qsTr("Coefficient Table")
			
		}
		
		
		CheckBox
		{
			name:                       "anova";label:                  qsTr("ANOVA Table")
		}
		
		
		
		CheckBox
		{
			name:                       "res";  label:                  qsTr("Residual Histogram")
		}
		
		CheckBox
		{
			name:                       "resNorm";label:                 qsTr("Normal Residual Plot")
		}
		
		CheckBox
		{
			name:                       "ResFitted";label:                 qsTr("Residual vs. Fitted Plot")
		}

		
		CheckBox
		{
			name:                       "pareto";label:                 qsTr("Pareto Plot of Standardized Effects")
		}
	}
	

	Section 

	{
		title: qsTr("Design Specification")

		
		GroupBox
		{
			title: 									qsTr("Design Information")
			name:									"designInfo"
			columns:								3
			
			RadioButtonGroup
			{
				columns: 2
				name:								"designType"
				RadioButton
				{
					id:								cube
					name:							"cube"
					label:							qsTr("Central Composite Design (CCD)")
					checked:						true
				}
				
				RadioButton
				{
					id:								star
					name:							"star"
					label:							qsTr("+ Star Points")
				}
			}

			IntegerField
			{
				id:									numberOfFactors
				name:								"numberOfFactors"
				label:								qsTr("Number of Factors")
				defaultValue:						2
				min:								2
				max:								256
			}
			IntegerField
			{
			
				id:									numberOfCubes
				name:								"numberOfCubes"
				label:								qsTr("Number of Cube Points")
				defaultValue:						0
				min:								0
				max:								256
				visible:							cube.checked
			
			
			}
			
			IntegerField
			{
			
				id:									numberOfStars
				name:								"numberOfStars"
				label:								qsTr("Number of Star Points")
				defaultValue:						0
				min:								0
				max:								256
				visible:							star.checked
				
			
			}
			
			IntegerField
			{
				id:									numberOfGenerators
				name:								"numberOfGenerators"
				label:								qsTr("Number of Generators")
				defaultValue:						0
				min:								0
				max:								256
				visible: 							cube.checked
			}
			
			CheckBox 
			{
				id:									randomize
				name:								"randomize"
				label:								qsTr("Randomize Design")
			}
			
			CheckBox
			{
				id:									inscribed
				name:								"inscribed"
				label:								qsTr("Inscribed Design")
				visible:							cube.checked
			
			}
			
			CheckBox
			{
				id:									oneBlock
				name:								"oneBlock"
				label:								qsTr("Force One Block")
				visible:							false
			}
			
			CheckBox 
			{
				id:									noModel
				name:								"noModel"
				label:								qsTr("Use # of Variables instead of Model")
				visible:							cube.checked
			}
			
			CheckBox 
			{
				id:									block
				name:								"block"
				label:								qsTr("Introduce Blocking")
				visible:							cube.checked
			}
			
			CheckBox 
			{
				id:									coded_out
				name:								"coded_out"
				label:								qsTr("Coded Output")
			}
			
			
			DropDown
			{
			  name: 								"alpha"
			  indexDefaultValue: 					0
			  label:								qsTr("Alpha Type")
			  values: 								["Orthogonal", "Rotatable", "Spherical", "Faces"]
			  visible:								star.checked
			}

			IntegerField
			{
				visible:                            false
				id:                                 numberOfFactorsForTable
				name:                               "numberOfFactorsForTable"
				defaultValue:                       numberOfFactors.value
			}
			
			

	//		DropDown
	//		{
	//            debug:                              true
	//            id:                                 numberOfLevels
	//            name:                               "numberOfLevels"
	//            label:                              qsTr("Number of factor levels")
	//            indexDefaultValue:                  0
	//			values:
	//				[
	//				{ value: "2", label: qsTr("2")}
	//				//            { value: "3", label: qsTr("3")},
	//				//            { value: "Mixed", label: qsTr("Mixed")}
	//			]
//			}
		}
		ColumnLayout
		{
			spacing:                                0
			Layout.preferredWidth:					parent.width
			Layout.columnSpan:						2

			RowLayout
			{
				Label { text: qsTr("Factor");			Layout.leftMargin: 5 * preferencesModel.uiScale;		Layout.preferredWidth: 42 * preferencesModel.uiScale}
				Label { text: qsTr("Name");				Layout.preferredWidth: 150 * preferencesModel.uiScale}
				Label { text: qsTr("Centre");			Layout.preferredWidth: 100 * preferencesModel.uiScale}
				Label { text: qsTr("Distance to |1|");	Layout.preferredWidth: 100 * preferencesModel.uiScale}
	//			Label { visible: 					numberOfLevels.currentIndex == 1;
	//                    text: qsTr("Level 3");		Layout.preferredWidth: 100 * preferencesModel.uiScale	}
			}

			ComponentsList
			{
				name:								"factors"
				addItemManually:                    false
				values:                             numberOfFactorsForTable.value // update only when numberOfFactors.value gets "entered"

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
							placeholderText:		qsTr("x") + (rowIndex + 1)
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
							name: 					"centre"
							placeholderText:		qsTr("x") + (rowIndex + 1) + qsTr(" Centre")
							fieldWidth:				100 * preferencesModel.uiScale
							useExternalBorder:		false
							showBorder:				true
						}
					}
					Row //Level2
					{
						spacing:					5 * preferencesModel.uiScale
						Layout.preferredWidth:		100 * preferencesModel.uiScale
						TextField
						{
							label: 					""
							name: 					"distance"
							placeholderText:		qsTr("x") + (rowIndex + 1) + qsTr(" Distance")
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
			id:									designModel
			name:								"designModel"
			title:								"Specify Model for CCD"
			height:                     		100 * preferencesModel.uiScale
			width:                      		250 * preferencesModel.uiScale
			visible:							cube.checked && !noModel.checked
		}
		
		IntegerField
		{


			visible:                            false
			id:                                 numberOfGeneratorsForTable
			name:                               "numberOfGeneratorsForTable"
			defaultValue:                       numberOfGenerators.value
		}
		
		ColumnLayout
		{
			spacing:                                0
			Layout.preferredWidth:					parent.width
			Layout.columnSpan:						2

			RowLayout
			{
				
				Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale; visible: cube.checked }
				Label { text: qsTr("Formula");		Layout.preferredWidth: 100 * preferencesModel.uiScale; visible: cube.checked }

			}

			ComponentsList
			{
				name:								"generators"
				addItemManually:                    false
				values:                             numberOfGeneratorsForTable.value //
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
							placeholderText:		qsTr("Generator Name")
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
							placeholderText:		qsTr("Generator Formula")
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
			title:								"Specify Blocks for CCD"
			height:                     		100 * preferencesModel.uiScale
			width:                      		250 * preferencesModel.uiScale
			visible:							cube.checked && block.checked
		}
		
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
		title: qsTr("Contour Plots")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{

				name:  "rsmVariables2";	    source:"rsmVariables"

			}




			AssignedPairsVariablesList
			{	name:  "pairs";				suggestedColumns: ["scale", "ordinal", "nominal"] }


		} 
		

		Group
		{
			title: qsTr("Contour Plot Options")
			
			CheckBox
			{
				name:                      "contour";label:   qsTr("Contour Surface")
				columns: 2
				CheckBox
				{
					name:                       "cplot"
					label:                      qsTr("Only show 2D plot")
					id:                         cplot
				}

				CheckBox
				{
					name:                       "coded"
					label:                      qsTr("Show analysis and graphs in coded form")
					enabled:					cplot.checked
				}

				CheckBox
				{
					name:                       "legend"
					label:                      qsTr("Show legend next to graph")
					enabled:					!cplot.checked
				}
				DropDown
				{
					name:                       "divide"
					label:                      qsTr("Divide response surface into N parts")
					values:                     [2,3,4,5,6,7]
					enabled:					!cplot.checked
				}

				Slider
				{
					name:                       "phi"
					label:                      qsTr("Rotating angle (vertical plane)")
					value:                      0
					enabled:					!cplot.checked


				}

				Slider
				{
					name:                       "theta"
					label:                      qsTr("Rotating angle (horizontal plane)")
					value:                      0.5
					vertical:                   false
					enabled:					!cplot.checked
				}

			}
			
		
			
			
			
		}
	}
	
	
	Section
	{
		title: qsTr("Desirability")
		CheckBox 
		{
			name: "desirability";label: "Calculate Desirability"
			
		}
		VariablesForm 
		{
			AvailableVariablesList 
			{ 
				name: "rsmDesirability";       label: qsTr("Response Variable List");    source: "rsmResponseVariables" 
				
			}
			AssignedVariablesList  
			{
				name: "rsmMin";	            title: qsTr("Minimum [Min/Max]");     suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min"; negativeValues: true}
					DoubleField {name: "Point_Max"; negativeValues: true; defaultValue: 1}
				}

			}
			AssignedVariablesList  
			{
				name: "rsmMax";	            title: qsTr("Maximum [Min/Max]");     suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_1"; negativeValues: true}
					DoubleField {name: "Point_Max_1"; negativeValues: true; defaultValue: 1}
				}

			}
			AssignedVariablesList  
			{
				name: "rsmTar";	            title: qsTr("Target [Min/Target/Max]");suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_2"; negativeValues: true}

					DoubleField {name: "Point_Tar_2"; negativeValues: true; defaultValue: 1}
					DoubleField {name: "Point_Max_2"; negativeValues: true; defaultValue: 2}
				}

			}
		
		}
		
		
		
	
	
	}
		
	

	



	Section
	{
		title: 							qsTr("Box designs")

		CheckBox
		{
			name:                       "showDesign";label:            qsTr("Central composite design")
			IntegerField
			{
				name:                       "factorResponse"
				label:                      "Number of factors"
				defaultValue:               2
				min:                        2
				max:                        50
			}

			IntegerField
			{
				name:						"responseSurfaceCentre"
				label:						qsTr("Number of centre points")
				defaultValue:				3
				min:						1
				max:						50
			}


			IntegerField
			{
				name:						"responseSurfaceReplicationStar"
				label:						qsTr("Number of replicates")
				defaultValue:				3
				min:						1
				max:						50
			}



		}



	}


}
