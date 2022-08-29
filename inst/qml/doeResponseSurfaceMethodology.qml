
// Copyright (C) 2013-2023 University of Amsterdam
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

import QtQuick 									2.15
import QtQuick.Layouts 							1.3
import QtQuick.Controls							2.12
import JASP										1.0
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

import "./common"	as Common

Form
{
	id: form
	columns: 1

	RadioButtonGroup
	{
		name		: "designType"
		title		: qsTr("Design Type")

<<<<<<< HEAD
		RadioButton { name:	 "centralCompositeDesign";		label: qsTr("Central Composite Design");	checked: true;		id: centralCompositeDesign	}
		RadioButton { name:	 "boxBehnkenDesign";			label: qsTr("Box-Behnken Design");															}

	}
=======
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
					id:								cubePoints
					name:							"cubePoints"
					label:							qsTr("Cube points")
					checked:						true
					childrenOnSameRow: 				true

					IntegerField
					{
			
						id:									cubePointsNumber
						name:								"cubePointsNumber"
						defaultValue:						3
						min:								0
						max:								256
					}
			
				}
				
				RadioButton
				{
					id:								axialPoints
					name:							"axialPoints"
					label:							qsTr("Axial points")
					childrenOnSameRow: 				true
				

					IntegerField
					{
			
						id:									axialPointsNumber
						name:								"axialPointsNumber"
						defaultValue:						numberOfFactors.value * 2
						min:								0
						max:								256
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
		//		visible: 							cubePoints.checked
		//	}

			Group 
			{
			
				title:								qsTr("Design options")

				CheckBox
				{
					id:									inscribedDesign
					name:								"inscribedDesign"
					label:								qsTr("Inscribed design")
					enabled:							cubePoints.checked
					checked: 							(cubePoints.checked) ? false : false
			
				}
			
				CheckBox
				{
					id:									oneBlock
					name:								"oneBlock"
					label:								qsTr("Force one block")
					visible:							false
				}
			
				CheckBox 
				{
					id:									noModel
					name:								"noModel"
					label:								qsTr("Use # of variables instead of model")
					enabled:							cubePoints.checked
					checked: 							true
					visible: 							false
				}
			
				CheckBox 
				{
					id:									block
					name:								"block"
					label:								qsTr("Introduce blocking")
					enabled:							cubePoints.checked
					checked: 							(cubePoints.checked) ? false : false
				}
			
				CheckBox 
				{
					id:									codedOutput
					name:								"codedOutput"
					label:								qsTr("Coded output")
				}
			
			
				DropDown
				{
				  name: 								"alphaType"
				  indexDefaultValue: 					0
				  label:								qsTr("Alpha type")
				  values: 								["orthogonal", "rotatable", "spherical", "faces"]
				  enabled:								axialPoints.checked
				}

				IntegerField
				{
					visible:							false
					id:									numberOfFactorsForTable
					name:								"numberOfFactorsForTable"
					defaultValue:						numberOfFactors.value
				}
			}
			
			RadioButtonGroup
			{
				name:									"runOrder"
				title:									qsTr("Run Order")

				RadioButton
				{
					SetSeed{}
					name:								"random"
					label:								qsTr("Random")
					checked:							true
				}

				RadioButton
				{
					name:								"standard"
					label:								qsTr("Standard")
				}
			}
>>>>>>> 2781b38 (Renaming Response Surface Design)

	Group
	{

		// Could probably use a custom IntegerField type...
		IntegerField { id: numberOfContinuous;		label: qsTr("Number of continuous factors");	name: "numberOfContinuous";		min: centralCompositeDesign.checked ? 2 : 3;	defaultValue: centralCompositeDesign.checked ? 2 : 3;	max: 10
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfCategorical;		label: qsTr("Number of categorical factors");	name: "numberOfCategorical";	min: 0;		defaultValue: 0;	max: 10
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfLevels;			label: qsTr("Maximum categorical levels");		name: "categoricalNoLevels";	min: 2;		defaultValue: 2;	max: 10
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}

<<<<<<< HEAD
		TableView
		{

			id: continuousVariablesTable
			modelType			: JASP.Simple

			implicitWidth		: form.implicitWidth
			implicitHeight		: 140 * preferencesModel.uiScale // about 3 rows

			initialRowCount		: numberOfContinuous.intValue
			initialColumnCount	: 3

			rowCount			: numberOfContinuous.intValue
			columnCount			: 3

			name				: "continuousVariables"
			cornerText			: qsTr("Factor")
			columnNames			: [qsTr("Name"), qsTr("Low"), qsTr("High")]
			isFirstColEditable	: true
			itemType			: JASP.Double
			itemTypePerColumn	: [JASP.String] // first column is string, all others are double

			function getRowHeaderText(headerText, rowIndex)				{ return String.fromCharCode(65 + rowIndex);	}
			function getDefaultValue(columnIndex, rowIndex)				{ return columnIndex === 0 ? String.fromCharCode(65 + rowIndex) : 2 * columnIndex - 3;	}

			JASPDoubleValidator			{ id: doubleValidator; decimals: 3	}
			RegularExpressionValidator	{ id: stringValidator				}
			function getValidator(columnIndex, rowIndex)				{ return columnIndex === 0 ? stringValidator : doubleValidator							}
		}

		TableView
		{
			id: categoricalVariables

			implicitWidth		: form.implicitWidth
			implicitHeight		: 140 * preferencesModel.uiScale // about 3 rows

			modelType			: JASP.Simple

			isFirstColEditable	: true

			initialRowCount		: numberOfCategorical.intValue
			initialColumnCount	: 1 + parseInt(numberOfLevels.value)

			rowCount			: numberOfCategorical.intValue
			columnCount			: 1 + parseInt(numberOfLevels.value)
			name				: "categoricalVariables"
			cornerText			: qsTr("Factor")
			itemType			: JASP.String

			function getColHeaderText(headerText, colIndex)				{ return colIndex === 0 ? qsTr("Name") : qsTr("Level %1").arg(colIndex); }
			function getRowHeaderText(headerText, rowIndex)				{ return String.fromCharCode(65 + rowIndex + numberOfContinuous.intValue); }
			function getDefaultValue(columnIndex, rowIndex)				{ return String.fromCharCode(columnIndex === 0 ? 65 + rowIndex + numberOfContinuous.intValue : 97 + columnIndex - 1); }
		}

	}

	Group
	{

		Label	{ text : qsTr("Design Table")	}
		TableView
		{
			property int designDataColumns : centralCompositeDesign.checked ? 7 : 3
			property var designData: // it would be better to generate this...
			{
				const val = numberOfContinuous.intValue
				if (centralCompositeDesign.checked) // CCD
=======
			ColumnLayout
			{
				spacing:                                0
				Layout.preferredWidth:					parent.width
				Layout.columnSpan:						2

				RowLayout
				{
					Label { text: qsTr("Factor");			Layout.leftMargin: 5 * preferencesModel.uiScale;		Layout.preferredWidth: 42 * preferencesModel.uiScale}
					Label { text: qsTr("Name");				Layout.preferredWidth: 150 * preferencesModel.uiScale}
					Label { text: qsTr("Low");			Layout.preferredWidth: 100 * preferencesModel.uiScale}
					Label { text: qsTr("High");	Layout.preferredWidth: 100 * preferencesModel.uiScale}
	//			Label { visible: 					numberOfLevels.currentIndex == 1;
	//                    text: qsTr("Level 3");		Layout.preferredWidth: 100 * preferencesModel.uiScale	}
			}

			ComponentsList
			{
				name:								"factors"
				addItemManually:					false
				values:								numberOfFactorsForTable.value // update only when numberOfFactors.value gets "entered"

				rowComponent: 						RowLayout
>>>>>>> 2781b38 (Renaming Response Surface Design)
				{
					switch(val)
					{
						case 2:
							return	[
								"Full", 13, 1, 5, 0, 0, 1.414,
								"Full", 14, 2, 6, 3, 3, 1.414
							];
						case 3:	return	[
								"Full", 20, 1, 6, 0, 0, 1.682, // fixed manually
								"Full", 20, 2, 6, 4, 2, 1.633,
								"Full", 20, 3, 6, 4, 2, 1.633
							];
						case 4:	return	[
								"Full", 31, 1, 7, 0, 0, 2,
								"Full", 30, 2, 6, 4, 2, 2,
								"Full", 30, 3, 6, 4, 2, 2
							];
						case 5:	return	[
								"Half", 32, 1, 6, 0, 0, 2,
								"Half", 33, 2, 7, 6, 1, 2,
								"Full", 52, 1, 10, 0, 0, 2.378,
								"Full", 54, 2, 12, 8, 4, 2.366,
								"Full", 54, 3, 12, 8, 4, 2.366
							];
						case 6:	return	[
								"Half", 53, 1, 9, 0, 0, 2.378,
								"Half", 54, 2, 10, 8, 2, 2.366,
								"Half", 54, 3, 10, 8, 2, 2.366,
								"Full", 90, 1, 14, 0, 0, 2.828,
								"Full", 90, 2, 14, 8, 6, 2.828,
								"Full", 90, 3, 14, 8, 6, 2.828,
								"Full", 90, 5, 14, 8, 6, 2.828
							];
						case 7:	return	[
								"Half", 88, 1, 10, 0, 0, 2.828,
								"Half", 90, 2, 12, 8, 4, 2.828,
								"Half", 90, 3, 12, 8, 4, 2.828,
								"Half", 90, 5, 12, 8, 4, 2.828,
								"Full", 152, 1, 10, 0, 0, 3.364,
								"Full", 160, 2, 18, 8, 10, 3.364,
								"Full", 160, 3, 18, 8, 10, 3.364,
								"Full", 160, 5, 18, 8, 10, 3.364
							];
						case 8:	return	[
								"Quarter", 90, 1, 10, 0, 0, 2.828,
								"Quarter", 90, 2, 10, 8, 2, 2.828,
								"Quarter", 90, 3, 10, 8, 2, 2.828,
								"Quarter", 90, 5, 10, 8, 2, 2.828,
								"Half", 154, 1, 10, 0, 0, 3.364,
								"Half", 160, 2, 16, 8, 8, 3.364,
								"Half", 160, 3, 16, 8, 8, 3.364,
								"Half", 160, 5, 16, 8, 8, 3.364
							];
						case 9:	return	[
								"Quarter", 156, 1, 10, 0, 0, 3.364,
								"Quarter", 160, 2, 14, 8, 6, 3.364,
								"Quarter", 160, 3, 14, 8, 6, 3.364,
								"Quarter", 160, 5, 14, 8, 6, 3.364
							];
						case 10:	return	[
								"Eighth", 158, 1, 10, 0, 0, 3.364,
								"Eighth", 160, 2, 12, 8, 4, 3.364,
								"Eighth", 160, 3, 12, 8, 4, 3.364,
								"Eighth", 160, 5, 12, 8, 4, 3.364
							];
					}
				}
				else // BBD
				{
					switch(val)
					{
						case 3:		return	[15, 1, 3];
						case 4:		return	[27, 3, 3];
						case 5:		return	[46, 2, 3];
						case 6:		return	[54, 2, 6];
						case 7:		return	[62, 2, 6];
						case 9:		return	[130, "5 or 10", 10];
						case 10:	return	[170, 2, 10];
					}
				}
			}
<<<<<<< HEAD
=======
		}
		
	
		TextArea 
		{
			id:									designModel
			name:								"designModel"
			title:								"Specify model for CCD"
			height:								100 * preferencesModel.uiScale
			width:								250 * preferencesModel.uiScale
			visible:							cubePoints.checked && !noModel.checked
		}
		
		IntegerField
		{
>>>>>>> 2781b38 (Renaming Response Surface Design)

			id					: selectedDesign2
			implicitWidth		: form.implicitWidth
			implicitHeight		: 250 * preferencesModel.uiScale

<<<<<<< HEAD
			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: centralCompositeDesign.checked ? [qsTr("Runs"), qsTr("Blocks"), qsTr("Total"), qsTr("Cube"), qsTr("Axial"), qsTr("Alpha")] : [qsTr("Blocks"), qsTr("Centre points")]
			cornerText			: centralCompositeDesign.checked ? qsTr("Design") : qsTr("Runs")
			initialColumnCount	: designDataColumns - 1// -1 because the first "column" is not a column but the row header
			columnCount			: designDataColumns - 1
=======
			visible:							false
			id:									numberOfGeneratorsForTable
			name:								"numberOfGeneratorsForTable"
			defaultValue:						numberOfGenerators.value
		}
		
		ColumnLayout
		{
			spacing:								0
			Layout.preferredWidth:					parent.width
			Layout.columnSpan:						2

		//	RowLayout
		//	{
				
		//		Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale; visible: cubePoints.checked }
		//		Label { text: qsTr("Formula");		Layout.preferredWidth: 100 * preferencesModel.uiScale; visible: cubePoints.checked }
>>>>>>> 2781b38 (Renaming Response Surface Design)

			itemType			: JASP.Double
			rowCount			: designData.length / designDataColumns// numberOfContinuous.intValue
			initialRowCount		: designData.length / designDataColumns// numberOfContinuous.intValue

			itemDelegate: Item
			{
<<<<<<< HEAD
=======
				name:								"generators"
				addItemManually:					false
				values:								numberOfGeneratorsForTable.value //
				visible:							cubePoints.checked 
>>>>>>> 2781b38 (Renaming Response Surface Design)

				Rectangle
				{

					id: backgroundRect
					color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.white
					anchors
					{
						fill:			parent
						topMargin:		-selectedDesign2.view.itemVerticalPadding
						bottomMargin:	-selectedDesign2.view.itemVerticalPadding
					}

					MouseArea
					{
						anchors.fill: parent
						onClicked:
						{
							tableView.colSelected = columnIndex
							tableView.rowSelected = rowIndex
						}
					}
				}

				Label
				{
					text						: tableView.getDefaultValue(columnIndex, rowIndex)
					anchors.verticalCenter		: parent.verticalCenter
					anchors.horizontalCenter	: parent.horizontalCenter
					onTextChanged:
					{
						selectedDesign2.itemChanged(columnIndex, rowIndex, value, inputType)
					}
				}
			}
<<<<<<< HEAD

			rowNumberDelegate: Rectangle
=======
		}
		
		TextArea 
		{
			id:									blockDesign
			name:								"blockDesign"
			title:								"Specify blocks for CCD"
			height:								100 * preferencesModel.uiScale
			width:								250 * preferencesModel.uiScale
			visible:							cubePoints.checked && block.checked
		}

		Group{

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
			suggestedColumns:	["scale", "ordinal"]

			rowComponent: Row
>>>>>>> 2781b38 (Renaming Response Surface Design)
			{
				// identical to default but with changed colors
				color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.white// : jaspTheme.analysisBackgroundColor
				Text
				{
					text:					tableView.getRowHeaderText(headerText, rowIndex);
					color:					jaspTheme.textEnabled
					anchors.centerIn:		parent;
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					leftPadding:			3 * preferencesModel.uiScale
					elide:					Text.ElideRight;
					width:					parent.width
					height:					parent.width
					font:					jaspTheme.font
				}

				MouseArea
				{
					anchors.fill: parent
					onClicked:
					{
						if (tableView.rowSelected === rowIndex)
							rowIndex = -1
						tableView.rowSelected = rowIndex;
					}
				}
			}

			columnHeaderDelegate : Rectangle
			{
				// identical to the default definition in TableView, but this does not change color when the column is selected
				color: jaspTheme.analysisBackgroundColor
				Text { text: tableView.getColHeaderText(headerText, columnIndex); anchors.centerIn: parent; font: jaspTheme.font; color:	jaspTheme.textEnabled }
				MouseArea
				{
					anchors.fill: parent
					onClicked:
					{
						if (tableView.colSelected === columnIndex)
							columnIndex = -1
						tableView.colSelected = columnIndex;
					}
				}
			}

			function getRowHeaderText(headerText, rowIndex) { return designData !== undefined ? designData[					    selectedDesign2.designDataColumns * rowIndex] : ""; }
			function getDefaultValue(columnIndex, rowIndex) { return designData !== undefined ? designData[columnIndex + 1 +	selectedDesign2.designDataColumns * rowIndex] : ""; }

		}

		IntegerField { name: "selectedRow"; label: qsTr("debug selected row"); defaultValue: selectedDesign2.rowSelected; negativeValues: true; visible: false }
		IntegerField { name: "selectedCol"; label: qsTr("debug selected col"); defaultValue: selectedDesign2.colSelected; negativeValues: true; visible: false }

	}


	Group
	{
		columns: 2
<<<<<<< HEAD
=======
		
		CheckBox 
		{
			name:						"coefficientTable"
			label:						qsTr("Coefficients table")
			
		}
		
		
		CheckBox
		{
			name:						"residualHistogram"
			label:						qsTr("Residual histogram")
		}
>>>>>>> 2781b38 (Renaming Response Surface Design)

		RadioButtonGroup
		{
<<<<<<< HEAD
			visible:			centralCompositeDesign.checked
			name:				"alphaType"
			title:				qsTr("Alpha")

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true	}
			RadioButton { name:	 "faceCentered";	label: qsTr("Face centred");					}
			RadioButton { name:	 "custom";			label: qsTr("Custom");
				childrenOnSameRow: true
				DoubleField
				{
					name:	"customAlphaValue"
					min:	0
				}
			}
=======
			name:						"anova"
			label:						qsTr("ANOVA table")
		}
		
		
		CheckBox
		{
			name:						"normalProbabilityResidualPlot"
			label:						qsTr("Normal residual plot")
		}

		CheckBox
		{
			name:						"standardizedEffectNormalPlot"
			label:						qsTr("Normal plot of standardized effects")

			CheckBox
			{
				name:					"standardizedEffectNormalPlotGridLines"
				label:					qsTr("Add grid lines")
			}
		}
		
		CheckBox
		{
			name:						"residualsAgainstFittedValuesPlot"
			label:						qsTr("Residual vs. fitted plot")
		}
		
		CheckBox
		{
			name:						"standardizedEffectParetoPlot"
			label:						qsTr("Pareto plot of standardized effects")
>>>>>>> 2781b38 (Renaming Response Surface Design)
		}

		RadioButtonGroup
		{
<<<<<<< HEAD
			name:								"centerPointType"
			title:								qsTr("Center Points")
=======
			name:						"matrixResidualsPlot"
			label:						qsTr("Matrix residuals plot")
		}
	}	
}
	
	Section 
	{
		title: qsTr("Contour plots")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{

				name:  "rsmVariables2";		source:"rsmVariables"
>>>>>>> 2781b38 (Renaming Response Surface Design)

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true	}
			RadioButton { name:	 "custom";			label: qsTr("Custom");
				// TODO: these doublefields should only be enable if the selected element has a nonzero number of cube/ axial points
				childrenOnSameRow: !centralCompositeDesign.checked
				DoubleField
				{
					label	: centralCompositeDesign.checked ? qsTr("Cube block") : "";
					name	: "customCubeBlock"
					min		: 0
				}
				DoubleField
				{
					label		: qsTr("Axial block");
					name		: "customAxialBlock"
					min			: 0
					visible		: centralCompositeDesign.checked
				}
			}
<<<<<<< HEAD
		}

		// show user labels or just -1, 1?
		SetSeed{}
		IntegerField	{ name: "replicates";	label: qsTr("Replicates");			defaultValue: 1; min: 1; max: 100	}

	}

	Common.ShowAndExportDesign {}

	// Section
	// {
	// 	title: qsTr("Desirability")
	// 	CheckBox { name: "desirability";	label: qsTr("Calculate desirability") }
	// 	VariablesForm
	// 	{
	// 		AvailableVariablesList	{ name: "rsmDesirability";		label: qsTr("Response variable list");	source: "rsmResponseVariables" }
	// 		AssignedVariablesList	{ name: "rsmMin";				title: qsTr("Minimum [Min/Max]");		suggestedColumns: ["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min";		negativeValues: true}
	// 				DoubleField {name: "Point_Max";		negativeValues: true; defaultValue: 1}
	// 			}
	// 		}

	// 		AssignedVariablesList
	// 		{
	// 			name: "rsmMax";		title: qsTr("Maximum [Min/Max]");	suggestedColumns:	["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min_1";	negativeValues: true}
	// 				DoubleField {name: "Point_Max_1";	negativeValues: true; defaultValue: 1}
	// 			}
	// 		}

	// 		AssignedVariablesList
	// 		{
	// 			name: "rsmTar";		title: qsTr("Target [Min/Target/Max]");	suggestedColumns: ["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min_2";	negativeValues: true					}
	// 				DoubleField {name: "Point_Tar_2";	negativeValues: true;	defaultValue: 1	}
	// 				DoubleField {name: "Point_Max_2";	negativeValues: true;	defaultValue: 2	}
	// 			}
	// 		}
	// 	}
	// }

=======

			AssignedPairsVariablesList
			{	name:  "pairs";				suggestedColumns: ["scale", "ordinal", "nominal"] }
			} 
		

		Group
		{
			title: qsTr("Contour plot options")
			
			CheckBox
			{
				name:							"contourSurfacePlot"
				label:							qsTr("Contour surface")
				columns: 2

				CheckBox
				{
					name:						"contourSurfacePlotTwoDimensional"
					label:						qsTr("Only show 2D plot")
					id:							contourSurfacePlotTwoDimensional
				}

				CheckBox
				{
					name:						"contourSurfacePlotCoded"
					label:						qsTr("Show analysis and graphs in coded form")
					enabled:					contourSurfacePlotTwoDimensional.checked
				}

				CheckBox
				{
					name:						"contourSurfacePlotLegend"
					label:						qsTr("Show legend next to graph")
					enabled:					!contourSurfacePlotTwoDimensional.checked
				}

				DropDown
				{
					name:						"contourSurfacePlotNumberDivisions"
					label:						qsTr("Divide response surface into N parts")
					values:						[2,3,4,5,6,7]
					enabled:					!contourSurfacePlotTwoDimensional.checked
				}

				Slider
				{
					name:						"contourSurfacePlotVerticalRotationAngle"
					label:						qsTr("Rotating angle (vertical plane)")
					value:						0
					enabled:					!contourSurfacePlotTwoDimensional.checked
				}

				Slider
				{
					name:						"contourSurfacePlotHorizontalRotationAngle"
					label:						qsTr("Rotating angle (horizontal plane)")
					value:						0.5
					vertical:					false
					enabled:					!contourSurfacePlotTwoDimensional.checked
				}
			}
		}
	}
	
	Section
	{
		title: qsTr("Desirability")

		CheckBox 
		{
			name: "desirability"
			label: qsTr("Calculate desirability")
		}

		VariablesForm 
		{
			AvailableVariablesList 
			{ 
				name: "rsmDesirability"
				label: qsTr("Response variable list")
				source: "rsmResponseVariables" 
				
			}

			AssignedVariablesList  
			{
				name:					"rsmMin"
				title:					qsTr("Minimum [Min/Max]")
				suggestedColumns:		["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min"; negativeValues: true}
					DoubleField {name: "Point_Max"; negativeValues: true; defaultValue: 1}
				}
			}

			AssignedVariablesList  
			{
				name:					"rsmMax"
				title:					qsTr("Maximum [Min/Max]")
				suggestedColumns:		["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_1"; negativeValues: true}
					DoubleField {name: "Point_Max_1"; negativeValues: true; defaultValue: 1}
				}
			}

			AssignedVariablesList  
			{
				name:					"rsmTar"
				title:					qsTr("Target [Min/Target/Max]")
				suggestedColumns:		["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_2"; negativeValues: true}
					DoubleField {name: "Point_Tar_2"; negativeValues: true; defaultValue: 1}
					DoubleField {name: "Point_Max_2"; negativeValues: true; defaultValue: 2}
				}
			}
		}
	}
>>>>>>> 2781b38 (Renaming Response Surface Design)
}
