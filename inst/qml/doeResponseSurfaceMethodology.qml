
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

Form
{
	id: form
	columns: 1

	Group
	{

		RadioButtonGroup
		{
			name		: "designType"
			title		: qsTr("Design Type")

			RadioButton { name:	 "centralCompositeDesign";		label: qsTr("Central Composite Design");	checked: true	}
			RadioButton { name:	 "boxBehnkenDesign ";			label: qsTr("Box-Behnken Design ");							}
		}

		TableView
		{
			property var designData:	parseInt(numberOfContinuous.value) === 2 ?

											[
												"full", 13, 1, 5, 0, 0, Math.round(Math.sqrt(2), 3),
												"full", 14, 2, 6, 3, 3, Math.round(Math.sqrt(2), 3)
											]
								:
											[
												"full", 20, 1, 6, 0, 0, Math.round(2**(3/4), 3),
												"full", 20, 2, 6, 4, 2, Math.round(2**(3/4), 3),
												"full", 20, 3, 6, 4, 2, Math.round(2**(3/4), 3)
											]

			id					: selectedDesign2
			implicitWidth		: form.implicitWidth

			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: [qsTr("Runs"), qsTr("Blocks"), qsTr("Total"), qsTr("Cube"), qsTr("Axial"), qsTr("Alpha")]
			cornerText			: qsTr("Design")
			initialColumnCount	: 6
			itemType			: JASP.String
			rowCount			: numberOfContinuous.value
			initialRowCount		: numberOfContinuous.value

			itemDelegate: Item
			{

				Rectangle
				{

					color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.analysisBackgroundColor
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
				}
			}

			columnHeaderDelegate : Rectangle
			{
				// identical to the default definition in TableView, but this does not change color when the column is selected
				color: jaspTheme.analysisBackgroundColor
//				color: columnIndex === tableView.colSelected ? jaspTheme.grayLighter : jaspTheme.analysisBackgroundColor
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

			function getRowHeaderText(headerText, rowIndex)	{
//					console.log("7 * rowIndex: " +7 * rowIndex);
//					console.log("designData[7 * rowIndex]: " + designData[7 * rowIndex]);
					return designData[					7 * rowIndex];
				}
			function getDefaultValue(columnIndex, rowIndex)	{
//				console.log("columnIndex + 1 +	7 * rowIndex: " + columnIndex + 1 +	7 * rowIndex);
//				console.log("designData[columnIndex + 1 +	7 * rowIndex]: " + designData[columnIndex + 1 +	7 * rowIndex]);
				return designData[columnIndex + 1 +	7 * rowIndex]
			}
		}

		IntegerField { name: "selectedRow"; label: qsTr("debug selected row"); value: selectedDesign2.rowSelected; negativeValues: true }
		IntegerField { name: "selectedCol"; label: qsTr("debug selected col"); value: selectedDesign2.colSelected; negativeValues: true }

		// TODO: should probably come after specifying the variables
		RowLayout
		{
			id: textAbove
			Label { text: qsTr("Design");	Layout.preferredWidth: 40 * preferencesModel.uiScale; Layout.leftMargin: 45 * preferencesModel.uiScale}
			Label { text: qsTr("Runs");		Layout.preferredWidth: 40 * preferencesModel.uiScale }
			Label { text: qsTr("Blocks");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
	//			Label { text: qsTr("Center Points") }
			Label { text: qsTr("Total");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
			Label { text: qsTr("Cube");		Layout.preferredWidth: 40 * preferencesModel.uiScale }
			Label { text: qsTr("Axial");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
			Label { text: qsTr("Alpha");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
		}

		ComponentsList
		{

			width:					form.implicitWidth//textAbove.width
			id:						selectedDesign
			name:					"selectedDesign"
			optionKey:				"name"
			addItemManually:		false

//			values:		parseInt(numberOfContinuous.value) === 2 ?

//							[
//								{"design": "full", "runs": 13, "blocks": 1, "total" : 5, "cube": 0, "axial": 0, "alpha": Math.round(Math.sqrt(2), 3)},
//								{"design": "full", "runs": 14, "blocks": 2, "total" : 6, "cube": 3, "axial": 3, "alpha": Math.round(Math.sqrt(2), 3)}
//							]
//				:
//							[
//								{"design": "full", "runs": 20, "blocks": 1, "total" : 6, "cube": 0, "axial": 0, "alpha": Math.round(2**(3/4), 3)},
//								{"design": "full", "runs": 20, "blocks": 2, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)},
//								{"design": "full", "runs": 20, "blocks": 3, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)}
//							]

			source:		{"values": parseInt(numberOfContinuous.value) === 2 ?

							[
								{"design": "full", "runs": 13, "blocks": 1, "total" : 5, "cube": 0, "axial": 0, "alpha": Math.round(Math.sqrt(2), 3)},
								{"design": "full", "runs": 14, "blocks": 2, "total" : 6, "cube": 3, "axial": 3, "alpha": Math.round(Math.sqrt(2), 3)}
							]
				:
							[
								{"design": "full", "runs": 20, "blocks": 1, "total" : 6, "cube": 0, "axial": 0, "alpha": Math.round(2**(3/4), 3)},
								{"design": "full", "runs": 20, "blocks": 2, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)},
								{"design": "full", "runs": 20, "blocks": 3, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)}
							]
			};


//						})

	//			showAddIcon:			false
	//			sgo

	//			values: {
	////				switch (numberOfContinuous.value)
	////				{
	////					case 2:
	//					if (numberOfContinuous.value)
	//					{
	//						return [
	//							{"design": "full", "runs": 13, "blocks": 1, "total" : 5, "cube": 0, "axial": 0, "alpha": Math.round(Math.sqrt(2), 3)},
	//							{"design": "full", "runs": 14, "blocks": 2, "total" : 6, "cube": 3, "axial": 3, "alpha": Math.round(Math.sqrt(2), 3)}
	//						];
	//					}
	//					else
	//					{
	////					default:
	////					case 3:
	//						return [
	//							{"design": "full", "runs": 20, "blocks": 1, "total" : 6, "cube": 0, "axial": 0, "alpha": Math.round(2**(3/4), 3)},
	//							{"design": "full", "runs": 20, "blocks": 2, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)},
	//							{"design": "full", "runs": 20, "blocks": 3, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)}
	//						];
	//					}
	//				}
	////			})




	//			ButtonGroup { id: radioGroup }
	//			RadioButtonGroup
			RadioButtonGroup{ id: radioGroup; name: "selected" }

			rowComponent: RowLayout
			{

	//				RadioButtonGroup{ id: selected; name: "selected"; RadioButton { name: "selectedValue"; label: ""; checked: false; ButtonGroup.group: radioGroup } }
				RadioButton { name: 1 + rowIndex; label: ""; checked: false; buttonGroup: radioGroup.buttonGroup }

				// TODO: use label!
				TextField		{ id: design;	name: "design";		editable: false;	fieldWidth: 40				} // could also be a dropdown?
				IntegerField	{ id: runs;		name: "runs";		editable: false									}
				IntegerField	{ id: blocks;	name: "blocks";		editable: false									}
				IntegerField	{ id: total;	name: "total";		editable: false									}
				IntegerField	{ id: cube;		name: "cube";		editable: false;	negativeValues: true		}
				IntegerField	{ id: axial;	name: "axial";		editable: false;	negativeValues: true		}
				DoubleField		{ id: alpha;	name: "alpha";		editable: false									}

			}
		}
	}

	Group
	{
		IntegerField { id: numberOfContinuous; label: qsTr("Number of continuous factors"); name: "numberOfContinuous"; min: 0; defaultValue: 2; max: 20    }


		TableView
		{
			JASPDoubleValidator			{ id: doubleValidator; decimals: 3	}
			RegularExpressionValidator	{ id: stringValidator				}


			id: continuousVariablesTable
			modelType			: JASP.Simple

			width				: implicitWidth
			height				: implicitHeight

			initialRowCount		: numberOfContinuous.value
			initialColumnCount	: 3

			rowCount			: numberOfContinuous.value
			columnCount			: 3

			name				: "continuousVariables"
			cornerText			: qsTr("Factor")
			columnNames			: [qsTr("Name"), qsTr("Low"), qsTr("High")]
			isFirstColEditable	: true
//			itemType			: JASP.Double
			itemTypePerColumn	: [JASP.String].concat(Array(19).fill().map(JASP.Double)) // at most 20 items anyway

			function getRowHeaderText(headerText, rowIndex)				{ return String.fromCharCode(65 + rowIndex);	}
			function getDefaultValue(columnIndex, rowIndex)				{ return columnIndex === 0 ? String.fromCharCode(65 + rowIndex) : 2 * columnIndex - 3;	}
			function getValidator(columnIndex, rowIndex)				{ return columnIndex === 0 ? stringValidator : doubleValidator							}
		}

		IntegerField { id: numberOfCategorical;		label: qsTr("Number of categorical factors");	name: "numberOfCategorical";	min: 0;	defaultValue: 0;	max: 20	}
		IntegerField { id: numberOfLevels;			label: qsTr("Maximum levels");					name: "categoricalNoLevels";	min: 2;	defaultValue: 2;	max: 10	}

		TableView
		{
			id: categoricalVariables
			modelType			: JASP.Simple

			isFirstColEditable	: true

			initialRowCount		: numberOfCategorical.value
			initialColumnCount	: 1 + parseInt(numberOfLevels.value)

			rowCount			: numberOfCategorical.value
			columnCount			: 1 + parseInt(numberOfLevels.value)
			name				: "categoricalVariables"
			cornerText			: qsTr("Factor")
			itemType			: JASP.String

			function getColHeaderText(headerText, colIndex)				{ return colIndex === 0 ? qsTr("Name") : qsTr("Level %1").arg(colIndex); }
			function getRowHeaderText(headerText, rowIndex)				{ return String.fromCharCode(65 + rowIndex + parseInt(numberOfContinuous.value)); }
			function getDefaultValue(columnIndex, rowIndex)				{ return String.fromCharCode(columnIndex === 0 ? 65 + rowIndex + parseInt(numberOfContinuous.value) : 97 + columnIndex - 1); }
		}

	}

	Group
	{
		columns: 2

		RadioButtonGroup
		{
			name:								"alphaType"
			title:								qsTr("Alpha")

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true	}
			RadioButton { name:	 "faceCentered";	label: qsTr("Face centred");					}
			RadioButton { name:	 "custom";			label: qsTr("Custom");
				DoubleField
				{
					name:	"customAlphaValue"
					min:	0
				}
			}
		}

		RadioButtonGroup
		{
			name:								"centerPointType"
			title:								qsTr("Center Points")

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true	}
			RadioButton { name:	 "custom";			label: qsTr("Custom");
				// TODO: these doublefields should only be enable if the selected element has a nonzero number of cube/ axial points
				DoubleField
				{
					label: qsTr("Cube block");
					name:	"customCubeBlock"
					min:	0
				}
				DoubleField
				{
					label: qsTr("Axial block");
					name:	"customAxialBlock"
					min:	0
				}
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

		CheckBox
		{
			id:								coded_out
			name:							"codedOutput"
			label:							qsTr("Show coded output") // show user labels or just -1, 1?
		}

//		TODO: remove this? can also do nothing until people select a design
		// Show the design in the output (because people want a button...)
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

		// Export the design to a csv
		Group
		{
			FileSelector
			{
				name:		"exportDesignFile"
				label:		qsTr("Export design:")
				filter:		"*.csv"
				save:		true
			}

			Button
			{
				anchors.right:		parent.right
				anchors.bottom:		parent.bottom
				text: 				actualExporter.checked ? qsTr("Sync Design: On") : qsTr("Sync Design: Off")
				onClicked: 			actualExporter.click()
			}

			CheckBox
			{
				id:					actualExporter
				name:				"actualExporter"
				visible:			false
			}
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
						{ label: "",				value: "nothing"	},// TODO: what is this empty value?
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
