
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

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

import "./common"	as Common

Form
{
	id: form
	columns: 1
	info: qsTr("A response surface design is an experimental framework used to optimize and understand the relationships between several explanatory variables and one or more response variables. By systematically varying the input variables, researchers can explore the effects of these variables on the response, identify optimal conditions, and understand the interactions between variables. Response surface designs are particularly useful in situations where the goal is to optimize a process or product, as they provide a detailed map of the response landscape, enabling precise fine-tuning and improvement.")
	infoBottom: "## " + qsTr("Background") + "\n"
				+ qsTr("A response surface experiment extends the screening design to allow estimation of quadratic effects. After resolving confounding, the screening experiment allows coefficient estimates for main effects and interactions. The response surface experiment expands this equation by adding quadratic or even cubic terms. The form of the empirical model with quadratic terms is:") + "\n\n"
				+ "*y = c<sub>0</sub> + c<sub>1</sub>X<sub>1</sub> + c<sub>2</sub>X<sub>2</sub> + ... + c<sub>n</sub>X<sub>n</sub> + c<sub>12</sub>X<sub>1</sub>X<sub>2</sub> + c<sub>13</sub>X<sub>1</sub>X<sub>3</sub> + ... + c<sub>(n-1)(n)</sub>X<sub>n-1</sub>X<sub>n</sub> + c<sub>11</sub>X<sub>1</sub><sup>2</sup> + c<sub>22</sub>X<sub>2</sub><sup>2</sup> + ... + c<sub>nn</sub>X<sub>n</sub><sup>2</sup>*" + "\n\n"
				+ qsTr("Estimating the coefficients of the model shown in the above equation requires an experiment with at least three factor levels. A full factorial experiment with three levels includes enough trials to estimate all interactions, but quickly becomes large as the number of factors increases.") + "\n\n"
				+ qsTr("Full factorial experiments include all interactions. The hierarchical ordering principle and the effect heredity principle imply that higher order interactions may be ignored, and that any interaction will not be statistically significant at a level of 5% unless the corresponding main effects are also statistically significant at a level of 5%. This allows accurate predictions with 2nd or 3rd order polynomials which can be created with significantly less trials than a full factorial experiment.") + "\n\n"
				+ "### " + qsTr("Central composite design") + "\n"
				+ qsTr("*Central composite designs* (CCD) were first proposed by Box and Wilson for building a second order (quadratic) model for the response variable without needing to use a complete three-level factorial experiment. A central composite design contains an embedded factorial (or fractional factorial of resolution V) design with factorial points and centre points, augmented with a group of star (axial) points that allows estimation of curvature. A central composite design always contains twice as many star (or axial) points (2k) as there are factors (k) in the design. These designs are constructed from three main parts, each of which comprises a set of experimental points:") + "\n\n"
				+ "- " + qsTr("The 2<sup>k</sup> vertices (±1, ±1, ..., ±1) of a *k*-dimensional \"cube\". This part is simply a 2<sup>k</sup> factorial design or a fractional factorial design. The variable coding is chosen so that the two levels (low and high) are -1 or +1.") + "\n"
				+ "- " + qsTr("The 2k vertices (±α, 0, ..., 0), (0, ±α, 0, ..., 0), ..., (0, ..., 0, ±α) of a *k*-dimensional \"star\". These are pairs of experimental points on the coordinate axes, all at a distance α from the origin. The quantity α is often a value between 1.4 and 2, but depends on the number of variables and the properties of the design under study. This part is often referred to as \"star\" or \"axial\".") + "\n"
				+ "- " + qsTr("A number of centre points (0, 0, ..., 0).") + "\n\n"
				+ "![" + qsTr("The design shapes: cube, star, and cube plus star") + "](<%HELP_FOLDER%images/doeResponseSurfaceMethodology/rsmDesignShapes.JPG>)" + "\n\n"
				+ qsTr("There are three general types of central composite designs:") + "\n\n"
				+ "- **" + qsTr("Central composite circumscribed (CCC)") + "**: " + qsTr("CCC designs are the original form of the central composite design. The star (or axial) points are located on a circle encompassing the screening design. These designs have circular, spherical, or hyper-spherical symmetry and require five levels for each factor. It may not be possible to augment a screening design, represented by the corners of the square, if the levels of the screening design are absolute minimum and maximum, as the star (or axial) points exceed these values.") + "\n"
				+ "- **" + qsTr("Face centred central composite (CCF)") + "**: " + qsTr("In the CCF design the star points are located at the centre of each face, i.e. at the middle lines of the square (for two factor designs) or at the centre of the square of the cube (for three factors) formed by the screening design. This variety requires three levels of each factor. This design is not rotatable, and has slightly less precision for coefficient estimation. Its advantage is that star (or axial) points can be added to prior screening designs.") + "\n"
				+ "- **" + qsTr("Central composite inscribed (CCI)") + "**: " + qsTr("For those situations in which the limits specified for factor settings are truly limits, the CCI design uses star (or axial) points which are set at the minimum and maximum levels. If augmenting a screening design, the levels of the screening design will be within the absolute limits to allow the expanded settings for the star (or axial) points. This design also requires five levels of each factor.") + "\n\n"
				+ "![" + qsTr("The types of central composite designs: circumscribed, face centred, and inscribed") + "](<%HELP_FOLDER%images/doeResponseSurfaceMethodology/rsmCCDTypes.JPG>)" + "\n\n"
				+ "### " + qsTr("Box-Behnken design") + "\n"
				+ qsTr("A *Box-Behnken design* (BBD) is an independent quadratic design in that it does not contain an embedded factorial (or fractional factorial) design, i.e. all factors are never set at their high levels simultaneously. In this design the treatment combinations are at the midpoints of edges of the process space and at the centre. These designs are rotatable (or near rotatable) and require three levels of each factor. The designs have limited capability for orthogonal blocking compared to the central composite designs.") + "\n\n"
				+ "![" + qsTr("A Box-Behnken design") + "](<%HELP_FOLDER%images/doeResponseSurfaceMethodology/rsmBBD.JPG>)" + "\n\n"
				+ qsTr("The Box-Behnken set-up consists of twelve \"edge\" points (shown as red dots) all lying on a single sphere about the centre of the experimental region (shown as a blue dot), plus a few replicates of the centre point, e.g., three (blue) centre points.") + "\n\n"
				+ "---\n"
				+ "## " + qsTr("References") + "\n"
				+ "- Automotive Industry Action Group (2005). *Statistical process control (SPC) - Reference manual*. AIAG." + "\n"
				+ "- Box, G. E. P., & Wilson, K. B. (1951). On the experimental attainment of optimum conditions. *Journal of the Royal Statistical Society, Series B, 13*(1), 1-45." + "\n"
				+ "- Dodson, L., Dodson, M., & Klerx, R. (2010). *A study of the hierarchical ordering principle and the effect heredity principle in factorial experimental designs*. 10th Annual Conference of ENBIS, Antwerp." + "\n"

	Common.ShowAndExportDesign {}

	RadioButtonGroup
	{
		name		: "designType"
		title		: qsTr("Design type")
		info		: qsTr("Select the type of response surface design to generate. See the background section below.")

		RadioButton { name:	 "centralCompositeDesign";		label: qsTr("Central composite design");	checked: true;		id: centralCompositeDesign;	info: qsTr("Generate a central composite design.")	}
		RadioButton { name:	 "boxBehnkenDesign";			label: qsTr("Box-Behnken design");										info: qsTr("Generate a Box-Behnken design.")				}

	}

	Group
	{

		// Could probably use a custom IntegerField type...
		IntegerField { id: numberOfContinuous;		label: qsTr("Number of continuous factors");	name: "numberOfContinuous";		min: centralCompositeDesign.checked ? 2 : 3;	defaultValue: centralCompositeDesign.checked ? 2 : 3;	max: centralCompositeDesign.checked ? 10 : 7
			info: qsTr("Set the total number of continuous factors in the design.")
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfCategorical;		label: qsTr("Number of discrete factors");	name: "numberOfCategorical";	min: 0;		defaultValue: 0;	max: 10
			info: qsTr("Set the total number of discrete factors in the design.")
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfLevels;			label: qsTr("Maximum discrete levels");		name: "categoricalNoLevels";	min: 2;		defaultValue: 2;	max: 10
			info: qsTr("Set the maximum number of levels for the discrete factors in the design. Continuous factors always have two defined levels (low/high). If a discrete factor has fewer levels than the maximum number of levels in the design, leave the remaining level cells empty or add a space.")
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}

		TableView
		{

			id: continuousVariablesTable
			infoLabel			: qsTr("Continuous factors")
			info				: qsTr("Set the names and the low and high levels of the continuous factors.")
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
			visible: numberOfCategorical.value != 0
			infoLabel: qsTr("Discrete factors")
			info: qsTr("Set the names of the discrete factors and give the values for each level.")

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
			property int designDataColumns : centralCompositeDesign.checked ? 6 : 3
			property var designData: // it would be better to generate this...
			{
				const val = numberOfContinuous.intValue
				if (centralCompositeDesign.checked) // CCD
				{
					switch(val)
					{
						case 2:
							return	[
								"Full", 13, 5, 0, 0, 1.414,
								"Full", 14, 6, 3, 3, 1.414
							];
						case 3:	return	[
								"Full", 20, 6, 0, 0, 1.682, // fixed manually
								"Full", 20, 6, 4, 2, 1.633
							];
						case 4:	return	[
								"Full", 31, 7, 0, 0, 2,
								"Full", 30, 6, 4, 2, 2
							];
						case 5:	return	[
								"Full", 52, 10, 0, 0, 2.378,
								"Full", 54, 12, 8, 4, 2.366
							];
						case 6:	return	[
								"Full", 90, 14, 0, 0, 2.828,
								"Full", 90, 14, 8, 6, 2.828
							];
						case 7:	return	[
								"Full", 152, 10, 0, 0, 3.364,
								"Full", 160, 18, 8, 10, 3.364
							];
						case 8:	return	[
								"Full", 282, 10, 0, 0, 4,
								"Full", 288, 16, 8, 8, 4
							];
						case 9:	return	[
								"Full", 540, 10, 0, 0, 4.76,
								"Full", 544, 14, 8, 6, 4.76
							];
						case 10:	return	[
								"Full", 1054, 10, 0, 0, 5.66,
								"Full", 1056, 12, 8, 4, 5.66
							];
					}
				}
				else // BBD
				{
					switch(val)
					{
						case 3:		return	[15, 1, 3];
						case 4:		return	[33, 3, 3];
						case 5:		return	[46, 2, 3];
						case 6:		return	[54, 1, 6];
						case 7:		return	[62, 1, 6];
						case 9:		return	[130, 1, 10];
						case 10:	return	[170, 1, 10];
					}
				}
			}

			id					: selectedDesign2
			infoLabel			: qsTr("Design table")
			info				: qsTr("Select one of the pre-set designs by clicking on it in the table.")
			implicitWidth		: form.implicitWidth
			implicitHeight		: 250 * preferencesModel.uiScale

			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: centralCompositeDesign.checked ? [qsTr("Runs"), qsTr("Total"), qsTr("Cube"), qsTr("Axial"), qsTr("Alpha")] : [qsTr("Blocks"), qsTr("Centre points")]
			cornerText			: centralCompositeDesign.checked ? qsTr("Design") : qsTr("Runs")
			initialColumnCount	: designDataColumns - 1// -1 because the first "column" is not a column but the row header
			columnCount			: designDataColumns - 1

			itemType			: JASP.Double
			rowCount			: designData.length / designDataColumns// numberOfContinuous.intValue
			initialRowCount		: designData.length / designDataColumns// numberOfContinuous.intValue

			itemDelegate: Item
			{

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

			rowNumberDelegate: Rectangle
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

		RadioButtonGroup
		{
			visible:			centralCompositeDesign.checked
			name:				"alphaType"
			title:				qsTr("Alpha")
			info:				qsTr("Select how the alpha value (distance of the axial points from the centre) in the design is determined.")

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true;	info: qsTr("Use the default alpha value displayed in the design table.")	}
			RadioButton { name:	 "faceCentered";	label: qsTr("Face centred");					info: qsTr("Use a face centred alpha value, i.e. the axial points lie on the faces of the cube.")	}
			RadioButton { name:	 "custom";			label: qsTr("Custom");							info: qsTr("Enter a custom alpha value.")
				childrenOnSameRow: true
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
			title:								qsTr("Center points")
			info:								qsTr("Select how the number of centre points in the design is determined.")

			RadioButton { name:	 "default";			label: qsTr("Default");			checked: true;	info: qsTr("Use the default number of centre points displayed in the design table.")	}
			RadioButton { name:	 "custom";			label: qsTr("Custom");							info: qsTr("Enter a custom number of centre points.")
				// TODO: these doublefields should only be enable if the selected element has a nonzero number of cube/ axial points
				childrenOnSameRow: !centralCompositeDesign.checked
				DoubleField
				{
					label	: centralCompositeDesign.checked ? qsTr("Cube block") : "";
					info	: centralCompositeDesign.checked ? qsTr("Enter the number of cube centre points.") : qsTr("Enter the number of centre points.")
					name	: "customCubeBlock"
					min		: 0
				}
				DoubleField
				{
					label		: qsTr("Axial block");
					info		: qsTr("Enter the number of axial centre points.")
					name		: "customAxialBlock"
					min			: 0
					visible		: centralCompositeDesign.checked
				}
			}
		}

		// show user labels or just -1, 1?
		SetSeed{ info: qsTr("Set a seed to reproduce a certain random order.") }
		IntegerField	{ name: "replicates";	label: qsTr("Replicates");			defaultValue: 1; min: 1; max: 100;	info: qsTr("Select the number of replications of the whole design.")	}

	}

	// Section
	// {
	// 	title: qsTr("Desirability")
	// 	CheckBox { name: "desirability";	label: qsTr("Calculate desirability") }
	// 	VariablesForm
	// 	{
	// 		AvailableVariablesList	{ name: "rsmDesirability";		label: qsTr("Response variable list");	source: "rsmResponseVariables" }
	// 		AssignedVariablesList	{ name: "rsmMin";				title: qsTr("Minimum [Min/Max]");		allowedColumns: ["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min";		negativeValues: true}
	// 				DoubleField {name: "Point_Max";		negativeValues: true; defaultValue: 1}
	// 			}
	// 		}

	// 		AssignedVariablesList
	// 		{
	// 			name: "rsmMax";		title: qsTr("Maximum [Min/Max]");	allowedColumns:	["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min_1";	negativeValues: true}
	// 				DoubleField {name: "Point_Max_1";	negativeValues: true; defaultValue: 1}
	// 			}
	// 		}

	// 		AssignedVariablesList
	// 		{
	// 			name: "rsmTar";		title: qsTr("Target [Min/Target/Max]");	allowedColumns: ["scale", "ordinal", "nominal"]
	// 			rowComponent: Row
	// 			{
	// 				DoubleField {name: "Point_Min_2";	negativeValues: true					}
	// 				DoubleField {name: "Point_Tar_2";	negativeValues: true;	defaultValue: 1	}
	// 				DoubleField {name: "Point_Max_2";	negativeValues: true;	defaultValue: 2	}
	// 			}
	// 		}
	// 	}
	// }

}
