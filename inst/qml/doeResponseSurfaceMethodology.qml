
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

	Common.ShowAndExportDesign {}

	RadioButtonGroup
	{
		name		: "designType"
		title		: qsTr("Design type")

		RadioButton { name:	 "centralCompositeDesign";		label: qsTr("Central composite design");	checked: true;		id: centralCompositeDesign	}
		RadioButton { name:	 "boxBehnkenDesign";			label: qsTr("Box-Behnken design");															}

	}

	Group
	{

		// Could probably use a custom IntegerField type...
		IntegerField { id: numberOfContinuous;		label: qsTr("Number of continuous factors");	name: "numberOfContinuous";		min: centralCompositeDesign.checked ? 2 : 3;	defaultValue: centralCompositeDesign.checked ? 2 : 3;	max: centralCompositeDesign.checked ? 10 : 7
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfCategorical;		label: qsTr("Number of discrete factors");	name: "numberOfCategorical";	min: 0;		defaultValue: 0;	max: 10
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfLevels;			label: qsTr("Maximum discrete levels");		name: "categoricalNoLevels";	min: 2;		defaultValue: 2;	max: 10
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}

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
			visible: numberOfCategorical.value != 0

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
		}

		RadioButtonGroup
		{
			name:								"centerPointType"
			title:								qsTr("Center points")

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
		}

		// show user labels or just -1, 1?
		SetSeed{}
		IntegerField	{ name: "replicates";	label: qsTr("Replicates");			defaultValue: 1; min: 1; max: 100	}

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
