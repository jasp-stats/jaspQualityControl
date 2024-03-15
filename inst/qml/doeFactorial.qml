
// Copyright (C) 2013-2021 University of Amsterdam
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

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0
import JASP										1.0

import "./common"	as Common

Form
{
	columns:									1

	Common.ShowAndExportDesign {}

	Group
	{
		columns: 2

		Group
		{

			IntegerField { id: numberOfCategorical;		label: qsTr("Number of factors");	name: "numberOfCategorical";	min: 2;		defaultValue: 3;	max: 256
				property int intValue: defaultValue
				onValueChanged : { intValue = value !== "" ? value : 0 }
			}
			IntegerField { id: numberOfLevels;			label: qsTr("Maximum levels");		name: "categoricalNoLevels";	min: 2;		defaultValue: 2;	max: 20; 	enabled: factorialType.value == "generalFullFactorial"
				property int intValue: defaultValue
				onValueChanged : { intValue = value !== "" ? value : 0 }
			}
		}

		RadioButtonGroup
		{
			name: 								"factorialType"
			id:									factorialType

			RadioButton
			{
				id:								factorialTypeDefault
				name:							"factorialTypeDefault"
				label:							qsTr("2-level factorial (default generator)")
				checked:						true
			}

			RadioButton
			{
				id:								factorialTypeSpecify
				name:							"factorialTypeSpecify"
				label:							qsTr("2-level factorial (specify generator)")

				TextArea
				{
					name:						"factorialTypeSpecifyGenerators"
					height:						100 * preferencesModel.uiScale
					width:						250 * preferencesModel.uiScale
					visible:					factorialTypeSpecify.checked
					title:						qsTr("Design generator")
					textType:					JASP.TextTypeSource
				}
			}

			RadioButton
			{
				id:								factorialTypeSplit
				visible:						numberOfCategorical.value > 3 | factorialRuns.currentIndex > 0
				name:							"factorialTypeSplit"
				label:							qsTr("2-level split-plot (hard-to-change factors)")

				IntegerField
				{
					name:						"factorialDesignTypeSplitPlotNumberHardToChangeFactors"
					label:						qsTr("Number of hard-to-change factors")
					visible:					factorialTypeSplit.checked
					defaultValue:				1
					min:						1
					max:						numberOfCategorical.value-1
				}
			}

			RadioButton
			{
				id:								generalFullFactorial
				name:							"generalFullFactorial"
				label:							qsTr("General full factorial")
			}
		}
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
		function getRowHeaderText(headerText, rowIndex)				{ return String.fromCharCode(65 + rowIndex); }
		function getDefaultValue(columnIndex, rowIndex) {
														if (columnIndex > 2) {
															return "";  // Return an empty string for columnIndex > 2
														} else if (columnIndex === 0) {
															return String.fromCharCode(65 + rowIndex);  // Uppercase letter for columnIndex 0
														} else {
															return String.fromCharCode(97 + columnIndex - 1);  // Lowercase letter otherwise
														}
													}

	}

	Group
	{
		Label	{ text : qsTr("Design Table")	}
		visible: factorialType.value != "generalFullFactorial"
		TableView
		{
			property int designDataColumns : 3
			property var designData: // it would be better to generate this...
			{
				if (numberOfLevels.value > 2) {
					return["Full factorial", numberOfLevels.intValue**numberOfCategorical.intValue, "Full"]
				} else {
					const val = numberOfCategorical.intValue
					if (val == 2) {
						return	[
									"Full factorial", 4, "Full"
								];
					} else if (val == 3) {
						return	[
									"1/2 fraction", 4, "III",
									"Full factorial", 8, "Full"
								];
					} else if (val == 4) {
						return	[
									"1/2 fraction", 8, "IV",
									"Full factorial", 16, "Full"
								];
					} else if (val == 5) {
						return	[
									"1/4 fraction", 8, "III",
									"1/2 fraction", 16, "V",
									"Full factorial", 32, "Full",
								];
					} else if (val == 6) {
						return	[
									"1/8 fraction", 8, "III",
									"1/4 fraction", 16, "IV",
									"1/2 fraction", 32, "VI",
									"Full factorial", 64, "Full",
								];
					} else if (val == 7) {
						return	[
									"1/16 fraction", 8, "III",
									"1/8 fraction", 16, "IV",
									"1/4 fraction", 32, "IV",
									"1/2 fraction", 64, "VII",
									"Full factorial", 128, "Full",
								];
					} else if (val == 8) {
						return	[
									"1/16 fraction", 16, "IV",
									"1/8 fraction", 32, "IV",
									"1/4 fraction", 64, "V",
									"1/2 fraction", 128, "VIII"
								];
					} else if (val == 9) {
						return	[
									"1/32 fraction", 16, "III",
									"1/16 fraction", 32, "IV",
									"1/8 fraction", 64, "IV",
									"1/4 fraction", 128, "VI"
								];
					} else if (val == 10) {
						return	[
									"1/64 fraction", 16, "III",
									"1/32 fraction", 32, "IV",
									"1/16 fraction", 64, "IV",
									"1/8 fraction", 128, "V",
								];
					} else if (val == 11) {
						return	[
									"1/128 fraction", 16, "III",
									"1/64 fraction", 32, "IV",
									"1/32 fraction", 64, "IV",
									"1/16 fraction", 128, "V",
								];
					} else if (val >= 12) {
						return	[
									"1/256 fraction", 16, "III",
									"1/128 fraction", 32, "IV",
									"1/64 fraction", 64, "IV",
									"1/32 fraction", 128,  "IV",
								];
					}
				}
			}

			id					: selectedDesign2
			implicitWidth		: form.implicitWidth
			implicitHeight		: 150 * preferencesModel.uiScale

			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: [qsTr("Runs"), qsTr("Resolution")]
			cornerText			: qsTr("Design")
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
		CheckBox { name: "showAliasStructure"; label: qsTr("Alias structure"); enabled: numberOfLevels.value == 2 & factorialTypeDefault.checked}
		SetSeed{}

	}

	Group
	{
		columns: 1

		Group
		{
			IntegerField
			{
				name:						"blocks"
				enabled:					!factorialTypeSplit.checked & !factorialTypeSpecify.checked & numberOfLevels.value == 2
				label:						qsTr("Blocks")
				defaultValue:				1
				min:						1
				max:						2**factorialRuns.currentIndex
			}

			IntegerField
			{
				enabled:					!factorialTypeSplit.checked & numberOfLevels.value == 2
				name:						"centerpoints"
				label:						qsTr("Centre points per block")
				defaultValue:				0
				min:						0
				max:						2**(numberOfCategorical.value - 1)
			}

			IntegerField
			{
				name:						"replications"
				label:						qsTr("Replications")
				defaultValue:				1
				min:						1
				max:						8
			}

			IntegerField
			{
				name:						"repetitions"
				label:						qsTr("Repetitions")
				defaultValue:				0
				min:						0
				max:						10
			}
		}
	}
}
