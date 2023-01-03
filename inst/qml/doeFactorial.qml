
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

	Group
	{
		columns: 2
		width: parent.Width

		Group
		{

		IntegerField { id: numberOfCategorical;		label: qsTr("Number of factors");	name: "numberOfCategorical";	min: 2;		defaultValue: 2;	max: 256
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}
		IntegerField { id: numberOfLevels;			label: qsTr("Maximum levels");		name: "categoricalNoLevels";	min: 2;		defaultValue: 2;	max: 16
			property int intValue: defaultValue
			onValueChanged : { intValue = value !== "" ? value : 0 }
		}

					RadioButtonGroup
				{
					name:						"designBy"
					enabled:						factorialTypeDefault.checked | factorialTypeSpecify.checked | factorialTypeSplit.checked

					RadioButton
					{
						name:					"designByRuns"
						label: 					qsTr("Number of runs")
						childrenOnSameRow:		true
						checked:				true

						DropDown
						{
							id:					factorialRuns
							name: 				"factorialRuns"
							indexDefaultValue: 	0
							values:
							[
								{ value: 2**(1+Math.floor(Math.log2(numberOfCategorical.value))), label: Number(2**(1+Math.floor(Math.log2(numberOfCategorical.value))))},
								{ value: 2**(2+Math.floor(Math.log2(numberOfCategorical.value))), label: Number(2**(2+Math.floor(Math.log2(numberOfCategorical.value))))},
								{ value: 2**(3+Math.floor(Math.log2(numberOfCategorical.value))), label: Number(2**(3+Math.floor(Math.log2(numberOfCategorical.value))))},
								{ value: 2**(4+Math.floor(Math.log2(numberOfCategorical.value))), label: Number(2**(4+Math.floor(Math.log2(numberOfCategorical.value))))},
								{ value: 2**(5+Math.floor(Math.log2(numberOfCategorical.value))), label: Number(2**(5+Math.floor(Math.log2(numberOfCategorical.value))))},
							]
						}
					}

					RadioButton
					{
						id:						designByResolution
						name:					"designByResolution"
						enabled:				factorialTypeDefault.checked | factorialTypeSplit.checked
						label: 					qsTr("Resolution")
						childrenOnSameRow:		true

						DropDown
						{
							name: 				"factorialResolution"
							indexDefaultValue: 	1
							values:
							[
								{ value: "Full",label: qsTr("Full")},
								{ value: "III", label: qsTr("III") 	},
								{ value: "IV", 	label: qsTr("IV") 	},
								{ value: "V", 	label: qsTr("V") 	},
								{ value: "VI", 	label: qsTr("VI")	},
								{ value: "VII", label: qsTr("VII")	},
								{ value: "VIII",label: qsTr("VIII")}
							]
						}
					}

					RadioButton
					{
						name:					"designByFraction"
						label:					qsTr("Fraction")
						childrenOnSameRow:		true

						DropDown
						{
							name:				"factorialFraction"
							indexDefaultValue:	0
							values:
							[
								{ value: "0.5", label: qsTr("1/2")},
								{ value: numberOfCategorical.value > 4 ? "0.25" : "0.5", label: numberOfCategorical.value > 4 ? qsTr("1/4") : qsTr("1/2")},
								{ value: numberOfCategorical.value > 5 ? "0.125" : numberOfCategorical.value > 4 ? "0.25" : "0.5", label: numberOfCategorical.value > 5 ? qsTr("1/8") : numberOfCategorical.value > 4 ? qsTr("1/4") : qsTr("1/2")}
							]
						}
					}
				}
	}


			Group
			{
				IntegerField
				{
					enabled:					!factorialTypeSplit.checked
					name:						"factorialCenterPoints"
					label:						qsTr("Number of center points per block")
					defaultValue:				0
					min:						0
					max:						2**(numberOfCategorical.value - 1)
				}

				IntegerField
				{
					id:							factorialCornerReplicates
					name:						"factorialCornerReplicates"
					label:						qsTr("Number of replications")
					defaultValue:				1
					min:						1
					max:						8
				}

				CheckBox
				{
					visible:					factorialCornerReplicates.value > 1
					name:						"factorialRepeats"
					label:						"Repeats only"
				}

				IntegerField
				{
					name:						"factorialBlocks"
					enabled:					!factorialTypeSplit.checked & !designByResolution.checked & !factorialTypeSpecify.checked
					label:						qsTr("Number of blocks")
					defaultValue:				1
					min:						1
					max:						2**factorialRuns.currentIndex
				}

				IntegerField
				{
					name:						"repeatRuns"
					label:						qsTr("Number of random runs to repeat")
					defaultValue:				0
					min:						0
					max:						10
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
			function getDefaultValue(columnIndex, rowIndex)				{ return String.fromCharCode(columnIndex === 0 ? 65 + rowIndex : 97 + columnIndex - 1); }
		}

	Group
	{
		columns: 2

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

		RadioButtonGroup
		{
			name: 								"factorialType"
			title:								qsTr("Type of Factorial Design")

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
					name:						"numberHTCFactors"
					label:						qsTr("Number of hard-to-change factors")
					visible:					factorialTypeSplit.checked
					defaultValue:				1
					min:						1
					max:						numberOfCategorical.value-1
				}
			}
		}
	}

	Group
	{
		CheckBox
		{
			name:								"showAvailableDesigns"
			label:								qsTr("Show available designs")
			checked:							true
		}

		CheckBox
		{
			name:								"showAliasStructure"
			label:								qsTr("Show alias structure")
			enabled:							displayDesign.checked & factorialTypeDefault.checked
		}
		
		CheckBox		{ name: "codedOutput";	label: qsTr("Show coded output")										}

		Common.ShowAndExportDesign {}
	}
}
