
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

import QtQuick                                  2.8
import QtQuick.Layouts                          1.3
import JASP.Controls                            1.0
import JASP.Widgets                             1.0

Form
{
	usesJaspResults:                            true
    columns:                                    1

	GroupBox
	{
        title: 									qsTr("Design Space")
		name:									"designInfo"

		IntegerField
		{
			id:									numberOfFactors
			name:								"numberOfFactors"
			label:								qsTr("Number of factors")
            defaultValue:						3
			min:								2
			max:								256
		}

		IntegerField
		{
			visible:                            false
			id:                                 numberOfFactorsForTable
			name:                               "numberOfFactorsForTable"
			defaultValue:                       numberOfFactors.value
		}
	}

	RadioButtonGroup
	{
        title:                                  qsTr("Unit Display")
		name:                                   "dataCoding"

		RadioButton
		{
			name:                               "dataCoded"
			label:                              qsTr("Coded")
			checked:                            true

		}

		RadioButton
		{
			name:                               "dataUncoded"
			label:                              qsTr("Uncoded")

		}
	}

	RadioButtonGroup
	{
		name:                                   "runOrder"
		title:                                  qsTr("Run Order")
		enabled:                                !factorialTypeSplit.checked

        RadioButton
        {
            name:                               "runOrderRandom"
            label:                              qsTr("Random")
            checked:                            true
        }

		RadioButton
		{
			name:                              "runOrderStandard"
			label:                              qsTr("Standard")
		}

	}

	ColumnLayout
	{
		spacing:                                0
		Layout.preferredWidth:					parent.width
        Layout.columnSpan:						1

		RowLayout
		{
			Label { text: qsTr("Factor");		Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
			Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale}
            Label { text: qsTr("Level 1");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
            Label { text: qsTr("Level 2");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
        }

		ComponentsList
		{
			name:								"factors"
			addItemManually:                    false
            values:                             numberOfFactorsForTable.value

			rowComponent: 						RowLayout
			{
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		40 * preferencesModel.uiScale
					Label
					{
						text: 					rowIndex + 1
					}
				}
                Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale

					TextField
					{
						id:						factorName
						label: 					""
						name: 					"factorName"
						placeholderText:		qsTr("Factor ") + (rowIndex + 1)
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
                Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"low"
                        placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 1")
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
                Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"high1"
						placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 2")
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("Factorial Design Options")
		columns:								2

		RadioButtonGroup
		{
			name: 								"factorialType"
			title:								qsTr("Type of Factorial Design")

			RadioButton
			{
				id:                             factorialTypeDefault
				name:							"factorialTypeDefault"
                label:							qsTr("2-level factorial (default generator)")
                checked:						true
			}

			RadioButton
			{
				id:                             factorialTypeSpecify
				name:							"factorialTypeSpecify"
                label:							qsTr("2-level factorial (specify generator)")

				TextArea
				{
					name:						"factorialTypeSpecifyGenerators"
					height:                     100 * preferencesModel.uiScale
					width:                      250 * preferencesModel.uiScale
					visible:                    factorialTypeSpecify.checked
					title:                      qsTr("Design generators")
					textType:                   JASP.TextTypeSource
				}
			}

			RadioButton
			{
				id:                             factorialTypeSplit
				name:                           "factorialTypeSplit"
				label:							qsTr("2-level split-plot (hard-to-change factors)")

				IntegerField
				{
					name:						"numberHTCFactors"
					label:						qsTr("Number of hard-to-change factors")
					visible:                    factorialTypeSplit.checked
					defaultValue:				1
					min:						1
					max:						numberOfFactors.value

				}
			}

			RadioButton
			{
				id:                             factorialTypeFull
				name:							"factorialTypeFull"
				label:							qsTr("General full factorial design")
			}
		}

		ColumnLayout
		{

			GroupBox
			{
                title: 							qsTr("Design Options")
				enabled:                        factorialTypeDefault.checked | factorialTypeSpecify.checked | factorialTypeSplit.checked

				RadioButtonGroup
				{
					name:						"designBy"

					RadioButton
					{
						name:					"designByRuns"
						label: 					qsTr("Number of runs")
						childrenOnSameRow:		true
						checked:				true

						DropDown
						{
							name: 				"factorialRuns"
							indexDefaultValue: 	0
							values:
                            [
                                { value: 2**(1+Math.floor(Math.log2(numberOfFactors.value))), label: Number(2**(1+Math.floor(Math.log2(numberOfFactors.value))))},
                                { value: 2**(2+Math.floor(Math.log2(numberOfFactors.value))), label: Number(2**(2+Math.floor(Math.log2(numberOfFactors.value))))},
                                { value: 2**(3+Math.floor(Math.log2(numberOfFactors.value))), label: Number(2**(3+Math.floor(Math.log2(numberOfFactors.value))))},
                                { value: 2**(4+Math.floor(Math.log2(numberOfFactors.value))), label: Number(2**(4+Math.floor(Math.log2(numberOfFactors.value))))},
                                { value: 2**(5+Math.floor(Math.log2(numberOfFactors.value))), label: Number(2**(5+Math.floor(Math.log2(numberOfFactors.value))))},
							]

						}
					}

					RadioButton
					{
						id:                     designByResolution
						name:					"designByResolution"
						enabled:                factorialTypeDefault.checked | factorialTypeSplit.checked
						label: 					qsTr("Resolution")
						childrenOnSameRow:		true

						DropDown
						{
							name: 				"factorialResolution"
                            indexDefaultValue: 	1
							values:
                            [
                                { value: "Full", label: qsTr("Full")},
                                { value: "III", label: qsTr("III") 	},
								{ value: "IV", 	label: qsTr("IV") 	},
								{ value: "V", 	label: qsTr("V") 	},
								{ value: "VI", 	label: qsTr("VI")	},
                                { value: "VII", label: qsTr("VII")	},
                                { value: "VIII", label: qsTr("VIII")}
							]
						}
                    }

                    RadioButton
                    {
                        name:                       "byFraction"
                        label:                      qsTr("Fraction")
                        childrenOnSameRow:          true

                        DropDown
                        {
                            name:                   "factorialFraction"
                            indexDefaultValue:      0
                            values:
                            [
                                { value: "0.5", label: qsTr("1/2")  },
                                { value: "0.25", label: qsTr("1/4")  },
                                { value: "0.125", label: qsTr("1/8")  }
                            ]
                        }
                    }
				}
			}

			GroupBox
			{
				title:                          qsTr("Additional Options")

				IntegerField
				{
					enabled:                    !factorialTypeSplit.checked
					name:						"factorialCenterPoints"
					label:						qsTr("Number of center points per block")
					defaultValue:				0
					min:						0
					max:						2**(numberOfFactorsForTable.value - 1)
				}

				IntegerField
				{
					id:                         factorialCornerReplicates
					name:						"factorialCornerReplicates"
					label:						qsTr("Number of replicates for corner points")
					defaultValue:               1
					min:						1
                    max:						8
				}

				CheckBox
				{
					visible:                    factorialCornerReplicates.value > 1
					name:                       "factorialRepeats"
					label:                      "Repeats only"
				}

				IntegerField
				{
					name:						"factorialBlocks"
                    enabled:                    !factorialTypeSplit.checked & !designByResolution.checked & !factorialTypeSpecify.checked
					label:						qsTr("Number of blocks")
					defaultValue:				1
					min:						1
                    max:						8
				}

                IntegerField
                {
                    name:                       "repeatRuns"
                    label:                      qsTr("Number of random runs to repeat")
                    defaultValue:               0
                    min:                        0
                    max:                        10
                }
			}
		}
	}

	GroupBox
	{
		CheckBox
		{
			name:                               "showAvailableDesigns"
			label:                              "Show available designs"
		}

		CheckBox
		{
            id:                                 displayDesign
            name:                               "displayDesign"
			label:                              "Display selected design"
		}


        CheckBox
        {
            name:                               "showAliasStructure"
            label:                              "Show alias structure"
            enabled:                            displayDesign.checked
        }

		FileSelector
		{
			id:                                 file
			name:                               "file"
			label:                              qsTr("Save as:")
			filter:                             "*.csv"
			save:                               true
		}

		Button
		{
			id: 								exportDesign
			anchors.right:						parent.right
			anchors.bottom:						parent.bottom
			text: 								qsTr("<b>Export Design</b>")
			onClicked: 							actualExporter.click()
		}

		CheckBox
		{
			id:                                 actualExporter
			name:                               "actualExporter"
			visible:                            false
		}
	}
}
