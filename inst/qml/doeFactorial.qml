
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
import JASP				                        1.0

Form
{
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
			SetSeed{}
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
                    title:                      qsTr("Design generator")
					textType:                   JASP.TextTypeSource
				}
			}

			RadioButton
			{
                id:                             factorialTypeSplit
                visible:                        numberOfFactorsForTable.value > 3 | factorialRuns.currentIndex > 0
				name:                           "factorialTypeSplit"
				label:							qsTr("2-level split-plot (hard-to-change factors)")

				IntegerField
				{
					name:						"numberHTCFactors"
					label:						qsTr("Number of hard-to-change factors")
                    visible:                    factorialTypeSplit.checked
					defaultValue:				1
					min:						1
                    max:						numberOfFactors.value-1

				}
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
                            id:                 factorialRuns
                            name: 				"factorialRuns"
							indexDefaultValue: 	0
							values:
                            [
                                { value: 2**(1+Math.floor(Math.log2(numberOfFactorsForTable.value))), label: Number(2**(1+Math.floor(Math.log2(numberOfFactorsForTable.value))))},
                                { value: 2**(2+Math.floor(Math.log2(numberOfFactorsForTable.value))), label: Number(2**(2+Math.floor(Math.log2(numberOfFactorsForTable.value))))},
                                { value: 2**(3+Math.floor(Math.log2(numberOfFactorsForTable.value))), label: Number(2**(3+Math.floor(Math.log2(numberOfFactorsForTable.value))))},
                                { value: 2**(4+Math.floor(Math.log2(numberOfFactorsForTable.value))), label: Number(2**(4+Math.floor(Math.log2(numberOfFactorsForTable.value))))},
                                { value: 2**(5+Math.floor(Math.log2(numberOfFactorsForTable.value))), label: Number(2**(5+Math.floor(Math.log2(numberOfFactorsForTable.value))))},
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
                        name:                   "designByFraction"
                        label:                  qsTr("Fraction")
                        childrenOnSameRow:      true

                        DropDown
                        {
                            name:               "factorialFraction"
                            indexDefaultValue:  0
                            values:
                            [
                                {
                                    value: "0.5",
                                    label: qsTr("1/2")
                                },
                                {
                                    value: numberOfFactorsForTable.value > 4
                                           ? "0.25"
                                           : "0.5",
                                    label: numberOfFactorsForTable.value > 4
                                           ? qsTr("1/4")
                                           : qsTr("1/2")
                                },
                                {
                                    value: numberOfFactorsForTable.value > 5
                                           ? "0.125"
                                           : numberOfFactorsForTable.value > 4
                                             ? "0.25"
                                             : "0.5",
                                    label: numberOfFactorsForTable.value > 5
                                           ? qsTr("1/8")
                                           : numberOfFactorsForTable.value > 4
                                             ? qsTr("1/4")
                                             : qsTr("1/2")
                                }
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
                    max:						2**factorialRuns.currentIndex
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
            enabled:                            displayDesign.checked & factorialTypeDefault.checked
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
            text: 								actualExporter.checked ? qsTr("<b>Sync Design: On</b>") : qsTr("<b>Sync Design: Off</b>")
			onClicked: 							actualExporter.click()
		}

		CheckBox
		{
			id:                                 actualExporter
			name:                               "actualExporter"
			visible:                            false
		}
	}

	Section 
	{
		title: qsTr("Design Analysis")
		columns: 1

	VariablesForm
	{
		AvailableVariablesList
		{
			name:                               "FAallVariables"
			label:                              qsTr("Available factors")
		}

		AssignedVariablesList
		{
			name:                               "FAresponse"
            allowedColumns:                     ["scale", "ordinal", "nominal"]
			singleVariable:                     true
			label:                              qsTr("Response variable")
		}

		AssignedVariablesList
		{
			name:                               "FAassignedFactors"
            allowedColumns:                     ["scale", "ordinal", "nominal"]
			label:                              qsTr("Assigned factors")
		}

		AssignedVariablesList
		{
			debug:                              true
			name:                               "FAblocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
		}

		AssignedVariablesList
		{
            id:                                 runOrder
            name:                               "FArunOrder"
            allowedColumns:                     ["scale", "ordinal", "nominal"]
			singleVariable:                     true
			label:                              qsTr("Run order")
		}
	}
	CheckBox 
	{
		name:                                   "enabledIntOrder"
		id :                                    "enabledIntOrder"
		childrenOnSameRow: true

		IntegerField
		{
			name:                                   "intOrder"
			label:                                  qsTr("Highest order interaction term:")
			defaultValue:                           1
			min:                                    1
			max:                                    5 // change this to number of items in FAassignedFactors
		}
	}

	Group
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["FAassignedFactors"]}
			AssignedVariablesList {  name: "modelTerms"; id: modelTerms; title: qsTr("Model Terms"); listViewType: JASP.Interaction;
			enabled: !enabledIntOrder.checked }
		}

	}

Group
{
	columns: 2
	title: qsTr("Plots")

	Group 
	{
		title: qsTr("Design plots")

		CheckBox
		{
			name:                               "showAliasStructure2"
			label:                              "Show alias structure"
			enabled:							runOrder.count > 0
		}

		//CheckBox
		//{
		//	name:                                   "NormalPlot"
		//	label:                                  qsTr("Normal Plot of the Standardized Effect")

		//	CheckBox
		//	{
		//	name:                                   "addGridlines"
		//	label:                                  qsTr("Display grid lines")
		//	}
		//}

		CheckBox
		{
			name:                                   "paretoPlot"
			label:                                  qsTr("Pareto Plot of Standardized Effects")
		}
	}

	Group
	{
		name:                                   "resPlots"
		title:                                  qsTr("Residuals plots")

		CheckBox
		{
			name:                               "resNorm"
			label:                              qsTr("Normal probability plot of residuals")
		}

		CheckBox
		{
			name:                               "resHist"
			label:                              qsTr("Histogram of residuals")
		}

		CheckBox
		{
			name:                               "resFitted"
			label:                              qsTr("Residuals vs fitted value")
		}

		CheckBox
		{
			name:                               "resOrder"
			label:                              qsTr("Residuals vs run/standard order")
			enabled:							runOrder.count > 0

			RadioButtonGroup
			{
				name:                                   "runOrderPlot"

				
				RadioButton
				{
					name:                              "runOrderStandardPlot"
					label:                              qsTr("Standard")		
					checked:                            true		
			    }

				RadioButton
				{
					name:                               "runOrderRandomPlot"
					label:                              qsTr("Run")
				}
	}
		}

	    CheckBox
		{
			name:                               "fourInOne"
			label:                              qsTr("Matrix residuals plot")
			enabled:							runOrder.count > 0
		}
	}
}
	}
}
