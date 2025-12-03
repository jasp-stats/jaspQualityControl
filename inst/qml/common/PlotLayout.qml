//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls


Group
{
	id: root
	property	string	baseName: "posteriorDistributionPlot"
	property	string	baseLabel: qsTr("Posterior distribution")
	property	bool	hasPrior: 		true
	property	bool	hasEstimate: 	true
	property	bool	hasCi: 			true
	property	bool	hasType: 		false

	readonly property alias checked:	mainCheckBox.checked

	CheckBox
	{
		id:							mainCheckBox
		name: 						baseName
		label: 						baseLabel

		// Group so the options are shown in a 2-column layout
		Group
		{

			columns: 					2
			columnSpacing: 				10 * jaspTheme.columnGroupSpacing

			// Group so point estimate and CI options are shown in a single column
			Group
			{
				enabled:			hasEstimate || hasCi
				visible:			hasEstimate || hasCi

				CheckBox
				{
					enabled:			hasEstimate
					visible:			hasEstimate
					label:				qsTr("Point estimate")
					name:				baseName + "IndividualPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	baseName + "IndividualPointEstimateType"
						label:	""
						values:	[
							{label: qsTr("mean"),					value: "mean"},
							{label: qsTr("median"),					value: "median"},
							{label: qsTr("mode"),					value: "mode"}
						]
					}
				}

				// Group so CI checkbox and options are shown in a single column (with subgroup so CI options are indented)
				Group
				{
					enabled:			hasCi
					visible:			hasCi

					columns: 1
					CheckBox
					{
						name:				baseName + "IndividualCi"
						label:				qsTr("CI")
						id:					posteriorPlotIndividualCI
						childrenOnSameRow:	true

						DropDown
						{
							name:		baseName + "IndividualCiType"
							label:		""
							id:			posteriorPlotIndividualType
							values:		[
								{label: qsTr("central"),				value: "central"},
								{label: qsTr("HPD"),					value: "HPD"},
								{label: qsTr("custom"),					value: "custom"}//,
								// {label: qsTr("support"),				value: "support"}
							]
						}
					}

					Group
					{
						columns: 2
						indent: true
						enabled:		posteriorPlotIndividualCI.checked

						CIField
						{
							visible:		posteriorPlotIndividualType.currentValue === "central" || posteriorPlotIndividualType.currentValue === "HPD"
							name:			baseName + "IndividualCiMass"
							label:			qsTr("Mass")
							fieldWidth:		50
							defaultValue: 	95
							min:			1
							max:			100
							inclusive:		JASP.MinMax
						}

						DoubleField
						{
							visible:		posteriorPlotIndividualType.currentValue === "custom"
							name:			baseName + "IndividualCiLower"
							label:			qsTr("Lower")
							id:				plotsPosteriorLower
							fieldWidth:		50
							defaultValue:	0
							negativeValues: true
							inclusive:		JASP.MinMax
						}

						DoubleField
						{
							visible:		posteriorPlotIndividualType.currentValue === "custom"
							name:			baseName + "IndividualCiUpper"
							label:			qsTr("Upper")
							id:				plotsPosteriorUpper
							fieldWidth:		50
							defaultValue:	1
							negativeValues: true
							inclusive:		JASP.MinMax
						}

						FormulaField
						{
							visible:		posteriorPlotIndividualType.currentValue === "support"
							name:			baseName + "IndividualCiBf"
							label:			qsTr("BF")
							fieldWidth:		50
							defaultValue:	"1"
							min:			0
							inclusive:		JASP.None
						}
					}
				}
			}

			Group
			{
				enabled:			hasType
				visible:			hasType

				title:		qsTr("Type")

				columns: 2
				FormulaField
				{
					name:				baseName + "TypeLower"
					label:				qsTr("Lower")
					id:					typeLower
					fieldWidth:			50
					defaultValue:		0.0
					max:				typeUpper.value
				}

				FormulaField
				{
					name:				baseName + "TypeUpper"
					label:				qsTr("Upper")
					id:					typeUpper
					fieldWidth:			50
					defaultValue:		1.0
					min:				typeLower.value

				}
			}

			RadioButtonGroup
			{
				name:		baseName + "PanelLayout"
				title:		qsTr("Layout")
				id:			posteriorDistributionPlotPanelLayout

				RadioButton { value: "multiplePanels"; 	label: qsTr("One plot per metric"); 	checked: true	}
				RadioButton { value: "singlePanel";		label: qsTr("All metrics in one plot") 					}

			}

			RadioButtonGroup
			{
				name:		baseName + "Axes"
				title:		qsTr("Axes")
				id:			posteriorDistributionPlotAxes

				RadioButton { value: "identical"; 	label: qsTr("Automatic"); 					checked: true	}
				RadioButton { value: "automatic";	label: qsTr("Identical across panels"); 	enabled: posteriorDistributionPlotPanelLayout.value === "multiplePanels"	}
				RadioButton { value: "custom";		label: qsTr("Custom axes");	}
			}

			Group
			{

			title: qsTr("Custom axes")
			enabled: posteriorDistributionPlotAxes.value === "custom"
			visible: posteriorDistributionPlotAxes.value === "custom"

				GridLayout
				{
					columns: 5
					columnSpacing: 2
					rowSpacing: jaspTheme.rowGridSpacing / 3
					id: customAxesLayout
					property int dbWidth: 50
					property int txtWidth: 100

					// Row 0: Headers
					Label {text: qsTr("Axis")}
					Item{}
					Label {text: qsTr("Min")}
					Item{}
					Label {text: qsTr("Max")}

					// Row 1: x axis
					Label { text: qsTr("x axis"); }
					Item{}
					DoubleField { name: baseName + "custom_x_min"; id: custom_x_min; fieldWidth: customAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: true; max: custom_x_max.value}
					Item{}
					DoubleField { name: baseName + "custom_x_max"; id: custom_x_max; fieldWidth: customAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: true; min: custom_x_min.value}

					// Row 2: y axis
					Label { text: qsTr("y axis"); }
					Item{}
					DoubleField { name: baseName + "custom_y_min"; id: custom_y_min; fieldWidth: customAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: false; max: custom_y_max.value}
					Item{}
					DoubleField { name: baseName + "custom_y_max"; id: custom_y_max; fieldWidth: customAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: false; min: custom_y_min.value}
				}
			}

			CheckBox
			{
				enabled:	hasPrior
				visible:	hasPrior
				name:		baseName + "PriorDistribution"
				label:		qsTr("Show prior distribution")
				checked:	false
			}
		}
	}
}