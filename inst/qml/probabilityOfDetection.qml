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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{
	columns: 1
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"																				}
		AssignedVariablesList	{ name: "outcome";			title: qsTr("Outcomes");	allowedColumns: ["ordinal", "nominal"]		}
		AssignedVariablesList	{ name: "covariate";		title: qsTr("Covariates");	singleVariable: true							}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox
		{
			name:			"modelFitTable"
			label:			qsTr("Model fit table")
		}
	}

	Group
	{
		Layout.rowSpan: 2

		title: qsTr("Detection plot")
		CheckBox
		{
			name: "detectionPlotDataDisplay"
			label: qsTr("Show data")
			checked: true

			RadioButtonGroup
			{
				name: "detectionPlotDataDisplayType"

				RadioButton	{	label: qsTr("Rugs");			value: "rug"							}
				RadioButton	{
					label: qsTr("Points");			value: "points"
					childrenOnSameRow: true
					CheckBox { name: "detectionPlotDataDisplayTypePointsJitter"; label: qsTr("Add jitter"); checked: true	}
				}
			}
		}

		CheckBox	{	name: "detectionPlotDensityDisplay"; label: qsTr("Show density"); checked: true	}

		CheckBox
		{
			name: "detectionPlotCi"; label: qsTr("Confidence interval")
			childrenOnSameRow: true
			CIField { name: "detectionPlotCiLevel" }
		}

		RadioButtonGroup
		{
			name: "xAxisTicksType"
			title: qsTr("Ticks x-axis")
			RadioButton	{	label: qsTr("based on data");					value: "dataBased"				}
			RadioButton	{	label: qsTr("based on data and model");			value: "dataAndModelBased"		}
		}

		Group
		{
			CheckBox		{ name: "logarithmicXAxis";	label: qsTr("Logarithmic x-axis"); checked: false	}
			//			CheckBox		{ name: "logarithmicYAxis";	label: qsTr("Logarithmic y-axis"); checked: false; debug: true	}
		}


	}

	Section
	{
		columns: 1
		title: qsTr("Asymptotes")
		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width

			Label { text: qsTr("Horizontal asymptotes");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 210 * preferencesModel.uiScale}

			RowLayout
			{
				Label { text: qsTr("Label");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 210 * preferencesModel.uiScale}
				Label { text: qsTr("y-coordinate");		Layout.preferredWidth: 97 * preferencesModel.uiScale}
			}

			ComponentsList
			{
				name:					"horizontalAsymptotes"
				defaultValues: 			[]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	210 * preferencesModel.uiScale
						TextField
						{
							label: 				""
							name: 				"horizontalAsymptoteName"
							startValue:			qsTr("Asymptote ") + (rowIndex + 1)
							fieldWidth:			160 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}

					Row
					{
						spacing:				4 * preferencesModel.uiScale
						DoubleField
						{
							label:				""//qsTr("y-coordinate")
							name:				"horizontalAsymptoteValue"
							defaultValue:		0.5
							min:				0.0
							max:				1.0
							inclusive:			JASP.None
							fieldWidth:			70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
							controlXOffset:		6 * preferencesModel.uiScale
						}
					}
				}
			}
		}

		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width

			Label { text: qsTr("Vertical asymptotes");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 210 * preferencesModel.uiScale}

			RowLayout
			{
				Label { text: qsTr("Label");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 210 * preferencesModel.uiScale}
				Label { text: qsTr("x-coordinate");		Layout.preferredWidth: 97 * preferencesModel.uiScale}
			}

			ComponentsList
			{
				name:					"verticalAsymptotes"
				defaultValues: 			[]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	210 * preferencesModel.uiScale
						TextField
						{
							label: 				""
							name: 				"verticallAsymptoteName"
							startValue:			qsTr("Asymptote ") + (rowIndex + 1)
							fieldWidth:			160 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}

					Row
					{
						spacing:				4 * preferencesModel.uiScale
						DoubleField
						{
							label:				""//qsTr("y-coordinate")
							name:				"verticalAsymptoteValue"
							defaultValue:		0.5
							negativeValues:		true
							inclusive:			JASP.None
							fieldWidth:			70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
							controlXOffset:		6 * preferencesModel.uiScale
						}
					}
				}
			}
		}

	}

	Section
	{
		title: qsTr("Fitting options")

		RadioButtonGroup
		{
			name: "linkFunction"
			title: qsTr("Link function")

			RadioButton	{	label: qsTr("Logit");		value: "logit";		checked: true		}
			RadioButton	{	label: qsTr("Probit");		value: "probit"							}
		}

		CheckBox	{	name: "logTransformedCovariate"; label: qsTr("Log transform covariate"); checked: false	}
	}
}
