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
import JASP.Controls

Form
{
	columns: 1

	info:		qsTr("Probability of Detection (POD) models the probability that an inspection detects a flaw as a function of a covariate such as flaw size. A binary detection outcome is regressed on the covariate using a logistic or probit link, producing a detection curve from which detection thresholds (e.g. the size detected with 90%% probability) can be read.")

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"																				}
		AssignedVariablesList	{ name: "outcome";			title: qsTr("Outcomes");	allowedColumns: ["nominal"];	info: qsTr("The binary detection outcome (detected / not detected).")		}
		AssignedVariablesList	{ name: "covariate";		title: qsTr("Covariates");	singleVariable: true;	info: qsTr("The continuous covariate that drives detection, e.g. flaw size.")						}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox
		{
			name:			"modelFitTable"
			label:			qsTr("Model fit table")
			info:			qsTr("Display the model fit table with the fitted coefficients of the detection model.")
		}
	}

	Group
	{
		Layout.rowSpan: 2

		title: qsTr("Detection plot")
		info: qsTr("Plot the fitted probability of detection curve against the covariate.")
		CheckBox
		{
			name: "detectionPlotDataDisplay"
			label: qsTr("Show data")
			checked: true
			info: qsTr("Overlay the observed data on the detection plot.")

			RadioButtonGroup
			{
				name: "detectionPlotDataDisplayType"
				info: qsTr("How the observed data are displayed on the plot.")

				RadioButton	{	label: qsTr("Rugs");			value: "rug";	info: qsTr("Display the data as rug marks along the axis.")						}
				RadioButton	{
					label: qsTr("Points");			value: "points"
					info: qsTr("Display the data as points.")
					childrenOnSameRow: true
					CheckBox { name: "detectionPlotDataDisplayTypePointsJitter"; label: qsTr("Add jitter"); checked: true;	info: qsTr("Add random jitter to the points to reduce overlap.")	}
				}
			}
		}

		CheckBox	{	name: "detectionPlotDensityDisplay"; label: qsTr("Show density"); checked: true;	info: qsTr("Overlay the density of the covariate on the plot.")	}

		CheckBox
		{
			name: "detectionPlotCi"; label: qsTr("Confidence interval")
			childrenOnSameRow: true
			info: qsTr("Display a confidence interval band around the detection curve; set its width alongside.")
			CIField { name: "detectionPlotCiLevel" }
		}

		RadioButtonGroup
		{
			name: "xAxisTicksType"
			title: qsTr("Ticks x-axis")
			info: qsTr("Whether the x-axis tick marks are based on the observed data only or on both the data and the fitted model.")
			RadioButton	{	label: qsTr("based on data");					value: "dataBased"				}
			RadioButton	{	label: qsTr("based on data and model");			value: "dataAndModelBased"		}
		}

		Group
		{
			CheckBox		{ name: "logarithmicXAxis";	label: qsTr("Logarithmic x-axis"); checked: false;	info: qsTr("Use a logarithmic scale for the x-axis (covariate).")	}
			//			CheckBox		{ name: "logarithmicYAxis";	label: qsTr("Logarithmic y-axis"); checked: false; debug: true	}
		}


	}

	Section
	{
		columns: 1
		title: qsTr("Asymptotes")
		info: qsTr("Add horizontal and vertical reference lines (asymptotes) to the detection plot, for example to mark a target detection probability and the corresponding covariate value.")
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
			info: qsTr("Link function used to fit the detection model.")

			RadioButton	{	label: qsTr("Logit");		value: "logit";		checked: true		}
			RadioButton	{	label: qsTr("Probit");		value: "probit"							}
		}

		CheckBox	{	name: "logTransformedCovariate"; label: qsTr("Log transform covariate"); checked: false;	info: qsTr("Log-transform the covariate before fitting the model.")	}
	}
}
