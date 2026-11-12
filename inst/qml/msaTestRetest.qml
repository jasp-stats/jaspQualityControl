
// Copyright (C) 2013-2018 University of Amsterdam
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

Form
{
	columns:									1

	info:										qsTr("The Test-retest (range method) study approximates an overall picture of the measurement system's variability. It does not separate the variability into its repeatability and reproducibility components.")

	infoBottom: 								"## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- tidyr\n- ggplot2\n"

	DropDown
	{
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue:						0
		info:									qsTr("Layout of the measurement data: all observations in one column (\"Single column\") or spread across rows with a subgroup index (\"Across rows\"). The paired range method requires exactly two measurement sets. In across-rows format, select two measurement columns and use one row per unique part; rows with a missing value are excluded. In single-column format, use exactly two operator/repetition levels, with one reading for every part at each level; the first level is plotted as the 1st measurement. Rows with missing values are excluded, and a part left without a reading in both levels produces an error, as do duplicate readings and extra levels.")
		values: [
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"}
		]
		onValueChanged:
		{
			measurementsWideFormat.itemDoubleClicked(0)
			measurementLongFormat.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		id:										variablesFormLongFormat
		visible:								dataFormat.currentValue == "longFormat"
		preferredHeight:						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesFormLongFormat"
		}
		
		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			singleVariable:						true
			allowedColumns:						["scale"]
			info:								qsTr("The observations/data collected from the process.")
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"operator"
			title:								qsTr("Operator/Repetition")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The operators of the measurement system (or repetition index).")
		}
	}

	VariablesForm
	{
		id:										variablesFormWideFormat
		visible:								dataFormat.currentValue == "wideFormat"
		preferredHeight:						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesFormWideFormat"
		}

		
		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			id:									measurementsWideFormat
			visible:							dataFormat.currentValue == "wideFormat"
			allowedColumns:						["scale"]
			info:								qsTr("The measurement columns (repeated observations of each part).")
			maxRows:							2
			height:								75 * preferencesModel.uiScale
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal", "ordinal", "scale"]
			info:								qsTr("The parts of the measurement system.")
		}
	}
		

	Group
	{
		title: 								qsTr("Analysis options")

		CheckBox
		{
			name:							"manualProcessSd"
			label:							qsTr("Process std. dev.")
			childrenOnSameRow:				true
			info:							qsTr("Specify the historical process standard deviation.")

			DoubleField
			{
				name:						"manualProcessSdValue"
				defaultValue:				1
			}
		}

		CheckBox
		{
			name:							"tolerance"
			label:							qsTr("Tolerance")
			childrenOnSameRow: 				true
			info:							qsTr("Include a tolerance (specification) value in the analysis.")

			DoubleField
			{
				name:						"toleranceValue"
				defaultValue:				1
			}
		}

		CheckBox
		{
			name: 							"repeatabilityAndReproducibilityTable"
			label: 							qsTr("r&R table")
			checked: 						true
			info:							qsTr("Output the short gauge study table.")
		}
	}

	Group
	{
		title: 								qsTr("Plots")

		CheckBox
		{
			name: 							"runChartPart"
			label:							qsTr("Run chart of parts")
			info:							qsTr("Plot the measurement values against the parts.")
		}

		CheckBox
		{
			name:							"scatterPlotMeasurement"
			label:							qsTr("Scatter plot measurement")
			checked:						true
			info:							qsTr("Plot a scatter plot of the measurements.")

			CheckBox
			{
				name:						"scatterPlotMeasurementFitLine"
				label:						qsTr("Fit line")
				checked:					true
				info:						qsTr("Fit a regression line to the scatter plot.")
			}

			CheckBox
			{
				name:						"scatterPlotMeasurementAllValues"
				label:						qsTr("Display all measurements")
				info:						qsTr("Display all measurement values on the scatter plot.")
			}
		}

		CheckBox
		{
			name: 							"rChart"
			label: 							qsTr("Range chart")
			info:							qsTr("Plot the range chart by part.")
		}

		CheckBox
		{
			name: 							"trafficLightChart"
			label: 							qsTr("Traffic light chart")
			info:							qsTr("Plot the traffic light graph (GRR as a percentage of the standard deviation and tolerance).")
		}
	}
}
