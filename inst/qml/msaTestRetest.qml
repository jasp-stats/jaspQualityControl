
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

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Short gauge study table: sample size, R-bar, historical standard deviation, tolerance, GRR, and GRR as a percentage of the standard deviation and tolerance.") + "\n"
		+ "- " + qsTr("Traffic light graph: GRR as a percentage of the standard deviation and tolerance.") + "\n"
		+ "- " + qsTr("Run chart of parts: all measurement values per part.") + "\n"
		+ "- " + qsTr("Range chart by part: range chart for the measurements using the parts as subgroups.") + "\n"
		+ "- " + qsTr("Scatter plot of the measurements.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- tidyr\n- ggplot2\n"

	DropDown
	{
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue:						0
		info:									qsTr("Layout of the measurement data: all observations in one column (\"Single column\") or spread across rows with a subgroup index (\"Across rows\").")
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
