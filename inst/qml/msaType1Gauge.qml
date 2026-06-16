
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
	columns:									2

	info:										qsTr("A Type 1 Instrument Capability Study (Type 1 Gauge) is performed before a Gauge r&R study to assess the capability of a measurement instrument using bias and repeatability. A single part with a known reference value is measured repeatedly by one operator.")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Run chart: plots the measurement values across observations.") + "\n"
		+ "- " + qsTr("Basic statistics table: reference value, mean, bias, standard deviation, study variation (measurement SD times the study variance multiplier), tolerance, and bias as a percentage of the tolerance.") + "\n"
		+ "- " + qsTr("Capability table: Cg, CgK, and the percentage of variation due to repeatability and to repeatability plus bias.") + "\n"
		+ "- " + qsTr("T-test of observed bias: degrees of freedom, bias, confidence interval limits, t-statistic, and p-value for the test of the observed bias against zero.") + "\n"
		+ "- " + qsTr("Bias histogram: histogram of the measurements.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- jaspDescriptives\n- tidyr\n- ggplot2\n- ggrepel\n"

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"measurement"
			title:								qsTr("Measurement")
			singleVariable:						true
			allowedColumns:						["scale"]
			info:								qsTr("The observations/data collected from the process. Repeated measurements of a single reference part.")
		}
	}

	Group
	{
	title: 									qsTr("Analysis options")

		DoubleField
		{
			name: 								"referenceValue"
			label: 								qsTr("Reference/master value")
			defaultValue: 						0
			negativeValues: 					true
			decimals: 							9
			fieldWidth: 						60
			info:								qsTr("The known reference (master) value of the measured part.")
		}

		DoubleField
		{
			name: 								"toleranceRange"
			label: 								qsTr("Tolerance range")
			defaultValue: 						1
			negativeValues: 					false
			decimals: 							9
			fieldWidth: 						60
			info:								qsTr("The width of the tolerance (specification) range of the process.")
		}

		DoubleField
		{
			name: 								"percentToleranceForCg"
			label: 								qsTr("Percent of tolerance for Cg")
			defaultValue: 						20
			negativeValues: 					false
			min:								0.001
			max:								100
			info:								qsTr("Percentage of the tolerance used to compute the capability index Cg.")
		}

		DropDown
		{
			name: 								"studyVarianceMultiplier"
			label: 								qsTr("Number of std. dev. for instrument variation")
			id: 								studyVarMultiplier
			indexDefaultValue: 					0
			values:
			[
				{ label: qsTr("6"), value: 6},
				{ label: qsTr("4"), value: 4}
			]
			info:								qsTr("Number of standard deviations used to express the instrument (study) variation.")
		}
	}
	
	Group
	{
		title: 									qsTr("Bias study options")


		CheckBox
		{
			name: 								"biasTable"
			label: 								qsTr("Bias and instrument capability table")
			checked: 							true
			info:								qsTr("Display the basic statistics and capability tables.")
		}

		CheckBox
		{
			name: 								"tTest"
			label: 								qsTr("One sample t-test")
			checked:							true
			info:								qsTr("Display the one-sample t-test table for the observed bias against zero.")

			CIField
			{
				name: 							"tTestCiLevel"
				label: 							qsTr("Confidence interval for bias")
				info:							qsTr("Width of the confidence interval for the bias.")
			}
		}
	}

	Group
	{
		title: 									qsTr("Plots")

		CheckBox
		{
			name: 								"runChart"
			label: 								qsTr("Run chart")
			checked: 							true
			info:								qsTr("Display the run chart of the measurements across observations.")

			CheckBox
			{
				name: 							"runChartIndividualMeasurementDots"
				label: 							qsTr("Display individual measurements")
				checked: 						true
				info:							qsTr("Show the individual measurement values as dots on the run chart.")
			}

			CheckBox
			{
				name: 							"runChartToleranceLimitLines"
				label: 							qsTr("Display boundaries of the reference interval")
				checked: 						true
				info:							qsTr("Show the boundaries of the reference interval (tolerance limits) on the run chart.")
			}
		}

		CheckBox
		{
			name: 								"histogram"
			label: 								qsTr("Histogram")
			info:								qsTr("Display a histogram of the measurements (bias).")

			DropDown
			{
				name: 					"histogramBinBoundaryDirection"
				id: 					histogramBinBoundaryDirection
				label: 					qsTr("Histogram bin boundaries")
				info:					qsTr("Whether the histogram bin intervals are left-open or right-open.")
				values:
				[
					{ label: qsTr("Left open"),		value: "left"},
					{ label: qsTr("Right open"),	value: "right"}

				]
			}

			DropDown
			{
				name:							"histogramBinWidthType"
				label:							qsTr("Bin width type")
				id: 							binWidthType
				indexDefaultValue:				0
				info:							qsTr("Method used to determine the histogram bin width.")
				values: [
					{ label: qsTr("Sturges"), value: "sturges"},
					{ label: qsTr("Scott"), value: "scott"},
					{ label: qsTr("Freedman-Diaconis"), value: "freedmanDiaconis"},
					{ label: qsTr("Manual"), value: "manual"}
				]
			}

			DoubleField
			{
				name:							"histogramManualNumberOfBins"
				label:							qsTr("Number of bins")
				defaultValue:					30
				min:							3
				max:							10000
				enabled:						binWidthType.currentValue === "manual"
				info:							qsTr("Number of bins to use when the bin width type is set to Manual.")
			}

			CheckBox
			{
				name: 							"histogramMeanLine"
				label: 							qsTr("Display mean")
				checked: 						true
				info:							qsTr("Show the mean value of the measurements on the histogram.")

				CheckBox
				{
					name: 						"histogramMeanCi"
					label: 						qsTr("Confidence interval for mean")
					checked:					true
					childrenOnSameRow:			true
					info:						qsTr("Show a confidence interval for the mean; set its width below.")

					CIField
					{
						name: 					"histogramMeanCiLevel"
					}
				}
			}

			CheckBox
			{
				name: 							"histogramReferenceValueLine"
				label: 							qsTr("Display reference value")
				checked: 						true
				info:							qsTr("Show the reference value on the histogram.")
			}
		}
	}
}
