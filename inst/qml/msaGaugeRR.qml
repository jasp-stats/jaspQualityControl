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

import "./common" as Common

Form
{
	columns:									1

	info:										qsTr("Gauge Repeatability and Reproducibility (Gauge r&R) quantifies the variation in measurements attributable to the measurement system. The variation is split into repeatability (equipment variation) and reproducibility (operator variation). Both variable and attribute measurement data can be used.")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("ANOVA table: two-way ANOVA tables with and without interaction for the input variables, repeatability, reproducibility, and total Gauge r&R.") + "\n"
		+ "- " + qsTr("Gauge r&R variance components: variance and percentage contribution of the input variables, repeatability, reproducibility, and total Gauge r&R.") + "\n"
		+ "- " + qsTr("Gauge evaluation: standard deviations, study variations, and percentage of study variation and tolerance for the input variables, repeatability, reproducibility, and total Gauge r&R.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- ggplot2\n- tidyr\n- qcc\n- cowplot\n"

	DropDown
	{
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue: 						0
		info:									qsTr("Layout of the measurement data: all observations in one column (\"Single column\") or spread across rows with a subgroup index (\"Across rows\").")
		values:
		[
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"},
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
			name:								"operatorLongFormat"
			title:								qsTr("Operator")
			id:									operatorLongFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							!type3.checked
			info:								qsTr("The operators in the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
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
			singleVariable:						false
			allowedColumns:						["scale"]
			info:								qsTr("The measurement columns (one per repeated observation) collected from the process.")
		}

		AssignedVariablesList
		{
			name:								"operatorWideFormat"
			title:								qsTr("Operator")
			id:									operatorWideFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							!type3.checked
			info:								qsTr("The operators in the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
		}
	}

	CheckBox
	{
		name:								"type3"
		id:									type3
		label:								qsTr("Type 3 study (automatic equipment)")
		info:								qsTr("Treat the study as a Type 3 study for automatic equipment, where there is no operator effect; the operator variable is then ignored.")
		onCheckedChanged:
		{
			operatorLongFormat.itemDoubleClicked(0)
			operatorWideFormat.itemDoubleClicked(0)
		}
	}

	Group
	{
		title: 									qsTr("Analysis options")
		columns: 								2

		Group
		{

			DropDown
			{
				name: 							"processVariationReference"
				label: 							qsTr("Std. dev. source")
				id: 							variationReference
				indexDefaultValue: 				0
				info:							qsTr("Source of the process standard deviation: estimated from the data (Study std. dev.) or a historically known value (Historical std. dev.).")
				values:
				[
					{ label: qsTr("Study std. dev."), value: "studySd" },
					{ label: qsTr("Historical std. dev."), value: "historicalSd" }
				]
			}

			DoubleField
			{
				name:							"historicalSdValue"
				label:							qsTr("Historical std. dev.:")
				defaultValue:					3
				min: 							0.000000001
				decimals: 						9
				enabled:						variationReference.currentValue == "historicalSd"
				info:							qsTr("The historically known process standard deviation. Only used when the std. dev. source is set to Historical.")
			}

			CheckBox
			{
				name: 							"tolerance"
				label: 							qsTr("Tolerance width")
				childrenOnSameRow: 				true
				info:							qsTr("Include a tolerance (specification) width in the analysis.")

				DoubleField
				{
					name: 						"toleranceValue"
					id:							toleranceValue
					defaultValue: 				10
					min: 						0.000000001
					decimals: 					9
				}
			}

			CheckBox
			{
				name: 							"anova"
				label: 							qsTr("r&R ANOVA table")
				checked: 						true
				info:							qsTr("Display the Gauge r&R ANOVA tables (the analysis is based on analysis of variance).")

				DropDown
				{
					name: 						"anovaModelType"
					label: 						qsTr("Type of model for F-statistic")
					info:						qsTr("Whether the operator and part effects are treated as fixed or random effects when computing the F-statistic.")
					values:
					[
						{ label: qsTr("Fixed effects"), value: "fixedEffect"},
						{ label: qsTr("Random effects"), value: "randomEffect"}
					]
					indexDefaultValue: (type3.checked) ? 1 : 2
					enabled:      !type3.checked
				}

				DoubleField
				{
					name: 						"anovaAlphaForInteractionRemoval"
					label: 						qsTr("Alpha interaction removal")
					fieldWidth: 				60
					defaultValue: 				0.05
					max: 						1
					decimals: 					3
					info:						qsTr("Alpha level above which the part × operator interaction is removed from the model.")
				}

				DropDown
				{
					name: 						"studyVarianceMultiplierType"
					label: 						qsTr("Study var. multiplier type")
					id: 						studyVarianceMultiplierType
					indexDefaultValue: 			0
					info:						qsTr("Whether the study variation multiplier is expressed in standard deviations or as a percentage.")
					values:
					[
						{ label: qsTr("Std. Deviation"), value: "sd" },
						{ label: qsTr("Percent"), value: "percent" }
					]
				}

				DoubleField
				{
					name: 						"studyVarianceMultiplierValue"
					label: 						qsTr("Study var. multiplier value")
					fieldWidth: 				60
					defaultValue: 				6
					min:						0.001
					max:						99.999
					decimals: 					3
					info:						qsTr("Value of the study variation multiplier.")
				}
			}
		}

		Group
		{
			title: 								qsTr("Plots")

			CheckBox
			{
				name: 						"varianceComponentsGraph"
				label: 						qsTr("Components of variation")
				checked: 					true
				info:						qsTr("Display the components of variation (contribution, study variation, and tolerance) plot.")
			}

			CheckBox
			{
				name: 							"rChart"
				label: 							qsTr("Range charts by operator")
				enabled:						!type3.checked
				info:							qsTr("Display the range chart across operators by part.")
			}

			CheckBox
			{
				name:							"xBarChart"
				label:							qsTr("Average chart bys operator")
				enabled:						!type3.checked
				info:							qsTr("Display the average (X-bar) chart across operators by part.")
			}

			CheckBox
			{
				name: 							"scatterPlot"
				label:							qsTr("Scatter plots operators")
				enabled:						!type3.checked
				info:							qsTr("Display a matrix of scatter plots of the relationship between operators.")

				CheckBox
				{
					name:						"scatterPlotFitLine"
					label:						qsTr("Fit line")
					info:						qsTr("Fit a linear line to the data points.")
				}

				CheckBox
				{
					name:						"scatterPlotOriginLine"
					label:						qsTr("Show origin line")
					info:						qsTr("Display the origin line of the data points.")
				}
			}

			CheckBox
			{
				name:							"partMeasurementPlot"
				label:							qsTr("Measurements by part plot")
				info:							qsTr("Display a plot of the mean measurement across parts.")

				CheckBox
				{
					name:						"partMeasurementPlotAllValues"
					label:						qsTr("Display all measurements")
					info:						qsTr("Display all measurement values across parts.")
				}
			}

			CheckBox
			{
				name:							"operatorMeasurementPlot"
				label:							qsTr("Measurements by operator plot")
				enabled:						!type3.checked
				info:							qsTr("Display a box plot of the measurement values across operators.")
			}

			CheckBox
			{
				name: 							"partByOperatorMeasurementPlot"
				label: 							qsTr("Part × operator interaction plot")
				enabled:						!type3.checked
				info:							qsTr("Display the mean measurement across parts per operator.")
			}

			CheckBox
			{
				name: 							"trafficLightChart"
				label: 							qsTr("Traffic light chart")
				info:							qsTr("Display the total Gauge r&R in relation to the tolerance and process variation as a percentage.")
			}
		}
	}

	Section
	{
		title:	qsTr("Report options")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
			id:		anovaGaugeReport
			columns: 1
			info:	qsTr("Display a formatted Gauge r&R report combining the selected metadata and output components.")

			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 2
				info:		qsTr("Include a metadata header (title, part, gauge, operator, date, etc.) in the report.")

				CheckBox
				{
					name:					"reportTitle"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportTitleText"
						label: 								qsTr("Title")
						id:									reportTitleText
						placeholderText:					qsTr("Gauge r&R Report")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPartName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPartNameText"
						label: 								qsTr("Part name")
						id:									reportPartNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}


				CheckBox
				{
					name:					"reportGaugeName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNameText"
						label: 								qsTr("Gauge name")
						id:									reportGaugeNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportCharacteristic"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportCharacteristicText"
						label: 								qsTr("Characteristic")
						id:									reportCharacteristicText
						placeholderText:					qsTr("Characteristic")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportGaugeNumber"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNumberText"
						label: 								qsTr("Gauge number")
						id:									reportGaugeNumberText
						placeholderText:					qsTr("Number")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportTolerance"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportToleranceText"
						label: 								qsTr("Tolerance")
						id:									reportToleranceText
						placeholderText:					qsTr("Tolerance")
						fieldWidth:							100
					}
				}
				
				CheckBox
				{
					name:					"reportLocation"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportLocationText"
						label: 								qsTr("Location")
						id:									reportLocationText
						placeholderText:					qsTr("Location")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPerformedBy"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPerformedByText"
						label: 								qsTr("Performed by")
						id:									reportPerformedByText
						placeholderText:					qsTr("Analyst")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportDate"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportDateText"
						label: 								qsTr("Date")
						id:									reportDate
						placeholderText:					qsTr("Date")
						fieldWidth:							100
					}
				}
			}
		
			Group
			{
				title:			qsTr("Select Report Components")
				info:			qsTr("Choose which tables and plots are included in the report.")

				CheckBox
				{
					name:		"reportGaugeTable"
					label:		qsTr("Show gauge evaluation table")
					checked:	true
				}
			
				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByPartPlot"
					label:		qsTr("Show measurements by part")
					checked:	true
				}
			
				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show range charts by operator")
					checked:	!type3.checked
					enabled:	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByOperatorPlot"
					label:		qsTr("Show measurements by operator")
					checked:	!type3.checked
					enabled:	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportAverageChartByOperator"
					label:		qsTr("Show average charts by operator")
					checked:	!type3.checked
					enabled:	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportPartByOperatorPlot"
					label:		qsTr("Show part × operator interaction")
					checked:	!type3.checked
					enabled:	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportTrafficLightChart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("Advanced Options")
		columns:								1

		Common.ControlChartTests {}
	}
}
