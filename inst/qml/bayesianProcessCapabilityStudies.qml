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
	function sortIntervalValues() {

		var values = [
			interval1.displayValue,
			interval2.displayValue,
			interval3.displayValue,
			interval4.displayValue
		]
		values.sort(function(a, b) { return a - b })
		interval1.value = values[0]
		interval2.value = values[1]
		interval3.value = values[2]
		interval4.value = values[3]
		interval1b.value = values[0]
		interval2b.value = values[1]
		interval3b.value = values[2]
		interval4b.value = values[3]
	}
	function sortIntervalValuesb() {

		var values = [
			interval1b.displayValue,
			interval2b.displayValue,
			interval3b.displayValue,
			interval4b.displayValue
		]
		values.sort(function(a, b) { return a - b })
		interval1.value = values[0]
		interval2.value = values[1]
		interval3.value = values[2]
		interval4.value = values[3]
		interval1b.value = values[0]
		interval2b.value = values[1]
		interval3b.value = values[2]
		interval4b.value = values[3]
	}
	columns:	 2

	VariablesForm
	{
		id:									variablesFormLongFormat

		AvailableVariablesList
		{
			name:							"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementLongFormat"
			title:							qsTr("Measurement")
			id:								measurementLongFormat
			allowedColumns:					["scale"]
			singleVariable:					true
		}

	}


	// Section
	// {
	// 	title: qsTr("Process capability options")

		Group
		{
			title:					qsTr("Type of data distribution")


			RadioButtonGroup
			{
				name: 					"capabilityStudyType"
				id: 					capabilityStudyType

				RadioButton
				{
					name: 				"normalCapabilityAnalysis"
					id : 				normalCapabilityAnalysis
					label: 				qsTr("Normal distribution")
					checked: 			true
				}

				RadioButton
				{
					name: 				"tCapabilityAnalysis"
					id : 				tCapabilityAnalysis
					label: 				qsTr("Student's t-distribution")
					// checked: 			true
				}

			}
		}

		Group
		{
			columns: 2
			title: qsTr("Metrics")
			info: qsTr("Select the process capability metrics to report.")
			CheckBox { name: "Cp";   label: qsTr("Cp");  checked: true }
			CheckBox { name: "Cpu";	 label: qsTr("Cpu"); checked: true }
			CheckBox { name: "Cpl";	 label: qsTr("Cpl"); checked: true }
			CheckBox { name: "Cpk";	 label: qsTr("Cpk"); checked: true }
			CheckBox { name: "Cpc";	 label: qsTr("Cpc"); checked: true }
			CheckBox { name: "Cpm";	 label: qsTr("Cpm"); checked: true }
		}

		Group
		{
			title: 							qsTr("Capability Study")

			CheckBox
			{
				name: 						"lowerSpecificationLimit"
				label: 						qsTr("Lower specification limit")
				id:							lowerSpecificationLimit
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"lowerSpecificationLimitValue"
					id:						lowerSpecificationLimitValue
					negativeValues:			true
					defaultValue:			-1
					decimals:				9
				}

			}

			CheckBox
			{
				name: 						"target"
				label: 						qsTr("Target value")
				id:							target
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"targetValue"
					id:						targetValue
					negativeValues:			true
					defaultValue:			0
					decimals:				9
				}
			}

			CheckBox
			{
				name: 						"upperSpecificationLimit"
				label: 						qsTr("Upper specification limit")
				id:							upperSpecificationLimit
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"upperSpecificationLimitValue"
					id:						upperSpecificationLimitValue
					negativeValues:			true
					defaultValue:			1
					decimals:				9
				}

			}

		}

		Group
		{

			title: qsTr("Process Criteria")
			GridLayout
			{
				// title: qsTr("Process Criteria")
				columns: 5
				columnSpacing: 2
				rowSpacing: jaspTheme.rowGridSpacing / 3
				id: intervalRow
				property int dbWidth: 50
				property int txtWidth: 100

				// Row 0: Headers
				Label {text: qsTr("Left bound")}
				Item{}
				Label {text: qsTr("Classification")}
				Item{}
				Label {text: qsTr("Right bound")}

				// Row 1: Incapable
				Item{}
				Item{}
				TextField { name: "intervalLabel1"; defaultValue: qsTr("Incapable"); fieldWidth: intervalRow.txtWidth}
				Label { text: "<"; }
				DoubleField { name: "interval1"; id: interval1; fieldWidth: intervalRow.dbWidth; defaultValue: 1.00; onEditingFinished: sortIntervalValues() }

				// Row 2: Capable
				DoubleField { name: "interval1b";id: interval1b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval1.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel2"; defaultValue: qsTr("Capable"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval2"; id: interval2; fieldWidth: intervalRow.dbWidth; defaultValue: 1.33; onEditingFinished: sortIntervalValues() }

				// Row 3: Satisfactory
				DoubleField { name: "interval2b"; id: interval2b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval2.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel3"; defaultValue: qsTr("Satisfactory"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval3"; id: interval3; fieldWidth: intervalRow.dbWidth; defaultValue: 1.50; onEditingFinished: sortIntervalValues() }

				// Row 4: Excellent
				DoubleField { name: "interval3b"; id: interval3b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval3.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel4"; defaultValue: qsTr("Excellent"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval4"; id: interval4; fieldWidth: intervalRow.dbWidth; defaultValue: 2.00; onEditingFinished: sortIntervalValues() }

				// Row 5: Super
				DoubleField { name: "interval4b"; id: interval4b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval4.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: ">"; }
				TextField { name: "intervalLabel5"; defaultValue: qsTr("Super"); fieldWidth: intervalRow.txtWidth}
				Item{}
				Item{}
			}
		}

	// }

	// Section
	// {
	// 	title: qsTr("Prior distributions")

	// }

	Section
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "intervalTable"
			label: qsTr("Interval table")
			info: qsTr("Show the posterior probabilities of the interval specified with the input on the right. Note that the input is automatically sorted and that the first and last fields are always negative and positive infinity.")
		}
		CIField
		{
			name: "credibleIntervalWidth"
			label: qsTr("Credible interval")
			info: qsTr("Width of the credible interval used for the posterior distribution in the Capability table.")
		}
		CheckBox
		{
			name: "intervalPlot"
			label: qsTr("Interval plot")
			info: qsTr("Show the posterior probabilities of the intervals using pie charts.")
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Inference")

		Common.PlotLayout {}

		Common.PlotLayout
		{
			baseName: "priorDistributionPlot"
			baseLabel: qsTr("Prior distribution")
			hasPrior: false
		}

	}

	Section
	{
		title: qsTr("Sequential Analysis")

		Common.PlotLayout
		{
			id: sequentialAnalysisPointEstimatePlot
			baseName: "sequentialAnalysisPointEstimatePlot"
			baseLabel: qsTr("Point estimate plot")
			hasPrior: false
		}

		Common.PlotLayout
		{
			id: sequentialAnalysisIntervalEstimatePlot
			baseName: "sequentialAnalysisPointIntervalPlot"
			baseLabel: qsTr("Interval estimate plot")
			hasPrior: false
			hasEstimate: false
			hasCi: false
			hasType: true
		}

		Group
		{
			CheckBox
			{
				enabled:	sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
				id:			sequentialAnalysisAdditionalInfo
				name:		"sequentialAnalysisPlotAdditionalInfo"
				label:		qsTr("Show process criteria")
				checked:	true
				info:		qsTr("Add a secondary right axis with condition bounds for the process")
			}

			CheckBox
			{
				// TODO:
				enabled:	sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
				name:		"sequentialAnalysisUpdatingTable"
				label:		qsTr("Posterior updating table")
				checked:	false
				info:		qsTr("Show the data from the sequential analysis in a table. Will show both the information for the point estimate and interval estimate plots, if both are selected.")
			}
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Predictive Plots")

		Common.PlotLayout
		{
			baseName: "posteriorPredictiveDistributionPlot"
			baseLabel: qsTr("Posterior predictive distribution")
			hasPrior: false
			hasAxes: false
			hasPanels: false
		}

		Common.PlotLayout
		{
			baseName: "priorPredictiveDistributionPlot"
			baseLabel: qsTr("Prior predictive distribution")
			hasPrior: false
			hasAxes: false
			hasPanels: false
		}

	}


	Section
	{
		title: qsTr("Prior distributions")

		// TODO: this dropdown should just show the same GUI as the custom one
		// but disable e.g., the DropDown itself and instead show the prior
		// also disable all truncation for non-custom ones
		// NOTE: the above is done, but default values cannot be set yet.

		DropDown
		{
			id: priorSettings
			name: "priorSettings"
			label: qsTr("Prior distributions")
			values:
			[
				{label: qsTr("Default"),					value: "default"},
				{label: qsTr("Weakly informed conjugate"),	value: "conjugate"},
				{label: qsTr("Informed conjugate"),			value: "weaklyInformativeConjugate"},
				{label: qsTr("Informed uniform"),			value: "weaklyInformativeUniform"},
				{label: qsTr("Custom informative"),			value: "customInformative"},
			]
		}

		Common.PriorsNew
		{

			// visible: priorSettings.currentValue === "customInformative"
			priorType: capabilityStudyType.value === "normalCapabilityAnalysis" ? "normalModel" : "tModel"

			hasTruncation: priorSettings.currentValue === "customInformative"
			hasParameters: priorSettings.currentValue !== "default"

			dropDownValuesMap: {
				switch (priorSettings.currentValue) {
					case "default":
						return {
							"mean": 	[{ label: qsTr("Jeffreys"),				value: "jeffreys"}],
							"sigma": 	[{ label: qsTr("Jeffreys"),				value: "jeffreys"}],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						}
					case "conjugate":
						return {
							"mean": 	[{ label: qsTr("Normal(μ,σ)"),			value: "normal"}],
							"sigma": 	[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						};
					case "weaklyInformativeConjugate":
						return {
							"mean": 	[{ label: qsTr("Normal(μ,σ)"),			value: "normal"}],
							"sigma": 	[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						}
					case "weaklyInformativeUniform":
						return {
							"mean": 	[{ label: qsTr("Uniform(a,b)"),			value: "uniform"}],
							"sigma": 	[{ label: qsTr("Uniform(a,b)"),			value: "uniform"}],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						}
					case "customInformative":
						return undefined;
				}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced options")

		Group
		{
			title: qsTr("MCMC Settings")
			info: qsTr("Adjust the Markov Chain Monte Carlo (MCMC) settings for estimating the posterior distribution.")
			IntegerField
			{
				name: "noIterations"
				label: qsTr("No. iterations")
				defaultValue: 5000
				min: 100
				max: 100000000
				info: qsTr("Number of MCMC iterations used for estimating the posterior distribution.")
			}
			IntegerField
			{
				name: "noWarmup"
				label: qsTr("No. warmup samples")
				defaultValue: 1000
				min: 0
				max: 100000000
				info: qsTr("Number of initial MCMC samples to discard.")
			}
			IntegerField
			{
				name: "noChains"
				label: qsTr("No. chains")
				defaultValue: 1
				min: 1
				max: 128
				info: qsTr("Number of MCMC chains to run.")
			}
		}
	}
}
