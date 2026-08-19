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

			CheckBox
			{
				name: "timeSeriesPlot"
				label: qsTr("Time series plot")
				checked: false
			}
		}

		Group
		{
			title: qsTr("Process Criteria")
			Layout.columnSpan: 2
			Layout.fillWidth: true
			preferredWidth: form.availableWidth

			ComponentsList
			{
				name: "processCriteria"
				id: processCriteria

				preferredWidth: form.availableWidth - jaspTheme.groupContentPadding

				optionKey: "upper"
				optionKeyLabel: "label"

				addItemManually: true
				minimumItems: 2

				defaultValues:
				[
					{ lower: -Infinity, label: qsTr("Incapable"),    upper: 1.00 },
					{ lower: 1.00,      label: qsTr("Capable"),      upper: 1.33 },
					{ lower: 1.33,      label: qsTr("Satisfactory"), upper: 1.50 },
					{ lower: 1.50,      label: qsTr("Excellent"),    upper: 2.00 },
					{ lower: 2.00,      label: qsTr("Super"),        upper: Infinity }
				]

				headerLabels:
				[
					{
						lower: qsTr("Left bound"),
						label: qsTr("Classification"),
						upper: qsTr("Right bound")
					}
				]
				property int rowRevision: 0
				property int previousCount: 0
				property int thresholdRevision: 0

				// function refreshThresholds()
				// {
				// 	thresholdRevision++
				// }
				function refreshThresholds()
				{
					var oldIndex = processOverviewThreshold.currentIndex

					thresholdRevision++

					Qt.callLater(function() {
						var newCount = overviewThresholdValues().length

						if (newCount === 0)
							return

						processOverviewThreshold.currentIndex =
							Math.min(oldIndex, newCount - 1)
					})
				}
				function initializeNewLastRow()
				{
					if (count < 2)
						return

					var largest = -Infinity

					// Find largest existing finite bound.
					for (var i = 0; i < count; ++i)
					{
						var row = rowAt(i)

						if (!row)
							continue

						var lower = Number(row.lowerValue)
						var upper = Number(row.upperValue)

						if (isFinite(lower) && lower > largest)
							largest = lower

						if (isFinite(upper) && upper > largest)
							largest = upper
					}

					// Sensible fallback if there somehow are no finite bounds.
					if (!isFinite(largest))
						largest = 0

					var newBoundary = largest + 1

					var previousLast = rowAt(count - 2)
					var newLast      = rowAt(count - 1)

					if (!previousLast || !newLast)
						return

					// The old last row now needs a finite upper bound,
					// and the new last row gets the matching lower bound.
					// previousLast.upperValue = newBoundary
					// newLast.lowerValue      = newBoundary

					// // Keep the final interval open-ended.
					// newLast.upperValue = Infinity

					previousLast.upperValue = newBoundary
					newLast.lowerValue      = newBoundary
					newLast.upperValue      = Infinity
					refreshThresholds()

				}

				onCountChanged:
				{
					var oldCount = previousCount
					previousCount = count

					if (oldCount > 0 && count > oldCount)
					{
						Qt.callLater(function() {
							processCriteria.initializeNewLastRow()
							processCriteria.refreshRowPositions()
						})
					}
					else
					{
						Qt.callLater(processCriteria.refreshRowPositions)
					}
				}

				Component.onCompleted:
				{
					previousCount = count

					Qt.callLater(function() {
						processCriteria.refreshRowPositions()
						processCriteria.sortAndSynchronize()
					})
				}

				function refreshRowPositions()
				{
					rowRevision++
				}

				function rowIndexOf(row)
				{
					for (var i = 0; i < count; ++i)
					{
						if (rowAt(i) === row)
							return i
					}

					return -1
				}



				function rowsInOrder()
				{
					var rows = []

					for (var i = 0; i < count; ++i)
					{
						var row = rowAt(i)

						if (row)
							rows.push(row)
					}

					return rows
				}

				// function overviewThresholdValues()
				// {
				// 	var thresholds = []

				// 	for (var i = 0; i < count; ++i)
				// 	{
				// 		var criterion = rowAt(i)

				// 		if (!criterion)
				// 			continue

				// 		var bounds = [
				// 			Number(criterion.lowerValue),
				// 			Number(criterion.upperValue)
				// 		]

				// 		for (var j = 0; j < bounds.length; ++j)
				// 		{
				// 			if (isFinite(bounds[j]) &&
				// 				thresholds.indexOf(bounds[j]) === -1)
				// 			{
				// 				thresholds.push(bounds[j])
				// 			}
				// 		}
				// 	}

				// 	thresholds.sort(function(a, b) {
				// 		return a - b
				// 	})

				// 	return thresholds.map(function(value) {
				// 		return {
				// 			label: String(value),
				// 			value: String(value)
				// 		}
				// 	})
				// }

				// function overviewThresholdValues()
				// {
				// 	var thresholds = []

				// 	for (var i = 0; i < processCriteria.count; i++)
				// 	{
				// 		var criterion = processCriteria.rowAt(i)

				// 		if (!criterion)
				// 			continue

				// 		var bounds = [
				// 			Number(criterion.lowerValue),
				// 			Number(criterion.upperValue)
				// 		]

				// 		for (var j = 0; j < bounds.length; j++)
				// 		{
				// 			if (isFinite(bounds[j]) &&
				// 				thresholds.indexOf(bounds[j]) === -1)
				// 			{
				// 				thresholds.push(bounds[j])
				// 			}
				// 		}
				// 	}

				// 	thresholds.sort(function(a, b) {
				// 		return a - b
				// 	})

				// 	return thresholds.map(function(value) {
				// 		return {
				// 			label: String(value),
				// 			value: String(value)
				// 		}
				// 	})
				// }
				function overviewThresholdValues()
				{
					var thresholds = []

					for (var i = 0; i < count - 1; ++i)
					{
						var criterion = rowAt(i)

						if (!criterion)
							continue

						var value = Number(criterion.upperValue)

						if (isFinite(value))
							thresholds.push(value)
					}

					return thresholds.map(function(value) {
						return {
							label: String(value),
							value: String(value)
						}
					})
				}
				// function boundaryEdited(boundaryIndex, newValue)
				// {
				// 	var rows = rowsInOrder()

				// 	if (rows.length < 2)
				// 		return

				// 	if (boundaryIndex < 0 || boundaryIndex >= rows.length - 1)
				// 		return

				// 	var values = []

				// 	for (var i = 0; i < rows.length - 1; ++i)
				// 	{
				// 		if (i === boundaryIndex)
				// 			values.push(newValue)
				// 		else
				// 			values.push(rows[i].upperValue)
				// 	}

				// 	values.sort(function(a, b) {
				// 		return a - b
				// 	})

				// 	for (var j = 0; j < values.length; ++j)
				// 	{
				// 		rows[j].upperValue = values[j]
				// 		rows[j + 1].lowerValue = values[j]
				// 	}
				// }

				function boundaryEdited(boundaryIndex, newValue)
				{
					var rows = rowsInOrder()

					if (rows.length < 2)
						return

					if (boundaryIndex < 0 || boundaryIndex >= rows.length - 1)
						return

					var values = []

					for (var i = 0; i < rows.length - 1; ++i)
					{
						if (i === boundaryIndex)
							values.push(newValue)
						else
							values.push(rows[i].upperValue)
					}

					values.sort(function(a, b) {
						return a - b
					})

					for (var j = 0; j < values.length; ++j)
					{
						rows[j].upperValue = values[j]
						rows[j + 1].lowerValue = values[j]
					}

					refreshThresholds()
				}
				// function sortAndSynchronize()
				// {
				// 	var rows = rowsInOrder()

				// 	if (rows.length < 2)
				// 		return

				// 	var values = []

				// 	for (var i = 0; i < rows.length - 1; ++i)
				// 		values.push(rows[i].upperValue)

				// 	values.sort(function(a, b) {
				// 		return a - b
				// 	})

				// 	for (var j = 0; j < values.length; ++j)
				// 	{
				// 		rows[j].upperValue = values[j]
				// 		rows[j + 1].lowerValue = values[j]
				// 	}
				// }
				function sortAndSynchronize()
				{
					var rows = rowsInOrder()

					if (rows.length < 2)
						return

					var values = []

					for (var i = 0; i < rows.length - 1; ++i)
						values.push(rows[i].upperValue)

					values.sort(function(a, b) {
						return a - b
					})

					for (var j = 0; j < values.length; ++j)
					{
						rows[j].upperValue = values[j]
						rows[j + 1].lowerValue = values[j]
					}

					refreshThresholds()
				}

				rowComponent: Row
				{
					id: criterionRow

					property alias lowerValue: lowerBound.value
					property alias upperValue: upperBound.value

					property int currentRowIndex:
						processCriteria.rowIndexOf(criterionRow)

					// property bool isFirstRow:
					// 	processCriteria.count > 0 &&
					// 	criterionRow === processCriteria.rowAt(0)

					// property bool isLastRow:
					// 	processCriteria.count > 0 &&
					// 	criterionRow === processCriteria.rowAt(processCriteria.count - 1)

					property bool isFirstRow:
					{
						var revision = processCriteria.rowRevision
						return rowIndex === 0
					}

					property bool isLastRow:
					{
						var revision = processCriteria.rowRevision

						if (processCriteria.count <= 0)
							return false

						return processCriteria.rowAt(processCriteria.count - 1) === criterionRow
					}

					// Left bound
					DoubleField
					{
						id: lowerBound
						name: "lower"

						/*
						* Keep the control in the layout so that the header
						* remains correctly aligned, but hide it visually
						* for the first row.
						*/
						opacity: criterionRow.isFirstRow ? 0 : 1
						enabled: !criterionRow.isFirstRow

						defaultValue: 0
						negativeValues: true
						decimals: 9
						fieldWidth: 80

						onEditingFinished:
						{
							var index =
								processCriteria.rowIndexOf(criterionRow)

							processCriteria.boundaryEdited(
								index - 1,
								Number(displayValue)
							)
						}
					}

					// <
					Label
					{
						text: criterionRow.isFirstRow ? "" : "<"
						width: 10
					}

					// Classification
					TextField
					{
						name: "label"
						startValue: qsTr("Region ") + (rowIndex + 1)
						fieldWidth: 120
					}

					// ≤
					Label
					{
						text: criterionRow.isLastRow ? "" : "≤"
						width: 10
					}

					// Right bound
					DoubleField
					{
						id: upperBound
						name: "upper"

						/*
						* Same trick for the final right bound.
						*/
						opacity: criterionRow.isLastRow ? 0 : 1
						enabled: !criterionRow.isLastRow

						defaultValue: 1
						negativeValues: true
						decimals: 9
						fieldWidth: 80

						onEditingFinished:
						{
							var index =
								processCriteria.rowIndexOf(criterionRow)

							processCriteria.boundaryEdited(
								index,
								Number(displayValue)
							)
						}
					}

					Component.onCompleted:
					{
						Qt.callLater(function() {
							processCriteria.refreshRowPositions()
							processCriteria.sortAndSynchronize()
						})
					}

					Component.onDestruction:
					{
						Qt.callLater(function() {
							processCriteria.refreshRowPositions()
							processCriteria.sortAndSynchronize()

							var rows = processCriteria.rowsInOrder()

							for (var i = 0; i < rows.length - 1; ++i)
								rows[i + 1].lowerValue = rows[i].upperValue
						})
					}
				}
			}

			CheckBox
			{
				name: "processOverview"
				id: processOverview
				label: qsTr("Process overview")
				info: qsTr("Show a four-panel overview of the process.")

				DropDown
				{
					name: "processOverviewMetric"
					label: qsTr("Capability metric")
					values:
					[
						{ label: "Cp",  value: "Cp"  },
						{ label: "Cpu", value: "Cpu" },
						{ label: "Cpl", value: "Cpl" },
						{ label: "Cpk", value: "Cpk" },
						{ label: "Cpc", value: "Cpc" },
						{ label: "Cpm", value: "Cpm" }
					]
					indexDefaultValue: 3
				}

				DropDown
				{
					name: "processOverviewThreshold"
					id: processOverviewThreshold
					label: qsTr("Threshold")
					indexDefaultValue: 1

					values:
					{
						var revision = processCriteria.thresholdRevision
						return processCriteria.overviewThresholdValues()
					}
				}
			}
		}

		// }



	Section
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "intervalTable"
			label: qsTr("Interval table")
			info: qsTr("Show posterior probabilities for the process criteria defined above.")
		}
		CIField
		{
			name: "credibleIntervalWidth"
			label: qsTr("Credible interval")
			info: qsTr("Width of the credible interval used for the posterior distribution in the Capability table.")
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
				{label: qsTr("Informed conjugate"),			value: "conjugate"},
				// {label: qsTr("Informed conjugate"),			value: "weaklyInformativeConjugate"},
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
			visible:       priorSettings.currentValue !== "default"

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
					// case "weaklyInformativeConjugate":
					// 	return {
					// 		"mean": 	[{ label: qsTr("Normal(μ,σ)"),			value: "normal"}],
					// 		"sigma": 	[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }],
					// 		"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
					// 	}
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
