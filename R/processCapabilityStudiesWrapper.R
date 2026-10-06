#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Process Capability Study
#'
#' @param processCapabilityTableZBench, Sigma level based on total defect probability across both specification limits, as opposed to Z (ST)/Z (LT) which equal 3·Cpk/3·Ppk (distance to the nearest bound).
#'    Defaults to \code{FALSE}.
processCapabilityStudies <- function(
          data = NULL,
          version = "0.97.1",
          axisLabels = list(types = list(), value = ""),
          capabilityStudyType = "normalCapabilityAnalysis",
          controlChart = TRUE,
          controlChartSdEstimationMethodGroupSize = "largerThanOne",
          controlChartSdEstimationMethodGroupSizeEqualOne = "meanMovingRange",
          controlChartSdEstimationMethodGroupSizeLargerThanOne = "sBar",
          controlChartSdEstimationMethodMeanMovingRangeLength = 2,
          controlChartSdUnbiasingConstant = TRUE,
          controlChartType = "xBarS",
          controlLimitsNumberOfSigmas = 3,
          dataFormat = "longFormat",
          dataTransformation = "none",
          dataTransformationContinuityAdjustment = FALSE,
          dataTransformationLambda = 0,
          dataTransformationMethod = "loglik",
          dataTransformationShift = 0,
          fixedSubgroupSizeValue = 5,
          groupingVariableMethod = "newLabel",
          histogram = TRUE,
          histogramBinBoundaryDirection = "left",
          histogramBinNumber = 10,
          histogramDensityLine = TRUE,
          historicalLocation = FALSE,
          historicalLocationValue = 1,
          historicalLogMean = FALSE,
          historicalLogMeanValue = 1,
          historicalLogStdDev = FALSE,
          historicalLogStdDevValue = 1,
          historicalMean = FALSE,
          historicalMeanValue = 0,
          historicalScale = FALSE,
          historicalScaleValue = 1,
          historicalShape = FALSE,
          historicalShapeValue = 1,
          historicalStdDev = FALSE,
          historicalStdDevValue = 1,
          historicalThreshold = FALSE,
          historicalThresholdValue = 1,
          lowerSpecificationLimit = FALSE,
          lowerSpecificationLimitBoundary = FALSE,
          lowerSpecificationLimitValue = -1,
          manualSubgroupSizeValue = 0,
          measurementLongFormat = list(types = list(), value = ""),
          measurementsWideFormat = list(types = list(), value = list()),
          nonNormalDistribution = "weibull",
          nonNormalMethod = "percentile",
          nullDistribution = "normal",
          plotHeight = 320,
          plotWidth = 480,
          probabilityPlot = TRUE,
          probabilityPlotGridLines = FALSE,
          probabilityPlotRankMethod = "bernard",
          processCapabilityPlot = TRUE,
          processCapabilityPlotBinNumber = 10,
          processCapabilityPlotDistributions = TRUE,
          processCapabilityPlotSpecificationLimits = TRUE,
          processCapabilityPlotXAxisModification = "none",
          processCapabilityTable = TRUE,
          processCapabilityTableCi = TRUE,
          processCapabilityTableCiLevel = 0.9,
          processCapabilityTableZ = FALSE,
          processCapabilityTableZBench = FALSE,
          report = FALSE,
          reportConclusion = TRUE,
          reportConclusionText = "",
          reportDate = TRUE,
          reportDateText = "",
          reportLine = TRUE,
          reportLineText = "",
          reportLocation = TRUE,
          reportLocationText = "",
          reportMachine = TRUE,
          reportMachineText = "",
          reportMetaData = TRUE,
          reportProbabilityPlot = TRUE,
          reportProcess = TRUE,
          reportProcessCapabilityPlot = TRUE,
          reportProcessCapabilityTables = TRUE,
          reportProcessStability = TRUE,
          reportProcessText = "",
          reportReportedBy = TRUE,
          reportReportedByText = "",
          reportTitle = TRUE,
          reportTitleText = "",
          reportVariable = TRUE,
          reportVariableText = "",
          rule1 = TRUE,
          rule2 = TRUE,
          rule2Value = 7,
          rule3 = TRUE,
          rule3Value = 7,
          rule4 = TRUE,
          rule4Value = 2,
          rule5 = TRUE,
          rule5Value = 15,
          rule6 = TRUE,
          rule6Value = 8,
          rule7 = FALSE,
          rule7Value = 4,
          rule8 = FALSE,
          rule8Value = 14,
          stagesLongFormat = list(types = list(), value = ""),
          stagesWideFormat = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          subgroupSizeType = "manual",
          subgroupSizeUnequal = "actualSizes",
          target = FALSE,
          targetValue = 0,
          testSet = "jaspDefault",
          upperSpecificationLimit = FALSE,
          upperSpecificationLimitBoundary = FALSE,
          upperSpecificationLimitValue = 1,
          xBarMovingRangeLength = 2,
          xmrChartMovingRangeLength = 2,
          xmrChartSpecificationLimits = FALSE) {

   defaultArgCalls <- formals(jaspQualityControl::processCapabilityStudies)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("axisLabels", "controlChartSdEstimationMethodGroupSize", "controlChartSdEstimationMethodGroupSizeEqualOne", "controlChartSdEstimationMethodGroupSizeLargerThanOne", "controlChartType", "dataFormat", "dataTransformation", "dataTransformationMethod", "groupingVariableMethod", "histogramBinBoundaryDirection", "measurementLongFormat", "measurementsWideFormat", "nonNormalDistribution", "nonNormalMethod", "nullDistribution", "probabilityPlotRankMethod", "stagesLongFormat", "stagesWideFormat", "subgroup", "testSet")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "processCapabilityStudies", "processCapabilityStudies.qml", options, version, FALSE))
}