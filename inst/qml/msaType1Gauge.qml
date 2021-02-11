
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

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form 
{
	usesJaspResults:							true
	columns:									1

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}
		AssignedVariablesList
		{
			id:									variable1
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						false
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
	}
	
		Group
	{
		title: qsTr("Determine Bias")
					DoubleField { name: "biasReferenceValue";	label: qsTr("Reference Value:");		defaultValue: 0;	negativeValues: true; decimals: 5; fieldWidth: 60}
					DoubleField { name: "biasTolerance";	label: qsTr("Tolerance Value:");		defaultValue: 1;	negativeValues: true; decimals: 5; fieldWidth: 60}
					DoubleField { name: "biasPercentCG";	label: qsTr("Percent of Tolerance for CG:");		defaultValue: 20;	negativeValues: false	}
					
		DropDown{
				name: "BiasStudyVarMultiplierType"
				label: qsTr("Study Variation Multiplier")
				indexDefaultValue: 0
				values:
						[
						{label: qsTr("Standard Deviation"),		value: "svmSD"},
						{label: qsTr("Percent"),				value: "svmPercent"},
						]
				id: studyVarMultiplierType
				}
										
		DoubleField{ 
					name: "BiasStudyVarMultiplier"
					label: qsTr("Value")
					fieldWidth: 60 
					defaultValue: 6  
					decimals: 3
					}
				CheckBox
		{
			name: "biasRun";		label: qsTr("Run Chart");		checked: true
		}
		CheckBox
		{
			name: "biasTable";		label: qsTr("Bias and Capability Table");		checked: true
		}
			
		CheckBox
		{
			name: "biasTtest";		label: qsTr("One Sample T-Test");		checked: true

		}
		CheckBox
		{
			name: "biasHistogram";		label: qsTr("Histogram")
		}


	}
}