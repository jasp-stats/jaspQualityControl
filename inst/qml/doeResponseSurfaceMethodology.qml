
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
	columns:									2

	VariablesForm
	{
		AvailableVariablesList { name: "rsmVariablesList" }
		AssignedVariablesList
		{
			name: "rsmVariables"
			title: qsTr("Predictors [Location in coded format]")
			suggestedColumns:   ["scale", "ordinal", "nominal"]

			rowComponent: Row
			{
				DoubleField {name: "Point_P"; negativeValues: true}
			}


		}
		AssignedVariablesList  { name: "rsmResponseVariables";	title: qsTr("Response");  suggestedColumns:   ["scale", "ordinal", "nominal"]}
		AssignedVariablesList  { name: "rsmBlocks";	            title: qsTr("Blocks (optional)");    suggestedColumns:   ["scale", "ordinal", "nominal"]; singleVariable: true}
	}


	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "components"; title: qsTr("Components"); source: "rsmVariables" }
		ModelTermsList
		{
			listViewType			: JASP.Interaction
			rowComponentTitle		: qsTr("Term Type")
			rowComponent			: DropDown
			{
				name: "TermType"
				label: ""
				values: [
					{ label: qsTr("FO + PQ"), value: "fopq"},
					{ label: qsTr(""), value: "nothing"},
					{ label: qsTr("FO"), value: "fo"},
					
					
				]
			}
		}
	}
	
	Group 
	{
		title: qsTr("Response Surface Summaries")
		columns: 3
		
		CheckBox 
		{
			name:                       "coef"; label:                  qsTr("Coefficient Table")
			
		}
		
		
		CheckBox
		{
			name:                       "anova";label:                  qsTr("ANOVA Table")
		}
		
		
		
		CheckBox
		{
			name:                       "res";  label:                  qsTr("Residual Histogram")
		}
		
		CheckBox
		{
			name:                       "resNorm";label:                 qsTr("Normal Residual Plot")
		}
		
		CheckBox
		{
			name:                       "ResFitted";label:                 qsTr("Residual vs. Fitted Plot")
		}

		
		CheckBox
		{
			name:                       "pareto";label:                 qsTr("Pareto Plot of Standardized Effects")
		}
	}
	
	
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			name:  "rsmVariables2";	    source:"rsmVariables"

		}




		AssignedPairsVariablesList
		{	name:  "pairs";				suggestedColumns: ["scale", "ordinal", "nominal"] }


	} 
	

	Group
	{
		title: qsTr("Contour Plot Options")
		
		CheckBox
		{
			name:                      "contour";label:   qsTr("Contour Surface")
			columns: 2
			CheckBox
			{
				name:                       "cplot"
				label:                      qsTr("Only show 2D plot")
				id:                         cplot
			}

			CheckBox
			{
				name:                       "coded"
				label:                      qsTr("Show analysis and graphs in coded form")
				enabled:					cplot.checked
			}

			CheckBox
			{
				name:                       "legend"
				label:                      qsTr("Show legend next to graph")
				enabled:					!cplot.checked
			}
			DropDown
			{
				name:                       "divide"
				label:                      qsTr("Divide response surface into N parts")
				values:                     [2,3,4,5,6,7]
				enabled:					!cplot.checked
			}

			Slider
			{
				name:                       "phi"
				label:                      qsTr("Rotating angle (vertical plane)")
				value:                      0
				enabled:					!cplot.checked


			}

			Slider
			{
				name:                       "theta"
				label:                      qsTr("Rotating angle (horizontal plane)")
				value:                      0.5
				vertical:                   false
				enabled:					!cplot.checked
			}

		}
		
	
		
		
		
	}
	
	
	Section
	{
		title: qsTr("Desirability")
		CheckBox 
		{
			name: "desirability";label: "Calculate Desirability"
			
		}
		VariablesForm 
		{
			AvailableVariablesList 
			{ 
				name: "rsmDesirability";       label: qsTr("Response Variable List");    source: "rsmResponseVariables" 
				
			}
			AssignedVariablesList  
			{
				name: "rsmMin";	            title: qsTr("Minimum [Min/Max]");     suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min"; negativeValues: true}
					DoubleField {name: "Point_Max"; negativeValues: true}
				}

			}
			AssignedVariablesList  
			{
				name: "rsmMax";	            title: qsTr("Maximum [Min/Max]");     suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_1"; negativeValues: true}
					DoubleField {name: "Point_Max_1"; negativeValues: true}
				}

			}
			AssignedVariablesList  
			{
				name: "rsmTar";	            title: qsTr("Target [Min/Target/Max]");suggestedColumns:   ["scale", "ordinal", "nominal"]
				rowComponent: Row
				{
					DoubleField {name: "Point_Min_2"; negativeValues: true}
					DoubleField {name: "Point_Tar_2"; negativeValues: true}
					DoubleField {name: "Point_Max_2"; negativeValues: true}
				}

			}
		
		}
		
		
		
	
	
	}
		
	

	



	Section
	{
		title: 							qsTr("Box designs")

		CheckBox
		{
			name:                       "showDesign";label:            qsTr("Central composite design")
			IntegerField
			{
				name:                       "factorResponse"
				label:                      "Number of factors"
				defaultValue:               2
				min:                        2
				max:                        50
			}

			IntegerField
			{
				name:						"responseSurfaceCentre"
				label:						qsTr("Number of centre points")
				defaultValue:				3
				min:						1
				max:						50
			}


			IntegerField
			{
				name:						"responseSurfaceReplicationStar"
				label:						qsTr("Number of replicates")
				defaultValue:				3
				min:						1
				max:						50
			}



		}



	}


}
