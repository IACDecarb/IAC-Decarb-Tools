# INDUSTRIAL DECARBONIZATION TOOLKIT

## Developed by: Berkeley Lab; UC Davis 

Tools in this package:

## 1. Facility Sankey Tool: [apps/fcft](apps/fcft)

Tool Description:
  Facility Sankey Tool assists facilities and entities in visualizing their Energy and CO2e flows for accounting and decarbonization purposes. To do so, the suite offers two tools – an MS Excel-based input workbook and a web-based visualization tool. This workbook serves as the input sheet, utilizing the user-provided process and equipment-level energy consumption data to calculate the corresponding energy and emissions. Whereas, the web-based tool utilizes the data from this workbook to generate a Sankey diagram.
  
  Suggested Citation: Khan, Ovais, Ali Qamar, Muhammad, Karki, Unique, Sinha, Vedant,  Kissock, John & Rao, Prakash. (2023, October 24). Facility CO2 Flow Tool (FCFT) v0.9. [Computer software]. https://github.com/IACDecarb/IAC-Decarb-Tools. https://doi.org/10.11578/dc.20240813.3

## 2. Levelized Cost Curve Tool: [apps/lcac](apps/lcac)

   Tool Description:
The Levelized Cost Curve (LCC) tool facilitates techno-economic calculations for various energy efficiency and decarbonization technologies. It comprises two tools to accomplish this: an input workbook based on MS Excel and a web-based output tool. This workbook acts as the input sheet, using the user-provided decarbonization measures and their impacts on lifetime energy consumption, energy costs, and CO2 emissions to compute the LCCE (Levelized Cost of Conserved Energy) and LCAC (Levelized Cost of Avoided CO2). On the other hand, the web-based tool employs data from this workbook to generate an LC curve. This graphical representation illustrates the economic comparison of all decarbonization measures for purposes such as implementation prioritization.

  Suggested Citation: Ali Qamar, Muhammad, Khan, Ovais, Karki, Unique, Sinha, Vedant,  Kissock, John & Rao, Prakash. (2023, October 24). Facility CO2 Flow Tool (FCFT) v0.9. [Computer software]. https://github.com/IACDecarb/IAC-Decarb-Tools. https://doi.org/10.11578/dc.20240813.3

## 3. Pinch Heat Integration Tool: [apps/pit](apps/pit)
  Tool Description:
  Pinch Analysis is a methodology used to optimize processes by maximizing heat recovery while minimizing the need for external heating and cooling utilities. It also provides insights into the optimal placement and sizing of heat pumps between process streams. The Pinch Heat Integration Tool (PIT) supports this optimization by generating pinch diagrams, including the Shifted Composite Curve and Grand Composite Curve. Beyond visualizing these curves, PIT also enables the simulation of heat exchangers and heat pumps between plant streams.

## 4. Electric Load Planning Tool: [apps/elpt](apps/elpt)
  The Electric Load Planning Tool (ELPT) is an R-Shiny-based online web application with a user-provided Excel input that assists facilities in understanding the economic and environmental impacts of their electricity consumption. By analyzing electricity use, costs, and grid CO2e emissions, ELPT helps facilities identify potential savings through load management strategies such as load shifting, shedding, and planning. These strategies involve altering the timing of electricity use to periods when electricity is both cheaper and cleaner.
ELPT considers the complexities of Time-of-Use (TOU) retail electricity tariffs and hourly emissions factors, which vary by location and time of day. This detailed analysis allows facilities to better understand and estimate the cost and emissions reductions that can be achieved through load planning. Users can input specific details about their facility’s load profile, location, year of analysis, and electricity billing tariff to receive customized insights. The primary output of the tool is visual representations of the cost and GHG impacts associated with a facility’s electricity consumption and impacts from strategies to shift, shed, or add loads. These visuals make it easier for users to interpret the data and understand the potential benefits of adjusting their electricity usage patterns. By aligning electricity consumption with periods of surplus renewable generation, facilities can achieve both cost savings and reductions in Scope 2 CO2e emissions.

