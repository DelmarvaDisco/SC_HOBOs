# SC_HOBOs
Managing SC data from HOBOs

Data overview: HOBO (U24-001) SpC sensors deployed on rafts and hanging inside groundwater wells from Spring 2021 through Winter 2022

Point of contact: James Maze (jtmaze@vt.edu)

Column names:

	Timestamp: The measurement time in Eastern Time (ET)
	Site_ID: Lists the sensor location according to the Delmarva Disco’s Site Directory
	Low_range_uScm: Unadjusted conductivity values in microsiemens per centimeter
	SpC_low_range: Temperature adjusted specific conductivity values using formula below.
	Temp_C: Temperature in degrees celcius
	Flag: Designates data quality.
	Notes: Explains the nature of flagged data.

Temperature Compensation for Specific Conductance: Uses the following formula

SpC_low_range = Low_range_uScm/(1 - ((25 - Temp_C) * 0.021))

Flagging and QAQC: Characterization of flags is…

 0 = data good to use
 1 = data useful with caveats
 2 = data is unacceptable for most circumstances. 

Full documentation on GitHub: https://github.com/DelmarvaDisco/SC_HOBOs