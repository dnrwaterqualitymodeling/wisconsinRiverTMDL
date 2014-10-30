Crop calibration refers to the assessment of how well SWAT simulated crop yields match the USDA-National Agriculture Statistics Service
	reported yields. These assessments were carried out for Corn grain, corn silage, soybeans, and alfalfa. This effort was carried out
	as preliminary check of how well SWAT was modeling crop yields, and was not meant to be a primary path for calibration; that task 
	will be left largely to the SWAT-CUP software. Figures from this effort have been placed in 
		T:\Projects\Wisconsin_River\Model_Documents\TMDL_report\figures\calibration_validation_figures.
	This work was begun around 15 September, 2014 by Tom Beneke, Dave Evans and Aaron Ruesch. 
	
Initial efforts obtained simulated data from the Access database created by ArcSWAT after running swat and importing text files to the 
	output database. Alfalfa, which is scheduled to be harvested (usually) three times a year, the yields for the harvest months were 
	summed to provide an annual yield. These yields were noticed to be quite low. Upon further analysis, it was noted that the monthly
	output appeared to be odd, possibly SWAT or ArcSWAT was averaging daily yields over the entire month. Inspection of the daily output 
	showed that there were reported yields only for the specific days on which the alfalfa was harvested, all other days having zero yield.
	Inspection of the raw, text file output from SWAT, output.hru, revealed these same oddities in yield reporting, but also found that 
	SWAT produce an annual yield value that corresponded exactly with the sum of the two harvest days. All further analysis of SWAT output
	will likely focus on using this "raw" output from SWAT, instead of the output database from ArcSWAT.

NASS Info from James Johanson - Alfalfa Specialist at USDA-NASS, p: 202-690-8533 (nice guy)
	Reported Alfalfa yields in NASS are annual totals, that is including all cuttings. 
	Anything reported as Hay or Hay and Haylage (not excluding or stating Alfalfa) is a weighted average of all 
		types of hay or hay and haylage. Hay and Haylage is a weighted average assuming certain moisture amounts
		for hay and haylage, though are initially reported without any knowledge of moisture content.
		
		Haylage vs Green chop: the former is fermented in bags/silos/bunkers and the latter is fed fresh to animals

	For comparison with SWAT, the most similar NASS reporting to SWAT is 'Hay/Haylage, Alfalfa'. This value is an average
		total of alfalfa harvested in a county.
	
NASS moisture content:
	Corn silage is reported in weight without any assumed moisture content.
	Corn grain is reported as 15.5% moisture. 
	Soybeans are reported as 12.5% moisture.
		Both corn grain and soybeans are assumed to correct by NASS. 
	
Input/Output documentation for SWAT says that all yield and biomass output numbers are on a dry-weight
	basis. It is not specified if this means oven-dried or air dried. Currently, it is assumed that 'dry-weight'
	means without water, and so NASS figures are reduced in weight until they are only dry matter.
	
Alfalfa yields were seen to be too low, and so were investigated as to how the yields were being calculated (touched on
	earlier in this README). It was thought that perhaps three cuttings were too excessive, and so one rotation was 
	reduced to two to investigate the impact on yield. Investigation of the dailies showed what each cutting produced.
	Comparing the rotations with three cuttings to the rotation with two cuttings it was seen that in both cases,
	the first cutting, on 1 June was harvesting around 48 kg, presumably because the alfalfa had not started to grow.
	On comparing yields between 2 cuttings and 3 cutting rotations, it was found that the 3 cutting rotation was 
	obtaining higher yields.
	From this investigation it was concluded to use 3 cuttings per year (outside of the first year of alfalfa) and 
	move the first cutting back. The cutting dates will be those following Murphy & Almendinger, 2007: 25 June, 10 Aug, 10 Sept,
	other than the first year, which will be harvested once on 10 August. Previously, the three harvest dates were
	1 June, 15 July, and 30 August.
	
	Also of note: Alfalfa yields seem to move in pulses over the 12 year simulation period, with high yields follow by yields 
		every other year. This is believed to be caused by the way crop rotation pattern and the number of alfalfa cuttings.
		The following table shows each rotation (A = alfa, C = corn), the staggering over the 6yr rotation period, and the 
		number of cuttings for the alfalfa (given as a the number next to the A). It can be seen that there are 50% more cuttings
		every other year.
							yr:		1	2	3	4	5	6	 
							Rot 1:	A1	A3	A3	A3	Cg	Cs
							Rot 2:	A3	A3	Cg	Cs	A1	A3
							Rot 3:	Cg	Cs	A1	A3	A3	A3	
						numAlfaCuts	4	6	4	6	4	6
 
	Increase in soybeans 
 
 
