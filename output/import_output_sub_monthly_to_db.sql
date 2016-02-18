CREATE TABLE fw_strings (string CHAR);
.import C:/Users/radeca/Desktop/DF_full/WRB.Sufi2.SwatCup/output.sub fw_strings

DELETE FROM fw_strings WHERE SUBSTR(string,1,6) <> 'BIGSUB';

CREATE TABLE Sub AS
    SELECT
        CAST(SUBSTR(string,7,4) AS INTEGER) AS SUB
        ,CAST(SUBSTR(string,11,9) AS INTEGER) AS GIS
        ,CAST(SUBSTR(string,20,5) AS INTEGER) AS MON
        ,CAST(SUBSTR(string,25,10) AS REAL) AS AREA
        ,CAST(SUBSTR(string,35,10) AS REAL) AS PRCIP
        ,CAST(SUBSTR(string,45,10) AS REAL) AS SNOWMELT
        ,CAST(SUBSTR(string,55,10) AS REAL) AS PET
        ,CAST(SUBSTR(string,65,10) AS REAL) AS ET
        ,CAST(SUBSTR(string,75,10) AS REAL) AS SW
        ,CAST(SUBSTR(string,85,10) AS REAL) AS PERC
        ,CAST(SUBSTR(string,95,10) AS REAL) AS SURQ
        ,CAST(SUBSTR(string,105,10) AS REAL) AS GW_Q
        ,CAST(SUBSTR(string,115,10) AS REAL) AS WYLD
        ,CAST(SUBSTR(string,125,10) AS REAL) AS SYLD
        ,CAST(SUBSTR(string,135,10) AS REAL) AS ORGN
        ,CAST(SUBSTR(string,145,10) AS REAL) AS ORGP
        ,CAST(SUBSTR(string,155,10) AS REAL) AS NSURQ
        ,CAST(SUBSTR(string,165,10) AS REAL) AS SOLP
        ,CAST(SUBSTR(string,175,10) AS REAL) AS SEDP
		,CAST(SUBSTR(string,185,10) AS REAL) AS LATQ
        ,CAST(SUBSTR(string,195,10) AS REAL) AS LATNO3
        ,CAST(SUBSTR(string,205,10) AS REAL) AS GWNO3
        ,CAST(SUBSTR(string,215,11) AS REAL) AS CHOLA
        ,CAST(SUBSTR(string,226,10) AS REAL) AS CBODU
        ,CAST(SUBSTR(string,236,10) AS REAL) AS DOXQ
        ,CAST(SUBSTR(string,246,10) AS REAL) AS TNO3
        
    FROM fw_strings;

DROP TABLE fw_strings;
