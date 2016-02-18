update mgt1
set CN2=31
where LANDUSE in ('CRRT', 'PAST') and (INT(SOIL) >= 100 and INT(SOIL) < 200);

update mgt1
set CN2=59
where LANDUSE in ('CRRT', 'PAST') and (INT(SOIL) >= 200 and INT(SOIL) < 300);

update mgt1
set CN2=72
where LANDUSE in ('CRRT', 'PAST') and (INT(SOIL) >= 300 and INT(SOIL) < 400);

update mgt1
set CN2=79
where LANDUSE in ('CRRT', 'PAST') and (INT(SOIL) >= 400 and INT(SOIL) < 500);

update mgt1
set CN2=72
where LANDUSE in ('CRRT', 'PAST') and INT(SOIL) = 99;
