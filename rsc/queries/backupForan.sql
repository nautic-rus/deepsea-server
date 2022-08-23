CREATE OR REPLACE PROCEDURE SYS.NAUTIC_BACKUP AS
    h1 NUMBER;               -- Data Pump job handle
    job_state VARCHAR2(30);  -- To keep track of job state
    --js ku$_JobStatus;        -- The job status from get_status
    sts ku$_Status;          -- The status object returned by get_status
    datestr VARCHAR2(30);
    userName VARCHAR2(30);
    projName VARCHAR2(30);
    projVer VARCHAR2(30);
    dmpFileName VARCHAR2(50);
    logFileName VARCHAR2(50);
    dumpDir VARCHAR2(30);
BEGIN
    FOR proj IN (select proj_id,version from FORANADM.FORAN_PROJECTS where proj_id in (&projects))
        LOOP
            datestr:=(TO_CHAR (SYSDATE, 'YYYY-MM-DD-HH24-MI-SS'));
            projName:=proj.proj_id;
            userName:='C'|| projName;
            projVer:=proj.version;
            dmpFileName:=LOWER(projName||'_'||projVer||'_'||datestr||'.FDP');
            logFileName:=LOWER(projName||'_'||projVer||'_'||datestr||'.LOG');
            dumpDir:='NAUTIC_PUMP_DIR';

            h1 := DBMS_DATAPUMP.OPEN('EXPORT','SCHEMA',NULL,'&jobname','11.2');
            DBMS_DATAPUMP.ADD_FILE(h1,dmpFileName,dumpDir);
            DBMS_DATAPUMP.ADD_FILE(handle => h1, filename => logFileName, directory => dumpDir, filetype => DBMS_DATAPUMP.KU$_FILE_TYPE_LOG_FILE);
            DBMS_DATAPUMP.METADATA_FILTER(h1,'SCHEMA_EXPR','IN ('||''''|| userName ||''''||')');
            DBMS_DATAPUMP.START_JOB(h1);

            job_state := 'UNDEFINED';
            while (job_state != 'COMPLETED') and (job_state != 'STOPPED') loop
                    dbms_datapump.get_status(h1,
                                             dbms_datapump.ku$_status_job_error +
                                             dbms_datapump.ku$_status_job_status +
                                             dbms_datapump.ku$_status_wip,-1,job_state,sts);
                    --js := sts.job_status;
                end loop;
            dbms_datapump.detach(h1);
        END LOOP;
END NAUTIC_BACKUP;
