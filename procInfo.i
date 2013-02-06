/*------------------------------------------------------------------------
  File:               procInfo.I
  Description:        Temp-table definitions used by all programs.
  Author:             Lic. Edgar Medrano Perez
  Created:            2005.06.18
  Company:            
  Notes:
------------------------------------------------------------------------*/

&IF DEFINED(PROCINFO_I_) = 0 &THEN
&GLOBAL-DEFINE PROCINFO_I_ TRUE

DEFINE TEMP-TABLE ttProcedure
  FIELD iProcId  AS INTEGER    COLUMN-LABEL "Pid"
  FIELD cName    AS CHARACTER  FORMAT "X(64)" COLUMN-LABEL "Name"
  FIELD cPath    AS CHARACTER  FORMAT "X(256)" COLUMN-LABEL "Path"
  INDEX piProcedure IS PRIMARY iProcId.

DEFINE TEMP-TABLE ttMethod
  FIELD iProcId   AS INTEGER   COLUMN-LABEL "Pid"
  FIELD cName     AS CHARACTER FORMAT "X(64)" COLUMN-LABEL "Procedure"
  FIELD lEnabled  AS LOGICAL   INITIAL TRUE COLUMN-LABEL "Enable" VIEW-AS TOGGLE-BOX
  INDEX piMethod IS PRIMARY lEnabled iProcId cName.

DEFINE TEMP-TABLE ttParam
  FIELD iProcId       AS INTEGER   COLUMN-LABEL "Pid"
  FIELD cMethodName   AS CHARACTER FORMAT "X(64)" COLUMN-LABEL "Procedure"
  FIELD cName         AS CHARACTER FORMAT "X(64)" COLUMN-LABEL "Parameter"
  FIELD iSeq          AS INTEGER   COLUMN-LABEL "Seq"
  FIELD cDataType     AS CHARACTER COLUMN-LABEL "Type"
  FIELD cDirection    AS CHARACTER COLUMN-LABEL "Dir"
  INDEX piParam IS PRIMARY iProcId cMethodName iSeq.

DEFINE TEMP-TABLE ttTable
  FIELD iProcId AS INTEGER   COLUMN-LABEL "Pid"
  FIELD cName   AS CHARACTER FORMAT "X(64)" COLUMN-LABEL "Table"
  INDEX piTable IS PRIMARY iProcId cName.

DEFINE TEMP-TABLE ttField
  FIELD iProcId       AS INTEGER   COLUMN-LABEL "Pid"
  FIELD cTableName    AS CHARACTER COLUMN-LABEL "Table"
  FIELD cName         AS CHARACTER COLUMN-LABEL "Field"
  FIELD iSeq          AS INTEGER   COLUMN-LABEL "Seq"
  FIELD cDataType     AS CHARACTER COLUMN-LABEL "Type"
  INDEX piField IS PRIMARY iProcId cTableName iSeq.

&ENDIF /*PROCINFO_I_*/
