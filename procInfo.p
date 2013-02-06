/*------------------------------------------------------------------------
  File:               procInfo.P
  Description:        Extracts the definitions of internal procedures, 
                      parameters and temp-tables. You can supply the 
                      path of a compilable source file or a .r file.
  Input Parameters:   ichProgramFile   Path to the procedure.
  Output Parameters:  ochResult        Error message or "" if there 
                                       isn't any
                      ttMethod         Internal procedures and Functions.                
                      ttParam          Procedure Parameters.                                         
                      ttTable          Temp-tables used by the procedures.                  
                      ttField          Temp-tables Fields.
  Author:             Lic. Edgar Medrano Pérez 
                      edgarmedrano@gmail.com
  Created:            2005.06.25
  Company:            
  Notes:              
------------------------------------------------------------------------*/
{o4glws/procInfo.i}

DEFINE INPUT  PARAMETER ichProgressFile  AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER ochResult        AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttMethod.
DEFINE OUTPUT PARAMETER TABLE FOR ttParam.
DEFINE OUTPUT PARAMETER TABLE FOR ttTable.
DEFINE OUTPUT PARAMETER TABLE FOR ttField.

DEFINE VARIABLE vchData             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vchFlag             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vchParam            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vchRCodeFile        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vchResult           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vchScratch          AS CHARACTER    NO-UNDO.

DEFINE VARIABLE vintSignature       AS INTEGER    NO-UNDO.
DEFINE VARIABLE vintLength          AS INTEGER    NO-UNDO.
DEFINE VARIABLE vintPosition        AS INTEGER    NO-UNDO.
DEFINE VARIABLE vchrBuffer          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE memBuffer           AS MEMPTR     NO-UNDO.

DEFINE VARIABLE vhaTemp             AS HANDLE       NO-UNDO.

DEFINE VARIABLE vinCntr             AS INTEGER      NO-UNDO.

DEFINE STREAM istInput.

SET-SIZE(memBuffer) = 68.
INPUT STREAM istInput FROM VALUE(ichProgressFile) BINARY NO-CONVERT.
IMPORT STREAM istInput memBuffer.

ASSIGN vintSignature = GET-LONG(memBuffer,1).

IF vintSignature = 164875862 OR vintSignature = 1456395017 THEN
DO:
    ASSIGN vchRCodeFile = ichProgressFile.
END.
ELSE
DO:
    ASSIGN vintSignature = 0.

    FILE-INFO:FILE-NAME = ichProgressFile.
    INPUT STREAM istInput CLOSE.

    ASSIGN vchRCodeFile = ENTRY(NUM-ENTRIES(ichProgressFile,"~\"),ichProgressFile,"~\")
      vchRCodeFile = ENTRY(NUM-ENTRIES(vchRCodeFile,"/"),vchRCodeFile,"/")
      vchRCodeFile = 
        IF R-INDEX(vchRCodeFile,".") > 0 THEN
          SUBSTRING(vchRCodeFile,1,R-INDEX(vchRCodeFile,".") - 1)
        ELSE
          vchRCodeFile
      vchRCodeFile    = SUBSTITUTE("&1&2.r", SESSION:TEMP-DIR,vchRCodeFile).

    COMPILE VALUE(ichProgressFile) SAVE INTO VALUE(SESSION:TEMP-DIR) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
    DO:
      DO vinCntr = 1 TO ERROR-STATUS:NUM-MESSAGES:
        ASSIGN ochResult = 
          ochResult 
          + (IF ochResult <> "" THEN "," ELSE "") 
          + ERROR-STATUS:GET-MESSAGE(vinCntr).
      END.
      ASSIGN ochResult = 
        "An error ocurred while compilating " 
        + ichProgressFile 
        + ": "
        + ochResult.
      RETURN.
    END.

    SET-SIZE(memBuffer) = 68.
    INPUT STREAM istInput FROM VALUE(vchRCodeFile) BINARY NO-CONVERT.
    IMPORT STREAM istInput memBuffer.
END.

ASSIGN vintLength = GET-LONG(memBuffer,65).
SET-SIZE(memBuffer) = 0.

FILE-INFO:FILE-NAME = vchRCodeFile.
SET-SIZE(memBuffer) = FILE-INFO:FILE-SIZE - vintLength.
vintPosition = 1.
IMPORT STREAM istInput memBuffer.
INPUT STREAM istInput CLOSE.

IF vintSignature = 0 THEN
DO:
  OS-DELETE VALUE(vchRCodeFile).
END.

ASSIGN vchData = "this will be erased".
DO WHILE vchData <> "":
    vchData = GET-STRING(memBuffer,vintPosition).
    vintPosition = vintPosition + LENGTH(vchData) + 1.
    vchFlag = ENTRY(1, vchData, " ").

    IF vchFlag = "MAIN"
    OR vchFlag = "PROCEDURE"
    OR vchFlag = "FUNCTION" THEN
    DO: 

        CREATE ttMethod.

        ASSIGN ttMethod.cName   = IF vchFlag = "MAIN" THEN "" ELSE ENTRY(2,ENTRY(1, vchData), " ").

        IF vchFlag = "FUNCTION" THEN
        DO:
            CREATE ttParam.
            ASSIGN ttParam.iSeq = 999
                   ttParam.cMethodName  = ttMethod.cName
                   ttParam.cName        = "returnValue"
                   ttParam.cDirection   = "RETURN"
                   ttParam.cDataType    = ENTRY(2, vchData).
        END. /** function **/

        DO vinCntr = 3 TO NUM-ENTRIES(vchData):
            vchScratch = ENTRY(vinCntr, vchData).
            IF vchScratch <> "" THEN
            DO:
                CREATE ttParam.
                ASSIGN ttParam.iSeq         = vinCntr - 2
                       ttParam.cMethodName  = ttMethod.cName
                       ttParam.cDataType    = ENTRY(3, vchScratch, " ")
                       ttParam.cName        = ENTRY(2, vchScratch, " ")
                       ttParam.cDirection   = ENTRY(1, vchScratch, " ").                    
            END. /** vchscratch <> "" **/
        END. /** vinCntr **/
    END. /** procedure, function **/

    IF vchFlag = "TEMP-TABLE" THEN
    DO:
        FIND FIRST ttParam WHERE ttParam.cName = ENTRY(2,ENTRY(1, vchData), " ") NO-ERROR.

        IF AVAILABLE ttParam THEN
        DO:
            CREATE ttTable.
            ASSIGN ttTable.cName    = ttParam.cName.

            DO vinCntr = 2 TO NUM-ENTRIES(vchData):
                vchScratch = ENTRY(vinCntr, vchData).
                CREATE ttField.
                ASSIGN ttField.cTableName   = ttTable.cName
                       ttField.cName        = ENTRY(1, vchScratch, " ")
                       ttField.iSeq         = vinCntr - 1
                       ttField.cDataType    = CAPS(ENTRY(2, vchScratch, " "))
                       .
            END. /** do vinCntr **/
        END. /** available ttParam **/                                      
    END. /** temp-table **/
END. /** repeat **/

