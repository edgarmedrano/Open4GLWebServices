{o4glws.i}

DEFINE VARIABLE vchrInput        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vchrOutput       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vhndRequestDoc   AS HANDLE     NO-UNDO. 
DEFINE VARIABLE vhndResponseDoc  AS HANDLE     NO-UNDO. 
DEFINE STREAM strFile.

IF NUM-ENTRIES(SESSION:PARAMETER) <> 2 THEN             
DO:
  MESSAGE "mpro -p " THIS-PROCEDURE:FILE-NAME "-param ""path_to_input.xml,path_to_ouput.xml""".
END.
ELSE
DO:
  ASSIGN
    vchrInput = ENTRY(1,SESSION:PARAMETER)
    vchrOutput = ENTRY(2,SESSION:PARAMETER).
	
  CREATE X-DOCUMENT vhndRequestDoc. 
  vhndRequestDoc:LOAD("FILE",vchrInput,FALSE).
	
  RUN PROCESS-REQUEST(vhndRequestDoc,OUTPUT vhndResponseDoc).
	
  OUTPUT STREAM strFile TO VALUE(vchrOutput).
  vhndResponseDoc:SAVE("STREAM", "strFile").
  DELETE OBJECT vhndResponseDoc. 
  OUTPUT STREAM strFile CLOSE. 
END.

