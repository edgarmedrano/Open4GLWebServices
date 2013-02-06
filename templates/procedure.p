{o4glws.i}

DEFINE INPUT  PARAMETER ipchrInput  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipchrOutput AS CHARACTER  NO-UNDO.

DEFINE VARIABLE vhndRequestDoc   AS HANDLE    NO-UNDO. 
DEFINE VARIABLE vhndResponseDoc  AS HANDLE    NO-UNDO. 
DEFINE STREAM strFile.

CREATE X-DOCUMENT vhndRequestDoc. 
vhndRequestDoc:LOAD("FILE",ipchrInput,FALSE).

RUN PROCESS-REQUEST(vhndRequestDoc,OUTPUT vhndResponseDoc).

OUTPUT STREAM strFile TO VALUE(ipchrOutput).
vhndResponseDoc:SAVE("STREAM", "strFile").
DELETE OBJECT vhndResponseDoc. 
OUTPUT STREAM strFile CLOSE. 
