/*------------------------------------------------------------------------
  File:               o4glws.I
  Description:        Common functions used by the webservices adapters.
  Author:             Lic. Edgar Medrano Perez
                      edgarmedrano@gmail.com
  Created:            2005.06.18
  Company:            Open 4GL webservices project
                      http://o4glws.sourceforge.net
  Notes:              
------------------------------------------------------------------------*/

&IF DEFINED(O4GLWS_I_) = 0 &THEN
&GLOBAL-DEFINE O4GLWS_I_ TRUE

DEFINE VARIABLE vintO4GLWS_ID AS INTEGER    NO-UNDO INITIAL -1.

FUNCTION getNextId RETURNS CHARACTER ():
  vintO4GLWS_ID = vintO4GLWS_ID + 1.
  RETURN "id" + STRING(vintO4GLWS_ID). 
END FUNCTION.       
     

/* Find the child node with the given name and id */
FUNCTION findChildWithId RETURNS HANDLE (INPUT iphndParent AS HANDLE, INPUT ipchrName AS CHARACTER, INPUT ipchrId AS CHARACTER):
  DEFINE VARIABLE vintChild AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vhndChild AS HANDLE     NO-UNDO. 
  DEFINE VARIABLE vchrName  AS CHARACTER  NO-UNDO.

  CREATE X-NODEREF vhndChild.

  DO vintChild = 1 TO iphndParent:NUM-CHILDREN:
    IF iphndParent:GET-CHILD(vhndChild,vintChild) THEN
    DO:
      IF vhndChild:SUBTYPE = "ELEMENT" THEN
      DO:
        IF vhndChild:NAME = ipchrName OR vhndChild:LOCAL-NAME = ipchrName THEN
        DO:
          IF ipchrId = ? THEN
            RETURN vhndChild.

          IF vhndChild:GET-ATTRIBUTE("id") = ipchrId THEN
            RETURN vhndChild.
        END.

        ASSIGN vchrName = 
          IF NUM-ENTRIES(vhndChild:NAME,":") = 2 THEN 
            ENTRY(2,vhndChild:NAME,":") 
          ELSE 
            vhndChild:NAME. 

        IF vchrName = ipchrName THEN
        DO:
          IF ipchrId = ? THEN
            RETURN vhndChild.

          IF vhndChild:GET-ATTRIBUTE("id") = ipchrId THEN
            RETURN vhndChild.
        END.        
      END.
    END.
  END.

  RETURN ?.
END FUNCTION.

/* Find the child node with the given name */
FUNCTION findChild RETURNS HANDLE (INPUT iphndParent AS HANDLE, INPUT ipchrName AS CHARACTER):
  RETURN findChildWithId(iphndParent,ipchrName,?).
END FUNCTION.

/* Creates a child node with the given name */
FUNCTION createChild RETURNS HANDLE (INPUT iphndParent AS HANDLE, INPUT ipchrName AS CHARACTER, INPUT ipchrNameSpace AS CHARACTER):
    DEFINE VARIABLE vhndChild AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndDoc   AS HANDLE    NO-UNDO. 

    IF ipchrNameSpace <> "" AND ipchrNameSpace <> ? THEN
    DO:
      ipchrName = ipchrNameSpace + ":" + ipchrName.
    END.

    CREATE X-NODEREF vhndChild.
    vhndDoc = IF iphndParent:TYPE = "X-DOCUMENT" THEN iphndParent ELSE iphndParent:OWNER-DOCUMENT.
    vhndDoc:CREATE-NODE(vhndChild,ipchrName,"ELEMENT").
    iphndParent:APPEND-CHILD(vhndChild).

    RETURN vhndChild.
END FUNCTION.

/* The next functions read the input parameters from the request node */
FUNCTION getInCharacter RETURNS CHARACTER (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER):
    DEFINE VARIABLE vhndNode AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndChild AS HANDLE    NO-UNDO. 

    CREATE X-NODEREF vhndChild.

    vhndNode = findChild(iphndMessage,ipchrParamName).
    IF VALID-HANDLE(vhndNode) THEN
    DO:
      IF vhndNode:NUM-CHILDREN = 1 THEN
      DO:
        vhndNode:GET-CHILD(vhndChild,1).
        RETURN vhndChild:NODE-VALUE.
      END.
    END.

    RETURN "".
END FUNCTION.

FUNCTION getInInteger RETURNS INTEGER (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER):
    RETURN INTEGER(getInCharacter(iphndMessage,ipchrParamName)).
END FUNCTION.

FUNCTION getInDecimal RETURNS DECIMAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER):
    RETURN DECIMAL(getInCharacter(iphndMessage,ipchrParamName)).
END FUNCTION.

FUNCTION getInLogical RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER):
    RETURN (getInCharacter(iphndMessage,ipchrParamName) = "true").
END FUNCTION.

FUNCTION getInDate RETURNS DATE (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER):
    DEFINE VARIABLE vchrDate AS CHARACTER  NO-UNDO.

    ASSIGN vchrDate = getInCharacter(iphndMessage,ipchrParamName).
    IF NUM-ENTRIES(vchrDate,"-") >= 3 THEN
    DO:
      RETURN DATE(INT(ENTRY(2,vchrDate,"-")),INT(ENTRY(3,vchrDate,"-")),INT(ENTRY(1,vchrDate,"-"))).
    END.

    RETURN ?.
END FUNCTION.

FUNCTION getInTable RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT iphndBuffer AS HANDLE):
    DEFINE VARIABLE vhndQuery       AS HANDLE     NO-UNDO.
    DEFINE VARIABLE vhndTableNode   AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndBodyNode    AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndRowNode     AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndFieldNode   AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndValueNode   AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndBufferField AS HANDLE     NO-UNDO.
    DEFINE VARIABLE vintI           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE vintJ           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE vchrId          AS CHARACTER  NO-UNDO INITIAL "".
    DEFINE VARIABLE vchrAux         AS CHARACTER  NO-UNDO.

    vhndTableNode = findChild(iphndMessage,ipchrParamName).
    IF VALID-HANDLE(vhndTableNode) THEN
    DO:
        CREATE X-NODEREF vhndBodyNode.
        CREATE X-NODEREF vhndRowNode.
        CREATE X-NODEREF vhndFieldNode.
        CREATE X-NODEREF vhndValueNode.

        iphndMessage:GET-PARENT(vhndBodyNode).

        /*Create row*/
        DO vintI = 1 TO vhndTableNode:NUM-CHILDREN:
           vhndTableNode:GET-CHILD(vhndRowNode,vintI).
           
           IF vhndRowNode:SUBTYPE = "ELEMENT" THEN
           DO:
             IF vhndRowNode:NAME = "item" OR vhndRowNode:LOCAL-NAME = "item" THEN
             DO:
               vchrId = vhndRowNode:GET-ATTRIBUTE("href").
               IF vchrId <> "" THEN
               DO:
                 IF NUM-ENTRIES(vchrId,"#") >= 2 THEN
                 DO:
                   vchrId = ENTRY(2,vchrId,"#").
                 END.

                 vhndRowNode = findChildWithId(vhndBodyNode,"multiRef",vchrId).
               END.
             END.

             iphndBuffer:BUFFER-CREATE().
             DO vintJ = 1 TO vhndRowNode:NUM-CHILDREN:
                 vhndRowNode:GET-CHILD(vhndFieldNode,vintJ).
                 IF vhndFieldNode:SUBTYPE = "ELEMENT" THEN
                 DO:
                   vhndBufferField = iphndBuffer:BUFFER-FIELD(
                       IF vhndFieldNode:LOCAL-NAME <> "" THEN vhndFieldNode:LOCAL-NAME 
                       ELSE vhndFieldNode:NAME).
                   IF vhndBufferField <> ? THEN 
                   DO:
                     IF vhndFieldNode:NUM-CHILDREN = 1 THEN
                     DO:
                       vhndFieldNode:GET-CHILD(vhndValueNode,1).
                       CASE (vhndBufferField:DATA-TYPE):
                           /*
                            WHEN "CHARACTER" 
                         OR WHEN "COM-HANDLE"
                         OR WHEN "HANDLE"
                         OR WHEN "MEMPTR"
                         OR WHEN "RAW"
                         OR WHEN "ROWID"
                         OR WHEN "WIDGET-HANDLE" THEN
                           */
                         WHEN "DATE" THEN
                         DO:
                           vchrAux = vhndValueNode:NODE-VALUE.
                           vhndBufferField:BUFFER-VALUE = STRING(DATE(INTEGER(ENTRY(2,vchrAux,"-"))
                                                                     ,INTEGER(ENTRY(3,vchrAux,"-"))
                                                                     ,INTEGER(ENTRY(1,vchrAux,"-")))).
                         END.
                         WHEN "LOGICAL" THEN
                         DO:
                           vhndBufferField:BUFFER-VALUE = STRING(vhndValueNode:NODE-VALUE = "true").
                         END.
                         OTHERWISE
                         DO:
                           vhndBufferField:BUFFER-VALUE = vhndValueNode:NODE-VALUE.
                         END.
                       END CASE. /** ichProgressDataType **/
                     END.
                   END.
                 END.
             END.
           END.
        END.
    END.

    RETURN FALSE.
END FUNCTION.

/* The next functions write the output parameters to the response node */
FUNCTION setOutCharacter RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT ipchrParamValue AS CHARACTER):
   DEFINE VARIABLE vhndNode AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE vhndChild AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE vhndDoc   AS HANDLE    NO-UNDO. 
   
   vhndNode = findChild(iphndMessage,ipchrParamName).
   IF NOT VALID-HANDLE(vhndNode) THEN
   DO:
     vhndNode = createChild(iphndMessage,ipchrParamName,"").
     CREATE X-NODEREF vhndChild.
     vhndDoc = vhndNode:OWNER-DOCUMENT.
     vhndDoc:CREATE-NODE(vhndChild,"","TEXT").
     vhndNode:APPEND-CHILD(vhndChild).
   END.
   ELSE
   DO:
       IF vhndNode:NUM-CHILDREN = 1 THEN
       DO:
         vhndNode:GET-CHILD(vhndChild,1).
       END.
   END.

   vhndChild:NODE-VALUE = ipchrParamValue.

   RETURN TRUE.
END FUNCTION.

FUNCTION setOutInteger RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT ipintParamValue AS INTEGER):
    RETURN setOutCharacter(iphndMessage,ipchrParamName,STRING(ipintParamValue)).
END FUNCTION.

FUNCTION setOutDecimal RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT ipdecParamValue AS DECIMAL):
    RETURN setOutCharacter(iphndMessage,ipchrParamName,STRING(ipdecParamValue)).
END FUNCTION.

FUNCTION setOutLogical RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT iplogParamValue AS LOGICAL):
    RETURN setOutCharacter(iphndMessage,ipchrParamName,TRIM(STRING(iplogParamValue,"true/false"))).
END FUNCTION.

FUNCTION setOutDate RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT ipdatParamValue AS DATE):
    RETURN setOutCharacter(iphndMessage,ipchrParamName,
                           STRING(YEAR(ipdatParamValue),"9999")
                           + "-" 
                           + STRING(MONTH(ipdatParamValue),"99")
                           + "-"
                           + STRING(DAY(ipdatParamValue),"99")).
END FUNCTION.

FUNCTION setOutTable RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT iphndBuffer AS HANDLE):
   DEFINE VARIABLE vhndQuery       AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vhndTableNode   AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndDoc         AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndBodyNode    AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vlogBody        AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vchrId          AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vhndItemNode    AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndRowNode     AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndFieldNode   AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndValueNode   AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndBufferField AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vintI           AS INTEGER    NO-UNDO.
   DEFINE VARIABLE vintRows        AS INTEGER    NO-UNDO INITIAL 0.
   
   vhndTableNode = createChild(iphndMessage,ipchrParamName,"").

   CREATE QUERY vhndQuery.

   /** Set query to the buffer we just created **/
   if not vhndQuery:set-buffers(iphndBuffer) then
   do:
       message "Set buff failed " view-as alert-box.
       leave.
   end.

   /** Create the query predicate **/
   if not (vhndQuery:query-prepare("for each " + iphndBuffer:name )) then
   do:
       message "query prepare failed " view-as alert-box.
       leave.
   end.

   /** Open the query **/
   if not vhndQuery:query-open() then
   do:
       message "query open failed " view-as alert-box.
       leave.
   end.

   /* Get X-DOCUMENT handler */
   ASSIGN vhndDoc = iphndMessage:OWNER-DOCUMENT.

   DEFINE VARIABLE vlogX AS LOGICAL    NO-UNDO.

   CREATE X-NODEREF vhndBodyNode.
   vlogBody = iphndMessage:GET-PARENT(vhndBodyNode).
   IF vlogBody THEN
   DO: 
     IF vhndBodyNode:NAME = "Body" 
       OR (NUM-ENTRIES(vhndBodyNode:NAME,":") = 2 
         AND ENTRY(2,vhndBodyNode:NAME,":") = "Body") THEN
       vlogBody = TRUE.
     ELSE 
       vlogBody = FALSE.
   END.
  
   /* Dump the data */
   repeatLoop:
   REPEAT:
       /** Get next record from query **/
       vhndQuery:GET-NEXT().
       IF vhndQuery:QUERY-OFF-END THEN LEAVE repeatLoop.
         
       vintRows = vintRows + 1. 

       /** Create parent node in XML **/
       IF vlogBody THEN
       DO:
           vchrId = getNextId().
           CREATE X-NODEREF vhndItemNode.
           vhndDoc:CREATE-NODE(vhndItemNode,"item","ELEMENT").
           vhndTableNode:APPEND-CHILD(vhndItemNode).
           vhndItemNode:SET-ATTRIBUTE("href","#" + vchrId). 

           CREATE X-NODEREF vhndRowNode.
           vhndDoc:CREATE-NODE(vhndRowNode,"multiRef","ELEMENT").
           vhndBodyNode:APPEND-CHILD(vhndRowNode).
           vhndRowNode:SET-ATTRIBUTE("id",vchrId).
       END.
       ELSE
       DO:
           CREATE X-NODEREF vhndRowNode.
           vhndDoc:CREATE-NODE(vhndRowNode,iphndBuffer:NAME,"ELEMENT").
           vhndTableNode:APPEND-CHILD(vhndRowNode).
       END.
       
       vhndRowNode:SET-ATTRIBUTE("root","0"). 
       vhndRowNode:SET-ATTRIBUTE("encodingStyle","0"). 
       vhndRowNode:SET-ATTRIBUTE("type",iphndBuffer:NAME). 

       REPEAT vintI = 1 TO iphndBuffer:NUM-FIELDS:
           vhndBufferField = iphndBuffer:BUFFER-FIELD(vintI).

           CASE (vhndBufferField:DATA-TYPE):
               /*
                WHEN "CHARACTER" 
             OR WHEN "COM-HANDLE"
             OR WHEN "HANDLE"
             OR WHEN "MEMPTR"
             OR WHEN "RAW"
             OR WHEN "ROWID"
             OR WHEN "WIDGET-HANDLE" THEN
               */
             WHEN "DATE" THEN
             DO:
               setOutDate(vhndRowNode,vhndBufferField:NAME,vhndBufferField:BUFFER-VALUE).
             END.
             WHEN "LOGICAL" THEN
             DO:
               setOutLogical(vhndRowNode,vhndBufferField:NAME,vhndBufferField:BUFFER-VALUE).
             END.
             OTHERWISE
             DO:
               setOutCharacter(vhndRowNode,vhndBufferField:NAME,vhndBufferField:BUFFER-VALUE).
             END.
           END CASE.
/*
           CREATE X-NODEREF vhndFieldNode.
           vhndDoc:CREATE-NODE(vhndFieldNode,vhndBufferField:NAME,"ELEMENT").
           vhndRowNode:APPEND-CHILD(vhndFieldNode).

           CREATE X-NODEREF vhndValueNode.
           vhndDoc:CREATE-NODE(vhndValueNode,"","TEXT").
           vhndFieldNode:APPEND-CHILD(vhndValueNode).
           vhndValueNode:NODE-VALUE = STRING(vhndBufferField:BUFFER-VALUE).
           */
       END. 
   END. /** repeatLoop: **/

   vhndTableNode:SET-ATTRIBUTE("type","Array").
   vhndTableNode:SET-ATTRIBUTE("arrayType",iphndBuffer:NAME + "[" + STRING(vintRows) + "]").
   
   vhndQuery:QUERY-CLOSE().
   DELETE OBJECT vhndQuery.

   RETURN FALSE.
END FUNCTION.

/* Builds the response document */
FUNCTION createResponse RETURNS HANDLE (
    INPUT iphndNS AS CHARACTER):
    DEFINE VARIABLE vhndDoc        AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndEnvelope   AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndHead       AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndBody       AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndMessage     AS HANDLE    NO-UNDO. 

    CREATE X-DOCUMENT vhndDoc. 
    
    vhndEnvelope = createChild(vhndDoc,"Envelope",iphndNS).
    vhndEnvelope:SET-ATTRIBUTE("xmlns:" + iphndNS,"http://schemas.xmlsoap.org/soap/envelope/").
    vhndEnvelope:SET-ATTRIBUTE("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance").
    vhndEnvelope:SET-ATTRIBUTE("xmlns:xsd","http://www.w3.org/2001/XMLSchema").

    vhndHead = createChild(vhndEnvelope,"Header",iphndNS).
    vhndBody = createChild(vhndEnvelope,"Body",iphndNS).

    RETURN vhndDoc.
END FUNCTION.

/* Builds a fault message */
FUNCTION createFault RETURNS HANDLE (
    INPUT iphndNode AS HANDLE,
    INPUT ipchrCode AS CHARACTER,
    INPUT ipchrString AS CHARACTER,
    INPUT ipchrActor AS CHARACTER,
    INPUT ipchrNS AS CHARACTER):
    DEFINE VARIABLE vhndFault AS HANDLE     NO-UNDO.

    vhndFault = createChild(iphndNode,"Fault",ipchrNS).
    setOutCharacter(vhndFault,"faultcode",ipchrCode).
    setOutCharacter(vhndFault,"faultstring",ipchrString).
    setOutCharacter(vhndFault,"faultactor",ipchrActor).

    RETURN vhndFault.    
END FUNCTION.

/* Extracts date and time from a string with the format
   "YYYY-MM-DDTHH:MM:SSZ" */
FUNCTION extractDateTime RETURNS LOGICAL (
   INPUT  ipchrDateTime AS CHARACTER
  ,OUTPUT opdatDate     AS DATE
  ,OUTPUT opintTime     AS INTEGER
  ):
  DEFINE VARIABLE vchrAux AS CHARACTER  NO-UNDO.

  IF NUM-ENTRIES(ipchrDateTime,"T") >= 2 THEN
  DO:
    ASSIGN 
      vchrAux = ENTRY(1,ipchrDateTime,"T")
      opdatDate = DATE(INT(ENTRY(2,vchrAux,"-")),INT(ENTRY(3,vchrAux,"-")),INT(ENTRY(1,vchrAux,"-")))
      vchrAux = ENTRY(2,ipchrDateTime,"T"). 

    IF NUM-ENTRIES(vchrAux,":") >= 3 THEN
    DO:
      ASSIGN 
        vchrAux = SUBSTRING(vchrAux,1,LENGTH(vchrAux) - 1)
        opintTime = 3600 * INTEGER(ENTRY(1,vchrAux,":")) + 60 * INTEGER(ENTRY(2,vchrAux,":")) + DECIMAL(ENTRY(3,vchrAux,":")).
    END.

    RETURN TRUE.
  END.

  RETURN FALSE.
END FUNCTION.


/* Translates the header parameters and call the supplied 
   procedure to do the security test */
FUNCTION SECURITY-TEST RETURNS LOGICAL
  (INPUT iphndRequestHead AS HANDLE,
   INPUT ipchrProcedure AS CHARACTER):
  DEFINE VARIABLE vhndSecurity      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vhndTimestamp     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vchrExpires       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vdatExpires       AS DATE       NO-UNDO.
  DEFINE VARIABLE vintExpires       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vhndUsernameToken AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vhndPassword      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vchrUsername      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrPassword      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrType          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrNonce         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrCreated       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vdatCreated       AS DATE       NO-UNDO.
  DEFINE VARIABLE vintCreated       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vlogValida        AS LOGICAL    NO-UNDO.
  
  IF VALID-HANDLE(iphndRequestHead) THEN
  DO:
    vhndSecurity = findChild(iphndRequestHead,"Security").
    IF VALID-HANDLE(vhndSecurity) THEN
    DO:
      vhndTimestamp = findChild(vhndSecurity,"Timestamp").
      IF VALID-HANDLE(vhndTimestamp) THEN
      DO:
        vchrExpires = getInCharacter(vhndTimestamp,"Expires").
        extractDateTime(vchrExpires, OUTPUT vdatExpires, OUTPUT vintExpires).
      END.

      vhndUsernameToken = findChild(vhndSecurity,"UsernameToken").
      IF VALID-HANDLE(vhndUsernameToken) THEN
      DO:
        vchrUsername = getInCharacter(vhndUsernameToken,"Username").
        vchrPassword = getInCharacter(vhndUsernameToken,"Password").
        vchrNonce    = getInCharacter(vhndUsernameToken,"Nonce").
        vchrCreated  = getInCharacter(vhndUsernameToken,"Created").

        vhndPassword = findChild(vhndUsernameToken,"Password").
        IF VALID-HANDLE(vhndPassword) THEN
        DO:
          vchrType = 
            IF INDEX(vhndPassword:GET-ATTRIBUTE("Type"),"PasswordText") > 0 THEN
              "PasswordText"
            ELSE
              "PasswordDigest".
        END.

        extractDateTime(vchrCreated, OUTPUT vdatCreated, OUTPUT vintCreated).

        RUN VALUE(ipchrProcedure) (
           vchrUsername
          ,vchrPassword
          ,vchrType
          ,vchrNonce
          ,vdatCreated
          ,vintCreated
          ,vdatExpires
          ,vintExpires
          ,OUTPUT vlogValida
          ).
      END.
    END.
  END.

  RETURN vlogValida.
END FUNCTION.

&ENDIF /*O4GLWS_I_*/
