/*------------------------------------------------------------------------
  File:               o4glws.I
  Description:        Common functions used by the webservices adapters.
  Author:             Lic. Edgar Medrano Perez
  Created:            2005.06.18
  Company:            
  Notes:
------------------------------------------------------------------------*/

&IF DEFINED(O4GLWS_I_) = 0 &THEN
&GLOBAL-DEFINE O4GLWS_I_ TRUE


/* Find the child node with the given name */
FUNCTION findChild RETURNS HANDLE (INPUT iphndParent AS HANDLE, INPUT ipchrName AS CHARACTER):
  DEFINE VARIABLE vintChild AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vhndChild AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vchrName  AS CHARACTER  NO-UNDO.

  CREATE X-NODEREF vhndChild.

  DO vintChild = 1 TO iphndParent:NUM-CHILDREN:
    IF iphndParent:GET-CHILD(vhndChild,vintChild) THEN
    DO:
      IF vhndChild:NAME = ipchrName THEN
      DO:
        RETURN vhndChild.
      END.

      ASSIGN vchrName = 
        IF NUM-ENTRIES(vhndChild:NAME,":") = 2 THEN 
          ENTRY(2,vhndChild:NAME,":") 
        ELSE 
          vhndChild:NAME. 

      IF vchrName = ipchrName THEN
      DO:
        RETURN vhndChild.
      END.
    END.
  END.

  RETURN ?.
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

FUNCTION getInTable RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT iphndBuffer AS HANDLE):
    DEFINE VARIABLE vhndQuery       AS HANDLE     NO-UNDO.
    DEFINE VARIABLE vhndTableNode   AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndRowNode     AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndColNode     AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vhndBufferField AS HANDLE     NO-UNDO.
    DEFINE VARIABLE vintI           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE vintJ           AS INTEGER    NO-UNDO.

    vhndTableNode = findChild(iphndMessage,ipchrParamName).
    IF VALID-HANDLE(vhndTableNode) THEN
    DO:
        CREATE X-NODEREF vhndRowNode.
        CREATE X-NODEREF vhndColNode.

        /*Crear el registro*/
        DO vintI = 1 TO vhndTableNode:NUM-CHILDREN:

           vhndTableNode:GET-CHILD(vhndRowNode,vintI).
           iphndBuffer:BUFFER-CREATE().
           DO vintJ = 1 TO vhndRowNode:NUM-CHILDREN:
               vhndRowNode:GET-CHILD(vhndColNode,vintJ).
               vhndBufferField = iphndBuffer:BUFFER-FIELD(vhndColNode:NAME).
               IF vhndBufferField <> ? THEN 
               DO:
                   vhndBufferField:BUFFER-VALUE = getInCharacter(vhndRowNode,vhndColNode:NAME).
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
    RETURN setOutCharacter(iphndMessage,ipchrParamName,STRING(iplogParamValue,"true/false")).
END FUNCTION.

FUNCTION setOutTable RETURNS LOGICAL (INPUT iphndMessage AS HANDLE, INPUT ipchrParamName AS CHARACTER, INPUT iphndBuffer AS HANDLE):
   DEFINE VARIABLE vhndQuery       AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vhndTableNode   AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndRowNode     AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndColNode     AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE vhndBufferField AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vintI           AS INTEGER    NO-UNDO.
   
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

   repeatLoop:
   REPEAT:
       /** Get next record from query **/
       vhndQuery:GET-NEXT().
       IF vhndQuery:QUERY-OFF-END THEN LEAVE repeatLoop.

       /** Create parent node in XML **/
       vhndRowNode = createChild(vhndTableNode,iphndBuffer:name,"").

       REPEAT vintI = 1 TO iphndBuffer:NUM-FIELDS:
           vhndBufferField = iphndBuffer:BUFFER-FIELD(vintI).
           setOutCharacter(vhndRowNode,vhndBufferField:NAME,STRING(vhndBufferField:BUFFER-VALUE)).
       END. 
   END. /** repeatLoop: **/

   vhndQuery:QUERY-CLOSE().
   DELETE OBJECT vhndQuery.

   RETURN FALSE.
END FUNCTION.

/* Builds the response document */
FUNCTION createResponse RETURNS HANDLE:
    DEFINE VARIABLE vhndDoc        AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndEnvelope   AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndHead       AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndBody       AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vhndMessage     AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE vchrEnvelopeNS AS CHARACTER  NO-UNDO INITIAL "s".

    CREATE X-DOCUMENT vhndDoc. 
    
    vhndEnvelope = createChild(vhndDoc,"Envelope",vchrEnvelopeNS).
    vhndEnvelope:SET-ATTRIBUTE("xmlns:" + vchrEnvelopeNS,"http://www.w3.org/2001/06/soap-envelope").

    vhndHead = createChild(vhndEnvelope,"Header",vchrEnvelopeNS).
    vhndBody = createChild(vhndEnvelope,"Body",vchrEnvelopeNS).

    RETURN vhndDoc.
END FUNCTION.

/* Translates the header parameters and call the supplied 
   procedure to do the security test */
FUNCTION SECURITY-TEST RETURNS LOGICAL
  (INPUT iphndRequestHead AS HANDLE,
   INPUT ipchrProcedure AS CHARACTER):
  DEFINE VARIABLE vhndSecurity      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vhndUsernameToken AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vchrUsername      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrPassword      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrType          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrNonce         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrCreated       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vdatCreated       AS DATE       NO-UNDO.
  DEFINE VARIABLE vintCreated       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vlogValida        AS LOGICAL    NO-UNDO.
  
  vhndSecurity = findChild(iphndRequestHead,"Security").
  vhndUsernameToken = findChild(vhndSecurity,"UsernameToken").
  vchrUsername = getInCharacter(vhndUsernameToken,"Username").
  vchrPassword = getInCharacter(vhndUsernameToken,"Password").
  vchrType = getInCharacter(vhndUsernameToken,"Type").
  vchrNonce = getInCharacter(vhndUsernameToken,"Nonce").
  vchrCreated = getInCharacter(vhndUsernameToken,"Created").

  /*Extract date and time*/

  RUN VALUE(ipchrProcedure) (
     vchrUsername
    ,vchrPassword
    ,vchrType
    ,vchrNonce
    ,vdatCreated
    ,vintCreated
    ,OUTPUT vlogValida
    ).

  RETURN vlogValida.
END FUNCTION.

&ENDIF /*O4GLWS_I_*/
