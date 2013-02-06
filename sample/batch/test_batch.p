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


/*Declarar handles para super-procedures*/
DEFINE VARIABLE vhndtest1 AS HANDLE     NO-UNDO.

PROCEDURE PROCESS-REQUEST:
  DEFINE INPUT  PARAMETER vhndRequestDoc  AS HANDLE     NO-UNDO.
  DEFINE OUTPUT PARAMETER vhndResponseDoc AS HANDLE     NO-UNDO.

  DEFINE VARIABLE vintI AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vchrMessage         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vhndRequestEnv      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseEnv     AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestHead     AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseHead    AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestBody     AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseBody    AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestMessage  AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseMessage AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vchrMessageNS       AS CHARACTER  NO-UNDO INITIAL "n".
  DEFINE VARIABLE vchrWebService      AS CHARACTER  NO-UNDO INITIAL "test".

  vhndRequestEnv = findChild(vhndRequestDoc,"Envelope").
  vhndRequestHead = findChild(vhndRequestEnv,"Head").
  vhndRequestBody = findChild(vhndRequestEnv,"Body").

  vhndResponseDoc = createResponse().
  vhndResponseEnv = findChild(vhndResponseDoc,"Envelope").
  vhndResponseBody = findChild(vhndResponseEnv,"Body").

  IF NOT SECURITY-TEST(vhndRequestHead,"o4glws/sample/securityTemplate.p") THEN
  DO:
    RETURN.
  END.

  /*Buscar todas las llamadas*/
  CREATE X-NODEREF vhndRequestMessage.
  DO vintI = 1 TO vhndRequestBody:NUM-CHILDREN:
      IF vhndRequestBody:GET-CHILD(vhndRequestMessage,vintI) THEN
      DO:
          /*Indentificar el metodo que sera llamado*/
          vchrMessage = 
             IF vhndRequestMessage:LOCAL-NAME = "" THEN 
               vhndRequestMessage:NAME
             ELSE
               vhndRequestMessage:LOCAL-NAME.

          CASE vchrMessage:
            WHEN "TEST" THEN
            DO:
              vhndResponseMessage = createChild(vhndResponseBody,"TESTResponse",vchrMessageNS).
              vhndResponseMessage:SET-ATTRIBUTE("xmlns:" + vchrMessageNS,vchrWebService).
              RUN TESTAdapter(vhndRequestMessage,vhndResponseMessage) NO-ERROR.
              DELETE PROCEDURE vhndtest1 NO-ERROR.
            END.
            WHEN "TEST_InTable" THEN
            DO:
              vhndResponseMessage = createChild(vhndResponseBody,"TEST_InTableResponse",vchrMessageNS).
              vhndResponseMessage:SET-ATTRIBUTE("xmlns:" + vchrMessageNS,vchrWebService).
              RUN TEST_InTableAdapter(vhndRequestMessage,vhndResponseMessage) NO-ERROR.
              DELETE PROCEDURE vhndtest1 NO-ERROR.
            END.
            WHEN "TEST_OutParam" THEN
            DO:
              vhndResponseMessage = createChild(vhndResponseBody,"TEST_OutParamResponse",vchrMessageNS).
              vhndResponseMessage:SET-ATTRIBUTE("xmlns:" + vchrMessageNS,vchrWebService).
              RUN TEST_OutParamAdapter(vhndRequestMessage,vhndResponseMessage) NO-ERROR.
              DELETE PROCEDURE vhndtest1 NO-ERROR.
            END.
            WHEN "TEST_OutTable" THEN
            DO:
              vhndResponseMessage = createChild(vhndResponseBody,"TEST_OutTableResponse",vchrMessageNS).
              vhndResponseMessage:SET-ATTRIBUTE("xmlns:" + vchrMessageNS,vchrWebService).
              RUN TEST_OutTableAdapter(vhndRequestMessage,vhndResponseMessage) NO-ERROR.
              DELETE PROCEDURE vhndtest1 NO-ERROR.
            END.
          END CASE.
      END.
  END.

  DELETE OBJECT vhndRequestDoc. 
END.

/*Declarar aqui tablas temporales usadas por datos complejos*/
DEFINE TEMP-TABLE TEST_ttTest
    FIELD Token AS CHARACTER
    .

PROCEDURE TESTAdapter:
    DEFINE  INPUT PARAMETER iphndRequestMessage  AS HANDLE     NO-UNDO.
    DEFINE  INPUT PARAMETER iphndResponseMessage AS HANDLE     NO-UNDO.

   /*Declarar variables de salida*/
   DEFINE VARIABLE oplogValida AS LOGICAL    NO-UNDO.

   /*Declarar handlers para leer/escribir temporales*/
   DEFINE VARIABLE hndTEST_ttTest AS HANDLE    NO-UNDO.

   /*Inicializar tablas de entrada/salida*/
   CREATE BUFFER hndTEST_ttTest FOR TABLE "TEST_ttTest".

   RUN o4glws/sample/test.p PERSISTENT SET vhndtest1 (
        getInCHARACTER(iphndRequestMessage,"TEST_ipchrCadena")
       ,getInCHARACTER(iphndRequestMessage,"TEST_ipchrUsername")
       ,OUTPUT TABLE TEST_ttTest
       ,getInCHARACTER(iphndRequestMessage,"TEST_ipchrPassword")
       ,getInCHARACTER(iphndRequestMessage,"TEST_ipchrType")
       ,getInCHARACTER(iphndRequestMessage,"TEST_ipchrNonce")
       ,getInDATE(iphndRequestMessage,"TEST_ipdatCreated")
       ,getInINTEGER(iphndRequestMessage,"TEST_ipintCreated")
       ,OUTPUT oplogValida
       ) NO-ERROR.

   /*Copiar los valores de las variables de salida*/
   setOutTable(iphndResponseMessage,"TEST_ttTestArray",hndTEST_ttTest).
   setOutLOGICAL(iphndResponseMessage,"oplogValida",oplogValida).

END PROCEDURE.

PROCEDURE TEST_InTableAdapter:
    DEFINE  INPUT PARAMETER iphndRequestMessage  AS HANDLE     NO-UNDO.
    DEFINE  INPUT PARAMETER iphndResponseMessage AS HANDLE     NO-UNDO.

   /*Declarar variables de salida*/
   DEFINE VARIABLE opchrCadena AS CHARACTER    NO-UNDO.

   /*Declarar handlers para leer/escribir temporales*/
   DEFINE VARIABLE hndTEST_ttTest AS HANDLE    NO-UNDO.

   /*Inicializar tablas de entrada/salida*/
   CREATE BUFFER hndTEST_ttTest FOR TABLE "TEST_ttTest".
   getInTable(iphndRequestMessage,"TEST_ttTestArray",hndTEST_ttTest).

   RUN TEST(iphndRequestMessage,iphndResponseMessage).
   RUN TEST_InTable IN vhndtest1 (
        INPUT TABLE TEST_ttTest
       ,OUTPUT opchrCadena
       ) NO-ERROR.

   /*Copiar los valores de las variables de salida*/
   setOutCHARACTER(iphndResponseMessage,"opchrCadena",opchrCadena).

END PROCEDURE.

PROCEDURE TEST_OutParamAdapter:
    DEFINE  INPUT PARAMETER iphndRequestMessage  AS HANDLE     NO-UNDO.
    DEFINE  INPUT PARAMETER iphndResponseMessage AS HANDLE     NO-UNDO.

   /*Declarar variables de salida*/
   DEFINE VARIABLE iopchrCadena AS CHARACTER    NO-UNDO.

   /*Inicializar valores de variables de entrada/salida*/
   iopchrCadena = getInCHARACTER(iphndRequestMessage,"iopchrCadena").

   RUN TEST(iphndRequestMessage,iphndResponseMessage).
   RUN TEST_OutParam IN vhndtest1 (
        INPUT-OUTPUT iopchrCadena
       ) NO-ERROR.

   /*Copiar los valores de las variables de salida*/
   setOutCHARACTER(iphndResponseMessage,"iopchrCadena",iopchrCadena).

END PROCEDURE.

PROCEDURE TEST_OutTableAdapter:
    DEFINE  INPUT PARAMETER iphndRequestMessage  AS HANDLE     NO-UNDO.
    DEFINE  INPUT PARAMETER iphndResponseMessage AS HANDLE     NO-UNDO.

   /*Declarar handlers para leer/escribir temporales*/
   DEFINE VARIABLE hndTEST_ttTest AS HANDLE    NO-UNDO.

   /*Inicializar tablas de entrada/salida*/
   CREATE BUFFER hndTEST_ttTest FOR TABLE "TEST_ttTest".

   RUN TEST(iphndRequestMessage,iphndResponseMessage).
   RUN TEST_OutTable IN vhndtest1 (
        getInCHARACTER(iphndRequestMessage,"ipchrCadena")
       ,OUTPUT TABLE TEST_ttTest
       ) NO-ERROR.

   /*Copiar los valores de las variables de salida*/
   setOutTable(iphndResponseMessage,"ttTestArray",hndTEST_ttTest).

END PROCEDURE.

