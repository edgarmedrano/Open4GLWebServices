DEFINE VARIABLE vchrWebServicePath AS CHARACTER  NO-UNDO INITIAL "".

{o4glws.i}
/* Progress Local Template */
PROCEDURE WSCall:
  DEFINE INPUT  PARAMETER vhndRequestDoc  AS HANDLE     NO-UNDO.
  DEFINE OUTPUT PARAMETER vhndResponseDoc AS HANDLE     NO-UNDO.
  
  RUN VALUE(vchrWebServicePath) (vhndRequestDoc, OUTPUT vhndResponseDoc).
END.

/* HTTP Template * /
PROCEDURE WSCall:
  DEFINE INPUT  PARAMETER vhndRequestDoc  AS HANDLE     NO-UNDO.
  DEFINE OUTPUT PARAMETER vhndResponseDoc AS HANDLE     NO-UNDO.
  
END.
*/

PROCEDURE TYPES_prointReq:
  DEFINE INPUT  PARAMETER ipint       AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER opchrTicket AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE vhndRequestDoc      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestEnv      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestBody     AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndRequestMessage  AS HANDLE    NO-UNDO.
  DEFINE VARIABLE vhndResponseDoc      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseEnv      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseBody     AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE vhndResponseMessage  AS HANDLE    NO-UNDO. 
  
  DEFINE VARIABLE vchrEnvelopeNS      AS CHARACTER  NO-UNDO INITIAL "s".
  DEFINE VARIABLE vchrMessageNS       AS CHARACTER  NO-UNDO INITIAL "n".
  DEFINE VARIABLE vchrWebService      AS CHARACTER  NO-UNDO INITIAL "PGTypes".

  vhndRequestDoc = createRequest(vchrEnvelopeNS).
  vhndRequestEnv = findChild(vhndRequestDoc,"Envelope").
  vhndRequestBody = findChild(vhndRequestEnv,"Body").
  vhndRequestMessage = createChild(vhndResponseBody,"TYPES_prointRequest",vchrMessageNS).
  vhndRequestMessage:SET-ATTRIBUTE("xmlns:" + vchrMessageNS,vchrWebService).
  setOutINTEGER(vhndRequestMessage,"ipint",ipint).
  
  RUN WSCall(vhndRequestDoc, OUTPUT vhndResponseDoc).
  
  vhndResponseEnv = findChild(vhndResponseDoc,"Envelope").
  vhndResponseBody = findChild(vhndResponseEnv,"Body").
  vhndResponseMessage = findChild(vhndResponseBody,"TicketResponse").
  
  opchrTicket = getInCHARACTER(vhndResponseMessage,"Ticket").  
END.

