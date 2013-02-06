/*------------------------------------------------------------------------
  File:               securityTemplate.p
  Description:        This is a dummy security procedure. 
  Input Parameters:   ipchrUsername    The username
                      ipchrPassword    The password
                      ipchrType        "PasswordText" or "PasswordDigest"
                      ipchrNonce       The nonce
                      ipdatCreated     The creation date
                      ipintCreated     The creation time
                      ipdatExpires     The expiration date
                      ipintExpires     The expiration time
  Output Parameters:  oplogValid       TRUE if the specified parameters
                                       pass the security test
  Author:             Lic. Edgar Medrano Perez
                      edgarmedrano@gmail.com
  Created:            2005.06.18
  Company:            Open 4GL webservices project
                      http://o4glws.sourceforge.net
  Notes:              
------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipchrUsername  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipchrPassword  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipchrType      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipchrNonce     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipdatCreated   AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER ipintCreated   AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ipdatExpires   AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER ipintExpires   AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER oplogValida    AS LOGICAL    NO-UNDO.

MESSAGE THIS-PROCEDURE:FILE-NAME.
MESSAGE "ipchrUsername: " ipchrUsername.
MESSAGE "ipchrPassword: " ipchrPassword.
MESSAGE "ipchrType:     " ipchrType.
MESSAGE "ipchrNonce:    " ipchrNonce.
MESSAGE "ipdatCreated:  " ipdatCreated.
MESSAGE "ipintCreated:  " ipintCreated.
MESSAGE "ipdatExpires:  " ipdatExpires.
MESSAGE "ipintExpires:  " ipintExpires.

ASSIGN oplogValida = TRUE.
