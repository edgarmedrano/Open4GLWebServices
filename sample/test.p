/*------------------------------------------------------------------------
  File:               test.p
  Description:        This is a sample procedure. 
  Input Parameters:   ipchrString      A comma separated list
  Output Parameters:  ttTest           A table containing the list
                                       elements of ipchrString
  Author:             Lic. Edgar Medrano Perez
                      edgarmedrano@gmail.com
  Created:            2005.06.25
  Company:            Open 4GL webservices project
                      http://o4glws.sourceforge.net
  Notes:              
------------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttTest
  FIELD Token AS CHARACTER.

DEFINE INPUT  PARAMETER ipchrString AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttTest.

DEFINE VARIABLE vintI AS INTEGER    NO-UNDO.

DO vintI = 1 TO NUM-ENTRIES(ipchrString):
  CREATE ttTest.
  ASSIGN ttTest.Token = ENTRY(vintI,ipchrString).
END.

PROCEDURE OutTable:
    DEFINE INPUT  PARAMETER ipchrString AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttTest.

    DEFINE VARIABLE vintI AS INTEGER    NO-UNDO.

    DO vintI = 1 TO NUM-ENTRIES(ipchrString):
      CREATE ttTest.
      ASSIGN ttTest.Token = ENTRY(vintI,ipchrString).
    END.
END PROCEDURE.

PROCEDURE InTable:
    DEFINE INPUT PARAMETER TABLE FOR ttTest.
    DEFINE OUTPUT PARAMETER opchrCadena AS CHARACTER  NO-UNDO.

    FOR EACH ttTest:
      ASSIGN opchrCadena = opchrCadena + "," + ttTest.Token.
    END.
END PROCEDURE.

PROCEDURE OutParam:
    DEFINE INPUT-OUTPUT  PARAMETER iopchrCadena AS CHARACTER  NO-UNDO.

END PROCEDURE.
