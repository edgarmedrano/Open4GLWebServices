&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&GLOBAL-DEFINE OFN_OVERWRITEPROMPT  2
&GLOBAL-DEFINE OFN_HIDEREADONLY     4
&GLOBAL-DEFINE OFN_NOCHANGEDIR      8
&GLOBAL-DEFINE OFN_ALLOWMULTISELECT 512
&GLOBAL-DEFINE OFN_PATHMUSTEXIST    2048
&GLOBAL-DEFINE OFN_FILEMUSTEXIST    4096
&GLOBAL-DEFINE OFN_NOREADONLYRETURN 32768
&GLOBAL-DEFINE OFN_EXPLORER         524288
 
PROCEDURE GetOpenFileNameA EXTERNAL "comdlg32.dll" :
  DEFINE INPUT  PARAMETER lpOfn   AS LONG.
  DEFINE RETURN PARAMETER pReturn AS LONG.
END PROCEDURE.

{o4glws/procInfo.i}

DEFINE TEMP-TABLE ttProcedureCopy LIKE ttProcedure.
DEFINE TEMP-TABLE ttMethodCopy LIKE ttMethod.
DEFINE TEMP-TABLE ttParamCopy LIKE ttParam.
DEFINE TEMP-TABLE ttTableCopy LIKE ttTable.
DEFINE TEMP-TABLE ttFieldCopy LIKE ttField.

DEFINE TEMP-TABLE ttProcedureAux LIKE ttProcedure.
DEFINE TEMP-TABLE ttMethodAux LIKE ttMethod.
DEFINE TEMP-TABLE ttParamAux LIKE ttParam.
DEFINE TEMP-TABLE ttTableAux LIKE ttTable.
DEFINE TEMP-TABLE ttFieldAux LIKE ttField.

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttProcedure.
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttMethod.
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttParam.
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttTable.
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttField.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE vint AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME broInternal

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttMethodCopy ttProcedureCopy

/* Definitions for BROWSE broInternal                                   */
&Scoped-define FIELDS-IN-QUERY-broInternal /*ttMethodCopy.iProcId*/ ttMethodCopy.lEnabled ttMethodCopy.cName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-broInternal   
&Scoped-define SELF-NAME broInternal
&Scoped-define OPEN-QUERY-broInternal OPEN QUERY {&SELF-NAME} FOR EACH ttMethodCopy WHERE ttMethodCopy.iProcId = ttProcedureCopy.iProcId.
&Scoped-define TABLES-IN-QUERY-broInternal ttMethodCopy
&Scoped-define FIRST-TABLE-IN-QUERY-broInternal ttMethodCopy


/* Definitions for BROWSE broProcedure                                  */
&Scoped-define FIELDS-IN-QUERY-broProcedure ttProcedureCopy.cPath   
&Scoped-define ENABLED-FIELDS-IN-QUERY-broProcedure   
&Scoped-define SELF-NAME broProcedure
&Scoped-define OPEN-QUERY-broProcedure OPEN QUERY {&SELF-NAME} FOR EACH ttProcedureCopy.
&Scoped-define TABLES-IN-QUERY-broProcedure ttProcedureCopy
&Scoped-define FIRST-TABLE-IN-QUERY-broProcedure ttProcedureCopy


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-broInternal}~
    ~{&OPEN-QUERY-broProcedure}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS broProcedure btnAdd broInternal btnRemove ~
Btn_OK Btn_Cancel Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShortName Dialog-Frame 
FUNCTION ShortName RETURNS CHARACTER
  ( INPUT ipchrPath AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToPropath Dialog-Frame 
FUNCTION ToPropath RETURNS CHARACTER
  (INPUT ipchrAbsolutePath AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "&Add..." 
     SIZE 19 BY 1.19.

DEFINE BUTTON btnRemove 
     LABEL "&Remove" 
     SIZE 19 BY 1.19.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY broInternal FOR 
      ttMethodCopy SCROLLING.

DEFINE QUERY broProcedure FOR 
      ttProcedureCopy SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE broInternal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS broInternal Dialog-Frame _FREEFORM
  QUERY broInternal DISPLAY
      /*ttMethodCopy.iProcId*/ ttMethodCopy.lEnabled ttMethodCopy.cName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 11.43 EXPANDABLE.

DEFINE BROWSE broProcedure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS broProcedure Dialog-Frame _FREEFORM
  QUERY broProcedure DISPLAY
      ttProcedureCopy.cPath
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 11.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     broProcedure AT ROW 1.95 COL 2
     btnAdd AT ROW 1.95 COL 42
     broInternal AT ROW 1.95 COL 63
     btnRemove AT ROW 3.38 COL 42
     Btn_OK AT ROW 14.33 COL 3
     Btn_Cancel AT ROW 14.33 COL 19
     Btn_Help AT ROW 14.33 COL 85
     "Procedures:" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 1 COL 2
     "Internal procedures:" VIEW-AS TEXT
          SIZE 25 BY .95 AT ROW 1 COL 63
     SPACE(14.39) SKIP(13.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Procedures"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB broProcedure 1 Dialog-Frame */
/* BROWSE-TAB broInternal btnAdd Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE broInternal
/* Query rebuild information for BROWSE broInternal
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttMethodCopy WHERE ttMethodCopy.iProcId = ttProcedureCopy.iProcId.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE broInternal */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE broProcedure
/* Query rebuild information for BROWSE broProcedure
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProcedureCopy.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE broProcedure */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Procedures */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME broProcedure
&Scoped-define SELF-NAME broProcedure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL broProcedure Dialog-Frame
ON VALUE-CHANGED OF broProcedure IN FRAME Dialog-Frame
DO: 
  OPEN QUERY broInternal FOR EACH ttMethodCopy WHERE ttMethodCopy.iProcId = ttProcedureCopy.iProcId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd Dialog-Frame
ON CHOOSE OF btnAdd IN FRAME Dialog-Frame /* Add... */
DO:
    DEFINE VARIABLE vchrFiles     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vlogOKCancel  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE vintI         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vchrAbsPath   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vchrProPath   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vchrName      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vintProcedure AS INTEGER    NO-UNDO.
    DEFINE VARIABLE vchrResult    AS CHARACTER  NO-UNDO.
     
    RUN SelectMultipleFileNames (INPUT "Compatible Files (*.r;*.w;*.p;*.htm*)|*.r;*.w;*.p;*.htm*" + "|" +
                                       "R-Code (*.r)|*.r" + "|" +
                                       "Compilable Source(*.w;*.p;*.htm*)|*.w;*.p;*.htm*" + "|" +
                                       "Windows (*.w)|*.w" + "|" +
                                       "Procedures (*.p)|*.p" + "|" +
                                       "HTML (*.htm*)|*.htm*" + "|" +
                                       "All Files (*.*)|*.*",
                                 INPUT "",
                                 INPUT "Select one or more procedures",
                                 INPUT FRAME Dialog-Frame:HWND,
                                 OUTPUT vchrFiles,
                                 OUTPUT vlogOKCancel
                                ).
     
    IF vlogOKCancel THEN 
    DO:
      DO vintI = 1 TO NUM-ENTRIES(vchrFiles):
        ASSIGN 
          vchrAbsPath = ENTRY(vintI,vchrFiles)
          vchrProPath = ToPropath(vchrAbsPath).

        FIND ttProcedureCopy
          WHERE ttProcedureCopy.cPath = vchrProPath
          NO-LOCK NO-ERROR.

        IF AVAILABLE ttProcedureCopy THEN
        DO:
          MESSAGE "The procedure " vchrAbsPath " is already added"  VIEW-AS ALERT-BOX.
          NEXT.
        END.
        ELSE
        DO:
          RUN o4glws/procInfo.p(
            vchrAbsPath,
            OUTPUT vchrResult,
            OUTPUT TABLE ttMethodAux,
            OUTPUT TABLE ttParamAux,
            OUTPUT TABLE ttTableAux,
            OUTPUT TABLE ttFieldAux).

          IF vchrResult <> "" THEN
          DO:
            MESSAGE vchrResult VIEW-AS ALERT-BOX.
            NEXT.
          END.
          ELSE
          DO:
            FIND LAST ttProcedureCopy
              NO-LOCK NO-ERROR.

            ASSIGN vintProcedure = IF AVAILABLE ttProcedureCopy THEN ttProcedureCopy.iProcId + 1 ELSE 1.

            CREATE ttProcedureCopy.
            ASSIGN
              ttProcedureCopy.iProcId = vintProcedure
              ttProcedureCopy.cPath = vchrProPath.

            ASSIGN ttProcedureCopy.cName = ShortName(vchrProPath).

            FOR EACH ttMethodAux:
              CREATE ttMethodCopy.
              BUFFER-COPY ttMethodAux TO ttMethodCopy
                ASSIGN ttMethodCopy.iProcId = vintProcedure.
            END.

            FOR EACH ttParamAux:
              CREATE ttParamCopy.
              BUFFER-COPY ttParamAux TO ttParamCopy
                ASSIGN ttParamCopy.iProcId = vintProcedure.
            END.

            FOR EACH ttTableAux:
              CREATE ttTableCopy.
              BUFFER-COPY ttTableAux TO ttTableCopy
                ASSIGN ttTableCopy.iProcId = vintProcedure.
            END.

            FOR EACH ttFieldAux:
              CREATE ttFieldCopy.
              BUFFER-COPY ttFieldAux TO ttFieldCopy
                ASSIGN ttFieldCopy.iProcId = vintProcedure.
            END.

            OPEN QUERY broProcedure FOR EACH ttProcedureCopy.
            APPLY "VALUE-CHANGED" TO broProcedure.
          END.
        END.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove Dialog-Frame
ON CHOOSE OF btnRemove IN FRAME Dialog-Frame /* Remove */
DO:
  DEFINE VARIABLE vintI      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vlogReturn AS LOGICAL    NO-UNDO.

  DO vintI = broProcedure:NUM-SELECTED-ROWS TO 1 by -1: 
      vlogReturn = broProcedure:FETCH-SELECTED-ROW(vintI). 
      GET CURRENT broProcedure EXCLUSIVE-LOCK.  

      FOR EACH ttMethodCopy
        WHERE ttMethodCopy.iProcId = ttProcedureCopy.iProcId
        EXCLUSIVE-LOCK:
        DELETE ttMethodCopy.
      END.

      FOR EACH ttParamCopy
        WHERE ttParamCopy.iProcId = ttProcedureCopy.iProcId
        EXCLUSIVE-LOCK:
        DELETE ttParamCopy.
      END.

      FOR EACH ttTableCopy
        WHERE ttTableCopy.iProcId = ttProcedureCopy.iProcId
        EXCLUSIVE-LOCK:
        DELETE ttTableCopy.
      END.

      FOR EACH ttFieldCopy
        WHERE ttFieldCopy.iProcId = ttProcedureCopy.iProcId
        EXCLUSIVE-LOCK:
        DELETE ttFieldCopy.
      END.

      DELETE ttProcedureCopy. 
  END.  
  vlogReturn = broProcedure:DELETE-SELECTED-ROWS().  
  OPEN QUERY broProcedure FOR EACH ttProcedureCopy.
  APPLY "VALUE-CHANGED" TO broProcedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN SAVE-TABLES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME broInternal
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

RUN LOAD-TABLES.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE broProcedure btnAdd broInternal btnRemove Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LOAD-TABLES Dialog-Frame 
PROCEDURE LOAD-TABLES :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH ttProcedure:
      CREATE ttProcedureCopy.
      BUFFER-COPY ttProcedure TO ttProcedureCopy.
    END.

    FOR EACH ttMethod:
      CREATE ttMethodCopy.
      BUFFER-COPY ttMethod TO ttMethodCopy.
    END.

    FOR EACH ttParam:
      CREATE ttParamCopy.
      BUFFER-COPY ttParam TO ttParamCopy.
    END.

    FOR EACH ttTable:
      CREATE ttTableCopy.
      BUFFER-COPY ttTable TO ttTableCopy.
    END.

    FOR EACH ttField:
      CREATE ttFieldCopy.
      BUFFER-COPY ttField TO ttFieldCopy.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SAVE-TABLES Dialog-Frame 
PROCEDURE SAVE-TABLES :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH ttProcedure:
      DELETE ttProcedure.
    END.

    FOR EACH ttProcedureCopy:
      CREATE ttProcedure.
      BUFFER-COPY ttProcedureCopy TO ttProcedure.
    END.

    FOR EACH ttMethod:
      DELETE ttMethod.
    END.

    FOR EACH ttMethodCopy:
      CREATE ttMethod.
      BUFFER-COPY ttMethodCopy TO ttMethod.
    END.

    FOR EACH ttParam:
      DELETE ttParam.
    END.

    FOR EACH ttParamCopy:
      CREATE ttParam.
      BUFFER-COPY ttParamCopy TO ttParam.
    END.

    FOR EACH ttTable:
      DELETE ttTable.
    END.

    FOR EACH ttTableCopy:
      CREATE ttTable.
      BUFFER-COPY ttTableCopy TO ttTable.
    END.

    FOR EACH ttField:
      DELETE ttField.
    END.

    FOR EACH ttFieldCopy:
      CREATE ttField.
      BUFFER-COPY ttFieldCopy TO ttField.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectMultipleFileNames Dialog-Frame 
PROCEDURE SelectMultipleFileNames :
/*------------------------------------------------------------------------------
  Purpose:     Replaces the SYSTEM-DIALOG-GET-FILE common dialog,
               supports multiselect.
  Parameters:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER FilterList       AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER InitialDirectory AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER DialogTitle      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER WindowHandle     AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER FileNames        AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER OK               AS INTEGER   NO-UNDO.
 
  DEFINE VARIABLE Flags           AS INTEGER NO-UNDO.
  DEFINE VARIABLE lpOfn           AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE lpstrFilter     AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE lpstrTitle      AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE lpstrInitialDir AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE lpstrFile       AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE offset          AS INTEGER NO-UNDO.
 
  /* Flags controls the behaviour and appearance of the dialog-box. 
           There is much room for experiments. This combination works nice: */
  Flags = {&OFN_ALLOWMULTISELECT} + 
          {&OFN_EXPLORER} + 
          {&OFN_NOCHANGEDIR}.
 
  /* convert the "|"-separated list of filters to a CHR(0)-separated 
     list and make sure it's terminated with a double CHR(0): */
  FilterList = TRIM(FilterList,"|") + "|". /* this will cause the double CHR(0) */
  SET-SIZE(lpstrFilter)      = LENGTH(FilterList) + 1.
  PUT-STRING(lpstrFilter, 1) = FilterList.
  DO offset=1 TO GET-SIZE(lpstrFilter) :
     IF GET-BYTE(lpstrFilter,offset)=124 /* =ASC("|") */ THEN 
        PUT-BYTE(lpstrFilter,offset)=0.
  END.
 
  /* get memory-pointers to the string parameters: */
  SET-SIZE(lpstrFile)   = 1024. /* room for a couple of files...     */
  PUT-BYTE(lpstrFile,1) = 0.    /* don't initialize dialog to a file */
 
  SET-SIZE(lpstrTitle) = LENGTH(DialogTitle) + 1.
  PUT-STRING(lpstrTitle,1) = DialogTitle.
 
  IF InitialDirectory NE ? THEN DO:
     SET-SIZE(lpstrInitialDir) = LENGTH(InitialDirectory) + 1.
     PUT-STRING(lpstrInitialDir,1) = InitialDirectory.
  END.
 
  /* create and initialize an OPENFILENAME structure: */
  SET-SIZE(lpOfn) = 76. /* = {&OPENFILENAME_SIZE_VERSION_400} 
                             to be used in NT4 and Windows 95/98. 
                             Windows 2000 supports a couple more fields. */
 
/* size */              PUT-LONG (lpOfn, 1) = GET-SIZE(lpOfn).
/* hwndOwner */         PUT-LONG (lpOfn, 5) = WindowHandle.
/* hInstance */         PUT-LONG (lpOfn, 9) = 0.
/* lpstrFilter */       PUT-LONG (lpOfn,13) = GET-POINTER-VALUE(lpstrFilter).
/* lpstrCustomFilter */ PUT-LONG (lpOfn,17) = 0.
/* nMaxCustFilter */    PUT-LONG (lpOfn,21) = 0.
/* nFilterIndex */      PUT-LONG (lpOfn,25) = 0.
/* lpstrFile */         PUT-LONG (lpOfn,29) = GET-POINTER-VALUE(lpstrFile).
/* nMaxFile */          PUT-LONG (lpOfn,33) = GET-SIZE(lpstrFile).
/* lpstrFileTitle */    PUT-LONG (lpOfn,37) = 0.
/* nMaxFileTitle */     PUT-LONG (lpOfn,41) = 0.
/* lpstrInitialDir */   PUT-LONG (lpOfn,45) = GET-POINTER-VALUE(lpstrInitialDir).
/* lpstrTitle */        PUT-LONG (lpOfn,49) = GET-POINTER-VALUE(lpstrTitle).
/* flags */             PUT-LONG (lpOfn,53) = Flags.
 
/* nFileOffset */       PUT-SHORT(lpOfn,57) = 0.
/* nFileExtension */    PUT-SHORT(lpOfn,59) = 0.
/* lpstrDefExt */       PUT-LONG (lpOfn,61) = 0.
/* lCustData */         PUT-LONG (lpOfn,65) = 0.
/* lpfnHook */          PUT-LONG (lpOfn,69) = 0.
/* lpTemplateName */    PUT-LONG (lpOfn,73) = 0.
 
  /* run the dialog: */
  RUN GetOpenFileNameA (GET-POINTER-VALUE(lpOfn), OUTPUT OK).
 
  /* release memory: */
  SET-SIZE(lpstrFilter)     = 0.
  SET-SIZE(lpOfn)           = 0.
  SET-SIZE(lpstrTitle)      = 0.
  SET-SIZE(lpstrInitialDir) = 0.
 
  /* lpstrFilter now contains a path, followed by CHR(0), followed 
     by a CHR(0)-separated list of filenames, terminated by a double CHR(0). 
     Unless the user selected only one file: then lpstrFilter will simply
     contain the fully-qualified filename.
     Either way, let's convert the result to a comma-separated list of 
     fully-qualified filenames: */
 
  IF OK NE 0 THEN DO:
    DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
 
    ASSIGN cPath  = GET-STRING(lpstrFile,1)
           offset = LENGTH(cPath) + 2.
 
    REPEAT:
      cFile = GET-STRING(lpstrFile, offset).
      IF cFile = "" THEN LEAVE.
      ASSIGN cList  = cList + ',' + cPath +  '\' + cFile
             offset = offset + LENGTH(cFile) + 1.
    END.
    ASSIGN cList     = TRIM(cList, ",")
           FileNames = IF cList = "" THEN cPath ELSE cList.
  END.
 
  SET-SIZE(lpstrFile) = 0.
 
END PROCEDURE. /* SelectMultipleFileNames */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShortName Dialog-Frame 
FUNCTION ShortName RETURNS CHARACTER
  ( INPUT ipchrPath AS CHARACTER ) :

  DEFINE VARIABLE vintI     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vchrC     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrShort AS CHARACTER  NO-UNDO.

  ASSIGN 
    ipchrPath = REPLACE(ipchrPath,"~\","/")
    ipchrPath = REPLACE(ipchrPath,"-","_")
    ipchrPath = ENTRY(NUM-ENTRIES(ipchrPath,"/"),ipchrPath,"/")
    vintI = R-INDEX(ipchrPath,".").

  IF vintI > 1 /*Si el punto no es inicial*/ THEN
  DO:
    ASSIGN ipchrPath = SUBSTRING(ipchrPath,1,vintI - 1).
  END.

  ASSIGN 
    ipchrPath = REPLACE(ipchrPath," ","_")
    ipchrPath = REPLACE(ipchrPath,"-","_")
    ipchrPath = REPLACE(ipchrPath,".","_").

  DO vintI = 1 TO LENGTH(ipchrPath):
    ASSIGN vchrC = CAPS(SUBSTRING(ipchrPath,vintI,1)).

    IF vchrC = "_" 
      OR (vchrC >= "A" AND vchrC <= "Z") 
      OR (vchrC >= "0" AND vchrC <= "9") THEN
    DO:
      vchrShort = vchrShort + vchrC.
    END.
  END.

  RETURN vchrShort.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToPropath Dialog-Frame 
FUNCTION ToPropath RETURNS CHARACTER
  (INPUT ipchrAbsolutePath AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vchrDir     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrBaseDir AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vintI       AS INTEGER    NO-UNDO.

  ASSIGN 
    ipchrAbsolutePath = REPLACE(ipchrAbsolutePath,"~\","/")
    vchrBaseDir = "".

  DO vintI = 1 TO NUM-ENTRIES(PROPATH):
    ASSIGN vchrDir = REPLACE(ENTRY(vintI, PROPATH),"~\","/").

    IF INDEX(ipchrAbsolutePath,vchrDir) = 1 THEN
    DO:
      IF LENGTH(vchrDir) > LENGTH(vchrBaseDir) THEN
      DO:
        ASSIGN vchrBaseDir = vchrDir.
      END.
    END.
  END.

  IF LENGTH(vchrBaseDir) > 0 AND LENGTH(vchrBaseDir) < LENGTH(ipchrAbsolutePath) THEN
  DO:
      ASSIGN ipchrAbsolutePath = SUBSTRING(ipchrAbsolutePath,LENGTH(vchrBaseDir) + 1).

      IF SUBSTRING(ipchrAbsolutePath,1,1) = "/" THEN
      DO:
        ASSIGN ipchrAbsolutePath = SUBSTRING(ipchrAbsolutePath,2).
      END.
  END.

  RETURN ipchrAbsolutePath.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

