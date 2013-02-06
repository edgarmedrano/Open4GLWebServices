&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME winWizard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS winWizard 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{o4glws/o4glws.i}
{o4glws/procInfo.i}

DEFINE STREAM strDir.

PROCEDURE SHBrowseForFolder EXTERNAL "shell32":
  DEFINE INPUT  PARAMETER  lpbi         AS LONG.
  DEFINE RETURN PARAMETER  lpItemIDList AS LONG.
END PROCEDURE.

PROCEDURE SHGetPathFromIDList EXTERNAL "shell32":
  DEFINE INPUT  PARAMETER  lpItemIDList AS LONG.
  DEFINE OUTPUT PARAMETER  pszPath      AS CHAR.
  DEFINE RETURN PARAMETER  ReturnValue  AS LONG.
END PROCEDURE.

PROCEDURE CoTaskMemFree EXTERNAL "ole32.dll" :
  DEFINE INPUT PARAMETER lpVoid AS LONG.
END PROCEDURE.


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE vintCurrentSlide AS INTEGER    NO-UNDO.
DEFINE VARIABLE vhndSlide        AS HANDLE     NO-UNDO EXTENT 7.
DEFINE VARIABLE vchrTitle        AS CHARACTER  NO-UNDO INITIAL "WS Wizard".
DEFINE VARIABLE vlogContinue     AS LOGICAL    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME broProcedures

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttProcedure

/* Definitions for BROWSE broProcedures                                 */
&Scoped-define FIELDS-IN-QUERY-broProcedures ttProcedure.cPath   
&Scoped-define ENABLED-FIELDS-IN-QUERY-broProcedures   
&Scoped-define SELF-NAME broProcedures
&Scoped-define OPEN-QUERY-broProcedures OPEN QUERY {&SELF-NAME} FOR EACH ttProcedure.
&Scoped-define TABLES-IN-QUERY-broProcedures ttProcedure
&Scoped-define FIRST-TABLE-IN-QUERY-broProcedures ttProcedure


/* Definitions for FRAME fraProcedures                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fraProcedures ~
    ~{&OPEN-QUERY-broProcedures}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnBack btnNext btnFinish 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToPropath winWizard 
FUNCTION ToPropath RETURNS CHARACTER
  (INPUT ipchrAbsolutePath AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR winWizard AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBack 
     LABEL "< Back" 
     SIZE 12 BY .95.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 12 BY .95.

DEFINE BUTTON btnFinish 
     LABEL "Finish" 
     SIZE 12 BY .95.

DEFINE BUTTON btnNext 
     LABEL "Next >" 
     SIZE 12 BY .95.

DEFINE VARIABLE edtBegin AS CHARACTER INITIAL "Welcome to the WebService Generator Wizard! During the next few steps, the wizard will lead you through creating a WebService. You will define what procedures (and internal procedures) will be published in the webservice." 
     VIEW-AS EDITOR
     SIZE 32 BY 8.1 NO-UNDO.

DEFINE IMAGE imgBegin
     FILENAME "o4glws/wizard3.gif":U
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 42 BY 7.14.

DEFINE IMAGE imgError
     FILENAME "adeicon/blank":U
     SIZE 39 BY 7.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 7.14.

DEFINE VARIABLE edtEndSuccess AS CHARACTER INITIAL "Now you can use the generated WSDL file and program." 
     VIEW-AS EDITOR NO-BOX
     SIZE 36 BY 2.38 NO-UNDO.

DEFINE IMAGE imgSucces
     FILENAME "adeicon/blank":U
     SIZE 39 BY 7.14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 7.14.

DEFINE BUTTON btnSave 
     LABEL "..." 
     SIZE 4 BY .95.

DEFINE VARIABLE cboTemplate AS CHARACTER FORMAT "X(256)":U 
     LABEL "Template" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE filSave AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .95 NO-UNDO.

DEFINE VARIABLE tglProgram AS LOGICAL INITIAL no 
     LABEL "WS Program using" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE tglSave AS LOGICAL INITIAL no 
     LABEL "Save WS project in file" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tglWSDL AS LOGICAL INITIAL no 
     LABEL "WSDL File" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE BUTTON btnLoadFile 
     LABEL "..." 
     SIZE 4 BY .95.

DEFINE VARIABLE filLoadFile AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE radLoad AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Create a new project", yes,
"Load an existing project", no
     SIZE 30 BY 2.38 NO-UNDO.

DEFINE BUTTON btnDefProcedures 
     LABEL "Define Procedures" 
     SIZE 26 BY 1.19.

DEFINE VARIABLE edtProcedures AS CHARACTER INITIAL "You need to define the procedure files that will be published in the webservice." 
     VIEW-AS EDITOR
     SIZE 26 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 9.05.

DEFINE BUTTON btnDestination 
     LABEL "..." 
     SIZE 5 BY .95.

DEFINE BUTTON btnSecurity 
     LABEL "..." 
     SIZE 5 BY .95.

DEFINE VARIABLE filAddress AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE filDestination AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95 NO-UNDO.

DEFINE VARIABLE filName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE filSecurity AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY broProcedures FOR 
      ttProcedure SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE broProcedures
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS broProcedures winWizard _FREEFORM
  QUERY broProcedures DISPLAY
      ttProcedure.cPath
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 48 BY 7.38 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCancel AT ROW 12.43 COL 32
     btnBack AT ROW 12.43 COL 45
     btnNext AT ROW 12.43 COL 58
     btnFinish AT ROW 12.43 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.8 BY 12.91
         DEFAULT-BUTTON btnCancel.

DEFINE FRAME fraSettings
     filName AT ROW 2.19 COL 3 COLON-ALIGNED NO-LABEL
     filDestination AT ROW 4.1 COL 3 COLON-ALIGNED NO-LABEL
     btnDestination AT ROW 4.1 COL 41
     filAddress AT ROW 6 COL 3 COLON-ALIGNED NO-LABEL
     filSecurity AT ROW 8.14 COL 3 COLON-ALIGNED NO-LABEL
     btnSecurity AT ROW 8.14 COL 41
     "Security procedure:" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 7.19 COL 5
     "Webservice name:" VIEW-AS TEXT
          SIZE 22 BY .71 AT ROW 1.48 COL 5
     "Code destination:" VIEW-AS TEXT
          SIZE 22 BY .71 AT ROW 3.38 COL 5
     "Server address:" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 5.29 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 10.14.

DEFINE FRAME fraGenerate
     tglWSDL AT ROW 1.48 COL 4
     tglProgram AT ROW 2.67 COL 4
     cboTemplate AT ROW 3.86 COL 16 COLON-ALIGNED
     tglSave AT ROW 5.29 COL 4
     filSave AT ROW 6.24 COL 6 COLON-ALIGNED NO-LABEL
     btnSave AT ROW 6.24 COL 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 10.14.

DEFINE FRAME fraLoad
     radLoad AT ROW 1.95 COL 3 NO-LABEL
     filLoadFile AT ROW 4.57 COL 4 COLON-ALIGNED NO-LABEL
     btnLoadFile AT ROW 4.57 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.6 BY 10.14.

DEFINE FRAME fraBegin
     edtBegin AT ROW 1.95 COL 50 NO-LABEL NO-TAB-STOP 
     imgBegin AT ROW 2.43 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.6 BY 10.14.

DEFINE FRAME fraEndError
     imgError AT ROW 1.95 COL 2
     RECT-4 AT ROW 1.95 COL 43
     "You have not completed the" VIEW-AS TEXT
          SIZE 38 BY .95 AT ROW 2.43 COL 44
     "webservice!" VIEW-AS TEXT
          SIZE 38 BY .95 AT ROW 3.86 COL 44
     "Go Back or press Cancel." VIEW-AS TEXT
          SIZE 30 BY 1.19 AT ROW 5.05 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 10.14.

DEFINE FRAME fraEndSuccess
     edtEndSuccess AT ROW 5.52 COL 44 NO-LABEL NO-TAB-STOP 
     imgSucces AT ROW 1.95 COL 2
     RECT-3 AT ROW 1.95 COL 43
     "You completed the webservice!" VIEW-AS TEXT
          SIZE 38 BY .95 AT ROW 3.86 COL 44
     "Congratulations!" VIEW-AS TEXT
          SIZE 38 BY .95 AT ROW 2.67 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84 BY 10.14.

DEFINE FRAME fraProcedures
     edtProcedures AT ROW 1.48 COL 58 NO-LABEL NO-TAB-STOP 
     broProcedures AT ROW 2.67 COL 6
     btnDefProcedures AT ROW 6.48 COL 58
     RECT-1 AT ROW 1.48 COL 4
     "Procedures:" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.71 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.6 BY 10.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW winWizard ASSIGN
         HIDDEN             = YES
         TITLE              = "WSGen wizard"
         HEIGHT             = 12.91
         WIDTH              = 84.8
         MAX-HEIGHT         = 35.67
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 35.67
         VIRTUAL-WIDTH      = 204.8
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT winWizard:LOAD-ICON("o4glws/transparent.ico":U) THEN
    MESSAGE "Unable to load icon: o4glws/transparent.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW winWizard
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME fraBegin:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fraEndError:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fraEndSuccess:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fraGenerate:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fraLoad:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fraSettings:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fraSettings:MOVE-BEFORE-TAB-ITEM (btnCancel:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME fraGenerate:MOVE-BEFORE-TAB-ITEM (FRAME fraSettings:HANDLE)
       XXTABVALXX = FRAME fraLoad:MOVE-BEFORE-TAB-ITEM (FRAME fraGenerate:HANDLE)
       XXTABVALXX = FRAME fraBegin:MOVE-BEFORE-TAB-ITEM (FRAME fraLoad:HANDLE)
       XXTABVALXX = FRAME fraEndError:MOVE-BEFORE-TAB-ITEM (FRAME fraBegin:HANDLE)
       XXTABVALXX = FRAME fraEndSuccess:MOVE-BEFORE-TAB-ITEM (FRAME fraEndError:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME fraBegin
                                                                        */
ASSIGN 
       edtBegin:READ-ONLY IN FRAME fraBegin        = TRUE.

/* SETTINGS FOR FRAME fraEndError
                                                                        */
/* SETTINGS FOR FRAME fraEndSuccess
                                                                        */
ASSIGN 
       edtEndSuccess:READ-ONLY IN FRAME fraEndSuccess        = TRUE.

/* SETTINGS FOR FRAME fraGenerate
                                                                        */
/* SETTINGS FOR FRAME fraLoad
                                                                        */
/* SETTINGS FOR BUTTON btnLoadFile IN FRAME fraLoad
   NO-ENABLE                                                            */
ASSIGN 
       btnLoadFile:HIDDEN IN FRAME fraLoad           = TRUE.

/* SETTINGS FOR FILL-IN filLoadFile IN FRAME fraLoad
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fraProcedures
                                                                        */
/* BROWSE-TAB broProcedures edtProcedures fraProcedures */
ASSIGN 
       edtProcedures:READ-ONLY IN FRAME fraProcedures        = TRUE.

/* SETTINGS FOR FRAME fraSettings
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winWizard)
THEN winWizard:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE broProcedures
/* Query rebuild information for BROWSE broProcedures
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProcedure.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE broProcedures */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME winWizard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winWizard winWizard
ON END-ERROR OF winWizard /* WSGen wizard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winWizard winWizard
ON WINDOW-CLOSE OF winWizard /* WSGen wizard */
DO:
  DEFINE VARIABLE vlogChoice AS LOGICAL    NO-UNDO.

  IF vintCurrentSlide < EXTENT(vhndSlide) - 2 THEN
  DO:
      MESSAGE "Are you sure that you want to cancel?" VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE vlogChoice.
      /* This event will close the window and terminate the procedure.  */
      IF vlogChoice THEN
      DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraGenerate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraGenerate winWizard
ON GO OF FRAME fraGenerate
DO:
  DEFINE VARIABLE vhndProjectDoc   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vhndProjectChild AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vchrName         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrFile         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrAddress      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrSecurity     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrResult       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vchrPath         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vintI            AS INTEGER    NO-UNDO.

  ASSIGN vlogContinue = FALSE.

  IF tglWSDL:CHECKED THEN
  DO WITH FRAME fraSettings:
      ASSIGN 
        vchrName = filName
        vchrFile = filDestination + "~\" + vchrName + ".wsdl"
        vchrAddress = filAddress + "~\" + vchrName.

      RUN o4glws/WSDL.p (
          INPUT TABLE ttProcedure,
          INPUT TABLE ttMethod,
          INPUT TABLE ttParam,
          INPUT TABLE ttTable,
          INPUT TABLE ttField,
          INPUT vchrFile,
          INPUT vchrName,
          INPUT vchrAddress,
          OUTPUT vchrResult
          ).

      IF vchrResult <> "" THEN
      DO:
        MESSAGE vchrResult VIEW-AS ALERT-BOX.
        RETURN.
      END.
  END.

  IF tglProgram:CHECKED THEN
  DO WITH FRAME fraSettings:
      ASSIGN 
        cboTemplate
        vchrName = filName
        vchrFile = filDestination + "/" + filName + "_" + cboTemplate
        vchrSecurity = filSecurity
        vchrPath = SEARCH("o4glws~\templates~\" + cboTemplate).

      RUN o4glws/Adapter.p(
          INPUT TABLE ttProcedure,
          INPUT TABLE ttMethod,
          INPUT TABLE ttParam,
          INPUT TABLE ttTable,
          INPUT TABLE ttField,
          INPUT vchrFile,
          INPUT vchrPath,
          INPUT vchrName,
          INPUT vchrSecurity,
          OUTPUT vchrResult
          ).

      IF vchrResult <> "" THEN
      DO:
        MESSAGE vchrResult VIEW-AS ALERT-BOX.
        RETURN.
      END.
  END.

  IF tglSave:CHECKED THEN
  DO:
      ASSIGN ERROR-STATUS:ERROR = FALSE.
      CREATE X-DOCUMENT vhndProjectDoc. 

      ASSIGN vhndProjectChild = createChild(vhndProjectDoc,"project","").
      RUN SAVE-STATE(vhndProjectChild,OUTPUT vlogContinue).

      vhndProjectDoc:SAVE("FILE",filSave:SCREEN-VALUE).
      DELETE OBJECT vhndProjectDoc. 

      IF ERROR-STATUS:ERROR THEN
      DO:
        DO vintI = 0 TO ERROR-STATUS:NUM-MESSAGES:
          vchrResult = vchrResult 
            + (IF vchrResult <> "" THEN ", " ELSE "")
            + ERROR-STATUS:GET-MESSAGE(vintI).
        END.
        MESSAGE "Project file is not valid!: " vchrResult VIEW-AS ALERT-BOX.
        RETURN.
      END.
  END.

  IF NOT (tglWSDL OR tglProgram OR tglSave) THEN
  DO:
    MESSAGE "You did not select anything!".
    RETURN.
  END.

  ASSIGN vlogContinue = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraLoad winWizard
ON GO OF FRAME fraLoad
DO:
  DEFINE VARIABLE vlogChoice       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE vhndProjectDoc   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vhndProjectChild AS HANDLE     NO-UNDO.

  ASSIGN vlogContinue = FALSE.

  IF VALID-HANDLE(vhndProjectDoc) THEN
  DO:
    MESSAGE "Do you want to startover?" VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE vlogChoice.

    IF NOT vlogChoice THEN
    DO:
      RETURN.
    END.

    DELETE OBJECT vhndProjectDoc NO-ERROR.
  END.

  CREATE X-DOCUMENT vhndProjectDoc. 

  /*Crear el documento o cargarlo desde el archivo*/
  IF radLoad THEN
  DO:
    ASSIGN vhndProjectChild = createChild(vhndProjectDoc,"Project","").
  END.
  ELSE
  DO:
    IF SEARCH(filLoadFile:SCREEN-VALUE) <> ? THEN
    DO:
      vhndProjectDoc:LOAD("FILE",filLoadFile:SCREEN-VALUE,FALSE).
      IF ERROR-STATUS:ERROR THEN
      DO:
        MESSAGE "Project file is not valid!" VIEW-AS ALERT-BOX.
        RETURN.
      END.

      ASSIGN vhndProjectChild = findChild(vhndProjectDoc,"Project").
      IF NOT VALID-HANDLE(vhndProjectChild) THEN
      DO:
        MESSAGE "Project file is not valid!" VIEW-AS ALERT-BOX.
        RETURN.
      END.
    END.
  END.

  RUN LOAD-STATE(vhndProjectChild,OUTPUT vlogContinue).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraProcedures
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraProcedures winWizard
ON GO OF FRAME fraProcedures
DO:
    ASSIGN vlogContinue = FALSE.

    IF NOT CAN-FIND(FIRST ttProcedure) THEN
    DO:
      MESSAGE "You have to define the procedures that will be published" VIEW-AS ALERT-BOX.
      RETURN.
    END.

    ASSIGN vlogContinue = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraSettings winWizard
ON GO OF FRAME fraSettings
DO:
  ASSIGN vlogContinue = FALSE.

  IF TRIM(filName:SCREEN-VALUE) = "" THEN
  DO:
    MESSAGE "You must supply a name" VIEW-AS ALERT-BOX.
    RETURN.
  END.

  IF TRIM(filDestination:SCREEN-VALUE) = "" THEN
  DO:
    MESSAGE "You must supply a directory" VIEW-AS ALERT-BOX.
    RETURN.
  END.

  IF TRIM(filAddress:SCREEN-VALUE) = "" THEN
  DO:
    MESSAGE "You must supply a server address" VIEW-AS ALERT-BOX.
    RETURN.
  END.

  ASSIGN vlogContinue = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBack winWizard
ON CHOOSE OF btnBack IN FRAME DEFAULT-FRAME /* < Back */
DO:
    IF vintCurrentSlide < EXTENT(vhndSlide) THEN
    DO:
      RUN SHOW-SLIDE(vintCurrentSlide - 1).
    END.
    ELSE
    DO:
      RUN SHOW-SLIDE(EXTENT(vhndSlide) - 2).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel winWizard
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraProcedures
&Scoped-define SELF-NAME btnDefProcedures
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefProcedures winWizard
ON CHOOSE OF btnDefProcedures IN FRAME fraProcedures /* Define Procedures */
DO:
  RUN o4glws/procDlg.w (
      INPUT-OUTPUT TABLE ttProcedure,
      INPUT-OUTPUT TABLE ttMethod,   
      INPUT-OUTPUT TABLE ttParam,    
      INPUT-OUTPUT TABLE ttTable,    
      INPUT-OUTPUT TABLE ttField
      ).

  /*ENABLE broProcedures WITH FRAME fraProcedures. */
  OPEN QUERY broProcedures FOR EACH ttProcedure NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraSettings
&Scoped-define SELF-NAME btnDestination
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDestination winWizard
ON CHOOSE OF btnDestination IN FRAME fraSettings /* ... */
DO:
    DEFINE VARIABLE vchrFolder AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vlogCancel AS LOGICAL    NO-UNDO.
     
    RUN BrowseForFolder ("Code destination",
                           OUTPUT vchrFolder, 
                           OUTPUT vlogCancel).
     
    IF NOT vlogCancel THEN
    DO WITH FRAME fraSettings:
      ASSIGN filDestination = vchrFolder.
      DISPLAY filDestination.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnFinish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFinish winWizard
ON CHOOSE OF btnFinish IN FRAME DEFAULT-FRAME /* Finish */
DO:
   RUN SHOW-SLIDE(EXTENT(vhndSlide) - (IF vlogContinue THEN 1 ELSE 0)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraLoad
&Scoped-define SELF-NAME btnLoadFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoadFile winWizard
ON CHOOSE OF btnLoadFile IN FRAME fraLoad /* ... */
DO:
    DEFINE VAR l_ok              AS LOGICAL             NO-UNDO.
    DEFINE VAR filename          AS CHARACTER           NO-UNDO.
    DEFINE VAR Filter_NameString AS CHARACTER           NO-UNDO.
    DEFINE VAR Filter_FileSpec   LIKE Filter_NameString NO-UNDO.

    /* Initialize the file filters */
    ASSIGN Filter_NameString = "Project files (*.xml)"
           Filter_FileSpec   = "*.xml". 

    /* Ask for a file name. NOTE: File-names to run must exist */                          
    filename = filLoadFile:SCREEN-VALUE.
    SYSTEM-DIALOG GET-FILE filename
        TITLE    "Open project"
        FILTERS  Filter_NameString   Filter_FileSpec
        MUST-EXIST
        UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
    IF l_ok THEN DO:
      filLoadFile:SCREEN-VALUE = filename.
      APPLY "RETURN":U TO filLoadFile.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext winWizard
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* Next > */
DO:
    ASSIGN vlogContinue = TRUE.

    APPLY "GO" TO vhndSlide[vintCurrentSlide].
    
    IF vintCurrentSlide < EXTENT(vhndSlide) - 2 THEN
    DO:
      IF vlogContinue THEN
      DO:
          RUN SHOW-SLIDE(vintCurrentSlide + 1).
      END.
    END.
    ELSE
    DO:
      APPLY "CHOOSE" TO btnFinish.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraGenerate
&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave winWizard
ON CHOOSE OF btnSave IN FRAME fraGenerate /* ... */
DO:
    DEFINE VAR l_ok              AS LOGICAL             NO-UNDO.
    DEFINE VAR filename          AS CHARACTER           NO-UNDO.
    DEFINE VAR Filter_NameString AS CHARACTER           NO-UNDO.
    DEFINE VAR Filter_FileSpec   LIKE Filter_NameString NO-UNDO.

    /* Initialize the file filters */
    ASSIGN Filter_NameString = "Project files (*.xml)"
           Filter_FileSpec   = "*.xml". 

    /* Ask for a file name. NOTE: File-names to run must exist */                          
    filename = filSave:SCREEN-VALUE.
    SYSTEM-DIALOG GET-FILE filename
        TITLE    "Save project"
        FILTERS  Filter_NameString   Filter_FileSpec
        SAVE-AS
        UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
    IF l_ok THEN DO:
      filSave:SCREEN-VALUE = filename.
      APPLY "RETURN":U TO filSave.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraSettings
&Scoped-define SELF-NAME btnSecurity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSecurity winWizard
ON CHOOSE OF btnSecurity IN FRAME fraSettings /* ... */
DO:
  DEFINE VAR l_ok              AS LOGICAL             NO-UNDO.
  DEFINE VAR filename          AS CHARACTER           NO-UNDO.

  /* Ask for a file name. NOTE: File-names to run must exist */                          
  filename = filSecurity:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE filename
      TITLE    "Security procedure"
      FILTERS  "Source files (*.p)" "*.p",
               "R-code files (*.r)" "*.r"
      MUST-EXIST
      UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  

  IF l_ok THEN DO:
    filSecurity:SCREEN-VALUE = ToPropath(filename).
    APPLY "RETURN":U TO filSecurity.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fraLoad
&Scoped-define SELF-NAME radLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radLoad winWizard
ON VALUE-CHANGED OF radLoad IN FRAME fraLoad
DO:
  DISPLAY radLoad:SCREEN-VALUE.

  ASSIGN 
    radLoad
    filLoadFile:SENSITIVE = NOT radLoad
    btnLoadFile:SENSITIVE = NOT radLoad
    btnLoadFile:VISIBLE = NOT radLoad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME broProcedures
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK winWizard 


/* ***************************  Main Block  *************************** */

ASSIGN 
  vhndSlide[1] = FRAME fraBegin:HANDLE
  vhndSlide[2] = FRAME fraLoad:HANDLE
  vhndSlide[3] = FRAME fraProcedures:HANDLE
  vhndSlide[4] = FRAME fraSettings:HANDLE
  vhndSlide[5] = FRAME fraGenerate:HANDLE
  vhndSlide[6] = FRAME fraEndSuccess:HANDLE
  vhndSlide[7] = FRAME fraEndError:HANDLE.

RUN LOAD-TEMPLATES.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN SHOW-SLIDE(1).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseForFolder winWizard 
PROCEDURE BrowseForFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER DialogTitle AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER FolderName  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Canceled    AS LOGICAL NO-UNDO.
 
DEF VAR MAX_PATH       AS INTEGER INITIAL 260.
DEF VAR lpbi           AS MEMPTR.  /* pointer to BROWSEINFO structure */
DEF VAR pszDisplayName AS MEMPTR.
DEF VAR lpszTitle      AS MEMPTR.
DEF VAR lpItemIDList   AS INTEGER NO-UNDO.
DEF VAR ReturnValue    AS INTEGER NO-UNDO.
 
SET-SIZE(lpbi)           = 32.
SET-SIZE(pszDisplayName) = MAX_PATH.
SET-SIZE(lpszTitle)      = LENGTH(DialogTitle) + 1.
 
PUT-STRING(lpszTitle,1)  = DialogTitle.
 
PUT-LONG(lpbi, 1) = 0.  /* hwnd for parent */
PUT-LONG(lpbi, 5) = 0.
PUT-LONG(lpbi, 9) = GET-POINTER-VALUE(pszDisplayName).
PUT-LONG(lpbi,13) = GET-POINTER-VALUE(lpszTitle).
PUT-LONG(lpbi,17) = 1. /* BIF_RETURNONLYFSDIRS = only accept a file system directory */
PUT-LONG(lpbi,21) = 0. /* lpfn, callback function */
PUT-LONG(lpbi,25) = 0. /* lParam for lpfn */
PUT-LONG(lpbi,29) = 0.
 
RUN SHBrowseForFolder ( INPUT  GET-POINTER-VALUE(lpbi), OUTPUT lpItemIDList).
 
/* parse the result: */
IF lpItemIDList=0 THEN DO:
   Canceled   = YES.
   FolderName = "".
END.
ELSE DO:
   Canceled = NO.
   FolderName = FILL(" ", MAX_PATH).
   RUN SHGetPathFromIDList(lpItemIDList, OUTPUT FolderName, OUTPUT ReturnValue).
   FolderName = TRIM(FolderName).
END.   
 
/* free memory: */
SET-SIZE(lpbi)=0.
SET-SIZE(pszDisplayName)=0.
SET-SIZE(lpszTitle)=0.
RUN CoTaskMemFree (lpItemIDList).
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI winWizard  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winWizard)
  THEN DELETE WIDGET winWizard.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI winWizard  _DEFAULT-ENABLE
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
  ENABLE btnCancel btnBack btnNext btnFinish 
      WITH FRAME DEFAULT-FRAME IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY edtBegin 
      WITH FRAME fraBegin IN WINDOW winWizard.
  ENABLE edtBegin imgBegin 
      WITH FRAME fraBegin IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraBegin}
  ENABLE imgError RECT-4 
      WITH FRAME fraEndError IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraEndError}
  DISPLAY edtEndSuccess 
      WITH FRAME fraEndSuccess IN WINDOW winWizard.
  ENABLE edtEndSuccess imgSucces RECT-3 
      WITH FRAME fraEndSuccess IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraEndSuccess}
  DISPLAY tglWSDL tglProgram cboTemplate tglSave filSave 
      WITH FRAME fraGenerate IN WINDOW winWizard.
  ENABLE tglWSDL tglProgram cboTemplate tglSave filSave btnSave 
      WITH FRAME fraGenerate IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraGenerate}
  DISPLAY radLoad filLoadFile 
      WITH FRAME fraLoad IN WINDOW winWizard.
  ENABLE radLoad 
      WITH FRAME fraLoad IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraLoad}
  DISPLAY edtProcedures 
      WITH FRAME fraProcedures IN WINDOW winWizard.
  ENABLE edtProcedures broProcedures btnDefProcedures RECT-1 
      WITH FRAME fraProcedures IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraProcedures}
  DISPLAY filName filDestination filAddress filSecurity 
      WITH FRAME fraSettings IN WINDOW winWizard.
  ENABLE filName filDestination btnDestination filAddress filSecurity 
         btnSecurity 
      WITH FRAME fraSettings IN WINDOW winWizard.
  {&OPEN-BROWSERS-IN-QUERY-fraSettings}
  VIEW winWizard.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LOAD-STATE winWizard 
PROCEDURE LOAD-STATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER vhndProjectChild AS HANDLE     NO-UNDO.
DEFINE OUTPUT PARAMETER vlogContinue     AS LOGICAL    NO-UNDO.

DEFINE VARIABLE hndTable AS HANDLE    NO-UNDO.

DO WITH FRAME fraSettings:
  ASSIGN 
    filName = getInCharacter(vhndProjectChild,"name")
    filDestination = getInCharacter(vhndProjectChild,"destination")
    filAddress = getInCharacter(vhndProjectChild,"address")
    filSecurity = getInCharacter(vhndProjectChild,"security").
  DISPLAY filName filDestination filAddress filSecurity.
END.

DO WITH FRAME fraProcedures:
    FOR EACH ttProcedure:
      DELETE ttProcedure.
    END.

    ASSIGN hndTable = BUFFER ttProcedure:HANDLE.
    getInTable(vhndProjectChild,"ProcedureArray",hndTable).

    FOR EACH ttMethod:
      DELETE ttMethod.
    END.

    ASSIGN hndTable = BUFFER ttMethod:HANDLE.
    getInTable(vhndProjectChild,"MethodArray",hndTable).

    FOR EACH ttParam:
      DELETE ttParam.
    END.

    ASSIGN hndTable = BUFFER ttParam:HANDLE.
    getInTable(vhndProjectChild,"ParamArray",hndTable).

    FOR EACH ttTable:
      DELETE ttTable.
    END.

    ASSIGN hndTable = BUFFER ttTable:HANDLE.
    getInTable(vhndProjectChild,"TableArray",hndTable).

    FOR EACH ttField:
      DELETE ttField.
    END.

    ASSIGN hndTable = BUFFER ttField:HANDLE.
    getInTable(vhndProjectChild,"FieldArray",hndTable).

    OPEN QUERY broProcedures FOR EACH ttProcedure NO-LOCK.
END.

DO WITH FRAME fraGenerate:
  ASSIGN 
    tglWSDL = getInLogical(vhndProjectChild,"wsdl")
    tglProgram = getInLogical(vhndProjectChild,"program")
    tglSave = getInLogical(vhndProjectChild,"save")
    filSave = getInCharacter(vhndProjectChild,"file").

  cboTemplate:SCREEN-VALUE = getInCharacter(vhndProjectChild,"template").

  DISPLAY tglWSDL tglProgram tglSave filSave.
END.

ASSIGN vlogContinue = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LOAD-TEMPLATES winWizard 
PROCEDURE LOAD-TEMPLATES :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE vchrPath    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vchrName    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vchrAttribs AS CHARACTER  NO-UNDO.

ASSIGN 
  vchrPath = SEARCH("o4glws~\templates~\web.html")
  vchrPath = SUBSTRING(vchrPath,1,R-INDEX(vchrPath,"~\"))
  FILE-INFO:FILE-NAME = vchrPath.

IF FILE-INFO:FILE-TYPE = ? THEN
DO:
  MESSAGE "templates directory cannot be found!" VIEW-AS ALERT-BOX.
  RETURN.
END.

INPUT STREAM strDir FROM OS-DIR(vchrPath).

REPEAT WITH FRAME fraGenerate:
  IMPORT STREAM strDir vchrName vchrPath vchrAttribs.
  IF INDEX(vchrAttribs,"F") > 0 THEN
  DO:
    cboTemplate:ADD-LAST(vchrName).
  END.
END.

INPUT STREAM strDir CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SAVE-STATE winWizard 
PROCEDURE SAVE-STATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER vhndProjectChild AS HANDLE     NO-UNDO.
DEFINE OUTPUT PARAMETER vlogContinue     AS LOGICAL    NO-UNDO.

DEFINE VARIABLE hndTable AS HANDLE    NO-UNDO.

DO WITH FRAME fraSettings:
  ASSIGN filName filDestination filAddress filSecurity.
  setOutCharacter(vhndProjectChild,"name",filName).
  setOutCharacter(vhndProjectChild,"destination",filDestination).
  setOutCharacter(vhndProjectChild,"address",filAddress).
  setOutCharacter(vhndProjectChild,"security",filSecurity).
END.

DO WITH FRAME fraProcedures:
  ASSIGN hndTable = BUFFER ttProcedure:HANDLE.
  setOutTable(vhndProjectChild,"ProcedureArray",hndTable).

  ASSIGN hndTable = BUFFER ttMethod:HANDLE.
  setOutTable(vhndProjectChild,"MethodArray",hndTable).

  ASSIGN hndTable = BUFFER ttParam:HANDLE.
  setOutTable(vhndProjectChild,"ParamArray",hndTable).

  ASSIGN hndTable = BUFFER ttTable:HANDLE.
  setOutTable(vhndProjectChild,"TableArray",hndTable).

  ASSIGN hndTable = BUFFER ttField:HANDLE.
  setOutTable(vhndProjectChild,"FieldArray",hndTable).
END.

DO WITH FRAME fraGenerate:
  ASSIGN tglWSDL tglProgram cboTemplate tglSave filSave.
  setOutLogical(vhndProjectChild,"wsdl",tglWSDL).
  setOutLogical(vhndProjectChild,"program",tglProgram).
  setOutCharacter(vhndProjectChild,"template",cboTemplate).
  setOutLogical(vhndProjectChild,"save",tglSave).
  setOutCharacter(vhndProjectChild,"file",filSave).
END.

ASSIGN vlogContinue = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHOW-SLIDE winWizard 
PROCEDURE SHOW-SLIDE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipintSlide AS INTEGER    NO-UNDO.
    
    DEFINE VARIABLE vintI AS INTEGER    NO-UNDO.
    
    DO vintI = 1 TO EXTENT(vhndSlide):
      ASSIGN vhndSlide[vintI]:VISIBLE = (vintI = ipintSlide).
    END.
    ASSIGN vintCurrentSlide = ipintSlide.
    
    DO WITH FRAME DEFAULT-FRAME:
        ASSIGN 
          btnBack:SENSITIVE = NOT (ipintSlide = 1)
          btnNext:SENSITIVE = NOT (ipintSlide >= EXTENT(vhndSlide) - 1)
          btnFinish:SENSITIVE = NOT (ipintSlide = 1 OR ipintSlide >= EXTENT(vhndSlide) - 1).
    END.
    
    ASSIGN winWizard:TITLE = vchrTitle + " - Page " + STRING(ipintSlide - (IF ipintSlide = EXTENT(vhndSlide) THEN 1 ELSE 0)) + " of " + STRING(EXTENT(vhndSlide) - 1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToPropath winWizard 
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

