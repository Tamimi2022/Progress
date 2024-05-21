&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Salesrep

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Salesrep.SalesRep ~
Salesrep.RepName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Salesrep.SalesRep ~
Salesrep.RepName 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Salesrep SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Salesrep SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.CustNum Customer.Name ~
Customer.Contact Customer.Country Customer.City Customer.Address ~
Customer.PostalCode Customer.Balance Customer.EmailAddress Customer.Phone ~
Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define ENABLED-TABLES Customer Salesrep
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-define SECOND-ENABLED-TABLE Salesrep
&Scoped-Define ENABLED-OBJECTS BtnOK BtnCancel 
&Scoped-Define DISPLAYED-FIELDS Customer.CustNum Customer.Name ~
Customer.Contact Customer.Country Customer.City Customer.Address ~
Customer.PostalCode Customer.Balance Customer.EmailAddress Customer.Phone ~
Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define DISPLAYED-TABLES Customer Salesrep
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-define SECOND-DISPLAYED-TABLE Salesrep


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 17 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 17 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      Salesrep SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Customer.CustNum AT ROW 1.71 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     BtnOK AT ROW 1.71 COL 62 WIDGET-ID 34
     Customer.Name AT ROW 2.91 COL 17 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 14 
     BtnCancel AT ROW 2.91 COL 62 WIDGET-ID 36
     Customer.Contact AT ROW 4.1 COL 17 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     Customer.Country AT ROW 5.29 COL 17 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 14 
     Customer.City AT ROW 6.48 COL 17 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 14 
     Customer.Address AT ROW 7.67 COL 17 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
          BGCOLOR 14 
     Customer.PostalCode AT ROW 8.86 COL 17 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 14 
     Customer.Balance AT ROW 10.05 COL 17 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     Customer.EmailAddress AT ROW 11.24 COL 17 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     Customer.Phone AT ROW 12.43 COL 17 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     Salesrep.SalesRep AT ROW 13.62 COL 17 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "HXM","HXM",
                     "DKP"," DKP",
                     "SLS"," SLS",
                     "JAL"," JAL",
                     "RDR"," RDR",
                     "DOS"," DOS",
                     "GPE"," GPE",
                     "KIK"," KIK"
          DROP-DOWN-LIST
          SIZE 15 BY 1
     Salesrep.RepName AT ROW 13.62 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     Customer.Comments AT ROW 14.81 COL 19 NO-LABEL WIDGET-ID 26
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 52 BY 4
     "Comments" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 14.81 COL 6 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 18.48
         FONT 6
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Customer Maintainance>"
         HEIGHT             = 18.48
         WIDTH              = 80
         MAX-HEIGHT         = 19.91
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 19.91
         VIRTUAL-WIDTH      = 80
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       Salesrep.RepName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "sports2000.Salesrep"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* <Customer Maintainance> */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON window-close OF C-Win /* <Customer Maintainance> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON choose OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
do:
    apply "close":u to this-procedure .  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON choose OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
do:
    if Customer.Name:screen-value = "" or Customer.Country:screen-value = "" or Customer.City:screen-value = "" or Customer.Address:screen-value = "" or Customer.PostalCode:screen-value = ""
        then 
    do:
        message "Everything in the yellow box are required fields." view-as alert-box.
        return no-apply.
    end.

    find first Customer where Customer.Name = Customer.Name:screen-value no-error.
    if available Customer then 
    do:
        assign
            Customer.Name       = Customer.Name:screen-value
            Customer.Country    = Customer.Country:screen-value
            Customer.City       = Customer.City:screen-value
            Customer.Address    = Customer.Address:screen-value
            Customer.PostalCode = Customer.PostalCode:screen-value .

        message "Customer record updated." view-as alert-box.
    end .
    else 
    do:
        create Customer.
        assign
            Customer.Name       = Customer.Name:screen-value
            Customer.Country    = Customer.Country:screen-value
            Customer.City       = Customer.City:screen-value
            Customer.Address    = Customer.Address:screen-value
            Customer.PostalCode = Customer.PostalCode:screen-value .
        message "Welcome New Customer" skip Customer.Name skip Customer.Country view-as alert-box.
    end.

    /* Hypothetical call to refresh other UI components */
    run initialize .

    apply "CLOSE":U to this-procedure.
    return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  run enable_UI.
  run initialize .
  if not this-procedure:persistent then
    wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomerChanged C-Win 
PROCEDURE CustomerChanged :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter piiCustNum as integer no-undo  .
    
        find first Customer where Customer.CustNum = piiCustNum no-lock no-error .
        find first Salesrep where Salesrep.SalesRep = Customer.SalesRep no-lock no-error .
        
            display {&DISPLAYED-FIELDS} with frame {&frame-name} .

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.CustNum Customer.Name Customer.Contact Customer.Country 
          Customer.City Customer.Address Customer.PostalCode Customer.Balance 
          Customer.EmailAddress Customer.Phone Customer.Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Salesrep THEN 
    DISPLAY Salesrep.SalesRep Salesrep.RepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Customer.CustNum BtnOK Customer.Name BtnCancel Customer.Contact 
         Customer.Country Customer.City Customer.Address Customer.PostalCode 
         Customer.Balance Customer.EmailAddress Customer.Phone 
         Salesrep.SalesRep Salesrep.RepName Customer.Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize C-Win 
PROCEDURE Initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    subscribe to "CustomerChanged" anywhere .

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

