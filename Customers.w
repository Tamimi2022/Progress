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
define variable hOrders          as handle no-undo . // For handle event Orders .
define variable hCustomerDetails as handle no-undo . // For handle event Customer Details .
define variable hCustomerMaint   as handle no-undo . // For handle event Customer Maintainance .

define variable iRow     as integer no-undo . // For SetButtons procedure .
define variable iNumRows as integer no-undo . // For SetButtons Procedure .

define variable hQuery  as handle    no-undo . // For SetSearchCustomers .
define variable cClause as character no-undo . // For SetSearchCustomers .

define variable lAnswer as logical no-undo . // For Delete .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brCustomer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Salesrep Order

/* Definitions for BROWSE brCustomer                                    */
&Scoped-define FIELDS-IN-QUERY-brCustomer Customer.CustNum Customer.Name ~
Customer.Country 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomer 
&Scoped-define QUERY-STRING-brCustomer FOR EACH Customer NO-LOCK
&Scoped-define OPEN-QUERY-brCustomer OPEN QUERY brCustomer FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Salesrep.SalesRep ~
Salesrep.RepName Order.Ordernum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Salesrep.SalesRep ~
Salesrep.RepName Order.Ordernum 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Salesrep Order
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Order
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomer}
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Salesrep SHARE-LOCK, ~
      EACH Order OF Salesrep SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Salesrep SHARE-LOCK, ~
      EACH Order OF Salesrep SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Salesrep Order
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define SECOND-TABLE-IN-QUERY-DEFAULT-FRAME Order


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.CustNum Customer.Name ~
Customer.Country Salesrep.SalesRep Salesrep.RepName Order.Ordernum 
&Scoped-define ENABLED-TABLES Customer Salesrep Order
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-define SECOND-ENABLED-TABLE Salesrep
&Scoped-define THIRD-ENABLED-TABLE Order
&Scoped-Define ENABLED-OBJECTS BtnFirst BtnPrev BtnNext BtnLast brCustomer ~
btnOrders 
&Scoped-Define DISPLAYED-FIELDS Customer.CustNum Customer.Name ~
Customer.Country Salesrep.SalesRep Salesrep.RepName Order.Ordernum 
&Scoped-define DISPLAYED-TABLES Customer Salesrep Order
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-define SECOND-DISPLAYED-TABLE Salesrep
&Scoped-define THIRD-DISPLAYED-TABLE Order


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SearchCustomers C-Win 
function SearchCustomers returns character
  (  ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Menu Definitions                                                     */
define sub-menu m_File 
       menu-item m_View         label "View"          
       menu-item m_New          label "New"           
       menu-item m_Modify       label "Modify"        
       menu-item m_Delete       label "Delete"        
       rule
       menu-item m_Exit         label "Exit"          .

define sub-menu m_Navigate 
       menu-item m_First        label "First"         
       menu-item m_Next         label "Next"          
       rule
       menu-item m_Prev         label "Prev"          
       menu-item m_Last         label "Last"          .

define menu MENU-BAR-C-Win menubar
       sub-menu  m_File         label "File"          
       sub-menu  m_Navigate     label "Navigate"      .


/* Definitions of the field level widgets                               */
define button BtnFirst 
     label "&First" 
     size 15 by 1.14
     bgcolor 8 font 6.

define button BtnLast 
     label "&Last" 
     size 15 by 1.14
     bgcolor 8 font 6.

define button BtnNext 
     label "&Next" 
     size 15 by 1.14
     bgcolor 8 font 6.

define button btnOrders 
     label "View Orders" 
     size 15 by 1.14.

define button BtnPrev 
     label "&Prev" 
     size 15 by 1.14
     bgcolor 8 font 6.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brCustomer for 
      Customer scrolling.

define query DEFAULT-FRAME for 
      Salesrep, 
      Order scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomer C-Win _STRUCTURED
  query brCustomer no-lock display
      Customer.CustNum format ">>>>9":U label-font 6
      Customer.Name format "x(30)":U label-font 6
      Customer.Country format "x(20)":U width 29.6 label-font 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 8.57
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     BtnFirst at row 1.48 col 4 widget-id 2
     BtnPrev at row 1.48 col 20 widget-id 4
     BtnNext at row 1.48 col 36 widget-id 6
     BtnLast at row 1.48 col 52 widget-id 8
     Customer.CustNum at row 2.91 col 2 colon-aligned no-label widget-id 18
          view-as fill-in 
          size 12 by 1
     Customer.Name at row 2.91 col 14 colon-aligned no-label widget-id 20
          view-as fill-in 
          size 37 by 1
     Customer.Country at row 2.91 col 51 colon-aligned no-label widget-id 22
          view-as fill-in 
          size 23 by 1
     brCustomer at row 3.86 col 4 widget-id 200
     Salesrep.SalesRep at row 12.91 col 18 colon-aligned widget-id 10
          view-as fill-in 
          size 14 by 1
          font 6
     Salesrep.RepName at row 14.33 col 18 colon-aligned widget-id 12
          view-as fill-in 
          size 43 by 1
     btnOrders at row 15.52 col 64 widget-id 16
     Order.Ordernum at row 15.76 col 18 colon-aligned widget-id 26
          view-as fill-in 
          size 14 by 1
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 80 by 16.1
         font 6 widget-id 100.


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
if session:display-type = "GUI":U then
  create window C-Win assign
         hidden             = yes
         title              = "<Customer Overview>"
         height             = 16.14
         width              = 80
         max-height         = 16.24
         max-width          = 80
         virtual-height     = 16.24
         virtual-width      = 80
         resize             = yes
         scroll-bars        = no
         status-area        = no
         bgcolor            = ?
         fgcolor            = ?
         keep-frame-z-order = yes
         three-d            = yes
         font               = 6
         message-area       = no
         sensitive          = yes.
else {&WINDOW-NAME} = current-window.

assign {&WINDOW-NAME}:MENUBAR    = menu MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brCustomer Country DEFAULT-FRAME */
assign 
       Order.Ordernum:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Salesrep.RepName:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Salesrep.SalesRep:READ-ONLY in frame DEFAULT-FRAME        = true.

if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomer
/* Query rebuild information for BROWSE brCustomer
     _TblList          = "sports2000.Customer"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > sports2000.Customer.CustNum
"Customer.CustNum" ? ? "integer" ? ? ? ? ? 6 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > sports2000.Customer.Name
"Customer.Name" ? ? "character" ? ? ? ? ? 6 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > sports2000.Customer.Country
"Customer.Country" ? ? "character" ? ? ? ? ? 6 no ? no no "29.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brCustomer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "sports2000.Salesrep,sports2000.Order OF sports2000.Salesrep"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* <Customer Overview> */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* <Customer Overview> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCustomer
&Scoped-define SELF-NAME brCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomer C-Win
on default-action of brCustomer in frame DEFAULT-FRAME
do:
    run OpenCustomerMaint .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomer C-Win
on value-changed of brCustomer in frame DEFAULT-FRAME
do:
    // For SalesRep in Customer
    find first Salesrep no-lock where Salesrep.SalesRep = Customer.SalesRep no-error .
        if available Salesrep then
            display Salesrep.SalesRep Salesrep.RepName with frame {&frame-name} .
    publish "GetCustomer" .
    publish "CustomerChanged" (input Customer.CustNum) .
    //publish "fetchNext":U.
    run SetButtons .
    
    // For Aantal Orders
    find Order where Order.Ordernum = Customer.CustNum no-error .
        if available Order then
            display Order.Ordernum with frame {&frame-name} .
            
            // For Mouse pointer
            brCustomer:load-mouse-pointer ("Glove") .            
            
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFirst C-Win
on choose of BtnFirst in frame DEFAULT-FRAME /* First */
do:
/*  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN                              */
/*    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN                                   */
/*      RUN notify IN THIS-PROCEDURE ("get-first") NO-ERROR.                   */
/*    &ELSE                                                                    */
/*      PUBLISH "fetchFirst":U.                                                */
/*    &ENDIF                                                                   */
/*  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN                     */
/*    /* This is a simple FIRST RECORD navigation button, useful for building  */
/*       test screens quickly.  NOTE: if there are no tables in the query, then*/
/*       this code will not compile; so use the preprocessor to skip it. */    */
        // Click the records to First
            apply "HOME" to {&BROWSE-NAME} .
            apply "value-changed" to browse {&BROWSE-NAME}.
/*      define variable op-supported as logical.                              */
/*      get first {&FRAME-NAME}.                                              */
/*      if available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} then do:           */
/*          display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.*/
/*          {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}                           */
/*      end.                                                                  */
/*  &ENDIF                                                                    */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast C-Win
on choose of BtnLast in frame DEFAULT-FRAME /* Last */
do:
/*  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN                              */
/*    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN                                   */
/*      RUN notify IN THIS-PROCEDURE ("get-last") NO-ERROR.                    */
/*    &ELSE                                                                    */
/*      PUBLISH "fetchLast":U.                                                 */
/*    &ENDIF                                                                   */
/*  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN                     */
/*    /* This is a simple LAST RECORD navigation button, useful for building   */
/*       test screens quickly.  NOTE: if there are no tables in the query, then*/
/*       this code will not compile; so use the preprocessor to skip it. */    */
        //Click the records to Last
            browse {&BROWSE-NAME}:query:get-last ().
            query {&BROWSE-NAME}:reposition-to-rowid (rowid(Customer)).
            apply "value-changed" to browse {&BROWSE-NAME}.
/*      get last {&FRAME-NAME}.                                        */
/*      if available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} then do:    */
/*   display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.*/
/*   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}                           */
/*      end.                                                           */
/*  &ENDIF                                                             */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext C-Win
on choose of BtnNext in frame DEFAULT-FRAME /* Next */
do:
/*  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN                              */
/*    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN                                   */
/*      RUN notify IN THIS-PROCEDURE ("get-next") NO-ERROR.                    */
/*    &ELSE                                                                    */
/*      PUBLISH "fetchNext":U.                                                 */
/*    &ENDIF                                                                   */
/*  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN                     */
/*    /* This is a simple NEXT RECORD navigation button, useful for building   */
/*       test screens quickly.  NOTE: if there are no tables in the query, then*/
/*       this code will not compile; so use the preprocessor to skip it. */    */
        // Click the records to Next
            browse {&BROWSE-NAME}:select-next-row().
            apply "value-changed" to browse {&BROWSE-NAME}.
/*      get next {&FRAME-NAME}.                                               */
/*      if not available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}}                */
/*          then get last {&FRAME-NAME}.                                      */
/*      if available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} then do:           */
/*          display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.*/
/*   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}                                  */
/*      end.                                                                  */

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrders C-Win
on choose of btnOrders in frame DEFAULT-FRAME /* View Orders */
do:
    if not valid-handle (hOrders) then
        run Orders.w persistent set hOrders .
    publish "CustomerChanged" (input Customer.CustNum) .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev C-Win
on choose of BtnPrev in frame DEFAULT-FRAME /* Prev */
do:
/*  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN                              */
/*    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN                                   */
/*      RUN notify IN THIS-PROCEDURE ("get-prev") NO-ERROR.                    */
/*    &ELSE                                                                    */
/*      PUBLISH "fetchPrev":U.                                                 */
/*    &ENDIF                                                                   */
/*  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN                     */
/*    /* This is a simple PREV RECORD navigation button, useful for building   */
/*       test screens quickly.  NOTE: if there are no tables in the query, then*/
/*       this code will not compile; so use the preprocessor to skip it. */    */
        // Click the records to Prev
            browse {&BROWSE-NAME}:select-prev-row().
            apply "value-changed" to browse {&BROWSE-NAME}.
/*      get prev {&FRAME-NAME}.                                      */
/*      if not available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}}       */
/*      then get first {&FRAME-NAME}.                                */
/*      if available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} then do:  */
/* display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.*/
/*        {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}                    */
/*      end.                                                         */
/*  &ENDIF                                                           */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Country C-Win
on value-changed of Customer.Country in frame DEFAULT-FRAME /* Country */
do:
        run SetSearchCustomers("Customer", "").
        apply "value-changed" to {&browse-name} .
            
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.CustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.CustNum C-Win
on value-changed of Customer.CustNum in frame DEFAULT-FRAME /* Cust Num */
do:
        run SetSearchCustomers("Customer", "").
        apply "value-changed" to {&browse-name} .
    
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete C-Win
on choose of menu-item m_Delete /* Delete */
do:
        define buffer bCust for Customer.
        
        if available (Customer) then
        do: 
            message "Are you more than a 1000000000% sure that you want to" skip 
                "delete this record?"
                view-as alert-box question buttons yes-no update lAnswer .                    
            if lanswer then 
            do:
                find bCust where rowid(bCust) = Rowid(Customer) exclusive-lock.
                delete bCust no-error.
                if not error-status:error then
                    browse {&BROWSE-NAME}:delete-current-row () .
            end.    
        end .
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
on choose of menu-item m_Exit /* Exit */
do: 
        /* Assume that handles hCustomerDetails, hOrders, hCustomerMaint represent the windows to close */
        if valid-handle(hCustomerDetails) then
            apply "CLOSE" to hCustomerDetails.

        if valid-handle(hOrders) then
            apply "CLOSE" to hOrders.

        if valid-handle(hCustomerMaint) then
            apply "CLOSE" to hCustomerMaint.
        
        // The last Window after Message box appear .
        if available Customer then 
            message "Good Bye"
                view-as alert-box.
        apply "close" to this-procedure .
        return .
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_First C-Win
on choose of menu-item m_First /* First */
do:                                                       
        // Click the records to First in Navigate 
        apply "HOME" to browse {&browse-name} .  
        run SetButtons.
        // Enabling and Disabling menu-item
/*        menu-item m_First:sensitive  */
/*        in menu menu-bar-c-win = no .*/
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Last C-Win
on choose of menu-item m_Last /* Last */
do:                                                       
        // Click the records to Last in Navigate 
        apply "END" to browse {&browse-name} .  
        run SetButtons.           
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Modify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Modify C-Win
on choose of menu-item m_Modify /* Modify */
do:
        run OpenCustomerMaint .
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_New C-Win
on choose of menu-item m_New /* New */
do:
        run CustomerMaint.w .
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Next C-Win
on choose of menu-item m_Next /* Next */
do:                                                       
        // Click the records to Next in Navigate 
        apply "CURSOR-DOWN" to browse {&browse-name} .  
        run SetButtons.           
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Prev C-Win
on choose of menu-item m_Prev /* Prev */
do:                                                       
        // Click the records to Prev in Navigate 
        apply "CURSOR-UP" to browse {&browse-name} . 
        run SetButtons.            
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_View
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_View C-Win
on choose of menu-item m_View /* View */
do:
        if not valid-handle (hCustomerDetails) then
        do:
            run CustomerDetails.w persistent set hCustomerDetails .
            //run CustomerDetails.w (input false) .
            publish "CustomerChanged" (input Customer.CustNum) .
            
            // Subscribe all buttons from CustomerDetails
            subscribe "getFirstCustomer" in hCustomerDetails .
            subscribe "getPrevCustomer" in hCustomerDetails .
            subscribe "getNextCustomer" in hCustomerDetails .
            subscribe "getLastCustomer" in hCustomerDetails .
        end .
    apply "value-changed" to brCustomer in frame default-frame.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Name C-Win
on value-changed of Customer.Name in frame DEFAULT-FRAME /* Name */
do:
        run SetSearchCustomers (input "Customer", "").
        apply "value-changed" to {&browse-name} .
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
    run SetButtons.
    //apply "value-changed" to browse {&browse-name} .
  if not this-procedure:persistent then
    wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
  then delete widget C-Win.
  if this-procedure:persistent then delete procedure this-procedure.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
procedure enable_UI :
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
  get first DEFAULT-FRAME.
  if available Customer then 
    display Customer.CustNum Customer.Name Customer.Country 
      with frame DEFAULT-FRAME in window C-Win.
  if available Order then 
    display Order.Ordernum 
      with frame DEFAULT-FRAME in window C-Win.
  if available Salesrep then 
    display Salesrep.SalesRep Salesrep.RepName 
      with frame DEFAULT-FRAME in window C-Win.
  enable BtnFirst BtnPrev BtnNext BtnLast Customer.CustNum Customer.Name 
         Customer.Country brCustomer Salesrep.SalesRep Salesrep.RepName 
         btnOrders Order.Ordernum 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getButtonsInfo C-Win 
procedure getButtonsInfo :
/*------------------------------------------------------------------------------
     Purpose: Retrieves the current row and total number of rows in the browse.
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter opiRow as integer no-undo.
    define output parameter opiNumRows as integer no-undo.
    assign 
        opiRow     = current-result-row('{&BROWSE-NAME}')
        opiNumRows = num-results('{&BROWSE-NAME}').   
    do with frame {&FRAME-NAME}:
        enable BtnFirst
            BtnPrev
            BtnLast
            BtnNext
            .
            end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenCustomerMaint C-Win 
procedure OpenCustomerMaint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    if not valid-handle (hCustomerMaint) then
        
        do:
            run CustomerMaint.w persistent set hCustomerMaint .
            publish "CustomerChanged" (input Customer.CustNum) .
        end .

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtons C-Win 
procedure SetButtons :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
        assign
            iRow     = current-result-row('{&BROWSE-NAME}')
            iNumRows = num-results('{&BROWSE-NAME}').

        do with frame {&FRAME-NAME}:
            enable BtnFirst
                BtnPrev
                BtnLast
                BtnNext
                .

            if iRow = 1 then
                do:
                    disable BtnFirst
                        BtnPrev
                        .
                    assign
                        menu-item m_First:SENSITIVE in menu menu-bar-c-win = no
                        menu-item m_Prev:SENSITIVE  in menu menu-bar-c-win = no.

/*                        {&ENABLED_FIRST} = "no" */
/*                        {&ENABLED_PREV}  = "no".*/
                end.
            else if iRow = iNumRows then
                do:
                    disable BtnLast
                        BtnNext
                        .
                    assign
                        menu-item m_Last:SENSITIVE  in menu menu-bar-c-win = no
                        menu-item m_Next:SENSITIVE  in menu menu-bar-c-win = no.
                    //assign
/*                        {&ENABLED_LAST} = "no" */
/*                        {&ENABLED_NEXT} = "no".*/
                end.
            else
                do:
                    assign
                        menu-item m_First:SENSITIVE in menu menu-bar-c-win = yes
                        menu-item m_Prev:SENSITIVE  in menu menu-bar-c-win = yes
                        menu-item m_Last:SENSITIVE  in menu menu-bar-c-win = yes
                        menu-item m_Next:SENSITIVE  in menu menu-bar-c-win = yes.
/*                    assign                      */
/*                        {&ENABLED_LAST} = "yes" */
/*                        {&ENABLED_NEXT} = "yes".*/
                end.
        end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSearchCustomers C-Win 
procedure SetSearchCustomers :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter piInputCustomer as character no-undo .
    define input parameter piSort as character no-undo .

    cClause = substitute("for each &1 &2 &3":u, piInputCustomer, SearchCustomers(), piSort) .
    
        hQuery = browse {&browse-name}:query .
        hQuery:query-close () .
        hQuery:query-prepare (cClause) .
        hQuery:query-open () .
        
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SearchCustomers C-Win 
function SearchCustomers returns character
  (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result   as character no-undo . // This variable is built automatically from this function.
    define variable iNumber  as integer   no-undo .
    define variable cName    as character no-undo .
    define variable cCountry as character no-undo .
            
    do with frame {&frame-name} :
        assign 
            iNumber  = integer(Customer.CustNum:screen-value)
            cName    = Customer.Name:screen-value
            cCountry = Customer.Country:screen-value .
    end .
            
    /*            if iNumber = 0 and cName = "" and cCountry = "" then*/
    /*            return result .                                     */
            
        if iNumber > 0 then
            assign result = substitute("Customer.CustNum >= &1 ", iNumber) .
         else "" .
                        
        if cName > "" then
            assign result = trim(substitute("&1 &2 &3", result, (if result > "" then "AND" else ""), substitute("Customer.Name begins &1", quoter(cName)))) .
                        
        if cCountry > "" then
            assign result = trim(substitute("&1 &2 &3", result, (if result > "" then "AND" else ""), substitute("Customer.Country begins &1", quoter(cCountry)))).
                        
        if result > "" then
            assign result = "where " + result .
        return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFirstCustomer C-Win 
procedure getFirstCustomer :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    if available Customer then
        apply "Home"          to brCustomer in frame {&frame-name}.
    apply "value-changed" to brCustomer in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLastCustomer C-Win 
procedure getLastCustomer :
    /*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    if available Customer then
        apply "END" to brCustomer in frame {&frame-name}.
    apply "value-changed" to brCustomer in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNextCustomer C-Win 
procedure getNextCustomer :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    if available Customer then 
        browse {&BROWSE-NAME}:select-next-row().
    apply "value-changed" to brCustomer in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrevCustomer C-Win 
procedure getPrevCustomer :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    if available Customer then
        browse {&BROWSE-NAME}:select-prev-row().
    apply "value-changed" to brCustomer in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME