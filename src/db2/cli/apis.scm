;; -*- mode:scheme; coding:utf-8; -*-
;; Generated by genbind
;; /usr/local/bin/genbind -DSQL_API_FN -DFAR -t HWND=void* -tSQL_BIGUINT_TYPE -t_SQLOLDCHAR /cygdrive/c/Program Files (x86)/IBM/SQLLIB/include/sqlcli1.h -o (db2 cli) -s db2cli.txt -Osrc
(library (db2 cli apis)
    (export
      sqlalloc-connect
      sqlalloc-env
      sqlalloc-stmt
      sqlalloc-handle
      sqlbind-col
      sqlcancel
      sqlcol-attribute
      sqlconnect
      sqldescribe-col
      sqldisconnect
      sqlerror
      sqlexec-direct
      sqlexecute
      sqlfetch
      sqlfree-connect
      sqlfree-env
      sqlfree-stmt
      sqlclose-cursor
      sqlget-cursor-name
      sqlget-data
      sqlnum-result-cols
      sqlprepare
      sqlrow-count
      sqlset-cursor-name
      sqlset-param
      sqltransact
      sqlend-tran
      sqlfree-handle
      sqlget-diag-rec
      sqlget-diag-field
      sqlcopy-desc
      sqlcreate-db
      sqldrop-db
      sqlcreate-pkg
      sqlget-desc-field
      sqlget-desc-rec
      sqlset-desc-field
      sqlset-desc-rec
      sqldriver-connect
      sqlbrowse-connect
      sqlbulk-operations
      sqlcol-attributes
      sqlcolumn-privileges
      sqldescribe-param
      sqlextended-fetch
      sqlforeign-keys
      sqlmore-results
      sqlnative-sql
      sqlnum-params
      sqlparam-options
      sqlprimary-keys
      sqlprocedure-columns
      sqlprocedures
      sqlset-pos
      sqltable-privileges
      sqldrivers
      sqlbind-parameter
      sqlalloc-handle-std
      sqlset-scroll-options
      trace-open-log-file
      trace-close-log-file
      trace-return
      trace-version
      trace-vscontrol
      odbcset-try-wait-value
      odbcget-try-wait-value
      fire-vsdebug-event
      sqlcolumns
      sqldata-sources
      sqlfetch-scroll
      sqlget-connect-attr
      sqlget-connect-option
      sqlget-functions
      sqlget-info
      sqlget-stmt-attr
      sqlget-stmt-option
      sqlget-type-info
      sqlparam-data
      sqlput-data
      sqlset-connect-attr
      sqlset-connect-option
      sqlset-stmt-attr
      sqlset-stmt-option
      sqlspecial-columns
      sqlstatistics
      sqltables
      sqlnext-result
      sqlcol-attribute-w
      sqlcol-attributes-w
      sqlconnect-w
      sqlconnect-w
      sqldescribe-col-w
      sqlerror-w
      sqlexec-direct-w
      sqlget-connect-attr-w
      sqlget-cursor-name-w
      sqlset-desc-field-w
      sqlget-desc-field-w
      sqlget-desc-rec-w
      sqlget-diag-field-w
      sqlget-diag-rec-w
      sqlget-env-attr-w
      sqlprepare-w
      sqlextended-prepare-w
      sqlset-connect-attr-w
      sqlset-cursor-name-w
      sqlset-env-attr-w
      sqlcolumns-w
      sqlget-info-w
      sqlget-connect-option-w
      sqlset-connect-option-w
      sqlget-type-info-w
      sqlspecial-columns-w
      sqlstatistics-w
      sqltables-w
      sqldata-sources-w
      sqldriver-connect-w
      sqlbrowse-connect-w
      sqlcolumn-privileges-w
      sqlget-stmt-attr-w
      sqlset-stmt-attr-w
      sqlforeign-keys-w
      sqlnative-sql-w
      sqlprimary-keys-w
      sqlprocedure-columns-w
      sqlprocedures-w
      sqlextended-procedure-columns-w
      sqlextended-procedures-w
      sqltable-privileges-w
      sqlcreate-db-w
      sqldrop-db-w
      sqlcreate-pkg-w
      sqldrop-pkg-w
      sqlbind-file-to-col
      sqlbind-file-to-param
      sqlget-length
      sqlget-position
      sqlget-sqlca
      sqlget-sub-string
      sqlset-col-attributes
      sqlextended-procedures
      sqlextended-procedure-columns
      sqlreload-config
      sqlreload-config-w
      sqlget-position-w
      sqlset-connection
      sqlget-env-attr
      sqlset-env-attr
      sqlbind-param
      sqlbuild-data-link
      sqlget-data-link-attr
      sqlextended-prepare
      sqlextended-bind
      sqlextended-describe
      sqldrop-pkg
     )
    (import
      (rnrs)
      (sagittarius)
      (sagittarius ffi)
      (db2 cli types)
     )
  (define shared-lib (open-shared-library (cond-expand ((or windows cygwin) "db2cli.dll") (else "libdb2.so.1"))))
  (define sqlalloc-connect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLAllocConnect (SQLHENV void*))))
  (define sqlalloc-env (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLAllocEnv (void*))))
  (define sqlalloc-stmt (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLAllocStmt (SQLHDBC void*))))
  (define sqlalloc-handle (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLAllocHandle (SQLSMALLINT SQLHANDLE void*))))
  (define sqlbind-col (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBindCol (SQLHSTMT SQLUSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER void*))))
  (define sqlcancel (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCancel (SQLHSTMT))))
  (define sqlcol-attribute (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColAttribute (SQLHSTMT SQLUSMALLINT SQLUSMALLINT SQLPOINTER SQLSMALLINT void* SQLPOINTER))))
  (define sqlconnect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLConnect (SQLHDBC void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldescribe-col (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDescribeCol (SQLHSTMT SQLUSMALLINT void* SQLSMALLINT void* void* void* void* void*))))
  (define sqldisconnect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDisconnect (SQLHDBC))))
  (define sqlerror (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLError (SQLHENV SQLHDBC SQLHSTMT void* void* void* SQLSMALLINT void*))))
  (define sqlexec-direct (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExecDirect (SQLHSTMT void* SQLINTEGER))))
  (define sqlexecute (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExecute (SQLHSTMT))))
  (define sqlfetch (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFetch (SQLHSTMT))))
  (define sqlfree-connect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFreeConnect (SQLHDBC))))
  (define sqlfree-env (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFreeEnv (SQLHENV))))
  (define sqlfree-stmt (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFreeStmt (SQLHSTMT SQLUSMALLINT))))
  (define sqlclose-cursor (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCloseCursor (SQLHSTMT))))
  (define sqlget-cursor-name (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetCursorName (SQLHSTMT void* SQLSMALLINT void*))))
  (define sqlget-data (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetData (SQLHSTMT SQLUSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER void*))))
  (define sqlnum-result-cols (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLNumResultCols (SQLHSTMT void*))))
  (define sqlprepare (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLPrepare (SQLHSTMT void* SQLINTEGER))))
  (define sqlrow-count (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLRowCount (SQLHSTMT void*))))
  (define sqlset-cursor-name (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetCursorName (SQLHSTMT void* SQLSMALLINT))))
  (define sqlset-param (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetParam (SQLHSTMT SQLUSMALLINT SQLSMALLINT SQLSMALLINT SQLUINTEGER SQLSMALLINT SQLPOINTER void*))))
  (define sqltransact (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLTransact (SQLHENV SQLHDBC SQLUSMALLINT))))
  (define sqlend-tran (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLEndTran (SQLSMALLINT SQLHANDLE SQLSMALLINT))))
  (define sqlfree-handle (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFreeHandle (SQLSMALLINT SQLHANDLE))))
  (define sqlget-diag-rec (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDiagRec (SQLSMALLINT SQLHANDLE SQLSMALLINT void* void* void* SQLSMALLINT void*))))
  (define sqlget-diag-field (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDiagField (SQLSMALLINT SQLHANDLE SQLSMALLINT SQLSMALLINT SQLPOINTER SQLSMALLINT void*))))
  (define sqlcopy-desc (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCopyDesc (SQLHDESC SQLHDESC))))
  (define sqlcreate-db (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCreateDb (SQLHDBC void* SQLINTEGER void* SQLINTEGER void* SQLINTEGER))))
  (define sqldrop-db (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDropDb (SQLHDBC void* SQLINTEGER))))
  (define sqlcreate-pkg (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCreatePkg (SQLHDBC void* SQLINTEGER void* SQLINTEGER))))
  (define sqlget-desc-field (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDescField (SQLHDESC SQLSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER void*))))
  (define sqlget-desc-rec (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDescRec (SQLHDESC SQLSMALLINT void* SQLSMALLINT void* void* void* void* void* void* void*))))
  (define sqlset-desc-field (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetDescField (SQLHDESC SQLSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER))))
  (define sqlset-desc-rec (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetDescRec (SQLHDESC SQLSMALLINT SQLSMALLINT SQLSMALLINT SQLINTEGER SQLSMALLINT SQLSMALLINT SQLPOINTER void* void*))))
  (define sqldriver-connect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDriverConnect (SQLHDBC SQLHWND void* SQLSMALLINT void* SQLSMALLINT void* SQLUSMALLINT))))
  (define sqlbrowse-connect (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBrowseConnect (SQLHDBC void* SQLSMALLINT void* SQLSMALLINT void*))))
  (define sqlbulk-operations (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBulkOperations (SQLHSTMT SQLSMALLINT))))
  (define sqlcol-attributes (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColAttributes (SQLHSTMT SQLUSMALLINT SQLUSMALLINT SQLPOINTER SQLSMALLINT void* void*))))
  (define sqlcolumn-privileges (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColumnPrivileges (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldescribe-param (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDescribeParam (SQLHSTMT SQLUSMALLINT void* void* void* void*))))
  (define sqlextended-fetch (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedFetch (SQLHSTMT SQLUSMALLINT SQLINTEGER void* void*))))
  (define sqlforeign-keys (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLForeignKeys (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlmore-results (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLMoreResults (SQLHSTMT))))
  (define sqlnative-sql (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLNativeSql (SQLHDBC void* SQLINTEGER void* SQLINTEGER void*))))
  (define sqlnum-params (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLNumParams (SQLHSTMT void*))))
  (define sqlparam-options (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLParamOptions (SQLHSTMT SQLUINTEGER void*))))
  (define sqlprimary-keys (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLPrimaryKeys (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlprocedure-columns (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLProcedureColumns (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlprocedures (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLProcedures (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlset-pos (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetPos (SQLHSTMT SQLUSMALLINT SQLUSMALLINT SQLUSMALLINT))))
  (define sqltable-privileges (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLTablePrivileges (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldrivers (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDrivers (SQLHENV SQLUSMALLINT void* SQLSMALLINT void* void* SQLSMALLINT void*))))
  (define sqlbind-parameter (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBindParameter (SQLHSTMT SQLUSMALLINT SQLSMALLINT SQLSMALLINT SQLSMALLINT SQLUINTEGER SQLSMALLINT SQLPOINTER SQLINTEGER void*))))
  (define sqlalloc-handle-std (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLAllocHandleStd (SQLSMALLINT SQLHANDLE void*))))
  (define sqlset-scroll-options (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetScrollOptions (SQLHSTMT SQLUSMALLINT SQLINTEGER SQLUSMALLINT))))
  (define trace-open-log-file (guard (e (else #f)) (c-function shared-lib RETCODE TraceOpenLogFile (LPWSTR LPWSTR DWORD))))
  (define trace-close-log-file (guard (e (else #f)) (c-function shared-lib RETCODE TraceCloseLogFile ())))
  (define trace-return (guard (e (else #f)) (c-function shared-lib void TraceReturn (RETCODE RETCODE))))
  (define trace-version (guard (e (else #f)) (c-function shared-lib DWORD TraceVersion ())))
  (define trace-vscontrol (guard (e (else #f)) (c-function shared-lib RETCODE TraceVSControl (DWORD))))
  (define odbcset-try-wait-value (guard (e (else #f)) (c-function shared-lib BOOL ODBCSetTryWaitValue (DWORD))))
  (define odbcget-try-wait-value (guard (e (else #f)) (c-function shared-lib DWORD ODBCGetTryWaitValue ())))
  (define fire-vsdebug-event (guard (e (else #f)) (c-function shared-lib void FireVSDebugEvent (PODBC_VS_ARGS))))
  (define sqlcolumns (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColumns (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldata-sources (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDataSources (SQLHENV SQLUSMALLINT void* SQLSMALLINT void* void* SQLSMALLINT void*))))
  (define sqlfetch-scroll (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLFetchScroll (SQLHSTMT SQLSMALLINT SQLINTEGER))))
  (define sqlget-connect-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetConnectAttr (SQLHDBC SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlget-connect-option (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetConnectOption (SQLHDBC SQLUSMALLINT SQLPOINTER))))
  (define sqlget-functions (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetFunctions (SQLHDBC SQLUSMALLINT void*))))
  (define sqlget-info (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetInfo (SQLHDBC SQLUSMALLINT SQLPOINTER SQLSMALLINT void*))))
  (define sqlget-stmt-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetStmtAttr (SQLHSTMT SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlget-stmt-option (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetStmtOption (SQLHSTMT SQLUSMALLINT SQLPOINTER))))
  (define sqlget-type-info (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetTypeInfo (SQLHSTMT SQLSMALLINT))))
  (define sqlparam-data (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLParamData (SQLHSTMT void*))))
  (define sqlput-data (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLPutData (SQLHSTMT SQLPOINTER SQLINTEGER))))
  (define sqlset-connect-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetConnectAttr (SQLHDBC SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlset-connect-option (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetConnectOption (SQLHDBC SQLUSMALLINT SQLUINTEGER))))
  (define sqlset-stmt-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetStmtAttr (SQLHSTMT SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlset-stmt-option (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetStmtOption (SQLHSTMT SQLUSMALLINT SQLUINTEGER))))
  (define sqlspecial-columns (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSpecialColumns (SQLHSTMT SQLUSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT SQLUSMALLINT SQLUSMALLINT))))
  (define sqlstatistics (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLStatistics (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT SQLUSMALLINT SQLUSMALLINT))))
  (define sqltables (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLTables (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlnext-result (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLNextResult (SQLHSTMT SQLHSTMT))))
  (define sqlcol-attribute-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColAttributeW (SQLHSTMT SQLUSMALLINT SQLUSMALLINT SQLPOINTER SQLSMALLINT void* SQLPOINTER))))
  (define sqlcol-attributes-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColAttributesW (SQLHSTMT SQLUSMALLINT SQLUSMALLINT SQLPOINTER SQLSMALLINT void* void*))))
  (define sqlconnect-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLConnectW (SQLHDBC void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlconnect-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLConnectW (SQLHDBC void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldescribe-col-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDescribeColW (SQLHSTMT SQLUSMALLINT void* SQLSMALLINT void* void* void* void* void*))))
  (define sqlerror-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLErrorW (SQLHENV SQLHDBC SQLHSTMT void* void* void* SQLSMALLINT void*))))
  (define sqlexec-direct-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExecDirectW (SQLHSTMT void* SQLINTEGER))))
  (define sqlget-connect-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetConnectAttrW (SQLHDBC SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlget-cursor-name-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetCursorNameW (SQLHSTMT void* SQLSMALLINT void*))))
  (define sqlset-desc-field-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetDescFieldW (SQLHDESC SQLSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER))))
  (define sqlget-desc-field-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDescFieldW (SQLHDESC SQLSMALLINT SQLSMALLINT SQLPOINTER SQLINTEGER void*))))
  (define sqlget-desc-rec-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDescRecW (SQLHDESC SQLSMALLINT void* SQLSMALLINT void* void* void* void* void* void* void*))))
  (define sqlget-diag-field-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDiagFieldW (SQLSMALLINT SQLHANDLE SQLSMALLINT SQLSMALLINT SQLPOINTER SQLSMALLINT void*))))
  (define sqlget-diag-rec-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDiagRecW (SQLSMALLINT SQLHANDLE SQLSMALLINT void* void* void* SQLSMALLINT void*))))
  (define sqlget-env-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetEnvAttrW (SQLHENV SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlprepare-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLPrepareW (SQLHSTMT void* SQLINTEGER))))
  (define sqlextended-prepare-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedPrepareW (SQLHSTMT void* SQLINTEGER SQLINTEGER SQLSMALLINT SQLINTEGER void* void*))))
  (define sqlset-connect-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetConnectAttrW (SQLHDBC SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlset-cursor-name-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetCursorNameW (SQLHSTMT void* SQLSMALLINT))))
  (define sqlset-env-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetEnvAttrW (SQLHENV SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlcolumns-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColumnsW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlget-info-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetInfoW (SQLHDBC SQLUSMALLINT SQLPOINTER SQLSMALLINT void*))))
  (define sqlget-connect-option-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetConnectOptionW (SQLHDBC SQLUSMALLINT SQLPOINTER))))
  (define sqlset-connect-option-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetConnectOptionW (SQLHDBC SQLUSMALLINT SQLUINTEGER))))
  (define sqlget-type-info-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetTypeInfoW (SQLHSTMT SQLSMALLINT))))
  (define sqlspecial-columns-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSpecialColumnsW (SQLHSTMT SQLUSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT SQLUSMALLINT SQLUSMALLINT))))
  (define sqlstatistics-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLStatisticsW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT SQLUSMALLINT SQLUSMALLINT))))
  (define sqltables-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLTablesW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqldata-sources-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDataSourcesW (SQLHENV SQLUSMALLINT void* SQLSMALLINT void* void* SQLSMALLINT void*))))
  (define sqldriver-connect-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDriverConnectW (SQLHDBC SQLHWND void* SQLSMALLINT void* SQLSMALLINT void* SQLUSMALLINT))))
  (define sqlbrowse-connect-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBrowseConnectW (SQLHDBC void* SQLSMALLINT void* SQLSMALLINT void*))))
  (define sqlcolumn-privileges-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLColumnPrivilegesW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlget-stmt-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetStmtAttrW (SQLHSTMT SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlset-stmt-attr-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetStmtAttrW (SQLHSTMT SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlforeign-keys-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLForeignKeysW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlnative-sql-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLNativeSqlW (SQLHDBC void* SQLINTEGER void* SQLINTEGER void*))))
  (define sqlprimary-keys-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLPrimaryKeysW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlprocedure-columns-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLProcedureColumnsW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlprocedures-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLProceduresW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlextended-procedure-columns-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedProcedureColumnsW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlextended-procedures-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedProceduresW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqltable-privileges-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLTablePrivilegesW (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlcreate-db-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCreateDbW (SQLHDBC void* SQLINTEGER void* SQLINTEGER void* SQLINTEGER))))
  (define sqldrop-db-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDropDbW (SQLHDBC void* SQLINTEGER))))
  (define sqlcreate-pkg-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLCreatePkgW (SQLHDBC void* SQLINTEGER void* SQLINTEGER))))
  (define sqldrop-pkg-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDropPkgW (SQLHDBC void* SQLINTEGER void* SQLINTEGER))))
  (define sqlbind-file-to-col (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBindFileToCol (SQLHSTMT SQLUSMALLINT void* void* void* SQLSMALLINT void* void*))))
  (define sqlbind-file-to-param (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBindFileToParam (SQLHSTMT SQLUSMALLINT SQLSMALLINT void* void* void* SQLSMALLINT void*))))
  (define sqlget-length (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetLength (SQLHSTMT SQLSMALLINT SQLINTEGER void* void*))))
  (define sqlget-position (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetPosition (SQLHSTMT SQLSMALLINT SQLINTEGER SQLINTEGER void* SQLINTEGER SQLUINTEGER void* void*))))
  (define sqlget-sqlca (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetSQLCA (SQLHENV SQLHDBC SQLHSTMT void*))))
  (define sqlget-sub-string (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetSubString (SQLHSTMT SQLSMALLINT SQLINTEGER SQLUINTEGER SQLUINTEGER SQLSMALLINT SQLPOINTER SQLINTEGER void* void*))))
  (define sqlset-col-attributes (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetColAttributes (SQLHSTMT SQLUSMALLINT void* SQLSMALLINT SQLSMALLINT SQLUINTEGER SQLSMALLINT SQLSMALLINT))))
  (define sqlextended-procedures (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedProcedures (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlextended-procedure-columns (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedProcedureColumns (SQLHSTMT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT void* SQLSMALLINT))))
  (define sqlreload-config (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLReloadConfig (SQLINTEGER void* SQLSMALLINT void*))))
  (define sqlreload-config-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLReloadConfigW (SQLINTEGER void* SQLSMALLINT void*))))
  (define sqlget-position-w (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetPositionW (SQLHSTMT SQLSMALLINT SQLINTEGER SQLINTEGER void* SQLINTEGER SQLUINTEGER void* void*))))
  (define sqlset-connection (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetConnection (SQLHDBC))))
  (define sqlget-env-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetEnvAttr (SQLHENV SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlset-env-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLSetEnvAttr (SQLHENV SQLINTEGER SQLPOINTER SQLINTEGER))))
  (define sqlbind-param (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBindParam (SQLHSTMT SQLUSMALLINT SQLSMALLINT SQLSMALLINT SQLUINTEGER SQLSMALLINT SQLPOINTER void*))))
  (define sqlbuild-data-link (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLBuildDataLink (SQLHSTMT void* SQLINTEGER void* SQLINTEGER void* SQLINTEGER void* SQLINTEGER void*))))
  (define sqlget-data-link-attr (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLGetDataLinkAttr (SQLHSTMT SQLSMALLINT void* SQLINTEGER SQLPOINTER SQLINTEGER void*))))
  (define sqlextended-prepare (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedPrepare (SQLHSTMT void* SQLINTEGER SQLINTEGER SQLSMALLINT SQLINTEGER void* void*))))
  (define sqlextended-bind (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedBind (SQLHSTMT SQLSMALLINT SQLSMALLINT void* void* void* void* void* void* void* void* void* void* void*))))
  (define sqlextended-describe (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLExtendedDescribe (SQLHANDLE SQLSMALLINT SQLUSMALLINT void* void* SQLSMALLINT void* void* void* void* void* void* void* void*))))
  (define sqldrop-pkg (guard (e (else #f)) (c-function shared-lib SQLRETURN SQLDropPkg (SQLHDBC void* SQLINTEGER void* SQLINTEGER))))
)