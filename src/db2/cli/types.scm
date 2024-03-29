;; -*- mode:scheme; coding:utf-8; -*-
;; Generated by genbind
;; /usr/local/bin/genbind -DSQL_API_FN -DFAR -t HWND=void* -tSQL_BIGUINT_TYPE -t_SQLOLDCHAR /cygdrive/c/Program Files (x86)/IBM/SQLLIB/include/sqlcli1.h -o (db2 cli) -s db2cli.txt -Osrc
(library (db2 cli types)
    (export
      sqluint8
      _TAGGUID
      tagSQL_YEAR_MONTH
      SQLVARCHAR
      sqluint32
      ULONG
      +sql-is-year+
      SQLHSTMT
      SQLNUMERIC
      SQLTIME
      +sql-is-hour+
      +sql-is-year-to-month+
      SQL_TIMESTAMP_STRUCT
      SQLDECIMAL
      PTR
      sqluint16
      SQLINTERVAL
      SQLHWND
      +sql-is-hour-to-minute+
      DATE_STRUCT
      SQLFLOAT
      +sql-is-day-to-minute+
      TAGGUID
      SFLOAT
      SQLDBCHAR
      UDWORD
      SLONG
      DWORD
      SQLHANDLE
      SQL_INTERVAL_STRUCT
      HENV
      sqlint32
      SQLCHAR
      SWORD
      SQLREAL
      SDWORD
      SQLDECIMAL128
      SQLDOUBLE
      sqlint8
      LPWSTR
      tagODBC_VS_ARGS
      +sql-is-day+
      SQLRETURN
      tagSQL_INTERVAL_STRUCT
      +sql-is-second+
      SQLUSMALLINT
      RETCODE
      sqlint16
      HDBC
      sqlint64
      LDOUBLE
      SQL_NUMERIC_STRUCT
      TIMESTAMP_STRUCT
      SQLUINTEGER
      TIMESTAMP_STRUCT_EXT_TZ
      SCHAR
      +sql-is-minute-to-second+
      SQLINTEGER
      SQLTIMESTAMP
      +sql-is-day-to-hour+
      SSHORT
      UWORD
      SQL_DATE_STRUCT
      SQLSCHAR
      sqlintptr
      SQL_DAY_SECOND_STRUCT
      SQLPOINTER
      SQL_NET_STATS
      SQLSMALLINT
      SQLHDBC
      SQLHENV
      sqlca
      SQL_TIMESTAMP_STRUCT_EXT_TZ
      +sql-is-month+
      SQLWCHAR
      SQLUBIGINT
      +sql-is-minute+
      TIME_STRUCT
      sqluintptr
      SQL_TIMESTAMP_STRUCT_EXT
      tagSQL_DAY_SECOND
      SQLHDESC
      tagSQL_NUMERIC_STRUCT
      ODBC_VS_ARGS
      SQLDATE
      sqluint64
      USHORT
      SQL_YEAR_MONTH_STRUCT
      BOOL
      HSTMT
      +sql-is-hour-to-second+
      SQLBIGINT
      SQLDECIMAL64
      SQLTCHAR
      WCHAR
      +sql-is-day-to-second+
      SQL_TIME_STRUCT
      TIMESTAMP_STRUCT_EXT
      UCHAR
      SDOUBLE
     )
    (import
      (rnrs)
      (only (sagittarius) define-constant)
      (sagittarius ffi)
     )
  (define-c-typedef void* HWND)
  (define-c-typedef char sqlint8)
  (define-c-typedef unsigned-char sqluint8)
  (define-c-typedef short sqlint16)
  (define-c-typedef unsigned-short sqluint16)
  (define-c-typedef long sqlint32)
  (define-c-typedef unsigned-long sqluint32)
  (define-c-typedef long-long sqlint64)
  (define-c-typedef unsigned-long-long sqluint64)
  (define-c-typedef sqlint32 sqlintptr)
  (define-c-typedef sqluint32 sqluintptr)
  (begin (define-c-struct sqlca (char array 8 sqlcaid) (sqlint32 sqlcabc) (sqlint32 sqlcode) (short sqlerrml) (char array 70 sqlerrmc) (char array 8 sqlerrp) (sqlint32 array 6 sqlerrd) (char array 11 sqlwarn) (char array 5 sqlstate)))
  (define-c-typedef signed-char SCHAR)
  (define-c-typedef unsigned-char UCHAR)
  (define-c-typedef short-int SWORD)
  (define-c-typedef unsigned-short USHORT)
  (define-c-typedef signed-short SSHORT)
  (define-c-typedef unsigned-short-int UWORD)
  (define-c-typedef long SDWORD)
  (define-c-typedef unsigned-long ULONG)
  (define-c-typedef unsigned-long UDWORD)
  (define-c-typedef long SLONG)
  (define-c-typedef double SDOUBLE)
  (define-c-typedef float SFLOAT)
  (define-c-typedef unsigned-char SQLDATE)
  (define-c-typedef unsigned-char SQLTIME)
  (define-c-typedef unsigned-char SQLTIMESTAMP)
  (define-c-typedef unsigned-char SQLDECIMAL)
  (define-c-typedef unsigned-char SQLNUMERIC)
  (define-c-typedef double LDOUBLE)
  (define-c-typedef void (* PTR))
  (define-c-typedef void (* HENV))
  (define-c-typedef void (* HDBC))
  (define-c-typedef void (* HSTMT))
  (define-c-typedef signed-short RETCODE)
  (define-c-typedef UCHAR SQLCHAR)
  (define-c-typedef UCHAR SQLVARCHAR)
  (define-c-typedef SCHAR SQLSCHAR)
  (define-c-typedef SDWORD SQLINTEGER)
  (define-c-typedef SWORD SQLSMALLINT)
  (define-c-typedef SDOUBLE SQLDOUBLE)
  (define-c-typedef SDOUBLE SQLFLOAT)
  (define-c-typedef SFLOAT SQLREAL)
  (define-c-typedef SQLSMALLINT SQLRETURN)
  (define-c-typedef UDWORD SQLUINTEGER)
  (define-c-typedef UWORD SQLUSMALLINT)
  (define-c-typedef PTR SQLPOINTER)
  (define-c-typedef unsigned-short SQLDBCHAR)
  (define-c-typedef unsigned-short SQLWCHAR)
  (define-c-typedef SQLCHAR SQLTCHAR)
  (define-c-typedef SQLINTEGER SQLHANDLE)
  (define-c-typedef SQLINTEGER SQLHENV)
  (define-c-typedef SQLINTEGER SQLHDBC)
  (define-c-typedef SQLINTEGER SQLHSTMT)
  (define-c-typedef HWND SQLHWND)
  (define-c-typedef SQLHANDLE SQLHDESC)
  (define-c-typedef long-long SQLBIGINT)
  (define-c-typedef unsigned-long-long SQLUBIGINT)
  (begin (define-c-struct DATE_STRUCT (SQLSMALLINT year) (SQLUSMALLINT month) (SQLUSMALLINT day)))
  (define-c-typedef DATE_STRUCT SQL_DATE_STRUCT)
  (begin (define-c-struct TIME_STRUCT (SQLUSMALLINT hour) (SQLUSMALLINT minute) (SQLUSMALLINT second)))
  (define-c-typedef TIME_STRUCT SQL_TIME_STRUCT)
  (begin (define-c-struct TIMESTAMP_STRUCT (SQLSMALLINT year) (SQLUSMALLINT month) (SQLUSMALLINT day) (SQLUSMALLINT hour) (SQLUSMALLINT minute) (SQLUSMALLINT second) (SQLUINTEGER fraction)))
  (define-c-typedef TIMESTAMP_STRUCT SQL_TIMESTAMP_STRUCT)
  (begin (define-c-struct TIMESTAMP_STRUCT_EXT (SQLSMALLINT year) (SQLUSMALLINT month) (SQLUSMALLINT day) (SQLUSMALLINT hour) (SQLUSMALLINT minute) (SQLUSMALLINT second) (SQLUINTEGER fraction) (SQLUINTEGER fraction2)))
  (define-c-typedef TIMESTAMP_STRUCT_EXT SQL_TIMESTAMP_STRUCT_EXT)
  (begin (define-c-struct TIMESTAMP_STRUCT_EXT_TZ (SQLSMALLINT year) (SQLUSMALLINT month) (SQLUSMALLINT day) (SQLUSMALLINT hour) (SQLUSMALLINT minute) (SQLUSMALLINT second) (SQLUINTEGER fraction) (SQLUINTEGER fraction2) (SQLSMALLINT timezone_hour) (SQLSMALLINT timezone_minute)))
  (define-c-typedef TIMESTAMP_STRUCT_EXT_TZ SQL_TIMESTAMP_STRUCT_EXT_TZ)
  (begin (begin (define-constant +sql-is-year+ 1) (define-constant +sql-is-month+ 2) (define-constant +sql-is-day+ 3) (define-constant +sql-is-hour+ 4) (define-constant +sql-is-minute+ 5) (define-constant +sql-is-second+ 6) (define-constant +sql-is-year-to-month+ 7) (define-constant +sql-is-day-to-hour+ 8) (define-constant +sql-is-day-to-minute+ 9) (define-constant +sql-is-day-to-second+ 10) (define-constant +sql-is-hour-to-minute+ 11) (define-constant +sql-is-hour-to-second+ 12) (define-constant +sql-is-minute-to-second+ 13)) (define-c-typedef int SQLINTERVAL))
  (begin (define-c-struct tagSQL_YEAR_MONTH (SQLUINTEGER year) (SQLUINTEGER month)) (define-c-typedef tagSQL_YEAR_MONTH SQL_YEAR_MONTH_STRUCT))
  (begin (define-c-struct tagSQL_DAY_SECOND (SQLUINTEGER day) (SQLUINTEGER hour) (SQLUINTEGER minute) (SQLUINTEGER second) (SQLUINTEGER fraction)) (define-c-typedef tagSQL_DAY_SECOND SQL_DAY_SECOND_STRUCT))
  (begin (begin (define-c-union @anon-union.0 (SQL_YEAR_MONTH_STRUCT year_month) (SQL_DAY_SECOND_STRUCT day_second))) (define-c-struct tagSQL_INTERVAL_STRUCT (SQLINTERVAL interval_type) (SQLSMALLINT interval_sign) (struct @anon-union.0 intval)) (define-c-typedef tagSQL_INTERVAL_STRUCT SQL_INTERVAL_STRUCT))
  (begin (define-c-struct tagSQL_NUMERIC_STRUCT (SQLCHAR precision) (SQLSCHAR scale) (SQLCHAR sign) (SQLCHAR array 16 val)) (define-c-typedef tagSQL_NUMERIC_STRUCT SQL_NUMERIC_STRUCT))
  (begin (begin (define-c-union @anon-union.1 (SQLDOUBLE dummy) (SQLCHAR array 8 dec64))) (define-c-struct SQLDECIMAL64 (struct @anon-union.1 udec64)))
  (begin (begin (define-c-union @anon-union.2 (SQLDOUBLE dummy) (SQLCHAR array 16 dec128))) (define-c-struct SQLDECIMAL128 (struct @anon-union.2 udec128)))
  (define-c-typedef SQLWCHAR (* LPWSTR))
  (define-c-typedef sqluint32 DWORD)
  (define-c-typedef unsigned BOOL)
  (define-c-typedef wchar_t WCHAR)
  (begin (define-c-struct _TAGGUID (unsigned-long Data1) (unsigned-short Data2) (unsigned-short Data3) (unsigned-char array 8 Data4)) (define-c-typedef _TAGGUID TAGGUID))
  (begin (begin (define-c-union @anon-union.3 (void* wszArg) (void* szArg))) (begin (define-c-union @anon-union.4 (void* wszCorrelation) (void* szCorrelation))) (define-c-struct tagODBC_VS_ARGS (void* pguidEvent) (DWORD dwFlags) (struct @anon-union.3 @anon-union.3) (struct @anon-union.4 @anon-union.4) (RETCODE RetCode)) (define-c-typedef tagODBC_VS_ARGS ODBC_VS_ARGS))
  (begin (define-c-struct SQL_NET_STATS (SQLINTEGER iNetStatsLength) (SQLUBIGINT uiNetStatsServerTime) (SQLUBIGINT uiNetStatsNetworkTime) (SQLUBIGINT uiNetStatsBytesSent) (SQLUBIGINT uiNetStatsBytesReceived) (SQLUBIGINT uiNetStatsRoundTrips)))
)
