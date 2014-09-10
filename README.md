DBD for DB2
===========

It turned out that on Windows, ODBC can be used without registering service
name. So this is more for Linux or other environment which doesn't have
ODBC support.

(db2) library
-------------

TBD

Using with DBI
--------------

TBD

Using ODBC
----------

Since 0.5.8, ODBC library supports SQLDriverConnect so connecting DB2 with
ODBC would look like something following;

```scheme
(define conn (dbi-connect "dbi:odbc:Driver={IBM DB2 ODBC DRIVER};\
                                    Database=Sample;\
                                    Hostname=localhost;\
                                    Port=50000;\
                                    Protocol=TCPIP"
                          :username "user" :password "pass"))
;; do whatever you want to with the DB connection
```

If the Sagittarius is built with ODBC support, for example with iODBC or 
unixODBC, then above can be used in Linux environment.

It is recommended to use ODBC if possible.
