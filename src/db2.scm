;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; db2.scm - DB2 CLI binding
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; usable layer of DB2 CLI
;; DB2 CLI is almost the same as ODBC APIs 
;; so looks pretty similar as builtin (odbc)
(library (db2)
    (export create-db2-env
	    db2-connect!
	    db2-set-connect-attr!
	    db2-get-connect-attr
	    db2-disconnect!
	    db2-connection-open?
	    db2-free-handle!
	    db2-statement
	    db2-prepare
	    db2-statement-open?
	    db2-num-params
	    db2-bind-parameter!
	    db2-execute!
	    db2-execute-direct!
	    db2-fetch!
	    db2-get-data
	    db2-row-count
	    db2-column-size
	    db2-column-count
	    db2-result-column
	    db2-commit!
	    db2-rollback!
	    ;; meta info
	    db2-tables
	    db2-columns
	    ;; predicates
	    db2-env?
	    db2-connection?
	    db2-statement?
	    db2-date?
	    db2-time?
	    db2-timestamp?
	    ;; converter
	    db2-date->date
	    db2-time->time
	    db2-timestamp->time
	    ;; others

	    db2-construct-dns
	    )
    (import (rnrs)
	    (db2 cli)
	    (clos core)
	    (clos user)
	    (sagittarius)
	    (sagittarius ffi)
	    (sagittarius object)
	    (sagittarius control)
	    (binary pack)
	    (srfi :13 strings)
	    (srfi :19 time))

  (define (db2-construct-dns db :key (host #f) (port 50000) (protocol 'TCPIP))
    (let ((base (string-concatenate (list (format "DATABASE=~a;" db)
					  (format "PORT=~a;" port)
					  (format "PROTOCOL=~a;" protocol)))))
      (if host
	  (string-append (format "HOSTNAME=~a;" host) base)
	  base)))

  (define-condition-type &db2-error &error make-db2-error db2-error?)

  (define-class <db2-ctx> ()
    ((type   :init-keyword :type)
     ;; pointer of real handle
     (handle :init-keyword :handle)
     ;; parameter holder to prevent GC
     (holder :init-value '())))

  (define (%free-handle! type handle)
    (sqlfree-handle type (pointer->integer handle)))
  (define (db2-free-handle! ctx)
    (unregister-ffi-finalizer (~ ctx 'handle))
    (%free-handle! (~ ctx 'type) (~ ctx 'handle))
    (set! (~ ctx 'handle) null-pointer))

  (define (check-error who ctx ret)
    (unless (or (= ret +sql-success+) (= ret +sql-no-data+))
      (let ((diag (make-bytevector 50))
	    (msg  (allocate-pointer 512))
	    (state (empty-pointer))
	    (len  (empty-pointer)))
	(sqlget-diag-rec-w (~ ctx 'type) 
			   (pointer->integer (~ ctx 'handle)) 1
			   diag (address state) msg 256 (address len))
	(let ((msg (wchar-pointer->string msg)))
	  (if (= ret +sql-success-with-info+)
	      (raise-continuable (condition (make-who-condition who)
					    (make-warning)
					    (make-message-condition msg)))
	      (raise (condition (make-db2-error)
				(make-who-condition who)
				(make-message-condition msg)
				(make-irritants-condition ret)))))))
    ret)
  (define (%make-db2-context type parent)
    (define (invoke-free pointer) (%free-handle! type pointer))
    (let ((hparent (if parent (~ parent 'handle) null-pointer))
	  (ctx (make <db2-ctx> :type type :handle (empty-pointer))))
      (check-error 'make-db2-context ctx
		   (sqlalloc-handle type 
				    (pointer->integer hparent) 
				    (address (~ ctx 'handle))))
      (register-ffi-finalizer (~ ctx 'handle) invoke-free)
      ;; do we need to set attr odbc-version?
      ctx))
  (define (db2-ctx-of? o type) (and (is-a? o <db2-ctx>)
				    (= (~ o 'type) type)))

  (define (create-db2-env) (%make-db2-context +sql-handle-env+ #f))
  (define (db2-env? o) (db2-ctx-of? o +sql-handle-env+))
  (define (db2-connection? o) (db2-ctx-of? o +sql-handle-dbc+))
  (define (db2-statement? o) (db2-ctx-of? o +sql-handle-stmt+))

  ;;;;
  ;;; Connection

  ;; DNS must be concatenation of following forms
  ;; Database=${dbname};
  ;; Hostname=${hostname};
  ;; Port=${port};
  ;; Protocol=${protocol};
  ;; port and protocol are optional (defaults are 50000 and TCPIP)
  (define (db2-connect! env dns user auth :optional (auto-commit? #f))
    (let* ((conn (%make-db2-context +sql-handle-dbc+ env))
	   (buffer (make-bytevector +sql-max-option-string-length+))
	   (len-ptr (integer->pointer (bytevector-length buffer))))
      (check-error 'db2-connect! conn
		   ;; can't use -w, we didn't hanel wchar_t properly
		   ;; in code generation...
		   (sqldriver-connect (pointer->integer (~ conn 'handle)) 
				      null-pointer
				      (string-append dns
						     ";UID=" user
						     ";PWD=" auth)
				      +sql-nts+
				      buffer (bytevector-length buffer)
				      (address len-ptr)
				      +sql-driver-noprompt+))
      (db2-set-connect-attr! conn +sql-attr-autocommit+ 
			     (if auto-commit?
				 +sql-autocommit-on+
				 +sql-autocommit-off+))
      conn))

  (define (db2-set-connect-attr! conn attr value)
    (unless (db2-connection? conn)
      (error 'db2-connection? "db2-connection required" conn))
    (let ((len (if (string? value) (string-length value) 0))
	  (ptr (if (string? value) value (integer->pointer value))))
      (check-error 'db2-set-connect-attr! conn
		   (sqlset-connect-attr (pointer->integer (~ conn 'handle))
					attr ptr len))))

  (define (db2-get-connect-attr conn attr buffer-length)
    (unless (db2-connection? conn)
      (error 'db2-connection-open? "db2-connection required" conn))
    (let* ((p (if (> buffer-length 0)
		  (allocate-pointer buffer-length)
		  (empty-pointer)))
	   (as (if (> buffer-length 0) buffer-length +sql-is-uinteger+))
	   (r (sqlget-connect-attr (pointer->integer (~ conn 'handle))
				  attr (address p)
				  as null-pointer)))
      (if (> buffer-length 0)
	  (values r (pointer->string p))
	  (values r (pointer->integer p)))))

  (define (db2-disconnect! conn)
    (unless (db2-connection? conn)
      (error 'db2-disconnect! "db2-connection required" conn))
    (check-error 'db2-disconnect! conn
		 (sqldisconnect (pointer->integer (~ conn 'handle))))
    #t)

  (define (db2-connection-open? conn)
    (let-values (((r b)
		  (db2-get-connect-attr conn +sql-attr-connection-dead+ 0)))
      (if (or (= r +sql-error+) (= r +sql-invalid-handle+))
	  #f
	  (not (= b +sql-cd-true+)))))

  ;;;;
  ;; statement

  (define (db2-statement conn)
    (%make-db2-context +sql-handle-stmt+ conn))

  (define (db2-prepare conn text)
    (let ((st (db2-statement conn)))
      (check-error 'db2-prepare st
		   (sqlprepare (pointer->integer (~ st 'handle))
			       text +sql-nts+))
      st))

  (define (db2-get-statement-attr stmt attr buffer-length)
    (unless (db2-statement? stmt)
      (error 'db2-get-statement-attr "db2-statement required" stmt))
    (let* ((p (if (> buffer-length 0)
		  (allocate-pointer buffer-length)
		  (empty-pointer)))
	   (as (if (> buffer-length 0) buffer-length +sql-is-uinteger+))
	   (r (sqlget-stmt-attr (pointer->integer (~ stmt 'handle))
				attr (address p)
				as null-pointer)))
      (if (> buffer-length 0)
	  (values r (pointer->string p))
	  (values r (pointer->integer p)))))

  (define (db2-statement-open? stmt)
    (let-values (((r b)
		  (db2-get-statement-attr stmt +sql-attr-async-enable+ 0)))
      (not (or (= r +sql-error+) (= r +sql-invalid-handle+)))))

  (define (db2-num-params stmt)
    (unless (db2-statement? stmt)
      (error 'db2-num-params "db2-statement required" stmt))
    (let ((num (empty-pointer)))
      (check-error 'db2-num-params stmt
		   (sqlnum-params (pointer->integer (~ stmt 'handle))
				  (address num)))
      (pointer->integer num)))

  ;; based on (oracle oci) bind-parameter
  (define *scheme->c-table* (make-eq-hashtable))
  (define-syntax define-scheme->c
    (syntax-rules ()
      ;; statement is not needed
      ((_ (class var) body ...)
       (set! (~ *scheme->c-table* class) (lambda (ignore var) body ...)))
      ; for blob
      ((_ (class stmt var) body ...)
       (set! (~ *scheme->c-table* class) (lambda (stmt var) body ...)))))

  ;; we convert Scheme data to mere byte array
  ;; todo more
  (define-scheme->c (<integer> n)
    (if (< #x-80000000 n #x7FFFFFFF)
	;; +sql-integer+
	(values +sql-c-slong+ +sql-integer+ 0 (pack "=l" n))
	;; +sql-bigint+
	(error #f "SQL_BIGINT is not supported yet" n)))
  (define-scheme->c (<string> s)
    (let ((bv (string->utf8 (string-append s "\x0;"))))
      (values +sql-c-char+ +sql-varchar+ (bytevector-length bv) bv)))
  (define-scheme->c (<bytevector> bv)
    (values +sql-c-binary+ +sql-varbinary+ (bytevector-length bv) bv))

  (define (db2-bind-parameter! stmt pos ovalue)
    (define (lookup-type value)
      (let1 class (class-of value)
	(or (and-let* ((conv (~ *scheme->c-table* class)))
	      (conv stmt value))
	    (error 'bind-parameter "given value is not supported" value))))
    (let-values (((value-type param-type size value) (lookup-type ovalue)))
      (check-error 'db2-bind-parameter! stmt
		   (sqlbind-parameter (pointer->integer (~ stmt 'handle))
				      pos +sql-param-input+ 
				      value-type param-type size 0
				      value 0 null-pointer))
      (set! (~ stmt 'holder)
	    (acons param-type (vector value ovalue) (~ stmt 'holder)))
      #t))

  (define (db2-execute! stmt)
    (= (check-error 'db2-execute! stmt 
		    (sqlexecute (pointer->integer (~ stmt 'handle))))
       +sql-success+))

  ;; todo db2-execute-direct!

  (define (db2-fetch! stmt)
    (let ((r (sqlfetch (pointer->integer (~ stmt 'handle)))))
      (if (= r +sql-no-data+)
	  #f
	  (and (check-error 'db2-fetch! stmt r) #t))))

  ;; should we export this?
  ;; column attr
  (define (db2-column-attribute stmt index type :key (buffer-size 512))
    ;; for now we supports very limited
    (define num-types `(,+sql-desc-type+ ,+sql-desc-octet-length+))
    (define str-types `(,+sql-column-table-name+ ,+sql-column-name+))
    (define (buffer&length type)
      (cond ((memv type num-types) (values null-pointer (empty-pointer)))
	    ((memv type str-types)
	     (let ((buffer (make-bytevector buffer-size)))
	       (values buffer (integer->pointer buffer-size))))
	    (else
	     (error 'db2-column-attribute "unsupported type" type))))
    (unless (db2-statement? stmt)
      (error 'db2-column-attribute "db2-statement is required" stmt))
    (let ((size (empty-pointer)))
      (let-values (((buffer size) (buffer&length type)))
	(check-error 'db2-column-attribute stmt
		     (sqlcol-attribute-w (pointer->integer (~ stmt 'handle))
					 index type buffer 
					 (pointer->integer size)
					 ;; FIXME pass string-length-ptr
					 ;; so that we can read all
					 ;; data.
					 null-pointer
					 (address size)))
	(if (eq? buffer null-pointer)
	    (pointer->integer size)
	    (wchar-pointer->string buffer)))))

  ;; DB data <-> C data
  ;; (db-type . #(c-type size conv))
  ;; time related struct is allocated so just convert
  (define (db2-time->time v size) 
    ;; unfortunately code generation only exports struct names...
    (let ((hour   (c-struct-ref v SQL_TIME_STRUCT 'hour))
	  (minute (c-struct-ref v SQL_TIME_STRUCT 'minute))
	  (second (c-struct-ref v SQL_TIME_STRUCT 'second)))
      (make-time time-monotonic 0 (+ (* hour 3600) (* minute 60) second))))
  (define (db2-date->date v size) 
    (let ((year   (c-struct-ref v SQL_DATE_STRUCT 'year))
	  (month  (c-struct-ref v SQL_DATE_STRUCT 'month))
	  (day    (c-struct-ref v SQL_DATE_STRUCT 'day)))
      (make-date 0 0 0 0 day month year (date-zone-offset (current-date)))))
  (define (db2-timestamp->time v size)
    (let ((year   (c-struct-ref SQL_TIMESTAMP_STRUCT v 'year))
	  (month  (c-struct-ref SQL_TIMESTAMP_STRUCT v 'month))
	  (day    (c-struct-ref SQL_TIMESTAMP_STRUCT v 'day))
	  (hour   (c-struct-ref SQL_TIMESTAMP_STRUCT v 'hour))
	  (minute (c-struct-ref SQL_TIMESTAMP_STRUCT v 'minute))
	  (second (c-struct-ref SQL_TIMESTAMP_STRUCT v 'second))
	  (fraction  (c-struct-ref SQL_TIMESTAMP_STRUCT v 'fraction)))
      ;; TODO probably we need to get database timezone.
      (make-date fraction second minute hour day month year 0)))

  (define (array-pointer->sinteger v size) 
    (let ((bv (pointer->bytevector v size)))
      (bytevector-uint-ref bv 0 (endianness native) size)))
  (define (pointer->string-with-length p size)
    (let ((bv (pointer->bytevector p size)))
      (bytevector->string bv (native-transcoder))))

  (define (array-pointer->float v size) 
    (let ((bv (pointer->bytevector v size)))
      (bytevector-ieee-single-native-ref bv 0)))
  (define (array-pointer->double v size) 
    (let ((bv (pointer->bytevector v size)))
      (bytevector-ieee-double-native-ref bv 0)))

  (define *db-type<->c-type*
    `((,+sql-time+      . #(,+sql-c-type-time+ ,(size-of-c-struct SQL_TIME_STRUCT) ,db2-time->time))
      (,+sql-date+      . #(,+sql-c-type-date+ ,(size-of-c-struct SQL_DATE_STRUCT) ,db2-date->date))
      (,+sql-timestamp+ . #(,+sql-c-type-timestamp+ ,(size-of-c-struct SQL_TIMESTAMP_STRUCT) ,db2-timestamp->time))
      (,+sql-decimal+   . #(,+sql-c-slong+     ,size-of-long  ,array-pointer->sinteger))
      (,+sql-integer+   . #(,+sql-c-slong+     ,size-of-long  ,array-pointer->sinteger))
      (,+sql-smallint+  . #(,+sql-c-sshort+    ,size-of-short ,array-pointer->sinteger))
      (,+sql-char+      . #(,+sql-c-char+      #f ,pointer->string-with-length))
      (,+sql-binary+    . #(,+sql-c-binary+    #f ,pointer->bytevector))

      (,+sql-bigint+    . #(,+sql-c-sbigint+   ,size-of-int64_t  ,array-pointer->sinteger))
      (,+sql-real+      . #(,+sql-c-float+     ,size-of-float    ,array-pointer->float))
      (,+sql-float+     . #(,+sql-c-float+     ,size-of-float    ,array-pointer->float))
      (,+sql-double+    . #(,+sql-c-double+    ,size-of-double   ,array-pointer->double))
      ))

  (define (db2-get-data stmt index)
    (define (get-data stmt index type len ind)
      (let ((buffer (allocate-pointer len)))
	(let ((r (sqlget-data (pointer->integer (~ stmt 'handle)) index
			      type buffer len
			      (if ind (address ind) null-pointer))))
	  (check-error 'db2-get-data stmt r)
	  (values buffer r ind))))
    (define (read-var-data type as-string?)
      (define (read)
	(let-values (((out extract) (open-bytevector-output-port)))
	  (let loop ((ind (empty-pointer)))
	    (let-values (((buffer r ind) (get-data stmt index type 512 ind)))
	      (let ((vind (pointer->integer ind)))
		(cond ((= r +sql-no-data+) (extract))
		      ((= vind +sql-null-data+) #f)
		      (else
		       (let ((bv (pointer->bytevector
				  buffer
				  (if (or (> vind 512) 
					  (= vind +sql-no-total+))
				      512
				      vind))))
			 (put-bytevector out bv)
			 (loop ind)))))))))
      (let ((bv (read)))
	(if bv
	    (if as-string? (bytevector->string bv (native-transcoder)) bv)
	    '())))
    (define (read-as-port type as-string?)
      (error 'db2-get-data "blob/clob is not supported, yet"))
    (let ((type (db2-column-attribute stmt index +sql-desc-type+))
	  (len  (db2-column-attribute stmt index +sql-desc-octet-length+)))
      ;; special case first
      (cond ((= type +sql-varchar+)       (read-var-data +sql-c-default+ #t))
	    ((= type +sql-varbinary+)     (read-var-data +sql-c-default+ #f))
	    ((= type +sql-longvarchar+)   (read-as-port +sql-c-default+ #t))
	    ((= type +sql-longvarbinary+) (read-as-port +sql-c-default+ #f))
	    ((assv type *db-type<->c-type*) =>
	     (lambda (slot)
	       (let* ((info (cdr slot))
		      (type (vector-ref info 0))
		      (size (vector-ref info 1))
		      (conv (vector-ref info 2)))
		 (let-values (((buffer ret ind) (get-data stmt index
							  type (or size len)
							  #f)))
		   (check-error 'db2-get-data stmt ret)
		   (conv buffer size)))))
	    (else (error 'db2-get-data "unsupported column type" type)))))
  


  (define (db2-row-count stmt)
    (let ((len (empty-pointer)))
      (check-error 'db2-row-count stmt
		   (sqlrow-count (pointer->integer (~ stmt 'handle))
				 (address len)))
      (pointer->integer len)))

  ;; todo db-column-size
  (define (db2-column-count stmt)
    (let ((len (empty-pointer)))
      (check-error 'db2-column-count stmt
		   (sqlnum-result-cols (pointer->integer (~ stmt 'handle))
				       (address len)))
      (pointer->integer len)))


)
