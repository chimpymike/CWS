(defpackage #:cws
  (:use #:cl))

(in-package #:cws)

;; Global variable for the server's address
(defparameter *server-address* nil)
;; Global variable for the server's main listening socket
(defparameter *server-socket* nil)

(defun start-server (address port &optional (timeout 5))
  (let ((socket (setf *server-socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
	(server-address (setf *server-address* (sb-bsd-sockets:make-inet-address address))))
    (sb-bsd-sockets:socket-bind socket server-address port)
    (sb-posix:setgid 1000)
    (sb-posix:setuid 1000)
    (sb-bsd-sockets:socket-listen socket 5))
  (sb-thread:make-thread (lambda () (server-accept-thread :timeout timeout)) :name "cws-accepting-thread"))

(defun stop-server ()
  (sb-bsd-sockets:socket-close *server-socket*))

(defun client-disconnect (client-socket)
  (sb-bsd-sockets:socket-close client-socket))

(defun client-send (client-stream message)
  (write-line message client-stream))

(defun client-receive (client-stream)
  (with-open-file (logfile #p"/home/mc/cws.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (print (loop for msg = (read-line client-stream)
       while (string-not-equal msg (string #\return))
       collect msg) logfile)))

(defun index-page-response (client-stream page-content)
  (client-send client-stream "HTTP/1.1 200 OK")
  (client-send client-stream "Content-Type: text/html")
  (client-send client-stream (concatenate 'string "Content-Length: " (write-to-string (length page-content))))
  (client-send client-stream "Connection: keep-alive")
  (client-send client-stream "")
  (client-send client-stream page-content))

(defun split-string (string &key (delimiter #\space))
  (loop for i = 0 then (1+ j)
       as j = (position delimiter string :start i)
       collect (subseq string i j)
       while j))

(defun client-handler (client-socket client-stream)
  (handler-case
      (loop (progn (let ((client-request (split-string (car (client-receive client-stream)))))
		     (cond
		       ((/= (length client-request) 3) (client-send client-stream "Bad Request. Not 3."))
		       ((string-not-equal "GET" (car client-request)) (client-send client-stream "Bad Request. No GET."))
		       ((string-not-equal (concatenate 'string "HTTP/1.1" (string #\return)) (third client-request)) (client-send client-stream "Bad Request. Protocol"))
		       ((string-not-equal "/" (second client-request)) (not-found-response client-stream))
		       (t (index-page-response client-stream "Hello Millie"))))))
    (sb-sys:io-timeout () (client-disconnect client-socket))
    (sb-int:simple-stream-error () (client-disconnect client-socket))
    (end-of-file () (client-disconnect client-socket))))

(defun not-found-response (client-stream)
  (client-send client-stream "HTTP/1.1 404 Not Found")
  (client-send client-stream "Server: cws/0.1")
  (client-send client-stream "Content-Type: text/html")
  (client-send client-stream "Content-Length: 13")
  (client-send client-stream "Connection: keep-alive")
  (client-send client-stream "")
  (client-send client-stream "404 Not Found"))

(defun server-accept-thread (&key (timeout 5))
  (loop
       (progn
	 (let* ((client-socket (sb-bsd-sockets:socket-accept *server-socket*))
		(client-stream (sb-bsd-sockets:socket-make-stream client-socket
								  :input t
								  :output t
								  :buffering :none
								  :timeout timeout)))
	   (sb-thread:make-thread (lambda () (client-handler client-socket client-stream))
				  :name (make-connection-thread-name client-socket))))))

(defun make-string-from-inet-address (inet-address)
  (format nil "~{~A~^.~}" (map 'list #'write-to-string inet-address)))

(defun make-connection-thread-name (client-socket)
  (multiple-value-bind (client-address client-port) (sb-bsd-sockets:socket-peername client-socket)
    (concatenate 'string
		 "cws-connection-thread-"
		 (make-string-from-inet-address client-address)
		 ":"
		 (write-to-string client-port))))

