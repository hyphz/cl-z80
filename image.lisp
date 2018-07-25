(in-package #:cl-z80) 

(defstruct page origin size image ip)

(defun default-page ()
  (make-page :origin 0
             :size #x2000
             :image (make-array #x2000 :adjustable t
                                :initial-element 0)
             :ip 0))

(defparameter *pages* (make-array 256 :initial-element nil))

(defparameter *current-pages* (list 0))

(defun defpage (index origin size)
  "Adds a new page to the global pages array, filled with zeroes."
  (setf (aref *pages* index)
        (make-page :origin origin
                   :size size
                   :image (make-array size)
                   :ip 0)))

(defpage 0 0 #x4000)

(defun set-page (&rest indices)
  (setq *current-pages* indices))

(defun get-page ()
  "Gets the number of the active page."
  (car *current-pages*))

(defun get-current-page ()
  "Gets the page data structure for the active page."
  (aref *pages* (get-page)))

(defun page-address ()
  "Gets the address of the IP on the current page."
  (let* ((page (get-current-page))
         (org (page-origin page))
         (ip (page-ip page)))
    (+ org ip)))

(defun out-of-bounds ()
  (let ((page (get-current-page)))
    (= (page-ip page) (page-size page))))

(defun move-to-next-page ()
  "Advances page through current page list until a page is found where
  ip is not at the end. This may NOT be exactly the next page."
  (if (null *current-pages*)
      (error "out of bounds")
      (progn
        (setq *current-pages* (cdr *current-pages*))
        (when (out-of-bounds)
          (move-to-next-page)))))

(defun emit-byte (n)
  (when (and (numberp n) (> n 255))
    (error 'jarl))
  (when (out-of-bounds)
    (move-to-next-page))
  (let* ((page (get-current-page))
         (image (page-image page))
         (ip (page-ip page)))
    (if (numberp n)
        (setf (aref image ip) n)
        (add-forward-label (get-page) ip n))
    (setf (page-ip page) (1+ ip))))

(defun emit (&rest l)
  "Adds all of the bytes in parms to the current page/ip."
  (dolist (i l)
    (emit-byte i)))

(defun emit-string (str)
  "Converts string to ASCII and emits it."
  (loop for i across str do
       (emit-byte (char-int i))))

(defun save-page (stream page)
  "Writes the contents of the given page to the stream."
  (let ((image (page-image page))
        (size (page-size page)))
    (dotimes (i size)
      (write-byte (aref image i) stream))))
            
(defun save-image (fname)
  (emit-forward-labels)
  (with-open-file (stream fname
                          :direction :output
                          :element-type 'unsigned-byte
                          :if-exists :supersede)
    (dotimes (i 256)
      (let ((page (aref *pages* i)))
        (when page (save-page stream page))))))
