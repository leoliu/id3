;;; id3.el --- emacs reads and writes ID3v2 tags     -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.5.0
;; Keywords: tools, multimedia, data
;; Created: 2013-08-28

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support ID3 v2.3 and v2.4. References: http://id3.org

;; TODO: ID3 Writer

;;; Code:

(eval-when-compile (require 'cl))

(defun id3-prefix-p (l1 l2)
  "Return non-nil if L1 is a prefix of L2."
  (cond
   ((null l1) t)
   ((eq (car l1) (car l2))
    (id3-prefix-p (cdr l1) (cdr l2)))))

(defun id3-split-list (list sep)
  ;; (id3-split-list '(1 2 3 0 0 4 5) '(0 0)) => ((1 2 3) (4 5))
  ;; (id3-split-list '(1 2 3 0 0 4 5) 3) => ((1 2 3) (0 0 4 5))
  (loop for tail on list
        for count from 0
        while (if (integerp sep)
                  (< count sep)
                (not (id3-prefix-p sep tail)))
        collect (car tail) into front
        finally return (list front (if (integerp sep)
                                       tail
                                     (nthcdr (length sep) tail)))))

(defun id3-split-list* (list seps)
  ;; (id3-split-list* '(1 2 3 0 0 4 5) '(1 (0 0) 1)) => ((1) (2 3) (4) (5))
  (if (null seps)
      (list list)
    (pcase-let ((`(,l1 ,l2)
                 (id3-split-list list (car seps))))
      (list* l1 (id3-split-list* l2 (cdr seps))))))

(defun id3-text-encoding (encoding-byte)
  (pcase encoding-byte
    (`0 'iso-8859-1)
    (`1 'utf-16)
    (`2 'utf-16be)
    (`3 'utf-8)))

(defun id3-string-terminator (encoding-byte)
  (if (memq encoding-byte '(1 2)) '(0 0) '(0)))

(defun id3-bytes-to-string (bytes &optional encoding-byte)
  (if encoding-byte
      (let ((str (decode-coding-string (apply #'unibyte-string bytes)
                                       (id3-text-encoding encoding-byte))))
        ;; Remove the string terminator.
        (if (string-match (concat (char-to-string 0) "+\\'") str)
            (substring str 0 (match-beginning 0))
          str))
    (apply #'unibyte-string bytes)))

;;;; ID3 Reader

(defun id3-read-byte ()
  (prog1 (get-byte)
    (forward-char 1)))

(defun id3-read-bytes (&optional n f)
  (let ((n (or n 1)))
    (check-type n integer)
    (mapcar (lambda (_)
              (if f
                  (funcall f (id3-read-byte))
                (id3-read-byte)))
            (number-sequence 1 n))))

(defun id3-file-reader (file)
  (let ((pos 0))
    (lambda (bytes)
      (insert-file-contents file nil pos (+ pos bytes))
      (setq pos (+ pos bytes)))))

;;;###autoload
(defun id3-read-file (file)
  "Read ID3 tag of FILE."
  (or (file-readable-p file) (error "File %s not readable" file))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((header (id3-read-header (id3-file-reader file))))
      (when (memq 'extended header)
        (warn "Extended header ignored")
        (id3-read-bytes (- (id3-read-size) 4)))
      (list* header (id3-read-frames (car header))))))

(defun id3-read-header (file-reader)
  (funcall file-reader 10)
  (or (equal (apply #'unibyte-string (id3-read-bytes 3)) "ID3")
      (error "File has no ID3 tag"))
  (let* ((version (prog1 (id3-read-byte) (id3-read-byte)))
         (_ (and (< version 3) (error "ID3 v2.%d unsupported" version)))
         (flags (id3-read-byte))        ; %abcd0000
         (size (id3-read-size)))
    (funcall file-reader size)
    (delq nil (list version
                    (and (= 1 (logand flags #b10000000)) 'unsync)
                    (and (= 1 (logand flags #b01000000)) 'extended)
                    (and (= 1 (logand flags #b00100000)) 'experimental)
                    (and (= 1 (logand flags #b00010000)) 'footer)))))

(defun id3-read-size (&optional bits)
  (let* ((bits (or bits 7))
         (bmask (1- (expt 2 bits))))
    (pcase-let ((`(,b4 ,b3 ,b2 ,b1)
                 (id3-read-bytes 4 (lambda (b) (logand bmask b)))))
      (logior (lsh (logior (lsh b4 bits) b3) (* 2 bits))
              (logior (lsh b2 bits) b1)))))

(defun id3-read-frames (version)
  (let ((frames))
    (ignore-errors
      (while (not (eobp))
        (push (id3-read-frame version) frames)))
    (nreverse frames)))

(defun id3-read-frame (version)
  (let* ((id (apply #'unibyte-string
                    (id3-read-bytes 4 #'id3-frame-id-check)))
         (size (id3-read-size (and (= version 3) 8)))
         (_flags (id3-read-bytes 2))
         (bytes (id3-read-bytes size))
         (value (cond
                 ((fboundp (intern-soft (format "id3-read-%s-frame" id)))
                  (funcall (intern-soft (format "id3-read-%s-frame" id)) bytes))
                 ((memq (aref id 0) '(?T ?W))
                  (id3-read-text-frame bytes))
                 ;; FIXME: signal an unimplemented error instead?
                 (t (id3-bytes-to-string bytes)))))
    (cons id value)))

(defun id3-frame-id-check (byte)
  ;; A-Z or 0-9
  (or (and (>= byte ?0) (<= byte ?9))
      (and (>= byte ?A) (<= byte ?Z))
      (error "Invalid char %s for frame id" (string byte)))
  byte)

;;; FRAMES

(defun id3-read-text-frame (bytes)      ; Text info frame
  (id3-bytes-to-string (cdr bytes) (car bytes)))

(defun id3-read-TXXX-frame (bytes)
  (split-string (id3-read-text-frame bytes) (char-to-string 0)))

(defun id3-read-APIC-frame (bytes)
  (pcase-let* ((enc (pop bytes))
               (`(,mime (,ptyp) ,descr ,data)
                (id3-split-list* bytes
                                 `((0) 1 ,(id3-string-terminator enc)))))
    (list (id3-bytes-to-string mime)
          ptyp
          (id3-bytes-to-string descr enc)
          (id3-bytes-to-string data))))

(defun id3-read-GEOB-frame (bytes)
  (pcase-let* ((enc (pop bytes))
               (`(,mime ,file ,descr ,data)
                (id3-split-list* bytes (list '(0)
                                             (id3-string-terminator enc)
                                             (id3-string-terminator enc)))))
    (list (id3-bytes-to-string mime)
          (id3-bytes-to-string file enc)
          (id3-bytes-to-string descr enc)
          (id3-bytes-to-string data))))

(defun id3-read-COMM-frame (bytes)
  (pcase-let* ((enc (pop bytes))
               (`(,lang ,descr ,text)
                (id3-split-list* bytes `(3 ,(id3-string-terminator enc)))))
    (list (id3-bytes-to-string lang)
          (id3-bytes-to-string descr enc)
          (id3-bytes-to-string text enc))))

(defun id3-read-WXXX-frame (bytes)
  (pcase-let* ((enc (pop bytes))
               (`(,descr ,url)
                (id3-split-list bytes (id3-string-terminator enc))))
    (list (id3-bytes-to-string descr enc)
          (id3-bytes-to-string url 'iso-8859-1))))

(defun id3-read-PRIV-frame (bytes)
  (pcase-let ((`(,id ,data)
               (id3-split-list bytes '(0))))
    (list (id3-bytes-to-string id)
          (id3-bytes-to-string data))))

;;;###autoload
(defun id3-view-tag-literally (file)
  "View the ID3 tag portion of FILE literally."
  (interactive "fFind file: ")
  (with-current-buffer (get-buffer-create "*ID3*")
    (erase-buffer)
    (set-buffer-multibyte nil)
    (id3-read-header (id3-file-reader file))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'id3)
;;; id3.el ends here
