#!/usr/bin/env gosh
;; -*- coding: utf-8; mode: gauche; -*-
;; Author: SAITO Atsushi

(define-constant *fsencode*  ;; file-system encoding
  (cond-expand (gauche.os.windows 'Shift_JIS)
               (else 'utf8)))

(define-syntax use* (syntax-rules () ((_ a ...) (begin (use a) ...))))

(use* rfc.http rfc.uri rfc.cookie rfc.json www.cgi file.util
      srfi-1 srfi-11 srfi-13 
      gauche.process  gauche.charconv gauche.parameter
      gauche.parseopt gauche.collection gauche.generator)

(define option-resume (make-parameter #f))
(define option-show (make-parameter #f))

(define (http-youtube path)
  (receive (status header body)
      (http-get "www.youtube.com" path)
    body))

(define (fsencode str)
  (ces-convert str (gauche-character-encoding) *fsencode*))

(define (video-info video-id)
  (cgi-parse-parameters :query-string
    (http-youtube
     `("/get_video_info"
       (video_id ,video-id)
       (el "detailpage") (ps "default") (eurl "") (gl "US") (hl "en")))))

(define (rxmatch-all reg str :optional (index 0))
  ($ generator->list $ gmap (cut <> index) $ grxmatch reg str))

(define-method ref ((lst <list>) (str <string>))
  (cgi-get-parameter str lst))

(define (ref$ index) (cut ref <> index))
(define fmt-stream-map (ref$ "url_encoded_fmt_stream_map"))
(define fmt-list (ref$ "fmt_list"))
(define rtmp? (ref$ "stream"))
(define title (ref$ "title"))
(define video-id (ref$ "video_id"))
(define timestamp (ref$ "timestamp"))
(define itag (ref$ "itag"))
(define conn (ref$ "conn"))
(define (movie-url info)
  (string-append (~ info "url") "&signature=" (~ info "sig")))

(define (url->video-id url)
  (if-let1 matchobj
      (#/((?:https?:\/\/)?(?:youtu\.be\/|(?:\w+\.)?youtube(?:-nocookie)?\.com\/(?:(?:v\/)|(?:(?:watch(?:_popup)?(?:\.php)?)?(?:\?|#!?)(?:.+&)?v=))))?([0-9A-Za-z_-]+)(?(1).+)?$/ url)
    (matchobj 2)
    #f))

(define video-extensions
  '(( "5" . "flv")  ( "6" . "flv")  ("34" . "flv")  ("35" . "flv")
    ("18" . "mp4")  ("22" . "mp4")  ("37" . "mp4")  ("38" . "mp4")
    ("82" . "mp4")  ("83" . "mp4")  ("84" . "mp4")  ("85" . "mp4")
    ("43" . "webm") ("44" . "webm") ("45" . "webm") ("46" . "flv")
    ("100" . "webm")("101" . "webm")("102" . "webm")
    ("13" . "3gp")  ("17" . "3gp")  ("36" . "flv")))

(define (fmt-split fmt)
  (map (pa$ cgi-parse-parameters :query-string)
       (string-split fmt #\,)))

(define (template-extract template params)
  (regexp-replace-all
   #/$\((id|title|ext|timestamp)\)/
   template
   (lambda(m)
     (if-let1 e (cgi-get-parameter (m 1) params)
       e
       (error "invalid template parameter:" e)))))

(define (sanitize title)
  (regexp-replace-all #/[\/()"?<>|:;\*\r\n]/ title "_"))

(define (make-template-items info format-number flag)
  `(("id" ,(video-id info))
    ("ext" ,(if flag "flv" (assoc-ref video-extensions format-number)))
    ("title" ,(fsencode (sanitize (title info))))
    ("timestamp" ,(timestamp info))))

(define select-max-quality car)

(define (format-ref formats i)
  (find (^x (string=? (itag x) i)) formats))

(define (url-split url)
  (let1 m (#/^http:\/\/(.+?)(\/[^?]+)(?:\?(.+))?$/ url)
    (values (m 1) (m 2) (m 3))))

(define (http-get* url file)
  (define ext
    (if (and (option-resume) (file-exists? file))
        (list :range #`"bytes=,(file-size file)-")
        '()))
  (call-with-output-file file
    (lambda (port)
      (let-values (((serv path query) (url-split url)))
        (apply http-get serv
               (string-append path "?" query)
               :sink port :flusher (lambda _ #t)
               ext)))
    :if-exists (if (null? ext) :supersede :append)))

(define (header->content-size header)
  (string->number
   (cond ((cgi-get-parameter "content-range" header)
          => (^x ((#/\/(\d+)/ x) 1)))
         ((cgi-get-parameter "content-length" header)
          => values)
         (else "0"))))

(define (fmt-url-split fmt)
  (map (cut string-split <> #\|) (string-split fmt #\,)))

(define (download-http info required-format-number template)
  (let* ((available-formats (fmt-split (fmt-stream-map info)))
         (selected-format
          (if required-format-number
              (format-ref available-formats required-format-number)
              (select-max-quality available-formats))))
    (let ((url (movie-url selected-format))
          (file (template-extract
                 template
                 (make-template-items info
                                      (itag selected-format)
                                      #f)))
          (temporary #`",(video-id info).,(itag selected-format).part"))
      (display #`",(video-id info) downloading ...")
      (flush)
      (unless (and (option-resume) (file-exists? file))
        (receive (status header body)
            (http-get* url temporary)
          (if (= (file-size temporary)
                 (header->content-size header))
              (begin
                (sys-rename temporary file)
                (display #`" complete.\n"))
              (display #`", failed.\n")))))))

(define (show-available-format infomation)
  (display #`",(video-id infomation)\n")
  (generator-for-each
   (^x (display #`"\t,(x 1) - ,(x 2) ,(assoc-ref video-extensions (x 1))\n"))
   (grxmatch #/(\d+)\/(\d+x\d+)(?:\/\d+){3}/ (fmt-list infomation))))

(define (download video-id format-number template)
  (let1 infomation (video-info video-id)
    (cond ((option-show) (show-available-format infomation))
          ((rtmp? infomation)
           (display #`"not support rtmp.\n"))
          (else
           (download-http infomation format-number template)))))

(define (usage path)
  (display
   #`"Usage: ,|path| [options] url...

Options:
  -h, --help            print this help text and exit
  -s, --show            show available format

  Video Format Options:
    -f FORMAT, --format=FORMAT     video format code (default is best)

  Filesystem Options:
    -o TEMPLATE, --output=TEMPLATE
                        output filename template
    -a FILE, --batch-file=FILE
                        file containing URLs to download
    -c, --continue      resume partially downloaded files")
  (exit))

(define (main args)
  (let-args (cdr args)
      ((template "o|output=s" "$(id) $(title).$(ext)")
       (listfile "a|batch-file=s" #f)
       (format   "f|format=s" #f)
       (show     "s|show" => (cut option-show #t))
       (continue "c|continue" => (cut option-resume #t))
       (help     "h|help" => (cut usage (car args)))
       . targets)
    (let1 targets (if listfile (file->list read-line listfile) targets)
      (if (null? targets)
          (usage (car args))
          (map (^x (download (url->video-id x) format template)) targets))
      0)))
