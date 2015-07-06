(use medea html-tags sxml-transforms vector-lib html-utils)

(generate-sxml? #t)

(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define cv-data (with-input-from-file "generator/cv.json" read-json))

(define (list-itemize . items)
  (itemize items))

(define (json-ref key json)
  (let ((r (assq key json)))
    (if r
      (cdr r)
      #f)))

(define (two-cols-block data title f #!key (left-col-size 3))
  (list 
    (<h3> title)
    (<div>
      (reverse 
        (vector-fold 
          (lambda (i tail data)
            (let-values (((col1 col2) (f i data)))
              (cons (<div> class: "row"
                           (<div> class: (string-append "col-" (number->string left-col-size) " text-right")
                                  col1)
                           (<div> class: (string-append "col-" (number->string (- 12 left-col-size)))
                                  col2))
                    tail)))
          '()
          data)))))

(define (positions-block)
  (two-cols-block (cdr (assq 'positions cv-data))
                  "Professional Experience"
                  (lambda (i position)
                    (let* ((company          (json-ref 'company   position))
                           (end-date         (json-ref 'endDate   position))
                           (start-date       (json-ref 'startDate position))
                           (summary          (json-ref 'summary   position))
                           (keywords         (json-ref 'keywords  position))
                           (title            (json-ref 'title     position)))
                      (values
                        (<b> start-date " - "
                             (if end-date
                               end-date
                               (<span> class: "label label-success" "[PRESENT]"))
                             "")
                        (list
                          (<b> title) " at " (<span> class: "company" company)
                          (<p>)
                          (<p> (if summary summary ""))
                          (if keywords
                            (<p> "Keywords: " (string-join (vector->list keywords) ", "))
                            "")))))
                  left-col-size: 3))

(define (publications-block)
  (two-cols-block (cdr (assq 'publications cv-data))
                  "Publications"
                  (lambda (i publication)
                    (let* ((authors (json-ref 'authors publication))
                           (date    (json-ref 'date    publication))
                           (title   (json-ref 'title   publication))
                           (where   (json-ref 'where   publication)))
                      (values
                        (<b> date) 
                        (<p> style: "list-style-type: none"
                              (<span> (<b> title))
                              (<br>)
                              (<span> where)
                              (<br>)
                              (<span> (<i> authors))))))
                  left-col-size: 3))

(define (heducation-block)
  (two-cols-block (cdr (assq 'higher-education cv-data))
                  "Higher Education"
                  (lambda (i education)
                    (let* ((institution (json-ref 'institution education))
                           (start-date  (json-ref 'startDate   education))
                           (end-date    (json-ref 'endDate     education))
                           (title       (json-ref 'title       education))
                           (comments    (json-ref 'comments    education)))
                      (values
                        (<b> start-date " - " (if end-date
                                                end-date 
                                                (<span> class: "label label-success" "[PRESENT]")))
                        (<span> title " at " institution " " comments))))))

(define (other-education-block)
  (two-cols-block (cdr (assq 'other-education cv-data))
                  "Other Education"
                  (lambda (i education)
                    (let* ((where (json-ref 'where education))
                           (title (json-ref 'title education))
                           (date  (json-ref 'date education))
                           (desc  (json-ref 'desc  education)))
                      (values (<b> date " - " where) (<span> title))))))

(define (skills-block)
  (two-cols-block (cdr (assq 'skills cv-data))
                  "Computer Skills"
                  (lambda (i skill)
                    (let* ((name (json-ref 'name skill))
                           (keys (json-ref 'keys skill)))
                      (values (<b> name ":") (string-join (vector->list keys) ", "))))))

(define (cv->html)
  (sxml->html
    (html-page
      (list
        (<div> class: "container"
               (<div> class: "page-header" (<h1> "Hugo Arregui"))
               (<div> class: "row"
                      (<div> class: "col-sm-9"
                             (positions-block)
                             (publications-block)
                             (heducation-block)
                             (other-education-block)
                             (skills-block))
                      (<div> class: "col-3"
                             (<h4> "Personal Information: ")
                             (itemize 
                               (list
                                 "Birth date: December 2, 1986" 
                                 "Nationality: Argentina" 
                                 (<a> href: "mailto:hugo.arregui.laboral@gmail.com" "hugo.arregui.laboral@gmail.com")))
                             (<h4> "Networks:")
                             (itemize
                               (list
                                 (<a> href: "http://ar.linkedin.com/in/hugoarregui/"      "LinkedIn Profile")
                                 (<a> href: "https://github.com/hugoArregui"              "GitHub Profile")
                                 (<a> href: "http://code.google.com/u/hugo.arregui/"      "Google Code Profile")
                                 (<a> href: "https://www.ohloh.net/accounts/hugo_arregui" "Ohloh Profile"))))))
        (<script> src: "http://code.jquery.com/jquery.js")
        (<script> src: "js/bootstrap.min.js"))
      charset: "utf-8"
      css: '("css/bootstrap.min.css" "css/site.css" "css/bootstrap-glyphicons.css")
      title: "Hugo Arregui")))

(print (cv->html))
