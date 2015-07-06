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

(define (positions-block)
  (let ((positions (cdr (assq 'positions cv-data))))
    (list 
      (<h3> "Professional Experience")
      (<div>
        (reverse 
          (vector-fold 
            (lambda (i tail position)
              (let* ((company          (json-ref 'company   position))
                     (end-date         (json-ref 'endDate   position))
                     (start-date       (json-ref 'startDate position))
                     (summary          (json-ref 'summary   position))
                     (keywords         (json-ref 'keywords  position))
                     (title            (json-ref 'title     position)))
                (cons (<div> class: "row"
                             (<div> class: "col-3 text-right"
                                    (<b> start-date " - "
                                         (if end-date
                                           end-date
                                           (<span> class: "label label-success" "[PRESENT]"))
                                         ""))
                             (<div> class: "col-8"
                                    (<b> title) " at " (<span> class: "company" company)
                                    (<p>)
                                    (<p> (if summary summary ""))
                                    (if keywords
                                      (<p> "Keywords: " (string-join (vector->list keywords) ", "))
                                      "")))
                      tail)))
            '()
            positions))))))

(define (publications-block)
  (let ((publications (cdr (assq 'publications cv-data))))
    (list 
      (<h3> "Publications")
      (<div>
        (reverse 
          (vector-fold 
            (lambda (i tail publication)
              (let* ((authors (json-ref 'authors publication))
                     (date    (json-ref 'date    publication))
                     (title   (json-ref 'title   publication))
                     (where   (json-ref 'where   publication)))
                (cons (list 
                        (<p> date " - " (<b> title ". "))
                        (<ul> style: "list-style-type: none"
                            (<li> where ". ")
                            (<li> (<i> authors))))
                      tail)))
            '()
            publications))))))

(define (heducation-block)
  (let ((education-data (cdr (assq 'higher-education cv-data))))
    (list 
      (<h3> "Higher Education")
      (<div>
        (reverse 
          (vector-fold 
            (lambda (i tail education)
              (let* ((institution (json-ref 'institution education))
                     (start-date  (json-ref 'startDate   education))
                     (end-date    (json-ref 'endDate     education))
                     (title       (json-ref 'title       education))
                     (comments    (json-ref 'comments    education)))
                (cons (<div> class: "row"
                             (<div> class: "col-4 text-right"
                                    (<b> start-date " - " (if end-date
                                                            end-date 
                                                            (<span> class: "label label-success" "[PRESENT]"))))
                             (<div> class: "col-8"
                                    (<span> title " at " institution " " comments)))
                      tail)))
            '()
            education-data))))))

(define (other-education-block)
  (let ((education-data (cdr (assq 'other-education cv-data))))
    (list 
      (<h3> "Other Education")
      (<div>
        (reverse 
          (vector-fold 
            (lambda (i tail education)
              (let* ((where (json-ref 'where education))
                     (title (json-ref 'title education))
                     (date  (json-ref 'date education))
                     (desc  (json-ref 'desc  education)))
                (cons (<div> class: "row"
                             (<div> class: "col-4 text-right"
                                    (<b> date " - " where))
                             (<div> class: "col-8"
                                    (<span> title)))
                      tail)))
            '()
            education-data))))))

(define (skills-block)
  (let ((skills (cdr (assq 'skills cv-data))))
    (list 
      (<h3> "Computer Skills")
      (<div>
        (reverse 
          (vector-fold 
            (lambda (i tail skill)
              (let* ((name (json-ref 'name skill))
                     (keys (json-ref 'keys skill)))
                (cons (<div> class: "row"
                             (<div> class: "col-4 text-right"
                                    (<b> name ":"))
                             (<div> class: "col-8"
                                    (string-join (vector->list keys) ", ")))
                      tail)))
            '()
            skills))))))

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
