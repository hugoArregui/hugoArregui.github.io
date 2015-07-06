(use html-tags sxml-transforms html-utils)

(generate-sxml? #t)

(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define cv-data (with-input-from-file "generator/data.scm" read))

(define (ref-cv-data key)
  (ref key cv-data))

(define (ref key data)
  (let ((r (assq key data)))
    (and r
         (cadr r))))

(define (two-cols-block data title f #!key (left-col-size 3))
  (list
    (<h3> title)
    (<div>
      (fold-right
        (lambda (data tail)
          (let-values (((col1 col2) (f data)))
            (cons (<div> class: "row"
                         (<div> class: (string-append "col-" (number->string left-col-size) " text-right")
                                col1)
                         (<div> class: (string-append "col-" (number->string (- 12 left-col-size)))
                                col2))
                  tail)))
        '()
        data))))

(define (positions-block)
  (two-cols-block (ref-cv-data 'positions)
                  "Professional Experience"
                  (lambda (position)
                    (let* ((ref        (cut ref <> position))
                           (company    (ref 'company))
                           (end-date   (ref 'endDate))
                           (start-date (ref 'startDate))
                           (summary    (ref 'summary))
                           (keywords   (ref 'keywords))
                           (title      (ref 'title)))
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
                            (<p> "Keywords: " (string-join keywords ", "))
                            "")))))
                  left-col-size: 3))

(define (publications-block)
  (two-cols-block (ref-cv-data 'publications)
                  "Publications"
                  (lambda (publication)
                    (let* ((ref     (cut ref <> publication))
                           (authors (ref 'authors))
                           (date    (ref 'date))
                           (title   (ref 'title))
                           (where   (ref 'where)))
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
  (two-cols-block (ref-cv-data 'higher-education)
                  "Higher Education"
                  (lambda (education)
                    (let* ((ref         (cut ref <> education))
                           (institution (ref 'institution))
                           (start-date  (ref 'startDate))
                           (end-date    (ref 'endDate))
                           (title       (ref 'title))
                           (comments    (ref 'comments)))
                      (values
                        (<b> start-date " - " (if end-date
                                                end-date
                                                (<span> class: "label label-success" "[PRESENT]")))
                        (<span> title " at " institution " " comments))))))

(define (other-education-block)
  (two-cols-block (ref-cv-data 'other-education)
                  "Other Education"
                  (lambda (education)
                    (let* ((ref   (cut ref <> education))
                           (where (ref 'where))
                           (title (ref 'title))
                           (date  (ref 'date))
                           (desc  (ref 'desc)))
                      (values (<b> date " - " where) (<span> title))))))

(define (skills-block)
  (two-cols-block (ref-cv-data 'skills)
                  "Computer Skills"
                  (lambda (skill)
                    (let* ((name (car skill))
                           (keys (cadr skill)))
                      (values (<b> name ":") (string-join keys ", "))))))

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

(with-output-to-file "cv.html" (lambda () (write-string (cv->html))))
