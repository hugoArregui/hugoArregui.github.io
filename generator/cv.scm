(use srfi-1 sxml-transforms html-conversion-rules )

(define cv-data (with-input-from-file "generator/data.scm" read))

(define sxml->html
  (let ((rules (rules)))
    (lambda (sxml)
      (with-output-to-string
        (cut SRV:send-reply (pre-post-order* sxml rules))))))

(define (ref-cv-data key)
  (ref key cv-data))

(define (ref key data)
  (let ((r (assq key data)))
    (and r
         (cadr r))))

(define (two-cols-section data title f #!key (left-col-size 3))
  `(div (@ (class "keeptogether"))
        (h3 ,title)
        (div
          ,(fold-right
             (lambda (data tail)
               (let-values (((col1 col2) (f data)))
                 (cons `(row (div (@ (class ,(string-append "col-" (number->string left-col-size) " text-right")))
                                  ,col1)
                             (div (@ (class ,(string-append "col-" (number->string (- 12 left-col-size)))))
                                  ,col2))
                       tail)))
             '()
             data))))

(define (positions-block)
  (two-cols-section (ref-cv-data 'positions)
                    "Professional Experience"
                    (lambda (position)
                      (let* ((ref        (cut ref <> position))
                             (company    (ref 'company))
                             (website    (ref 'company-website))
                             (end-date   (ref 'endDate))
                             (start-date (ref 'startDate))
                             (summary    (ref 'summary))
                             (keywords   (ref 'keywords))
                             (title      (ref 'title)))
                        (values
                          `(b ,start-date " - "
                              ,(or end-date
                                   '(span (@ (class "label label-success")) "[PRESENT]")))
                          `((b ,title) " at " 
                                       ,(if website
                                         `(a (@ (href ,website) (class "company")) ,company)
                                         `(span (@ (class "company")) ,company))
                                       (p)
                                       (p ,(or summary '()))
                                       ,(if keywords
                                          `(p (em "Keywords: ") ,(string-join keywords ", "))
                                          '())))))
                    left-col-size: 3))

(define (publications-block)
  (two-cols-section (ref-cv-data 'publications)
                    "Publications"
                    (lambda (publication)
                      (let* ((ref          (cut ref <> publication))
                             (authors      (ref 'authors))
                             (date         (ref 'date))
                             (title        (ref 'title))
                             (where        (ref 'where))
                             (article-link (ref 'article-link)))
                        (values
                          `(b ,date)
                          `(p
                             (span (b ,title))
                             (br)
                             (span ,where)
                             (br)
                             (span (i ,authors))
                             ,(if article-link
                                `((br)
                                  (a (@ (href ,article-link)) "link to the article"))
                                '())))))
                    left-col-size: 3))

(define (heducation-block)
  (two-cols-section (ref-cv-data 'higher-education)
                    "Higher Education"
                    (lambda (education)
                      (let* ((ref         (cut ref <> education))
                             (institution (ref 'institution))
                             (start-date  (ref 'startDate))
                             (end-date    (ref 'endDate))
                             (title       (ref 'title))
                             (comments    (ref 'comments)))
                        (values
                          `(b ,start-date " - " ,(or end-date
                                                     '(span (@ (class "label label-success" "[PRESENT]")))))
                          `(span ,title " at " ,institution " " ,comments))))))

(define (other-education-block)
  (two-cols-section (ref-cv-data 'other-education)
                    "Other Education"
                    (lambda (education)
                      (let* ((ref   (cut ref <> education))
                             (where (ref 'where))
                             (title (ref 'title))
                             (title-en (ref 'title-en))
                             (date  (ref 'date))
                             (desc  (ref 'desc)))
                        (values `(b ,date " - " ,where) `(div 
                                                              ,(if title-en
                                                                 `(span ,title(em " - (" ,title-en ")"))
                                                                 `(span ,title))))))))

(define (skills-block)
  (two-cols-section (ref-cv-data 'skills)
                    "Computer Skills"
                    (lambda (skill)
                      (let* ((name (car skill))
                             (keys (cadr skill)))
                        (values `(b ,name ":") (string-join keys ", "))))))

(define (personal-info-block)
  `(
    (itemize
      "Birth date: December 2, 1986"
      "Nationality: Argentina"
      (url "mailto:hugo.arregui.laboral@gmail.com" "hugo.arregui.laboral@gmail.com")
      (url "http://ar.linkedin.com/in/hugoarregui/" "LinkedIn Profile")
      (url "https://github.com/hugoArregui" "GitHub Profile")
      (url "http://code.google.com/u/hugo.arregui/" "Google Code Profile")
      (url "https://www.openhub.net/accounts/hugo_arregui" "OpenHub Profile"))))

(define (write-html sxml-content)
  (sxml->html 
    `((doctype-html)
      (html
        (head
          (title "Hugo Arregui")
          (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
          (css-link "css/bootstrap.css")
          (css-link "css/bootstrap-glyphicons.css")
          (css-link "css/site.css"))
        (body
          ,sxml-content
          (js-link "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js")
          (js-link "js/bootstrap.min.js"))))))

(define (generate)
  (with-output-to-file "cv.html" (cut write-string (write-html `(container
                                                                  (page-header (h1 "Hugo Arregui"))
                                                                  (row
                                                                    (div (@ (class "col-9"))
                                                                         ,(positions-block)
                                                                         ,(publications-block)
                                                                         ,(heducation-block)
                                                                         ,(other-education-block)
                                                                         ,(skills-block))
                                                                    (div (@ (class "col-3"))
                                                                         ,(personal-info-block)))))))
  (with-output-to-file "cv-for-pdf.html" (cut write-string (write-html `(container
                                                                          (h1 "Hugo Arregui")
                                                                          (hr)
                                                                          ,(personal-info-block)
                                                                          ,(positions-block)
                                                                          ,(publications-block)
                                                                          ,(heducation-block)
                                                                          ,(other-education-block)
                                                                          ,(skills-block))))))

(generate)
