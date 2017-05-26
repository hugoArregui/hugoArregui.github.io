(use srfi-1 sxml-transforms html-conversion-rules)

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

(define (ref-list key data)
  (let ((r (assq key data)))
    (and r
         (cdr r))))

(define (two-cols-section data title f #!key (left-col-size 3))
  `(div (@
         (class "section"))
        (h2 ,title)
        (div
         ,(fold-right
           (lambda (data tail)
             (let-values (((col1 col2) (f data)))
               (cons `(row (div (@ (class "three columns left-col"))
                                ,col1)
                           (div (@ (class "nine columns"))
                                ,col2))
                     tail)))
           '()
           data))))

(define (positions-block)
  (two-cols-section
   (ref-cv-data 'positions)
   "Professional Experience"
   (lambda (position)
     (let* ((ref        (cut ref <> position))
            (summary    (ref-list 'summary position))
            (company    (ref 'company))
            (website    (ref 'company-website))
            (end-date   (ref 'endDate))
            (start-date (ref 'startDate))
            (keywords   (ref 'keywords))
            (title      (ref 'title)))
       (values
        `(b ,start-date " - "
            ,(or end-date
                 '(span (@ (class "present")) "PRESENT")))
        `((b ,title) " at "
          ,(if website
               `(a (@ (href ,website) (class "company")) ,company)
               `(span (@ (class "company")) ,company))
          (p)
          ,(map (lambda (s) `(p ,s)) (or summary '()))
          ,(if keywords
               `(p (em ,(string-join keywords ", ")))
               '())))))
   left-col-size: 3))

(define (publications-block)
  (two-cols-section
   (ref-cv-data 'publications)
   "Publications"
   (lambda (publication)
     (let* ((ref          (cut ref <> publication))
            (authors      (ref 'authors))
            (date         (ref 'date))
            (title        (ref 'title))
            (where        (ref 'where))
            (article-link (ref 'article-link)))
       (values
        `(p (b ,date))
        `(div
          (p (b ,title) ,(if article-link
                             `(a (@ (href ,article-link)) (small "(read it)"))
                             '()))
          (p ,where)
          (p (i ,authors))))))
   left-col-size: 3))

(define (heducation-block)
  (two-cols-section
   (ref-cv-data 'higher-education)
   "Higher Education"
   (lambda (education)
     (let* ((ref         (cut ref <> education))
            (institution (ref 'institution))
            (start-date  (ref 'startDate))
            (end-date    (ref 'endDate))
            (title       (ref 'title))
            (comments    (ref 'comments)))
       (values
        `(b ,start-date
            " - "
            ,(or end-date
                 '(span (@ (class "label label-success" "[PRESENT]")))))
        `(span ,title " at " ,institution " " ,comments))))))

(define (other-education-block)
  (two-cols-section
   (ref-cv-data 'other-education)
   "Other Education"
   (lambda (education)
     (let* ((ref   (cut ref <> education))
            (where (ref 'where))
            (title (ref 'title))
            (title-en (ref 'title-en))
            (date  (ref 'date))
            (desc  (ref 'desc)))
       (values `(b ,date " - " ,where)
               `(div
                 ,(if title-en
                      `(span ,title(em " - (" ,title-en ")"))
                      `(span ,title))))))))

(define (personal-info-block)
  `((itemize
     (@
      (class "contact-information"))
     "Birth date: December 2, 1986"
     "Nationality: Argentina"
     (url "mailto:hugo.arregui.laboral@gmail.com" "Email")
     (url "http://ar.linkedin.com/in/hugoarregui/" "LinkedIn")
     (url "https://github.com/hugoArregui" "GitHub")
     (url "https://www.openhub.net/accounts/hugo_arregui" "OpenHub"))))

(define (intro-block)
  `((div
     (p (u "Some things I like:"))
     (itemize (@ (class "intro"))
              ,@(ref-cv-data 'likes))
     (p (u "Some things I would like to do:"))
     (itemize (@ (class "intro"))
              ,@(ref-cv-data 'to-do)))))

(define (write-html dest sxml-content)
  (sxml->html
   `((doctype-html)
     (html
      (head
       (title "Hugo Arregui")
       (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
       (meta (@ (name "viewport") (content "width=device-width, initial-scale=1">)))
       (link (@ (rel "shortcut icon") (type "image/x-icon") (href "favicon.ico")))
       (link (@ (href "//fonts.googleapis.com/css?family=Raleway:400,300,600" (rel "stylesheet") (type "text/css"))))
       (css-link "static/Skeleton-2.0.4/css/normalize.css")
       (css-link "static/Skeleton-2.0.4/css/skeleton.css")
       ,(if (eq? dest 'web)
            '(css-link "static/css/site.css")
            '(css-link "static/css/pdf.css")))
      (body
       ,sxml-content)))))

(define (generate)
  (with-output-to-file
      "cv.html"
    (cut write-string
         (write-html 'web
                     `(container
                       (h1 "Hugo Arregui")
                       (hr)
                       (row
                        (div (@ (class "nine columns"))
                             ,(intro-block)
                             ,(positions-block)
                             ,(publications-block)
                             ,(heducation-block)
                             ,(other-education-block))
                        (div (@ (class "three columns"))
                             ,(personal-info-block)))))))
  (with-output-to-file
      "cv-for-pdf.html"
    (cut write-string
         (write-html 'pdf
                     `(div
                       (h1 "Hugo Arregui")
                       (hr)
                       (div
                        (p "There is much nicer version of this CV " (a (@ (href "http://hugoarregui.github.io/cv.html")) "online"))
                        (br)
                        ,(personal-info-block)
                        (br)
                        ,(intro-block)
                        (br)
                        ,(positions-block)
                        ,(publications-block)
                        ,(heducation-block)
                        ,(other-education-block)))))))

(generate)
