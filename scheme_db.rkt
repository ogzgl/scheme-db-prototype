#lang racket
(define students-grades 
'(
    ("ali" "CENG212" "AA")
    ("ayse" "CENG463" "AA")
    ("oguz" "CENG213" "BA")
    ("elmas" "CENG212" "BB")            
    ("ahmet" "CENG213" "CC")
    ("ahmet" "EE175" "CC")
    
))
(define course-list 
'(
    ("CENG212" "Concepts-of-Programming-Languages")
    ("CENG463" "Introduction to Machine Learning")
    ("CENG213" "Theory of Computation")
))

(define (insert-std input)
    (append students-grades input)
)
(define (insert-course input)
    (append course-list input)
)

(define (insert input)
    (cond 
        [(= 2 (length input)) (insert-course (list input))]
        [(= 3 (length input)) (insert-std (list input))]
        (else
            (display "The number of given parameters does not hold. Try with different params. 2 p for course, 3 p for student.")    
        )
    )
)

(define (select-grade-helper criteria valid std-grades courses)
    (cond 
        [(null? std-grades) valid]
        (else
            (cond
            [(eq? (string-ref criteria 0) #\=)
                (if (string-suffix? criteria (last (car std-grades)))
                    (select-grade-helper criteria (append valid (list (car std-grades))) (cdr std-grades) courses)
                    (select-grade-helper criteria valid (cdr std-grades) courses)        
                )
            ]
            [(equal? (substring criteria 0 2) "<=")
                (if (string<=? (substring criteria 2 4) (last (car std-grades)))
                    (select-grade-helper criteria (append valid (list (car std-grades))) (cdr std-grades) courses)
                    (select-grade-helper criteria valid (cdr std-grades) courses)        
                )
            ]
            [(equal? (substring criteria 0 2) ">=")
                (if (string>=? (substring criteria 2 4) (last (car std-grades)))
                    (select-grade-helper criteria (append valid (list (car std-grades))) (cdr std-grades) courses)
                    (select-grade-helper criteria valid (cdr std-grades) courses)        
                )
            ]
            [(eq? (string-ref criteria 0) #\<)
                (if (string<? (substring criteria 1 3) (last (car std-grades)))
                    (select-grade-helper criteria (append valid (list (car std-grades))) (cdr std-grades) courses)
                    (select-grade-helper criteria valid (cdr std-grades) courses)        
                )
            ]
            [(eq? (string-ref criteria 0) #\>)
                (if (string>? (substring criteria 1 3) (last (car std-grades)))
                    (select-grade-helper criteria (append valid (list (car std-grades))) (cdr std-grades) courses)
                    (select-grade-helper criteria valid (cdr std-grades) courses)        
                )
            ]
            )
        )
    )
)

(define (select-std-crs input value)
  (filter (lambda (input) (member value input))
          input))


(define (select-one criteria result)
    (cond 
        [(null? criteria) result]
        (else 
            (if (null? (select-grade-helper (car criteria) '() students-grades course-list))
                (select-one (cdr criteria) result)
                (select-one (cdr criteria)  (append result (select-grade-helper (car criteria) '() students-grades course-list)))
            )
        )
    )
)

(define (search list value)
    (cond 
        [(member value list) #t]
        (else
            #f
        )
    )
)

(define (select-multiple criterias result std-grades courses) 
( cond 
    [(null? std-grades) (display result)]
    (else
        (cond
        [(and (search (car std-grades) (car (cdr criterias))) (eq? (string-ref (car criterias) 0) #\=))    
            (select-multiple criterias (append result (car std-grades)) (cdr std-grades) courses)]
        [(and (member (car (cdr criterias)) (car std-grades)) (equal? (substring (car criterias) 0 2) "<="))
            (select-multiple criterias (append result (car std-grades)) (cdr std-grades) courses)]
        [(and (member (car (cdr criterias)) (car std-grades)) (equal? (substring (car criterias) 0 2) ">="))
            (select-multiple criterias (append result (car std-grades)) (cdr std-grades) courses)]
        [(and (member (car (cdr criterias)) (car std-grades)) (eq? (string-ref (car criterias) 0) #\<))
            (select-multiple criterias (append result (car std-grades)) (cdr std-grades) courses)]
        [(and (member (car (cdr criterias)) (car std-grades)) (eq? (string-ref (car criterias) 0) #\>))
            (select-multiple criterias (append result (car std-grades)) (cdr std-grades) courses)]
        )
    )
)
)

(define (select input)
    (cond 
        [(= 1 (length input))
                (cond [(member (substring (car input) 0 1) '("<" ">" "=" "<=" ">=")) (select-one input '())]
                    (else (select-std-crs students-grades (car input)))
                )
        ]
        (else 
            (select-multiple input '() students-grades course-list)
        )
    )
)

(define (query type input)
    (cond
        [(eq? insert type) (insert input)]
        [(eq? select type) (select input)]    
    )
)

;To insert a student or course general query structure is like below.
;(query insert '("melis" "CENG463" "AA"))
;(query insert '("CENG112" "Introduction to Programming"))

;To operate on all students on all courses with a given grade criteria
;(query select '("=AA"))
;(query select '("<AA"))
;(query select '(">BB"))
;(query select '("<=AA"))
;(query select '(">=BB"))

;To query for a specific student below queries should be used.
;(query select '("CENG212"))
;(query select '("ahmet"))

;(query select '("=AA" "CENG212"))
