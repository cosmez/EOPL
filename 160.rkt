#lang racket
(define sine (compose sin degrees->radians))
(define cosine (compose cos degrees->radians))
(define tangent (compose tan degrees->radians))
(define asine (compose radians->degrees asin))
(define acosine (compose radians->degrees acos))
(define atangent (compose radians->degrees atan))