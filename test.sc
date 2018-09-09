
((:= (cons (lambda (x (lambda (y
        (lambda (i (if ((= i 0) (x y))))))))))) 

((:= (car (lambda (z (z 0)))))
((:= (cdr (lambda (z (z 1)))))
((:= (true (' true)))
((:= (false (' false)))
  
((:= (fac (lambda (self
                (lambda (n
                    (if ((= n 0) (1
                        (* n ((self self) (- n 1))))))))))))


; ((fac fac) 5)

((:= (zero (lambda (f 
                (lambda (x x))))))
((:= (+1 (lambda n
           (lambda (f
             (lambda (x (f ((n f) x)))))))))

 ;(((+1 (+1 zero)) (lambda (x (+ x x)))) 4)

 ((:= (; lambda (msg lambda (e
                        (if (true (e 0)))))))


((:= (rat (lambda (numer (lambda (denom
                ((cons numer) denom)))))))
((:= (numer (lambda (z (car z)))))
((:= (denom (lambda (z (cdr z)))))

 ((; (' this is just a test)) 
     (numer ((rat 2) 5)))


))))))))))))










