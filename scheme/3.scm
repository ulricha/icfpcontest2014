((lambda (z)
  (let ((id (lambda (x) (+ x z))))
    ((lambda (y) (+ x (id 3))) z))) 3)
