(cond-expand
  (guile)
  (gambit
   (define (sort lst proc) (list-sort proc lst))))

(define (show-triangles tris)
  (display "{")
  (for-each (lambda (tri)
              (display "{{")
              (for-each (lambda (index)
                          (display index)
                          (display ","))
                        tri)
              (display "}},"))
            tris)
  (display "},")
  (newline))

(define (bits k n)
  (if (= k 0)
      '()
      (if (= (remainder n 2) 1)
          (cons (- k 1) (bits (- k 1) (quotient n 2)))
          (bits (- k 1) (quotient n 2)))))

(define (bit->integer b)
  (vector-ref #(128 64 32 16 8 4 2 1) b))

(define adjacent-vertices
  #((1 2 4) (0 3 5) (0 3 6) (1 2 7) (0 5 6) (1 4 7) (2 4 7) (3 5 6)))

(define adjacent-edges
  #((0 4 8) (0 9 5) (1 10 4) (1 5 11) (2 8 6) (2 7 9) (3 6 10) (3 11 7)))

(define quads
  #((4 8 9 5) (4 5 11 10) (6 7 9 8) (6 10 11 7) (0 1 10 8) (0 9 11 1)
    (2 8 10 3) (2 3 11 9) (0 4 6 2) (0 2 7 5) (1 3 6 4) (1 5 7 3)))

(define hexas
  #((1 10 6 2 9 5) (1 4 8 2 7 11) (0 5 11 3 6 8) (0 9 7 3 10 4)
    (0 4 10 3 7 9) (0 8 6 3 11 5) (1 11 7 2 8 4) (1 5 9 2 6 10)))

(define (vertices->edge a b)
  (let ((i (intersect (vector-ref adjacent-edges a)
                      (vector-ref adjacent-edges b))))
    (if (null? i)
        #f
        (car i))))

(define (remove x lst)
  (if (null? lst)
      '()
      (if (eq? (car lst) x)
          (remove x (cdr lst))
          (cons (car lst)
                (remove x (cdr lst))))))

(define (intersect a b)
  (if (null? a)
      '()
      (if (memq (car a) b)
          (cons (car a)
                (intersect (cdr a) b))
          (intersect (cdr a) b))))

(define (unique lst)
  (let ((lst (sort lst <)))
    (let loop ((lst lst))
      (cond ((null? lst) '())
            ((null? (cdr lst)) lst)
            ((eq? (car lst) (cadr lst)) (loop (cdr lst)))
            (else (cons (car lst) (loop (cdr lst))))))))

(define (singleton lst)
  (let ((check
         (lambda (x)
           (let ((adj (vector-ref adjacent-vertices x)))
             (let loop ((lst lst))
               (cond ((null? lst) x)
                     ((eq? (car lst) x) (loop (cdr lst)))
                     ((memq (car lst) adj) #f)
                     (else (loop (cdr lst)))))))))
    (let loop ((lst lst))
      (if (null? lst)
          #f
          (or (check (car lst))
              (loop (cdr lst)))))))

(define (star lst)
  (let ((check
         (lambda (x)
           (let ((adj (vector-ref adjacent-vertices x)))
             (let loop ((lst lst))
               (cond ((null? lst) x)
                     ((eq? (car lst) x) (loop (cdr lst)))
                     ((memq (car lst) adj) (loop (cdr lst)))
                     (else #f)))))))
    (let loop ((lst lst))
      (if (null? lst)
          #f
          (or (check (car lst))
              (loop (cdr lst)))))))

(define (adjacent2 lst)
  (let ((adj (map (lambda (x)
                    (vector-ref adjacent-vertices x))
                  lst)))
    (append (intersect (list-ref adj 0) (list-ref adj 1))
            (intersect (list-ref adj 1) (list-ref adj 2))
            (intersect (list-ref adj 2) (list-ref adj 0)))))

(define (same-orientation base e1 e2)
  (let* ((q (vector-ref quads base))
         (index (lambda (e)
                  (do ((i 0 (+ i 1)))
                      ((= (list-ref q i) e) i))))
         (i1 (index e1))
         (i2 (index e2)))
    (= (modulo (+ i1 1) 4) i2)))

(define (gen-quad lst)
  (list (list (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
        (list (list-ref lst 2) (list-ref lst 3) (list-ref lst 0))))

(define (gen-penta lst)
  (list (list (list-ref lst 0) (list-ref lst 1) (list-ref lst 4))
        (list (list-ref lst 4) (list-ref lst 1) (list-ref lst 3))
        (list (list-ref lst 3) (list-ref lst 1) (list-ref lst 2))))

(define (gen-hexa lst)
  (list (list (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
        (list (list-ref lst 2) (list-ref lst 3) (list-ref lst 4))
        (list (list-ref lst 4) (list-ref lst 5) (list-ref lst 0))
        (list (list-ref lst 0) (list-ref lst 2) (list-ref lst 4))))

(define (trace lst)
  (let* ((a (do ((xs lst (cdr xs)))
                ((= (length (intersect (vector-ref adjacent-vertices (car xs))
                                       lst))
                    1)
                 (car xs))))
         (b (car (intersect (vector-ref adjacent-vertices a) lst)))
         (c (car (remove a (intersect (vector-ref adjacent-vertices b) lst))))
         (c1 (car (remove b (intersect (vector-ref adjacent-vertices c) lst))))
         (d (car (remove b (adjacent2 (list a b c)))))
         (a1 (- 7 c))
         (b1 (- 7 d))
         (d1 (- 7 b))
         (edges (list (vertices->edge d c)
                      (vertices->edge d1 c1)
                      (vertices->edge b1 c1)
                      (vertices->edge b b1)
                      (vertices->edge a a1)
                      (vertices->edge a d))))
    (if (same-orientation (vertices->edge c c1) (car edges) (cadr edges))
        (gen-hexa edges)
        (gen-hexa (reverse edges)))))

(define (gen-triangles n)
  (let* ((verts (bits 8 n))
         (edges (map (lambda (x)
                       (vector-ref adjacent-edges x))
                     verts))
         (num-edges (length (unique (apply append edges))))
         (single (singleton verts)))
    (if single
        (cons (vector-ref adjacent-edges single) ; cases 1, 3, 4, 6, 7, 12, 13
              (gen-triangles (- n (bit->integer single))))
        (case (length verts)
          ((0) '())                     ; case 0
          ((2)                          ; case 2
           (gen-quad (vector-ref quads (apply vertices->edge verts))))
          ((3)                          ; case 5
           (let* ((b (car (intersect (adjacent2 verts) verts)))
                  (ac (remove b verts))
                  (d (car (remove b (adjacent2 verts))))
                  (b1 (- 7 d))
                  (ac1 (remove b (vector-ref adjacent-vertices b1)))
                  (a (car ac))
                  (c (cadr ac))
                  (a1 (if (memq a (vector-ref adjacent-vertices (car ac1))) (car ac1) (cadr ac1)))
                  (c1 (if (eq? a1 (car ac1)) (cadr ac1) (car ac1)))
                  (edges (list (vertices->edge a a1)
                               (vertices->edge b b1)
                               (vertices->edge c c1)
                               (vertices->edge c d)
                               (vertices->edge d a))))
             (if (same-orientation (vertices->edge a b) (car edges) (cadr edges))
                 (gen-penta edges)
                 (gen-penta (reverse edges)))))
          ((4)
           (case num-edges
             ((8)                       ; case 8
              (cond ((and (member 0 verts) (member 3 verts)) (gen-quad '(8 9 11 10)))
                    ((and (member 0 verts) (member 5 verts)) (gen-quad '(4 6 7 5)))
                    ((and (member 0 verts) (member 6 verts)) (gen-quad '(0 1 3 2)))
                    (else (map reverse (gen-triangles (- 255 n))))))
             ((9)                       ; (cases 9 & 11 & 14)
              (let ((s (star verts)))
                (if s
                    (gen-hexa (vector-ref hexas s)) ; case 9
                    (trace verts))))                ; cases 11 & 14
             ((10)                                  ; case 10
              (let* ((a (car verts))
                     (b (let loop ((lst (cdr verts)))
                          (let ((adj (vector-ref adjacent-vertices (car lst))))
                            (if (memq a adj)
                                (car lst)
                                (loop (cdr lst))))))
                     (id1 (+ (bit->integer a) (bit->integer b)))
                     (id2 (- n id1)))
                (append (gen-triangles id1) (gen-triangles id2))))))
          (else (map reverse (gen-triangles (- 255 n))))))))

(define (gen-all)
  (display "// Generated by gentable.scm")
  (newline)
  (do ((i 0 (+ i 1)))
      ((= i 256))
    (show-triangles (gen-triangles i))))

(gen-all)
