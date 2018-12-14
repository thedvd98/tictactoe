(use matchable doodle nrepl)

;; logican tictactoe
;; 0 vuoto
(define *campo*
  (vector
	(vector #\0 #\0 #\0)
	(vector #\0 #\0 #\0)
	(vector #\0 #\0 #\0)))

(define *turno* #\X)
(define *num_mosse* 0)
;; definizioni per indicare come ha vinto il giocatore
(define *riga* 0)
(define *colonna* 1)
(define *diag-lr* 2)
(define *diag-rl* 3)

(define (inc-num-mosse)
  (add1 *num_mosse*))

(define (restart)
  (set! *campo*
	(vector
	  (vector #\0 #\0 #\0)
	  (vector #\0 #\0 #\0)
	  (vector #\0 #\0 #\0)))
  (set! *turno* #\X)
  (set! *num_mosse* 0)
  (clear-screen)
  (draw-grid))

(define (cambia-turno)
  (if (char=? *turno* #\X)
	(define *turno* #\O)
	(define *turno* #\X)))
(define (getsectorstate col line)
  (vector-ref (vector-ref *campo* line) col))
(define (setsectorstate col line state)
  (vector-set! (vector-ref *campo* line) col state))

(define (caniputinhere? col line)
  (print (getsectorstate col line))
  (cond
	((char=? (getsectorstate col line) #\0) #t)
	(else #f)))
;; verifica vincitore
(define (check-row riga)
  (define (check-row-ric col riga)
	(if (= col 0)
	  (getsectorstate 0 riga)
	  (if (char=? (check-row-ric (- col 1) riga) (getsectorstate col riga))
		(getsectorstate col riga)
		#\0)))
  (check-row-ric 2 riga))

(define (check-rows)
  (define (check-rows-ric n)
	(define ch (check-row n))
	(if (or
		  (= n 0)
		  (not (char=? ch #\0)))
	  (list ch n)
	  (check-rows-ric (- n 1))))
  (check-rows-ric 2))

(define (check-col col)
  (define (check-col-ric col riga)
	(if (= riga 0)
	  (getsectorstate col 0)
	  (if (char=?
			(check-col-ric col (- riga 1)) (getsectorstate col riga))
		(getsectorstate col riga)
		#\0)))
  (check-col-ric col 2))

(define (check-cols)
  (define (check-cols-ric n)
	(define ch (check-col n))
	(if (or
		  (= n 0)
		  (not (char=? ch #\0)))
	  (list ch n)
	  (check-cols-ric (- n 1))))
  (check-cols-ric 2))

(define (check-diag-left-right)
  (define (check-diag-lr-ric i j)
	(if (= i 0)
	  (getsectorstate 0 0)
	  (if (char=? 
			(check-diag-lr-ric (- i 1) (- j 1)) (getsectorstate i j))
		(getsectorstate i j)
		#\0)))
  (check-diag-lr-ric 2 2))

(define (check-diag-right-left)
  (define (check-diag-rl-ric i j)
	(if (= i 0)
	  (getsectorstate 0 2)
	  (if (char=? 
			(check-diag-rl-ric (- i 1) (+ j 1)) (getsectorstate i j))
		(getsectorstate i j)
		#\0)))
  (check-diag-rl-ric 2 0))

;; chi e come ha vinto
(define (who-how-win?)
  (define rig (check-rows))
  (define col (check-cols))
  (define leftright (check-diag-left-right))
  (define rightleft (check-diag-right-left))
  (cond
	((not (char=? (car rig) #\0)) (list rig *riga*))
	((not (char=? (car col) #\0)) (list col *colonna*))
	((not (char=? leftright #\0)) (list (list leftright 0) *diag-lr*))
	((not (char=? rightleft #\0)) (list (list leftright 0) *diag-rl*))
	(else '())))

;; drawing things
(define *paint* #f)

;; colori
(define red '(1 0 0 1))

(define black '(0 0 0 1))

;; colori oggetti
(define linesep-color red)
(define linex-color black)

;; geometria misure varie
(define winwidth 600)
(define winheight 400)

;; sector é una delle 9 parti in cui é diviso il campo
(define sector-length ;; lunghezza in orizzontale di un quadrato
  (/ winwidth 3))
(define sector-height;; lunghezza in verticaledi un quadrato
  (/ winheight 3))

;; ritorna il numero della colonna in cui é la x (0 1 2)
(define (colnum x)
  (cond
	((<= x sector-length) 0)
	((<= x (* 2 sector-length)) 1)
	((>= x (* 2 sector-length)) 2)
	(else '())
	)
  )
;; ritorna numero riga
(define (linenum y)
  (cond
	((<= y sector-height) 0)
	((<= y (* 2 sector-height)) 1)
	((>= y (* 2 sector-height)) 2)
	(else '()))
  )

(define (draw-grid)
  (line-width 5)
  ;; orizzontale
  (draw-line 0 (/ doodle-height 3)	;; x1 y1
			 doodle-width (/ doodle-height 3) ;; x2 y2
			 color: linesep-color)
  (draw-line 0 (* (/ doodle-height 3) 2)
			 doodle-width (* (/ doodle-height 3) 2)
			 color: linesep-color)
  ;; verticale
  (draw-line (/ doodle-width 3) 0
			 (/ doodle-width 3) doodle-height
			 color: linesep-color)
  (draw-line (* (/ doodle-width 3) 2) 0
			 (* (/ doodle-width 3) 2) doodle-height
			 color: linesep-color)
  )

;; disegna una X nella casella cliccata
(define (draw-x x y)
  (define col (colnum x))
  (define line (linenum y))
  ;; linea obliqua da sinistra a destra
  (define xtopleft
	(* sector-length col))
  (define ytopleft 
	(* sector-height line))
  (define xdownright
	(* sector-length (+ col 1)))
  (define ydownright 
	(* sector-height (+ line 1)))

  ;; linea obliqua da destra a sinistra 
  (define xtopright xdownright)
  (define ytopright ytopleft)
  (define xdownleft xtopleft)
  (define ydownleft ydownright)

  ;; linea obliqua da sinistra a destraA
  (define (draw)
	(draw-line xtopleft ytopleft
			   xdownright ydownright
			   color: linex-color)

	;; linea obliqua da destra a sinistra 
	(draw-line xtopright ytopright
			   xdownleft ydownleft
			   color: linex-color))
  (if (caniputinhere? col line)
	(begin
	  (setsectorstate col line #\X)
	  (draw))
	'()))

(define (draw-o x y)
  (define col (colnum x))
  (define line (linenum y))

  (define centerx (/ sector-length 2))
  (define centery (/ sector-height 2))

  (define (draw)
	(circle
	  (+ centerx (* sector-length col))
	  (+ centery (* sector-height line))
	  sector-height
	  black))
  (if (caniputinhere? col line)
	(begin
	  (setsectorstate col line #\O)
	  (draw))
	'()))

;;;;;;;;;;
;; MAIN ;;
;;;;;;;;;;
(thread-start! (lambda () (nrepl 1234)))

(world-inits
  (lambda ()
	(world-update-delay 1000000000)
	(clear-screen)
	(set-font! "Vollkorn" 18 red)
	(draw-grid)
	))

(world-changes
  (lambda (events dt exit)
	(for-each
	  (lambda (e)
		(match e
			   (('mouse 'pressed x y 1)
				(if (char=? *turno* #\X)
				  (draw-x x y)
				  (draw-o x y)
				  )
				(cambia-turno)
				(inc-num-mosse)
				)
			   (('mouse 'moved x y)
				(when *paint*
				  (filled-circle x y 10 red)))
			   (('key 'pressed #\esc)
				(exit #t))
			   (else (void))))
	  events)))

(new-doodle width: winwidth height: winheight title: "Doodle paint" background: solid-white)
(run-event-loop #:minimum-wait 0.1)

