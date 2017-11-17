#lang typed/racket
(require "../project2/option.rkt")
(require "../project2/loc.rkt")

;; CMSC 15100, Project 2
;; Adam Shaw, November 2016

;; version 0, 11/30/2016

;; version 1, 12/05/2016
;; - fixed type of update-castles, was (StdMove Castles -> Castles),
;;   is now (Move Castles -> Castles) (h/t Gregory Grant)
;; - fixed bug in lp in explore that allowed pieces to move
;    beyond captures (h/t Deqing Fu)

(require typed/test-engine/racket-tests)

(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")

(define-type PieceType
  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))

(define-type Player
  (U 'Black 'White))

(define-struct Piece
  ([type  : PieceType]
   [color : Player]))

(define-type Square
  (Option Piece))

(define-type Board
  (Listof (Listof Square)))

(define-struct StdMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]))

(define-struct CastleMove
  ([king-src : Loc]
   [king-dst : Loc]
   [rook-src : Loc]
   [rook-dst : Loc]
   [moved : Player]))

(define-type PromoteTo
  (U 'Queen 'Rook 'Bishop 'Knight))

(define-struct PromoMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]
   [promote-to : PromoteTo]))

(define-type Move
  (U StdMove CastleMove PromoMove))

(define-struct Castles
  ([black-toward-0-0 : Boolean]
   [black-toward-0-7 : Boolean]
   [white-toward-7-0 : Boolean]
   [white-toward-7-7 : Boolean]))

(define-struct ChessGame
  ([board : Board]
   [turn : Player]
   [history : (Listof Move)]
   [cas : Castles]))

(provide PieceType Player (struct-out Piece))
(provide Square)
(provide Board)
(provide (struct-out StdMove))
(provide (struct-out CastleMove))
(provide PromoteTo (struct-out PromoMove)) (provide Move)
(provide (struct-out ChessGame))
(provide legal-move?)
(provide new-game)
(provide board-ref)
(provide in-check?)
(provide checkmate?)
(provide stalemate?)
(provide apply-move)
(provide opponent)
(provide available-moves-piece)
(provide none?)
(provide val-of)


;; === option operations
;; NOTE it would make sense to put these in opt.rkt

(: none? (All (T) ((Option T) -> Boolean)))
;; test if an option is 'None
(define (none? opt)
  (match opt ['None #t] [_ #f]))

(: some? (All (T) ((Option T) -> Boolean)))
;; test if an option is Some
(define (some? opt)
  (not (none? opt)))

(: val-of (All (T) ((Option T) -> T)))
;; unwrap the value of a Some, raise error otherwise
(define (val-of opt)
  (match opt
    [(Some v) v]
    ['None (error "None")]))

(: opt=? (All (T) ((T T -> Boolean) (Option T) (Option T) -> Boolean)))
;; equality test, with option wrapper taken into account
(define (opt=? e? opt1 opt2)
  (match* (opt1 opt2)
    [('None 'None) #t]
    [((Some a) (Some b)) (e? a b)]
    [(_ _) #f]))
   
;; === general purpose utilities

(: list-upd (All (T) ((Listof T) Integer T -> (Listof T))))
;; functional list update
(define (list-upd xs i x)
  (if (zero? i)
      (cons x (rest xs))
      (cons (first xs) (list-upd (rest xs) (sub1 i) x))))

;; === board construction from strings

(: strings->board : (Listof String) -> Board)
;; turn a list of eight strings into a board
;; see starting-board below for an example
(define (strings->board ss)
  (local
    {(define (p c)
       (match c
         [#\- 'None]
         [#\P (Some (Piece 'Pawn 'Black))]
         [#\R (Some (Piece 'Rook 'Black))]
         [#\B (Some (Piece 'Bishop 'Black))]
         [#\N (Some (Piece 'Knight 'Black))]
         [#\K (Some (Piece 'King 'Black))]
         [#\Q (Some (Piece 'Queen 'Black))]
         [#\p (Some (Piece 'Pawn 'White))]
         [#\r (Some (Piece 'Rook 'White))]
         [#\b (Some (Piece 'Bishop 'White))]
         [#\n (Some (Piece 'Knight 'White))]
         [#\k (Some (Piece 'King 'White))]
         [#\q (Some (Piece 'Queen 'White))]))}
    (map (lambda ([s : String]) (map p (string->list s))) ss)))

;; === new chess game

(: starting-board : Board)
(define starting-board
  (strings->board (list "RNBQKBNR"
                        "PPPPPPPP"
                        "--------"
                        "--------"
                        "--------"
                        "--------"
                        "pppppppp"
                        "rnbqkbnr")))

(: new-game : ChessGame)
(define new-game
  (ChessGame starting-board 'White '() (Castles #t #t #t #t)))

;; === basic chess operations

(: board-ref : Board Loc -> Square)
;; return the (Option Piece) at the given location
(define (board-ref b loc)
  (match loc
    [(Loc rank file)
     (list-ref (list-ref b rank) file)]))

(: piece-at : Board Loc -> Piece)
;; similar to board ref, but return the piece or raise error
(define (piece-at b loc)
  (match (board-ref b loc)
    [(Some p) p]
    ['None (error "empty square")]))

(: unocc? : Board Loc -> Boolean)
;; return true if the location is unoccupied
(define (unocc? b loc)
  (none? (board-ref b loc)))

(: occ? : Board Loc -> Boolean)
;; return true if the location is occupied
(define (occ? b loc)
  (some? (board-ref b loc)))
             
(: board-update : Board Loc Square -> Board)
;; update the board location
(define (board-update b loc s)
  (match loc
    [(Loc r f)
     (list-upd b r (list-upd (list-ref b r) f s))]))

;; === general-purpose chess and chess move widgets

(: piece-type=? : PieceType PieceType -> Boolean)
(define (piece-type=? p1 p2)
  (symbol=? p1 p2))

(: player=? : Player Player -> Boolean)
(define (player=? p1 p2)
  (symbol=? p1 p2))

(: piece=? : Piece Piece -> Boolean)
(define (piece=? p1 p2)
  (and (symbol=? (Piece-type p1) (Piece-type p2))
       (symbol=? (Piece-color p1) (Piece-color p2))))

(: loc=? (Loc Loc -> Boolean))
;; equality tests for locations
(define (loc=? loc1 loc2)
  (and (= (Loc-row loc1) (Loc-row loc2))
       (= (Loc-col loc1) (Loc-col loc2))))

(: move=? (Move Move -> Boolean))
;; equality tests for moves
(define (move=? m1 m2)
  (match* (m1 m2)
    [((StdMove s1 d1 m1 c1) (StdMove s2 d2 m2 c2))
     (and (loc=? s1 s2) (loc=? d1 d2) (piece=? m1 m2) (opt=? piece=? c1 c2))]
    [((CastleMove ks1 kd1 rs1 rd1 m1) (CastleMove ks2 kd2 rs2 rd2 m2))
     (and (loc=? ks1 ks2) (loc=? kd1 kd2) (loc=? rs1 rs2) (loc=? rd1 rd2)
          (player=? m1 m2))]
    [((PromoMove s1 d1 m1 c1 pro1) (PromoMove s2 d2 m2 c2 pro2))
     (and (loc=? s1 s2) (loc=? d1 d2) (piece=? m1 m2) (opt=? piece=? c1 c2)
          (symbol=? pro1 pro2))]
    [(_ _) #f]))

(: opponent : Player -> Player)
(define (opponent p)
  (match p
    ['Black 'White]
    ['White 'Black]))

(: opponents? : Board Loc Loc -> Boolean)
;; are the pieces on each square opposing?
(define (opponents? b loc1 loc2)
  (match* ((board-ref b loc1) (board-ref b loc2))
    [((Some (Piece _  color1)) (Some (Piece _ color2)))
     (player=? color1 (opponent color2))]
    [(_ _) (error "one or both locations empty")]))

(: +loc : Loc Integer Integer -> (Option Loc))
;; compute a new location, return 'None if off the board
(define (+loc loc δ-rank δ-file)
  (match loc
    [(Loc rank file)
     (match* ((+ rank δ-rank) (+ file δ-file))
       [(r f) (if (and (<= 0 r 7) (<= 0 f 7))
                  (Some (Loc r f))
                  'None)])]))

(: move-if-unocc : Board Loc Integer Integer -> (Listof Move))
;; given a board, a location, a delta-rank and a delta-file,
;; if the new location is unoccupied, return it in a singleton list
(define (move-if-unocc b loc d-rank d-file)
  (match (+loc loc d-rank d-file)
    ['None '()]
    [(Some loc~)
     (if (unocc? b loc~)
         (list (StdMove loc loc~ (val-of (board-ref b loc)) 'None))
         '())]))

(: capture-if-occ : Board Loc Integer Integer -> (Listof Move))
;; given a board, a location, a delta-rank and a delta-file,
;; if the new location is unoccupied by an opponent's piece,
;; return it in a singleton list
(define (capture-if-occ b loc d-rank d-file)
  (match (+loc loc d-rank d-file)
    ['None '()]
    [(Some loc~)
     (if
      (and (occ? b loc~) (opponents? b loc loc~))
      (list (StdMove loc loc~ (val-of (board-ref b loc)) (board-ref b loc~)))
      '())]))

(: move-if-possible : Board Loc Integer Integer -> (Listof Move))
;; move to given other location, directly, with capture if possible
;; (this is useful for kings and knights)
;; NOTE: this returns either empty or a list of length 1
;; -- this convention is for use in various calls to append below
(define (move-if-possible b loc δ-rank δ-file)
  (local
    {(: mv : Loc -> (Listof Move))
     ;; construct a StdMove for the moving piece
     (define (mv dst)
       (list (StdMove loc dst (piece-at b loc) (board-ref b dst))))}
    (match (+loc loc δ-rank δ-file)
      ['None '()]
      [(Some loc~)
       (cond
         [(and (occ? b loc~) (opponents? b loc loc~)) (mv loc~)]
         [(unocc? b loc~) (mv loc~)]
         [else '()])])))

(: explore (Board Loc Integer Integer -> (Listof Move)))
;; explore in the given direction until the moving piece
;; either runs into a teammate, captures an opponent, or
;; walks off the board
(define (explore b loc δ-rank δ-file)
  (if (and (zero? δ-rank) (zero? δ-file))
      (error "saving you from an infinite loop")
      (local
        {(: mv : Loc -> StdMove)
         (define (mv dst)
           (StdMove loc dst (piece-at b loc) 'None))
         (: capture : Loc -> StdMove)
         (define (capture dst)
           (StdMove loc dst (piece-at b loc) (board-ref b dst)))
         (: lp : Loc -> (Listof Move))
         (define (lp curr)
           (match (+loc curr δ-rank δ-file)
             ['None '()]
             [(Some loc~)
              (cond
                [(occ? b loc~) (if (opponents? b loc loc~)
                                   (list (capture loc~))
                                   '())]
                [(unocc? b loc~) (cons (mv loc~) (lp loc~))]
                [else (error "this branch unreachable")])]))}
        (lp loc))))

(: piece-at? : Board Piece (Option Loc) -> Boolean)
;; check if there is a particular piece at the given loc
(define (piece-at? b pc opt-loc)
  (match opt-loc
    ['None #f]
    [(Some loc)
     (match (board-ref b loc)
       [(Some pc~) (and (piece=? pc pc~))]
       [_ #f])]))

(: piece-along? : Board Loc Piece Integer Integer -> Boolean)
;; move along the board until finding a piece or not
(define (piece-along? b loc pc δ-rank δ-file)
  (local
    {(: lp : Loc -> Boolean)
     (define (lp loc)
       (match (+loc loc δ-rank δ-file)
         ['None #f]
         [(Some loc~) 
          (match (board-ref b loc~)
            ['None (lp loc~)]
            [(Some pc~) (piece=? pc pc~)])]))}
    (lp loc)))
  
;; === move logic for the different pieces

(: moves-king (ChessGame Loc -> (Listof Move)))
;; return the list of moves available to a king
;; NOTE: this doesn't account for check
(define (moves-king CG L)
  (match CG
    [(ChessGame BD PY HIS (Castles upleft upright lowleft lowright))
     (match (board-ref BD L)
       [(Some (Piece (not 'King) _)) (error "not king")]
       ['None (error "empty square")]
       [_ (append (move-if-possible BD L -1  0)
                  (move-if-possible BD L -1  1)
                  (move-if-possible BD L  0  1)
                  (move-if-possible BD L  1  1)
                  (move-if-possible BD L  1  0)
                  (move-if-possible BD L  1 -1)
                  (move-if-possible BD L  0 -1)
                  (move-if-possible BD L -1 -1)
                  (if (equal? PY 'Black)
                      (cond
                        [(and upleft upright)
                         (append
                          (if (legal-castles? CG (CastleMove
                                                  (Loc 0 4) (Loc 0 1)
                                                  (Loc 0 0) (Loc 0 2) 'Black))
                              (list (CastleMove
                                                  (Loc 0 4) (Loc 0 1)
                                                  (Loc 0 0) (Loc 0 2) 'Black))
                              '())
                          (if (legal-castles? CG (CastleMove
                                                  (Loc 0 4) (Loc 0 6)
                                                  (Loc 0 7) (Loc 0 5) 'Black))
                              (list (CastleMove
                                                  (Loc 0 4) (Loc 0 6)
                                                  (Loc 0 7) (Loc 0 5) 'Black))
                              '()))]
                        [upleft
                         (if (legal-castles? CG
                                            (CastleMove
                                             (Loc 0 4) (Loc 0 1)
                                             (Loc 0 0) (Loc 0 2) 'Black))
                             (list (CastleMove
                                    (Loc 0 4) (Loc 0 1)
                                    (Loc 0 0) (Loc 0 2) 'Black))
                             '())]
                        [upright
                         (if (legal-castles? CG
                                            (CastleMove
                                             (Loc 0 4) (Loc 0 1)
                                             (Loc 0 0) (Loc 0 2) 'Black))
                             (list (CastleMove
                                    (Loc 0 4) (Loc 0 6)
                                    (Loc 0 7) (Loc 0 5) 'Black))
                             '())]
                        [else '()])
                      (cond
                        [(and lowleft lowright)
                         (append
                          (if (legal-castles? CG (CastleMove
                                                  (Loc 7 4) (Loc 7 1)
                                                  (Loc 7 0) (Loc 7 2) 'White))
                              (list (CastleMove
                                                  (Loc 7 4) (Loc 7 1)
                                                  (Loc 7 0) (Loc 7 2) 'White))
                              '())
                          (if (legal-castles? CG (CastleMove
                                                  (Loc 7 4) (Loc 7 6)
                                                  (Loc 7 7) (Loc 7 5) 'White))
                              (list (CastleMove
                                                  (Loc 7 4) (Loc 7 6)
                                                  (Loc 7 7) (Loc 7 5) 'White))
                              '()))]
                        [lowleft
                         (if (legal-castles? CG
                                            (CastleMove
                                             (Loc 7 4) (Loc 7 1)
                                             (Loc 7 0) (Loc 7 2) 'White))
                             (list (CastleMove
                                    (Loc 7 4) (Loc 7 1)
                                    (Loc 7 0) (Loc 7 2) 'Black))
                             '())]
                        [lowright
                         (if (legal-castles? CG
                                            (CastleMove
                                             (Loc 7 4) (Loc 7 1)
                                             (Loc 7 0) (Loc 7 2) 'White))
                             (list (CastleMove
                                    (Loc 7 4) (Loc 7 6)
                                    (Loc 7 7) (Loc 7 5) 'White))
                             '())]
                        [else '()])))])]))
                         
(: moves-rook (Board Loc -> (Listof Move)))
;; return the list of moves available to a rook
;; NOTE: this doesn't account for check
(define (moves-rook b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'Rook) _)) (error "not a rook")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Rook player))
     (append (explore b loc  0 -1)
             (explore b loc -1  0)
             (explore b loc  0  1)
             (explore b loc  1  0))]))

(: moves-bishop (Board Loc -> (Listof Move)))
;; return the list of moves available to a bishop
;; NOTE: this doesn't account for check
(define (moves-bishop b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'Bishop) _)) (error "not a bishop")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Bishop player))
     (append (explore b loc -1 -1)
             (explore b loc -1  1)
             (explore b loc  1 -1)
             (explore b loc  1  1))]))
                               
(: moves-queen (Board Loc -> (Listof Move)))
;; return the list of moves available to a queen
;; NOTE: this doesn't account for check
(define (moves-queen b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'Queen) _)) (error "not a queen")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Queen player))
     (append (explore b loc -1 -1)
             (explore b loc -1  0)
             (explore b loc -1  1)
             (explore b loc  0  1)
             (explore b loc  1  1)
             (explore b loc  1  0)
             (explore b loc  1 -1)
             (explore b loc  0 -1))]))

(: moves-knight (Board Loc -> (Listof Move)))
;; return the list of moves available to a knight
;; NOTE: this doesn't account for check
(define (moves-knight b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'Knight) _)) (error "not a queen")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Knight player))
     (append (move-if-possible b loc -2 -1)
             (move-if-possible b loc -2  1)
             (move-if-possible b loc -1  2)
             (move-if-possible b loc  1  2)
             (move-if-possible b loc  2  1)
             (move-if-possible b loc  2  -1)
             (move-if-possible b loc  1  -2)
             (move-if-possible b loc -1  -2))]))
            
(: moves-pawn (ChessGame Loc -> (Listof Move)))
;; return the list of moves available to a pawn
;; NOTE: this doesn't account for whether this exposes check
;; add en passant into previous moves-pawn
(define (moves-pawn g loc)
  (match g
    [(ChessGame b p _ _)
     (match (board-ref b loc)
       [(Some (Piece (not 'Pawn) _)) (error "not a pawn")]
       ['None (error "location unoccupied")]
       [(Some (Piece 'Pawn player))
        (local
          {(define δ-rank
             (match player ['Black 1] ['White -1]))}
          (match loc
            [(Loc 0 _) (error "wrong location")]
            [(Loc 7 _) (error "wrong location")]
            [(Loc rank col)
             (match
                 (append (capture-if-occ b loc δ-rank -1)
                         (move-if-unocc b loc δ-rank 0)
                         (if (and (= rank (match player ['Black 1] ['White 6]))
                                  (unocc? b (val-of (+loc loc δ-rank 0))))
                             (move-if-unocc b loc (* 2 δ-rank) 0)
                             '())
                         (capture-if-occ b loc δ-rank 1)
                         (filter
                          (lambda ([x : StdMove])
                            (and
                             (>= (Loc-row (StdMove-dst x)) 0)
                             (>= (Loc-col (StdMove-dst x)) 0)
                             (<= (Loc-row (StdMove-dst x)) 7)
                             (<= (Loc-col (StdMove-dst x)) 7)
                             (equal? (Some (Piece 'Pawn (opponent player)))
                                     (board-ref b
                                                (Loc (- (Loc-row
                                                         (StdMove-dst x))
                                                        δ-rank)
                                                     (Loc-col
                                                      (StdMove-dst x)))))))
                          (map (lambda ([n : Integer])
                                 (StdMove loc
                                          (Loc (+ rank δ-rank) (+ col n))
                                          (Piece 'Pawn player)
                                          (Some
                                           (Piece 'Pawn (opponent player)))))
                               (list 1 -1))))
               [moves
                (if
                 (or
                  (legal-promote? g (PromoMove loc (val-of (+loc loc δ-rank 0))
                                               (Piece 'Pawn p) 'None 'Queen))
                  (and (not (none? (+loc loc δ-rank -1)))
                       (legal-promote? g
                                       (PromoMove
                                        loc
                                        (val-of (+loc loc δ-rank -1))
                                        (Piece 'Pawn p)
                                        (Some (Piece 'Bishop (opponent p)))
                                        'Queen)))
                  (and (not (none? (+loc loc δ-rank 1)))
                       (legal-promote? g
                                       (PromoMove
                                        loc
                                        (val-of (+loc loc δ-rank 1))
                                        (Piece 'Pawn p)
                                        (Some (Piece 'Bishop (opponent p)))
                                        'Queen))))
                 (append
                  (list
                   (PromoMove loc (val-of (+loc loc δ-rank 0))
                              (Piece 'Pawn p)
                              (board-ref b (val-of (+loc loc δ-rank 0)))
                              'Queen))
                  (if (none? (+loc loc δ-rank 1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank 1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank 1)))
                                  'Queen)))
                  (if (none? (+loc loc δ-rank -1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank -1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank -1)))
                                  'Queen)))
                  (list
                   (PromoMove loc (val-of (+loc loc δ-rank 0))
                              (Piece 'Pawn p)
                              (board-ref b (val-of (+loc loc δ-rank 0)))
                              'Knight))
                  (if (none? (+loc loc δ-rank 1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank 1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank 1)))
                                  'Knight)))
                  (if (none? (+loc loc δ-rank -1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank -1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank -1)))
                                  'Knight)))
                  (list
                   (PromoMove loc (val-of (+loc loc δ-rank 0))
                              (Piece 'Pawn p)
                              (board-ref b (val-of (+loc loc δ-rank 0)))
                              'Bishop))
                  (if (none? (+loc loc δ-rank 1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank 1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank 1)))
                                  'Bishop)))
                  (if (none? (+loc loc δ-rank -1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank -1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank -1)))
                                  'Bishop)))
                  (list
                   (PromoMove loc (val-of (+loc loc δ-rank 0))
                              (Piece 'Pawn p)
                              (board-ref b (val-of (+loc loc δ-rank 0)))
                              'Rook))
                  (if (none? (+loc loc δ-rank 1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank 1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank 1)))
                                  'Rook)))
                  (if (none? (+loc loc δ-rank -1)) '()
                      (list
                       (PromoMove loc (val-of (+loc loc δ-rank -1))
                                  (Piece 'Pawn p)
                                  (board-ref b (val-of (+loc loc δ-rank -1)))
                                  'Rook)))
                  moves)
                 moves)])]))])]))


;; === checking for check
(: bishop-threatens? : Board Loc -> Boolean)
;; does an enemy bishop threaten the piece at the location?
(define (bishop-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define bish (Piece 'Bishop (opponent p)))
        (define (bad-bish? [d-row : Integer] [d-col : Integer])
          (piece-along? b loc bish d-row d-col))}
       (or (bad-bish? -1 -1) (bad-bish? -1  1)
           (bad-bish?  1  1) (bad-bish?  1 -1)))]))

(: rook-threatens? : Board Loc -> Boolean)
;; does an enemy rook threaten the piece at the location?
(define (rook-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define rook (Piece 'Rook (opponent p)))
        (define (bad-rook? [d-row : Integer] [d-col : Integer])
          (piece-along? b loc rook d-row d-col))}
       (or (bad-rook? -1 0) (bad-rook? 0  1)
           (bad-rook?  1 0) (bad-rook? 0 -1)))]))

(: queen-threatens? : Board Loc -> Boolean)
;; does an enemy queen threaten the piece at the location?
(define (queen-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define q (Piece 'Queen (opponent p)))
        (define (evil-queen? [d-row : Integer] [d-col : Integer])
          (piece-along? b loc q d-row d-col))} 
       (or (evil-queen? -1 -1) (evil-queen? -1  1)
           (evil-queen?  1 -1) (evil-queen?  1  1)
           (evil-queen? -1  0) (evil-queen?  0  1)
           (evil-queen?  1  0) (evil-queen?  0 -1)))]))

(: pawn-threatens? : Board Loc -> Boolean)
;; does an enemy pawn threaten the piece at the location?
(define (pawn-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define δ-row (match p ['Black 1] ['White -1]))
        (define opp-pawn (Piece 'Pawn (opponent p)))}
       (or (piece-at? b opp-pawn (+loc loc δ-row -1))
           (piece-at? b opp-pawn (+loc loc δ-row 1))))]))

(: knight-threatens? : Board Loc -> Boolean)
;; does an enemy knight threaten the piece at the location?
(define (knight-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define (opp-knight? [δ-row : Integer] [δ-col : Integer])
          (piece-at? b (Piece 'Knight (opponent p)) (+loc loc δ-row δ-col)))}
       (or (opp-knight? -1 -2) (opp-knight? -2 -1)
           (opp-knight? -2  1) (opp-knight? -1  2)
           (opp-knight?  1  2) (opp-knight?  2  1)
           (opp-knight?  2 -1) (opp-knight?  1 -2)))]))

(: get-king : Board Player -> Loc)
;; return the location of the given king
(define (get-king b p)
  (local
    {(: lp : Integer Integer -> Loc)
     (define (lp row col)
       (cond
         [(= row 8) (error "out of bounds")]
         [(= col 8) (lp (add1 row) 0)]
         [else (match (board-ref b (Loc row col))
                 ['None (lp row (add1 col))]
                 [(Some (Piece x p~))
                  (if (and (symbol=? x 'King) (symbol=? p p~))
                      (Loc row col)
                      (lp row (add1 col)))])]))}
    (lp 0 0)))

(: in-check? : ChessGame -> Boolean)
;; is the current player in check?
(define (in-check? g)
  (match g
    [(ChessGame b p hist cas)
     (local
       {(define king (get-king b p))}
       (or (pawn-threatens? b king)
           (bishop-threatens? b king)
           (rook-threatens? b king)
           (queen-threatens? b king)
           (knight-threatens? b king)))]))
                 
;; === available moves and legality
(: within? : Loc -> Boolean)
;;check if a location is in board
(define (within? loc)
  (match loc
    [(Loc r c)
     (and (<= r 7)
          (>= r 0)
          (<= c 7)
          (>= c 0))]))

(: king-attack? : ChessGame -> Boolean)
(define (king-attack? g)
  (match g
    [(ChessGame bd p _ _)
     (match (get-king bd p)
       [(Loc r c)
        (or
         (and (within? (Loc (add1 r) c))
              (equal? (board-ref bd (Loc (add1 r) c))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc (add1 r) (add1 c)))
              (equal? (board-ref bd (Loc (add1 r) (add1 c)))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc (add1 r) (sub1 c)))
              (equal? (board-ref bd (Loc (add1 r) (sub1 c)))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc (sub1 r) c))
              (equal? (board-ref bd (Loc (sub1 r) c))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc (sub1 r) (add1 c)))
              (equal? (board-ref bd (Loc (sub1 r) (add1 c)))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc (sub1 r) (sub1 c)))
              (equal? (board-ref bd (Loc (sub1 r) (sub1 c)))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc r (add1 c)))
              (equal? (board-ref bd (Loc r (add1 c)))
                      (Some (Piece 'King (opponent p)))))
         (and (within? (Loc r (sub1 c)))
              (equal? (board-ref bd (Loc r (sub1 c)))
                      (Some (Piece 'King (opponent p))))))])]))

(: available-moves-piece : ChessGame Loc -> (Listof Move))
;; add castling, en passant, and promotion to previous function
(define (available-moves-piece g loc)
  (match g
    [(ChessGame b p hist cas)
     (match (board-ref b loc)
       ['None '()]
       [(Some (Piece type py))
        (if (not (player=? p py))
            '()
            (local
              {(: legal? (Move -> Boolean))
               (define (legal? m)
                 (match m
                   [(StdMove src dst _ _)
                    (or (enpassant? g m)
                        (and (not (in-check? (relocate-piece g src dst)))
                             (not (king-attack? (relocate-piece g src dst)))))]
                   [(CastleMove king-src king-dst rook-src rook-dst moved)
                    (not (in-check? (relocate-piece
                                     (relocate-piece g king-src king-dst)
                                     rook-src rook-dst)))]
                   [(PromoMove src dst moved cap promoto)
                    (and
                     (match cap
                       [(Some (Piece 'Pawn pp)) (not (equal? pp p))]
                       [_ #t])
                    (not (in-check?
                          (ChessGame
                           (board-update
                            (board-update
                             (ChessGame-board g)
                             src 'None)
                            dst (Some (Piece promoto (Piece-color moved))))
                           (ChessGame-turn g)
                           (ChessGame-history g)
                           (ChessGame-cas g)))))]))}
              (filter legal?
                      (match type
                        ['Pawn   (moves-pawn g loc)]
                        ['Knight (moves-knight b loc)]
                        ['Bishop (moves-bishop b loc)]
                        ['Rook   (moves-rook b loc)]
                        ['Queen  (moves-queen b loc)]
                        ['King   (moves-king g loc)]))))])]))

(: legal-move? : ChessGame Move -> Boolean)
;; a StdMove is legal if
;; - the named piece is actually at the source location,
;; - the named piece is capable of moving to the dest location per chess rules,
;; - the captured piece is actually at the dest location, and
;; - the move does not expose the player to check
(define (legal-move? g m)
  (match g
    [(ChessGame b pl hist cas)
     (match m
       [(StdMove src dst piece capt)
        (and (piece-at? b piece (Some src))
             (ormap (λ ([m~ : Move]) (move=? m m~))
                    (available-moves-piece g src))
             (match capt
               ['None (unocc? b dst)]
               [(Some c) (or (enpassant? g m)(piece-at? b c (Some dst)))])
             (not (in-check? (relocate-piece g src dst))))]
       [(CastleMove _ _ _ _ _) (legal-castles? g m)]
       [(PromoMove _ _ _ _ _) (legal-promote? g m)])]))
(check-expect
(legal-move? 
  (ChessGame
   (strings->board
    (list
     "R-BQK--R" "-p------" "--------" "--------"
     "--------" "--------" "--------" "-----k--"))
   'Black
   '()
   (Castles #t #t #f #f))
  (PromoMove (Loc 1 1) (Loc 0 0) (Piece 'Pawn 'White)
             (Some (Piece 'Rook 'Black)) 'Queen))
 #t)



(: relocate-piece : ChessGame Loc Loc -> ChessGame)
;; uncritically relocate piece, don't update castling or history
;; and don't toggle player
(define (relocate-piece g src dst)
  (match g
    [(ChessGame b pl hist cas)
     (match (board-ref b src)
       ['None (error "no piece at source location")]
       [(Some pc)
        (local
          {(define b~ (board-update (board-update b src 'None) dst (Some pc)))}
        (ChessGame b~ pl hist cas))])]))

(: update-castles (Move Castles -> Castles))
;; flip the castles booleans as needed
(define (update-castles m cas)
  (match cas
    [(Castles b00 b07 w70 w77)
     (match m
       [(StdMove (Loc 0 0) _ (Piece 'Rook 'Black) _) (Castles #f b07 w70 w77)]
       [(StdMove (Loc 0 4) _ (Piece 'King 'Black) _) (Castles #f #f w70 w77)]
       [(StdMove (Loc 0 7) _ (Piece 'Rook 'Black) _) (Castles b00 #f w70 w77)]
       [(StdMove (Loc 7 0) _ (Piece 'Rook 'White) _) (Castles b00 b07 #f w77)]
       [(StdMove (Loc 7 4) _ (Piece 'King 'White) _) (Castles b00 b07 #f #f)]
       [(StdMove (Loc 7 7) _ (Piece 'Rook 'White) _) (Castles b00 b07 w70 #f)]
       [(CastleMove _ _ _ _ 'White) (Castles b00 b07 #f #f)]
       [(CastleMove _ _ _ _ 'Black) (Castles #f #f w70 w77)]
       [_ cas])]))

(: apply-move : ChessGame Move -> ChessGame)
;; apply move if legal
(define (apply-move g m)
  (if (not (legal-move? g m))
      (error "illegal move")
      (match g
        [(ChessGame BD PY HIS CAS)
         (match m
           [(StdMove src dst pc _)
            (if (enpassant? g m)
                (ChessGame
                 (board-update
                  (board-update
                   (board-update BD src 'None)
                   dst (Some pc))
                  (match (first HIS)
                    [(StdMove _ op-dst _ _)
                     op-dst]) 'None)
                 (opponent PY)
                 (cons m HIS)
                 CAS)
                (ChessGame (board-update (board-update BD src 'None) dst
                                         (Some pc))
                           (opponent PY)
                           (cons m HIS)
                           (update-castles m CAS)))]
           [(CastleMove king-src king-dst rook-src rook-dst moved)
            (ChessGame
             (board-update
              (board-update
               (board-update
                (board-update BD rook-dst (Some (Piece 'Rook moved)))
                rook-src 'None)
               king-dst (Some (Piece 'King moved)))
              king-src 'None)
             (opponent PY)
             (cons m HIS)
             (match CAS
               [(Castles upleft upright lowleft lowright)
                (match king-src
                  [(Loc 0 4) (Castles #f #f lowleft lowright)]
                  [(Loc 7 4) (Castles upleft upright #f #f)])]))]
           [(PromoMove src dst moved cap promo)
            (ChessGame
             (board-update
              (board-update BD dst (Some (Piece promo PY)))
              src 'None)
             (opponent PY)
             (cons m HIS)
             (match CAS
               [(Castles upleft upright lowleft lowright)
                (cond
                  [(equal? cap (Some (Piece 'Rook (opponent PY))))
                   (match dst
                     [(Loc 0 0) (Castles #f upright lowleft lowright)]
                     [(Loc 0 7) (Castles upleft #f lowleft lowright)]
                     [(Loc 7 0) (Castles upleft upright #f lowright)]
                     [(Loc 7 7) (Castles upleft upright lowleft #f)]
                     [_ (Castles upleft upright lowleft lowright)])]
                  [else (Castles upleft upright lowleft lowright)])]))])])))

(check-expect
 (apply-move
  (ChessGame
   (strings->board
    (list "R---K--R" "-p---P--" "--------" "--------"
          "--------" "--------" "--------" "-----k--"))
   'Black
   '()
   (Castles #t #t #f #f))
  (CastleMove (Loc 0 4) (Loc 0 1) (Loc 0 0) (Loc 0 2) 'Black))
 (ChessGame
  (strings->board
   (list "-KR----R" "-p---P--" "--------" "--------"
         "--------" "--------" "--------" "-----k--"))
  'White
  (list (CastleMove (Loc 0 4) (Loc 0 1) (Loc 0 0) (Loc 0 2) 'Black))
  (Castles #f #f #f #f)))

(check-expect
 (apply-move
  (ChessGame
   (strings->board
    (list "-KR----R" "-p---P--" "--------" "--------"
          "--------" "--------" "--------" "-----k--"))
   'White
   '()
   (Castles #f #f #f #f))
  (PromoMove (Loc 1 1) (Loc 0 2)
             (Piece 'Pawn 'White) (Some (Piece 'Rook 'Black))
             'Queen))
 (ChessGame
  (strings->board
   (list "-Kq----R" "-----P--" "--------" "--------"
         "--------" "--------" "--------" "-----k--"))
  'Black
  (list
   (PromoMove (Loc 1 1) (Loc 0 2)
              (Piece 'Pawn 'White) (Some (Piece 'Rook 'Black))
              'Queen))
  (Castles #f #f #f #f)))


(: available-moves-player : ChessGame -> (Listof Move))
;; collect all the available moves for the current player
(define (available-moves-player g)
  (match g
    [(ChessGame b pl hist cas)
     (local
       {(: lp (Integer Integer -> (Listof Move)))
        (define (lp row col)
          (cond
            [(> row 7) '()]
            [(> col 7) (lp (add1 row) 0)]
            [else
             (match (board-ref b (Loc row col))
               ['None (lp row (add1 col))]
               [(Some pc)
                (append (if (same-color? pc pl)
                            (available-moves-piece g (Loc row col))
                            '())
                        (lp row (add1 col)))])]))}
       (lp 0 0))]))

(: same-color? (Piece Player -> Boolean))
(define (same-color? pc pl)
  (player=? (Piece-color pc) pl))

;; === end game conditions

(: checkmate? : ChessGame -> Boolean)
(define (checkmate? g)
  (and (in-check? g)
       (empty? (available-moves-player g))))

(: stalemate? (ChessGame -> Boolean))
(define (stalemate? g)
  (and (not (in-check? g))
       (empty? (available-moves-player g))))

;; === visualization

(: square->char (Square -> Char))
;; return the matching unicode chess character, or - for empty
(define (square->char s)
  (match s
    ['None #\-]
    [(Some (Piece t c))
     (match t
       ['King   (match c ['White #\♔] [_ #\♚])]
       ['Queen  (match c ['White #\♕] [_ #\♛])]
       ['Rook   (match c ['White #\♖] [_ #\♜])]
       ['Bishop (match c ['White #\♗] [_ #\♝])]
       ['Knight (match c ['White #\♘] [_ #\♞])]
       ['Pawn   (match c ['White #\♙] [_ #\♟])])]))

(: draw-sq : Integer Image-Color -> (Char -> Image))
;; draw chess square
(define ((draw-sq size bg) piece)
  (local
    {(define s (square size "solid" bg))}
    (if (char=? #\- piece)
        s
        (overlay (text (string piece) 36 "black") s))))

(: draw-row : String Integer Image-Color Image-Color -> Image)
;; draw row of squares
(define (draw-row str row color1 color2)
  (local
    {(: lp ((Listof Char) Integer Image-Color Image-Color -> Image))
     (define (lp cs col c1 c2)
       (if (empty? cs)
           empty-image
           (beside ((draw-sq 50 c1) (first cs))
                   (lp (rest cs) (add1 col) c2 c1))))}
    (lp (string->list str) 0 color1 color2)))

(: draw-board : (Listof String) Image-Color Image-Color -> Image)
;; draw whole board
(define (draw-board rows light dark)
  (local
    {(: lp ((Listof String) Integer Image-Color Image-Color -> Image))
     (define (lp rows r c1 c2)
       (match rows
         ['() empty-image]
         [(cons first rest)
          (above (draw-row first r c1 c2)
                 (lp rest (add1 r) c2 c1))]))}
    (lp rows 0 light dark)))

(: board->strings (Board -> (Listof String)))
;; collect unicode characters for drawing
(define (board->strings b)
  (map (λ ([row : (Listof Square)]) (list->string (map square->char row))) b))

(: board->image (Board -> Image))
;; draw board with beige light and brown dark
(define (board->image b)
  (draw-board (board->strings b) 'beige 'brown))

(: enpassant? : ChessGame Move -> Boolean)
;; check whether a move is en passant
(define (enpassant? CG M)
  (match* (CG M)
    [((ChessGame BD P HIS _) (StdMove src dst moved captured))
     (match* (HIS src dst)
       [((cons pre-move _) (Loc r1 c1) (Loc r2 c2))
        (match pre-move
          [(StdMove cpdl1 cpdl2 mvd _)
           (match* (cpdl1 cpdl2)
             [((Loc cdr1 cdc1) (Loc cdr2 cdc2))
              (and (= cdc1 cdc2)
                   (= (abs (- cdr1 cdr2)) 2)
                   (= r2 (quotient (+ cdr1 cdr2) 2))
                   (= c2 cdc1)
                   (= r1 cdr2)
                   (= (abs (- cdc1 c1)) 1)
                   (or (equal? moved (Piece 'Pawn 'White))
                       (equal? moved (Piece 'Pawn 'Black)))
                   (or (equal? captured (Some (Piece 'Pawn 'White)))
                       (equal? captured (Some (Piece 'Pawn 'Black))))
                   (or (equal? mvd (Piece 'Pawn 'White))
                       (equal? mvd (Piece 'Pawn 'Black))))])]
          [_ #f])]
       [('() _ _) #f])]
    [(_ _) #f]))
(check-expect
 (enpassant?
  (ChessGame
   (strings->board
    (list
     "R-BQK--R" "-p------" "--------" "--------"
     "--------" "--------" "--------" "-----k--"))
   'White
   '()
   (Castles #t #t #f #f))
  (PromoMove (Loc 1 1) (Loc 0 1)
             (Piece 'Pawn 'White)
             'None
             'Queen)) #f)

(: legal-castles? : ChessGame Move -> Boolean)
;; to check whether a move is castling
(define (legal-castles? CG M)
  (match* (CG M)
    [((ChessGame BD PY HIS (Castles upleft upright lowleft lowright))
      (CastleMove king-src king-dst rook-src rook-dst moved-py))
     (match* ((board-ref BD king-src)
              (board-ref BD king-dst)
              (board-ref BD rook-src)
              (board-ref BD rook-dst))
       [((Some (Piece 'King py)) 'None (Some (Piece 'Rook py)) 'None)
        (and (not (in-check? CG))
             (not (in-check?
                   (ChessGame
                    (board-update
                     (board-update
                      (board-update
                       (board-update BD king-src 'None)
                       king-dst (Some (Piece 'King py)))
                      rook-src 'None)
                     rook-dst (Some (Piece 'Rook py)))
                    py HIS (ChessGame-cas CG))))
             (match king-src
               [(Loc 0 4)
                (or (and
                     upright
                     (match* (king-dst rook-src rook-dst)
                       [((Loc 0 6) (Loc 0 7) (Loc 0 5))
                        (match* ((board-ref BD (Loc 0 5))
                                 (board-ref BD (Loc 0 6)))
                          [('None 'None) #t]
                          [(_ _) #f])]
                       [(_ _ _) #f]))
                    (and
                     upleft
                     (match* (king-dst rook-src rook-dst)
                       [((Loc 0 1) (Loc 0 0) (Loc 0 2))
                        (match* ((board-ref BD (Loc 0 1))
                                 (board-ref BD (Loc 0 2))
                                 (board-ref BD (Loc 0 3)))
                          [('None 'None 'None) #t]
                          [(_ _ _) #f])]
                       [(_ _ _) #f])))]
               [(Loc 7 4)
                (or (and
                     lowright
                     (match* (king-dst rook-src rook-dst)
                       [((Loc 7 6) (Loc 7 7) (Loc 7 5))
                        (match* ((board-ref BD (Loc 7 5))
                                 (board-ref BD (Loc 7 6)))
                          [('None 'None) #t]
                          [(_ _) #f])]
                       [(_ _ _) #f]))
                    (and
                     lowleft
                     (match* (king-dst rook-src rook-dst)
                       [((Loc 7 1) (Loc 7 0) (Loc 7 2))
                        (match* ((board-ref BD (Loc 7 1))
                                 (board-ref BD (Loc 7 2))
                                 (board-ref BD (Loc 7 3)))
                          [('None 'None 'None) #t]
                          [(_ _ _) #f])]
                       [(_ _ _) #f])))]))]
       [(_ _ _ _) #f])]
    [(_ _) #f]))

(: legal-promote? : ChessGame Move -> Boolean)
;; check whether it is legal for a pawn promotion
(define (legal-promote? CG M)
  (match* (CG M)
    [((ChessGame BD PY _ _)
      (PromoMove src dst moved captured promote-to))
     (cond
       [(equal? moved (Piece 'Pawn 'Black))
        (match* (src dst)
          [((Loc r1 c1) (Loc r2 c2))
           (and (= r1 6)
                (= r2 7)
                (if (equal? captured 'None)
                    (and (= c1 c2) (equal? (board-ref BD dst) 'None))
                    (and (= (abs (- c1 c2)) 1)
                         (not (equal? (board-ref BD dst) 'None)))))])]
       [(equal? moved (Piece 'Pawn 'White))
        (match* (src dst)
          [((Loc r1 c1) (Loc r2 c2))
           (and (= r1 1)
                (= r2 0)
                (if (equal? captured 'None)
                    (and (= c1 c2) (equal? (board-ref BD dst) 'None))
                    (and (= (abs (- c1 c2)) 1)
                         (not (equal? (board-ref BD dst) 'None)))))])]
       [else #f])]
    [(_ _) #f]))

 ;=== graders' tests

(: GRADER_strings->board : (Listof String) -> Board)
(define (GRADER_strings->board ss)
  (local
    {(define (p c)
       (match c
         [#\- 'None]
         [#\P (Some (Piece 'Pawn 'Black))]
         [#\R (Some (Piece 'Rook 'Black))]
         [#\B (Some (Piece 'Bishop 'Black))]
         [#\N (Some (Piece 'Knight 'Black))]
         [#\K (Some (Piece 'King 'Black))]
         [#\Q (Some (Piece 'Queen 'Black))]
         [#\p (Some (Piece 'Pawn 'White))]
         [#\r (Some (Piece 'Rook 'White))]
         [#\b (Some (Piece 'Bishop 'White))]
         [#\n (Some (Piece 'Knight 'White))]
         [#\k (Some (Piece 'King 'White))]
         [#\q (Some (Piece 'Queen 'White))]))
     (: r : String -> (Listof Square))
     (define (r s)
       (map p (string->list s)))}
    (map r ss)))

(: GRADER_board-ref : Board Loc -> Square)
(define (GRADER_board-ref b loc)
  (match loc
    [(Loc rank file)
     (list-ref (list-ref b rank) file)]))

(: GRADER_list-upd (All (T) ((Listof T) Integer T -> (Listof T))))
(define (GRADER_list-upd xs i x)
  (if (zero? i)
      (cons x (rest xs))
      (cons (first xs) (GRADER_list-upd (rest xs) (sub1 i) x))))

(: GRADER_board-update : Board Loc Square -> Board)
(define (GRADER_board-update b loc s)
  (match loc
    [(Loc r f)
     (GRADER_list-upd b r (GRADER_list-upd (list-ref b r) f s))]))

(: GRADER_starting-board : Board)
(define GRADER_starting-board
  (GRADER_strings->board
   (list "RNBQKBNR"
         "PPPPPPPP"
         "--------"
         "--------"
         "--------"
         "--------"
         "pppppppp"
         "rnbqkbnr")))

(: GRADER_new-game : ChessGame)
(define GRADER_new-game
  (ChessGame GRADER_starting-board 'White '() (Castles #t #t #t #t)))

(: GRADER_opt-map (All (T U) ((T -> U) (Option T) U -> U)))
;; apply f to value if there is one, otherwise return default value
;; ex: (opt-map add1 'None 0)    => 0
;; ex: (opt-map add1 (Some 4) 0) => 5
(define (GRADER_opt-map f opt def)
  (match opt
    ['None def]
    [(Some x) (f x)]))

(: GRADER_move->str (Move -> String))
;; build a string version of the move, for purposes of comparison
;; note: there is a bijection between moves and strings (and must be)
(define (GRADER_move->str m)
  (match m
    [(StdMove src dst moved captured)
     (GRADER_pipes (list "StdMove"
                  (GRADER_loc->str src)
                  (GRADER_loc->str dst)
                  (GRADER_piece->str moved)
                  (GRADER_opt-map GRADER_piece->str captured "None")))]
    [(CastleMove ks kd rs rd moved)
     (GRADER_pipes (list "CastleMove"
                  (GRADER_loc->str ks)
                  (GRADER_loc->str kd)
                  (GRADER_loc->str rs)
                  (GRADER_loc->str rd)
                  (symbol->string moved)))]
    [(PromoMove src dst moved captured pro)
     (GRADER_pipes (list "PromoMove"
                  (GRADER_loc->str src)
                  (GRADER_loc->str dst)
                  (GRADER_piece->str moved)
                  (GRADER_opt-map GRADER_piece->str captured "None")
                  (symbol->string pro)))]))

(: GRADER_loc->str (Loc -> String))
;; return string representation of location
(define (GRADER_loc->str loc)
  (match loc
    [(Loc r c)
     (string-append "Loc:" (number->string r) "," (number->string c))]))

(: GRADER_piece->str (Piece -> String))
;; return string representation of piece
(define (GRADER_piece->str p)
  (match p
    [(Piece t pl)
     (string-append "Piece:"
                    (symbol->string t)
                    ","
                    (symbol->string pl))]))

(: GRADER_pipes ((Listof String) -> String))
;; connect strings with | character in between
;; ex: (pipes (list "a" "bb" "ccc")) ==&gt; "a|bb|ccc"
(define (GRADER_pipes ss)
  (match ss
    ['() ""]
    [(list s) s]
    [(cons s r) (string-append s "|" (GRADER_pipes r))]))

(: GRADER_move<? (Move Move -> Boolean))
;; move comparison for the purposes of sorting
(define (GRADER_move<? m1 m2)
  (string<? (GRADER_move->str m1) (GRADER_move->str m2)))

(: GRADER_sort-moves : (Listof Move) -> (Listof Move))
;; sort a list of moves into a canonical order
;; allowing for comparison with check-expect
;; note: uses the built-in sort operation
(define (GRADER_sort-moves moves)
  (sort moves GRADER_move<?))

(check-expect starting-board GRADER_starting-board)
(check-expect new-game GRADER_new-game)

(check-expect (board-ref GRADER_starting-board (Loc 0 0))
              (Some (Piece 'Rook 'Black)))

(check-expect (board-ref GRADER_starting-board (Loc 0 1))
              (Some (Piece 'Knight 'Black)))

(check-expect (board-ref GRADER_starting-board (Loc 0 7))
              (Some (Piece 'Rook 'Black)))

(check-expect (board-ref GRADER_starting-board (Loc 1 0))
              (Some (Piece 'Pawn 'Black)))

(check-expect (board-ref GRADER_starting-board (Loc 2 2))
              'None)

(check-expect (board-ref GRADER_starting-board (Loc 6 7))
              (Some (Piece 'Pawn 'White)))

(check-expect (board-ref GRADER_starting-board (Loc 7 7))
              (Some (Piece 'Rook 'White)))

(check-expect (board-update GRADER_starting-board (Loc 0 0)
                            (Some (Piece 'Rook 'Black)))
              GRADER_starting-board)

(check-expect (board-update GRADER_starting-board (Loc 0 0) 'None)
              (GRADER_board-update GRADER_starting-board (Loc 0 0) 'None))

(check-expect
 (board-update GRADER_starting-board (Loc 0 7)
               (Some (Piece 'Pawn 'White)))
 (GRADER_board-update GRADER_starting-board (Loc 0 7)
                      (Some (Piece 'Pawn 'White))))

(check-expect
 (board-update GRADER_starting-board (Loc 7 0)
               (Some (Piece 'Rook 'Black)))
 (GRADER_board-update GRADER_starting-board (Loc 7 0)
                      (Some (Piece 'Rook 'Black))))

(check-expect
 (board-update GRADER_starting-board (Loc 7 7)
               (Some (Piece 'Queen 'Black)))
 (GRADER_board-update GRADER_starting-board (Loc 7 7)
                      (Some (Piece 'Queen 'Black))))

(check-expect
 (board-update GRADER_starting-board (Loc 4 5)
               (Some (Piece 'Knight 'Black)))
 (GRADER_board-update GRADER_starting-board (Loc 4 5)
                      (Some (Piece 'Knight 'Black))))

(: GRADER_imrb : Board)
(define GRADER_imrb
  (strings->board
   (list  "-N-K-BN-"
          "P--PPPPP"
          "-P------"
          "--P---Q-"
          "R-ppB--R"
          "--------"
          "pp--pppp"
          "rnbqkbnr")))

(: GRADER_inmediares : ChessGame)
(define GRADER_inmediares
  (ChessGame GRADER_imrb 'Black '() (Castles #f #f #t #t)))

(: GRADER_icb : Board)
(define GRADER_icb
  (strings->board
   (list  "--------"
          "-K-Q----"
          "--------"
          "---b----"
          "--------"
          "--------"
          "--------"
          "-------k")))

(: GRADER_incheck : ChessGame)
(define GRADER_incheck
  (ChessGame GRADER_icb 'Black '() (Castles #f #f #t #t)))

(: GRADER_rcb : Board)
(define GRADER_rcb
  (strings->board
   (list  "--------"
          "-K------"
          "--Q-----"
          "---b----"
          "--------"
          "--------"
          "--------"
          "-------k")))

(: GRADER_revealcheck : ChessGame)
(define GRADER_revealcheck
  (ChessGame GRADER_rcb 'Black '() (Castles #f #f #t #t)))

(check-expect (in-check? GRADER_new-game) #f)

(check-expect (in-check? GRADER_inmediares) #f)

(check-expect (in-check? GRADER_incheck) #t)

(check-expect (in-check? GRADER_revealcheck) #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 1 4) (Loc 1 5) (Piece 'Pawn 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 6 4) (Piece 'Pawn 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 5 4) (Piece 'Pawn 'White) 'None))
 #t)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 4 4) (Piece 'Pawn 'White) 'None))
 #t)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 5 3) (Piece 'Pawn 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 5 6) (Piece 'Pawn 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 6 4) (Loc 3 4) (Piece 'Pawn 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 2 1) (Loc 4 1) (Piece 'Pawn 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 2)
                       (Loc 4 2)
                       (Piece 'Pawn 'Black)
                       (Some (Piece 'Pawn 'White))))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 2) (Loc 4 3) (Piece 'Pawn 'Black)
                       (Some (Piece 'Pawn 'White))))
 #t)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 7 1) (Loc 6 0) (Piece 'Knight 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 7 1) (Loc 6 3) (Piece 'Knight 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 7 1) (Loc 5 0) (Piece 'Knight 'White) 'None))
 #t)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 7 1) (Loc 5 2) (Piece 'Knight 'White) 'None))
 #t)

(check-expect
 (legal-move? GRADER_new-game
              (StdMove (Loc 7 1) (Loc 5 1) (Piece 'Knight 'White) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 3 5) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 3 4) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 3 3) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 3 2) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 3 7) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 2 6) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 1 6) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 4 6) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 5 6) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 6 6) (Piece 'Queen 'Black)
                       (Some (Piece 'Pawn 'White))))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 7 6) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 2 5) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 1 4) (Piece 'Queen 'Black)
                       (Some (Piece 'Pawn 'White))))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 2 7) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 4 5) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 5 4) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 6 3) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 7 2) (Piece 'Queen 'Black)
                       (Some (Piece 'Bishop 'White))))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 3 6) (Loc 4 7) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 3 3) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 2 2) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 1 1) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 0 0) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 3 5) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 2 6) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 1 7) (Piece 'Bishop 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 5 3) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 6 2) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4)
                       (Loc 7 1)
                       (Piece 'Bishop 'Black)
                       (Some (Piece 'Knight 'White))))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4) (Loc 5 5) (Piece 'Bishop 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 4)
                       (Loc 6 6)
                       (Piece 'Bishop 'Black)
                       (Some (Piece 'Pawn 'White))))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 4 6) (Piece 'Rook 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 4 5) (Piece 'Rook 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 4 4) (Piece 'Rook 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 3 7) (Piece 'Rook 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 2 7) (Piece 'Rook 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 1 7) (Piece 'Rook 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 5 7) (Piece 'Rook 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7)
                       (Loc 6 7)
                       (Piece 'Rook 'Black)
                       (Some (Piece 'Pawn 'White))))
 #t)

(check-expect
 (legal-move? GRADER_inmediares
              (StdMove (Loc 4 7) (Loc 7 7) (Piece 'Rook 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_incheck
              (StdMove (Loc 1 3) (Loc 0 3) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (legal-move? GRADER_incheck
              (StdMove (Loc 1 1) (Loc 1 0) (Piece 'King 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_incheck
              (StdMove (Loc 1 3) (Loc 2 2) (Piece 'Queen 'Black) 'None))
 #t)

(check-expect
 (legal-move? GRADER_incheck
              (StdMove (Loc 1 3)
                       (Loc 3 3)
                       (Piece 'Queen 'Black)
                       (Some (Piece 'Bishop 'White))))
 #t)

(check-expect
 (legal-move? GRADER_revealcheck
              (StdMove (Loc 2 2) (Loc 2 3) (Piece 'Queen 'Black) 'None))
 #f)

(check-expect
 (GRADER_sort-moves (available-moves-piece GRADER_new-game (Loc 6 4)))
 (GRADER_sort-moves
  (list
   (StdMove (Loc 6 4) (Loc 5 4) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 4) (Loc 4 4) (Piece 'Pawn 'White) 'None))))

(check-expect
 (GRADER_sort-moves (available-moves-piece GRADER_new-game (Loc 7 6)))
 (GRADER_sort-moves
  (list
   (StdMove (Loc 7 6) (Loc 5 5) (Piece 'Knight 'White) 'None)
   (StdMove (Loc 7 6) (Loc 5 7) (Piece 'Knight 'White) 'None))))

(check-expect (available-moves-piece GRADER_new-game (Loc 1 6)) '())

(check-expect
 (GRADER_sort-moves (available-moves-player GRADER_new-game))
 (GRADER_sort-moves
  (list
   (StdMove (Loc 6 0) (Loc 5 0) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 0) (Loc 4 0) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 1) (Loc 5 1) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 1) (Loc 4 1) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 2) (Loc 5 2) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 2) (Loc 4 2) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 3) (Loc 5 3) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 3) (Loc 4 3) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 4) (Loc 5 4) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 4) (Loc 4 4) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 5) (Loc 5 5) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 5) (Loc 4 5) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 6) (Loc 5 6) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 6) (Loc 4 6) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 7) (Loc 5 7) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 6 7) (Loc 4 7) (Piece 'Pawn 'White) 'None)
   (StdMove (Loc 7 1) (Loc 5 0) (Piece 'Knight 'White) 'None)
   (StdMove (Loc 7 1) (Loc 5 2) (Piece 'Knight 'White) 'None)
   (StdMove (Loc 7 6) (Loc 5 5) (Piece 'Knight 'White) 'None)
   (StdMove (Loc 7 6) (Loc 5 7) (Piece 'Knight 'White) 'None))))

(: GRADER_cmb : Board)
(define GRADER_cmb
  (strings->board
   (list "--------"
         "--------"
         "---PPP--"
         "----KP--"
         "----PP--"
         "--------"
         "---r----"
         "b------k")))

(: GRADER_checkmated : ChessGame)
(define GRADER_checkmated
  (ChessGame GRADER_cmb 'Black '() (Castles #f #f #f #f)))

(: GRADER_cmcb : Board)
(define GRADER_cmcb
  (strings->board
   (list "--------"
         "--------"
         "---PPP--"
         "----KP--"
         "----PP--"
         "--------"
         "R--r----"
         "b------k")))

(: GRADER_escape_cap : ChessGame)
(define GRADER_escape_cap (ChessGame GRADER_cmcb
  'Black '() (Castles #f #f #f #f)))

(: GRADER_cmmb : Board)
(define GRADER_cmmb
  (strings->board
   (list "--------"
         "--------"
         "---PPP--"
         "----KP--"
         "----PP--"
         "--------"
         "R-------"
         "b------k")))

(: GRADER_escape_move : ChessGame)
(define GRADER_escape_move
  (ChessGame GRADER_cmmb 'Black '() (Castles #f #f #f #f)))

(: GRADER_cmbb : Board)
(define GRADER_cmbb
  (strings->board
   (list "--------"
         "--------"
         "---PPP--"
         "---PKP--"
         "----PP--"
         "--------"
         "---r----"
         "b------k")))

(: GRADER_escape_block : ChessGame)
(define GRADER_escape_block
  (ChessGame GRADER_cmbb 'Black '() (Castles #f #f #f #f)))

(: GRADER_nmbb : Board)
(define GRADER_nmbb
  (strings->board
   (list "--------"
         "---r----"
         "-------r"
         "----K---"
         "-r------"
         "-----r--"
         "--------"
         "-------k")))

(: GRADER_no_move : ChessGame)
(define GRADER_no_move
  (ChessGame GRADER_nmbb 'Black '() (Castles #f #f #f #f)))

(: GRADER_krr : Board)
(define GRADER_krr
  (strings->board
   (list "--------"
         "--------"
         "--------"
         "--------"
         "--------"
         "--------"
         "-R------"
         "R------k")))

(define GRADER_krr-game
  (ChessGame GRADER_krr 'White '() (Castles #f #f #f #f)))
  
(check-expect (checkmate? GRADER_new-game) #f)
(check-expect (checkmate? GRADER_inmediares) #f)
(check-expect (checkmate? GRADER_incheck) #f)
(check-expect (checkmate? GRADER_revealcheck) #f)
(check-expect (checkmate? GRADER_checkmated) #t)
(check-expect (checkmate? GRADER_escape_cap) #f)
(check-expect (checkmate? GRADER_escape_move) #f)
(check-expect (checkmate? GRADER_escape_block) #f)
(check-expect (checkmate? GRADER_no_move) #f)
(check-expect (checkmate? GRADER_krr-game) #t)

(check-expect (stalemate? GRADER_new-game) #f)
(check-expect (stalemate? GRADER_inmediares) #f)
(check-expect (stalemate? GRADER_incheck) #f)
(check-expect (stalemate? GRADER_revealcheck) #f)
(check-expect (stalemate? GRADER_checkmated) #f)
(check-expect (stalemate? GRADER_escape_cap) #f)
(check-expect (stalemate? GRADER_escape_move) #f)
(check-expect (stalemate? GRADER_escape_block) #f)
(check-expect (stalemate? GRADER_no_move) #t)
(check-expect (stalemate? GRADER_krr-game) #f)

(: GRADER_casb : Board)
(define GRADER_casb
  (strings->board
   (list  "R---K--R"
          "--------"
          "--------"
          "--------"
          "--------"
          "--------"
          "--------"
          "r---k--r")))

(: GRADER_casgame-b : ChessGame)
(define GRADER_casgame-b
  (ChessGame GRADER_casb 'Black '() (Castles #t #t #t #t)))

(: GRADER_casgame-w : ChessGame)
(define GRADER_casgame-w
  (ChessGame GRADER_casb 'White '() (Castles #t #t #t #t)))

(check-expect
 (apply-move GRADER_new-game
             (StdMove (Loc 6 4) (Loc 5 4) (Piece 'Pawn 'White) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_starting-board (Loc 6 4) 'None)
   (Loc 5 4)
   (Some (Piece 'Pawn 'White)))
  'Black
  (list (StdMove (Loc 6 4) (Loc 5 4) (Piece 'Pawn 'White) 'None))
  (Castles #t #t #t #t)))

(check-expect
 (apply-move GRADER_casgame-b
             (StdMove (Loc 0 0) (Loc 1 0) (Piece 'Rook 'Black) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 0 0) 'None)
   (Loc 1 0) (Some (Piece 'Rook 'Black)))
  'White
  (list (StdMove (Loc 0 0) (Loc 1 0) (Piece 'Rook 'Black) 'None))
  (Castles #f #t #t #t)))

(check-expect
 (apply-move GRADER_casgame-b
             (StdMove (Loc 0 4) (Loc 1 4) (Piece 'King 'Black) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 0 4) 'None)
   (Loc 1 4) (Some (Piece 'King 'Black)))
  'White
  (list (StdMove (Loc 0 4) (Loc 1 4) (Piece 'King 'Black) 'None))
  (Castles #f #f #t #t)))

(check-expect
 (apply-move GRADER_casgame-b
             (StdMove (Loc 0 7) (Loc 1 7) (Piece 'Rook 'Black) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 0 7) 'None)
   (Loc 1 7) (Some (Piece 'Rook 'Black)))
  'White
  (list (StdMove (Loc 0 7) (Loc 1 7) (Piece 'Rook 'Black) 'None))
  (Castles #t #f #t #t)))

(check-expect
 (apply-move GRADER_casgame-w
             (StdMove (Loc 7 0) (Loc 6 0) (Piece 'Rook 'White) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 7 0) 'None)
   (Loc 6 0) (Some (Piece 'Rook 'White)))
  'Black
  (list (StdMove (Loc 7 0) (Loc 6 0) (Piece 'Rook 'White) 'None))
  (Castles #t #t #f #t)))

(check-expect
 (apply-move GRADER_casgame-w
             (StdMove (Loc 7 4) (Loc 6 4) (Piece 'King 'White) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 7 4) 'None)
   (Loc 6 4) (Some (Piece 'King 'White)))
  'Black (list (StdMove (Loc 7 4) (Loc 6 4) (Piece 'King 'White) 'None))
  (Castles #t #t #f #f)))

(check-expect
 (apply-move GRADER_casgame-w
             (StdMove (Loc 7 7) (Loc 6 7) (Piece 'Rook 'White) 'None))
 (ChessGame
  (GRADER_board-update
   (GRADER_board-update GRADER_casb (Loc 7 7) 'None)
   (Loc 6 7) (Some (Piece 'Rook 'White)))
  'Black (list (StdMove (Loc 7 7) (Loc 6 7) (Piece 'Rook 'White) 'None))
  (Castles #t #t #t #f)))

(check-expect
 (strings->board
  (list "RNBQKBNR"
        "PPPPPPPP"
        "--------"
        "--------"
        "--------"
        "--------"
        "pppppppp"
        "rnbqkbnr"))
 GRADER_starting-board)

;; eyeball tests

"starting board"
(board->image GRADER_starting-board)

"casgame-b"
(board->image (ChessGame-board GRADER_casgame-b))

"krr"
(board->image GRADER_krr)

"inmediares"
(board->image (ChessGame-board GRADER_inmediares))

(: board1 : Board)
(define board1
  (strings->board
   (list "--------" "----k---" "--------" "--------"
         "------K-" "-R------" "--------" "--------")))
(: game1 : ChessGame)
(define game1
  (ChessGame board1 'Black '() (Castles #t #t #t #t)))

(: board2 : Board)
(define board2
  (strings->board
   (list "----K---" "--npR---" "--N-B-Q-" "--------"
         "--------" "--------" "--------" "bqnkr---")))
(: game2 : ChessGame)
(define game2
  (ChessGame board2 'White '() (Castles #t #t #t #t)))

(: game3 : ChessGame)
(define game3
  (ChessGame board2 'Black '() (Castles #t #t #t #t)))

(: ck1 : Board)
(define ck1
  (strings->board
   (list "---K--r-" "--------" "---k----" "--------"
         "--------" "--------" "--------" "--------")))
(: checkmate1 : ChessGame)
(define checkmate1
  (ChessGame ck1 'Black '() (Castles #t #t #t #t)))

(: ck2 : Board)
(define ck2
  (strings->board
   (list "-q------" "-----PK-" "--P---P-" "-P--n--P"
         "-B-----p" "-BN-----" "--R---p-" "--k-----")))
(: checkmate2 : ChessGame)
(define checkmate2
  (ChessGame ck2 'White '() (Castles #t #t #t #t)))

(: st1 : Board)
(define st1
  (strings->board
   (list "-----K--" "-----p--" "-----k--" "--------"
         "--------" "--------" "--------" "--------")))
(: stalemate1 : ChessGame)
(define stalemate1
  (ChessGame st1 'Black '() (Castles #t #t #t #t)))

(: st2 : Board)
(define st2
  (strings->board
   (list "KB-----r" "--------" "-k------" "--------"
         "--------" "--------" "--------" "--------")))
(: stalemate2 : ChessGame)
(define stalemate2
  (ChessGame st2 'Black '() (Castles #t #t #t #t)))

;; TEST
;; ============== in-check? ==================
; in-check? test
(check-expect (in-check? game1) #f)
(check-expect (in-check? game2) #f)
(check-expect (in-check? game3) #t)
(check-expect (in-check? checkmate1) #t)
(check-expect (in-check? checkmate2) #t)
(check-expect (in-check? stalemate1) #f)
(check-expect (in-check? stalemate2) #f)
(check-expect (legal-move? new-game
                           (StdMove (Loc 6 0) (Loc 4 0)
                                    (Piece 'Pawn 'White) 'None)) #t)
(check-expect (legal-move? new-game
                           (StdMove (Loc 7 1) (Loc 5 0)
                                    (Piece 'Knight 'White) 'None)) #t)
(check-expect (legal-move? checkmate2
                           (StdMove (Loc 7 2) (Loc 7 3)
                                    (Piece 'King 'White) 'None)) #f)
(check-expect
 (legal-move? checkmate2
              (StdMove (Loc 5 2) (Loc 3 1)
                       (Piece 'Knight 'Black) (Some (Piece 'Pawn 'Black)))) #f)

(check-expect (stalemate? game1) #f)
(check-expect (stalemate? game2) #f)
(check-expect (stalemate? game3) #f)
(check-expect (stalemate? checkmate1) #f)
(check-expect (stalemate? checkmate2) #f)
(check-expect (stalemate? stalemate1) #t)
(check-expect (stalemate? stalemate2) #t)


(: bd1 : Board)
(define bd1
  (strings->board
   (list "--------" "----k---" "--------" "--------"
         "------K-" "--------" "----P---" "--------")))
(check-expect
 (legal-move?
  (ChessGame bd1 'Black '() (Castles #f #f #f #f))
  (PromoMove (Loc 6 4) (Loc 7 4) (Piece 'Pawn 'Black) 'None 'Queen)) #t)

(: bd2 : Board)
(define bd2
  (strings->board
   (list "--------" "----K---" "--------" "--------"
         "--------" "--------" "--------" "r---k---")))
(check-expect
 (legal-move?
  (ChessGame bd2 'White '() (Castles #f #f #t #t))
  (CastleMove (Loc 7 4) (Loc 7 1) (Loc 7 0) (Loc 7 2) 'White)) #t)
(test)

