#lang typed/racket

(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")
(require "../uchicago151/uc151universe.rkt")
(require "../project2/option.rkt")
(require "../project2/loc.rkt")
(require "chess-logic.rkt")

(define-struct Dims
  ([width  : Integer]
   [height : Integer]))

(define-struct (Sized T)
  ([t    : T]
   [dims : Dims]))

(define-type (Reporter T)
  (T -> String))

;; asked Deqing Fu about the structure of ChessWorld
(define-struct ChessWorld
  ([game : (Sized ChessGame)]
   [moveFrom : (Option Loc)]
   [moveTo : (Option Loc)]
   [promoStop? : Boolean]
   [info : (Sized (Reporter ChessWorld))]
   [quit? : Boolean]))

(: guarantee-byte (Integer -> Byte))
(define (guarantee-byte n)
  (if (byte? n) n (error "not a byte")))

(: repo (Reporter ChessWorld))
(define (repo world)
  (match world
    [(ChessWorld (Sized game dimB) LocF LocT promoS repo quit?)
     (match game
       [(ChessGame board p _ _)
        (cond
          [(checkmate? game)
           (string-append "CHECKMATE"
                          "  WINNER :  "
                          (symbol->string (opponent p))
                          ".")]
          [(stalemate? game) "STALEMATE"]
          [promoS (string-append
                   "PRESS KEY TO PROMOTE"
                   "q: Queen; r: Rook; b: Bishop; n: Knight")]
          [else (if (in-check? game)
                    (string-append
                     (symbol->string p)
                     "'s turn and you are in check")
                    (string-append
                     (symbol->string p) "'s TURN"))])])]))

(: new-chess-world : ChessGame Dims Dims -> ChessWorld)
;; make up a new chess world
(define (new-chess-world game dimbd dimrepo)
  (ChessWorld (Sized game dimbd) 'None 'None #f (Sized repo dimrepo) #f))

(: handle-click (ChessWorld Integer Integer Mouse-Event -> ChessWorld))
;; mouse events
(define (handle-click world x y mouse)
  (match mouse
    ["button-down"
     (match world
       [(ChessWorld (Sized game dim-bd) loc1 loc2 ps? repo q?)
        (if ps? world
            (match* (dim-bd game)
              [((Dims wid hei) (ChessGame bd py _ _))
               (local
                 {(define click-loc (Loc (quotient y (quotient hei 8))
                                         (quotient x (quotient wid 8))))}
                 (match* (loc1 loc2)
                   [('None 'None)
                    (ChessWorld (Sized game dim-bd)
                                (Some click-loc) 'None #f repo q?)]
                   [((Some loc) 'None)
                    (match (filter
                            (lambda ([m : Move])
                              (match m
                                [(StdMove src dst _ _)
                                 (equal? click-loc dst)]
                                [(CastleMove king-src king-dst _ _ _)
                                 (equal? click-loc king-dst)]
                                [(PromoMove src dst _ _ _)
                                 (equal? click-loc dst)]))
                            (filter
                             (lambda ([m : Move]) (legal-move? game m))
                             (available-moves-piece game loc)))
                      ['() (ChessWorld
                            (Sized game dim-bd) 'None 'None #f repo q?)]
                      [listofmove
                       (local
                         {(: contain-promo? : (Listof Move) -> Boolean)
                          (define (contain-promo? ms)
                            (match
                                (filter (lambda ([m : Move])
                                          (match m
                                            [(PromoMove _ _ _ _ _) #t]
                                            [_ #f])) ms)
                              ['() #f]
                              [_ #t]))}
                         (cond
                           [(contain-promo? listofmove)
                            (ChessWorld
                             (Sized game dim-bd) loc1
                             (Some click-loc) #t repo q?)]
                           [else
                            (ChessWorld
                             (Sized (apply-move game (first listofmove)) dim-bd)
                            'None 'None ps? repo q?)]))])]
                  [((Some _) (Some _)) world]))]))]
       [_ world])]
    [_ world]))

(: handle-key (ChessWorld String -> ChessWorld))
;; key event
(define (handle-key world str)
  (match world
    [(ChessWorld (Sized game dim-bd) loc1 loc2 ps? repo quit?)
     (match repo
       [(Sized _ dim-repo)
        (cond
          [ps?
           (match game
             [(ChessGame BD P HIS CAS)
              (match str
                [(or "q" "Q")
                 (ChessWorld
                  (Sized (apply-move
                          game
                          (PromoMove
                           (val-of loc1)
                           (val-of loc2)
                           (val-of (board-ref BD (val-of loc1)))
                           (board-ref BD (val-of loc2)) 'Queen)) dim-bd)
                  'None 'None #f repo quit?)]
                [(or "r" "R")
                 (ChessWorld
                  (Sized (apply-move
                          game
                          (PromoMove
                           (val-of loc1)
                           (val-of loc2)
                           (val-of (board-ref BD (val-of loc1)))
                           (board-ref BD (val-of loc2)) 'Rook)) dim-bd)
                  'None 'None #f repo quit?)]
                [(or "n" "N")
                 (ChessWorld
                  (Sized (apply-move
                          game
                          (PromoMove
                           (val-of loc1)
                           (val-of loc2)
                           (val-of (board-ref BD (val-of loc1)))
                           (board-ref BD (val-of loc2)) 'Knight)) dim-bd)
                  'None 'None #f repo quit?)]
                [(or "b" "B")
                 (ChessWorld
                  (Sized (apply-move
                          game
                          (PromoMove
                           (val-of loc1)
                           (val-of loc2)
                           (val-of (board-ref BD (val-of loc1)))
                           (board-ref BD (val-of loc2)) 'Bishop)) dim-bd)
                  'None 'None #f repo quit?)]
                ["e" (new-chess-world new-game dim-bd dim-repo)]
                ["escape" (ChessWorld (Sized game dim-bd)
                                      loc1 loc2 ps? repo #t)]
                [_ world])])]
          [else
           (match str
             ["e" (new-chess-world new-game dim-bd dim-repo)]
             ["escape" (ChessWorld (Sized game dim-bd)
                                      loc1 loc2 ps? repo #t)]
                [_ world])])])]))

(: draw-chess-board : ChessGame (Option Loc) (Option Loc) Dims -> Image)
;; draw chessboard according to given chessgame
(define (draw-chess-board game loc1 loc2 dim)
  (match* (dim game)
    [((Dims n _) (ChessGame board _ _ _))
     (local
       {(define length (quotient n 8))
        (: square->image : Square -> Image)
        ;; helper function to turn a square to image
        (define (square->image s)
          (match s
            ['None empty-image]
            [(Some (Piece 'Pawn 'Black))(text "♟" 30 "black")]
            [(Some (Piece 'Pawn 'White))(text "♙" 30 "black")]
            [(Some (Piece 'Rook 'Black))(text "♜" 30 "black")]
            [(Some (Piece 'Rook 'White))(text "♖" 30 "black")]
            [(Some (Piece 'Bishop 'Black))(text "♝" 30 "black")]
            [(Some (Piece 'Bishop 'White))(text "♗" 30 "black")]
            [(Some (Piece 'Knight 'Black))(text "♞" 30 "black")]
            [(Some (Piece 'Knight 'White))(text "♘" 30 "black")]
            [(Some (Piece 'King 'Black))(text "♚" 30 "black")]
            [(Some (Piece 'King 'White))(text "♔" 30 "black")]
            [(Some (Piece 'Queen 'Black))(text "♛" 30 "black")]
            [(Some (Piece 'Queen 'White))(text "♕" 30 "black")]))
        (: row->image : (Listof Square) Integer -> Image)
        ;; helper funtion to turn a row to image
        (define (row->image row x)
          (local
            {(: loop : Integer -> Image)
             (define (loop i)
               (cond
                 [(< i 0) empty-image]
                 [else
                  (beside (loop (sub1 i))
                                (if (none? loc1)
                                    (cond
                                  [(even? (+ x i))
                                   (overlay 
                                    (square->image (list-ref row i))
                                    (square length "solid" "beige"))]
                                  [else
                                   (overlay
                                    (square->image (list-ref row i))
                                    (square length "solid" "brown"))])
                                    (match loc1
                                      [(Some (Loc r c))
                                       (if (and (= x r) (= c i))
                                           (overlay 
                                            (square->image (list-ref row i))
                                            (square length "solid" "pink"))
                                           (cond
                                  [(even? (+ x i))
                                   (overlay 
                                    (square->image (list-ref row i))
                                    (square length "solid" "beige"))]
                                  [else
                                   (overlay
                                    (square->image (list-ref row i))
                                    (square length "solid" "brown"))]))])))]))}
            (loop 7)))
        (: board->image : Board -> Image)
        ;; draw a picture according to the given board
        (define (board->image bd)
          (above (row->image (first bd) 0)
                 (row->image (second bd) 1)
                 (row->image (third bd) 2)
                 (row->image (fourth bd) 3)
                 (row->image (fifth bd) 4)
                 (row->image (sixth bd) 5)
                 (row->image (seventh bd) 6)
                 (row->image (last bd) 7)))}
       (board->image board))]))

(: end-world? : ChessWorld -> Boolean)
;; check whether to end the world
(define (end-world? world)
  (ChessWorld-quit? world))

(: draw-chess-world : ChessWorld -> Image)
;; draw an image according to a chessworld given
(define (draw-chess-world world)
  (above
   (match world
     [(ChessWorld (Sized game dim-bd) loc1 loc2 _ _ _)
      (draw-chess-board game loc1 loc2 dim-bd)])
   (local
     {(: draw-report : ChessWorld -> Image)
      (define (draw-report w)
        (match w
          [(ChessWorld _ _ _ ps? (Sized _ dim-repo) _)
           (match dim-repo
             [(Dims wid hei)
              (overlay (above
                        (if ps?
                            (above
                             (text "PRESS KEY TO PROMOTE" 15 "black")
                             (text "Q → Queen, N → Knight" 15 "black")
                             (text "R → Rook,  B → Bishop" 15 "black"))
                            (text (repo w) 20 "black"))
                        (rectangle wid 10 "solid" "gray")
                        (text "PRESS ESC TO END THE GAME" 20 "black"))
                       (rectangle wid hei "solid" "gray"))])]))}
     (draw-report world))))

(: main (ChessGame Dims Dims -> ChessWorld))
;; draw the main
(define (main g dimBoard dimRepo)
  (big-bang
   (new-chess-world new-game dimBoard dimRepo) : ChessWorld
   [to-draw draw-chess-world]
   [on-mouse handle-click]
   [on-key handle-key]
   [stop-when end-world?]
   [name "Chess-World"]))

(main new-game (Dims 480 480) (Dims 480 100)) 

       
        
        
        
        