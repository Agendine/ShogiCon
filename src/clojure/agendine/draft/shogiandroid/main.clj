(ns agendine.draft.shogiandroid.main
  (:require [neko.activity :refer [defactivity set-content-view!]]
            [neko.activity :as activity]
            [neko.debug :refer [*a]]
            [neko.debug :as debug]
            [neko.notify :refer [toast]]
            [neko.resource :as res]
            [neko.find-view :refer [find-view]]
            [neko.ui.adapters :as adap]
            [neko.ui :as ui]
            [neko.intent :as nekointent]
            [neko.listeners.dialog :as listeners]
            [neko.dialog.alert :as dlg]
            ;; [neko.ui :refer [config]]
            [neko.ui.mapping :as mapping]
            [neko.threading :as threading]
            [neko.log :as log]
            [neko.threading :refer [on-ui]])
  (:import android.view.ViewGroup
           android.view.View$OnClickListener
           android.widget.EditText
           android.widget.LinearLayout
           android.widget.RelativeLayout
           android.app.AlertDialog
           android.content.DialogInterface
           android.content.Intent
           android.support.v4.app.FragmentActivity
           com.google.android.gms.auth.api.Auth
           com.google.android.gms.auth.api.signin.GoogleSignInAccount
           com.google.android.gms.auth.api.signin.GoogleSignInOptions
           com.google.android.gms.auth.api.signin.GoogleSignInOptions$Builder
           com.google.android.gms.auth.api.signin.GoogleSignInResult
           com.google.android.gms.common.ConnectionResult
           com.google.android.gms.common.SignInButton
           com.google.android.gms.common.api.GoogleApiClient
           com.google.android.gms.common.api.GoogleApiClient$OnConnectionFailedListener
           com.google.android.gms.common.api.GoogleApiClient$Builder
           com.google.android.gms.common.api.OptionalPendingResult
           com.google.android.gms.common.api.ResultCallback
           com.google.android.gms.common.api.Status
           android.widget.ImageButton))

;; We execute this function to import all subclasses of R class. This gives us
;; access to all application resources.
(res/import-all)




;; *****************************************************************
;; Generic Utility Functions:
;; *****************************************************************

(defn filter-keys-by-val
  "Returns all keys in map for which (pred value) returns true."
  [pred m]
  (when m
    (for [[key val] m :when (pred val)] key)))

;; (defn not-empty [val]
;; "Simple predicate for comparing a value to empty"
;; (false? (= val "empty")))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (assoc m k newmap))
      m)
    (dissoc m k)))


;; *****************************************************************
;; Movement:
;; *****************************************************************

;; Utility functions for movement
;; ---------------------------------------------

(defn is-on-board
  "Utility function to determine if a space is on the board"
  [x-coord y-coord]
  (and (and (> x-coord 0) (<= x-coord 9))
       (and (> y-coord 0) (<= y-coord 9))))

;; Core Movement Logic:
;; ---------------------------------------------

(defn move-direction
  "Quick implementation for the logic of generic movement in a
    direction, called recursively.

    Actually just tests if the current space is a semi-legal move, and if so whether to
    then test the next space in that direction, or else return a vector of the legal
    moves found. direction-x/y is +1 or -1 (- is up/left, + is down/right) and
    origin-x/y refers to the space being tested.  The remaining parameter allows
    for having either one-space or full-board movement in that direction.
    The accumulator parameter should be [] on an initial call, and will be returned
    as [[x y] [x2 y2]] style list of coordinate-pairs of legal moves.
    The player parameter indicates the moving piece's owner.
    Returns a vector of available moves in the direction."
  [game direction-x direction-y origin-x origin-y remaining player]
  ;; If the current space is on-board, continue, else end checking in this direction.
  (if (is-on-board origin-x origin-y)
    ;; If the piece has enough movement for the distance, continue, else end
    (if (< 0 remaining)
      ;; If the current space is unoccupied, then proceed on this direction
      (if (= (get-in game [:board origin-x origin-y]) nil)
        (into (move-direction game direction-x direction-y
                              (+ direction-x origin-x) (+ direction-y origin-y)
                              (- remaining 1) player) [[origin-x origin-y]])
        ;; If the space is occupied by Player's Piece, stop checking this direction
        (if (= (get-in game [:board origin-x origin-y :owner]) player)
          []
          [[origin-x origin-y]]))
      [])
    []))

(defn move-horizontal
  "Function for handling the fact that this piece can move
   left and right, with single-space or full-board movement
   determined by the value of the spaces variable.
   Returns a list of available moves, or an empty vector if none exist."
  [game spaces origin-x origin-y player]
  (let [available-moves (move-direction game -1 0 origin-x origin-y spaces player)]
    (into [] (distinct (into (move-direction game 1 0 origin-x origin-y spaces player)
                             available-moves)))))


(defn move-diagonal
  "Function for handling the fact that this piece can
   move in 4 diagonal directions, with single-space or
   full-board movement determined by the value of the spaces variable.
   Returns a list of available moves, or an empty vector if none exist."
  [game spaces origin-x origin-y player]
  (let [available-moves-left (into (move-direction game -1 -1
                                                    (- origin-x 1) (- origin-y 1)
                                                    spaces player)
                                   (move-direction game -1 1
                                                    (- origin-x 1) (+ origin-y 1)
                                                    spaces player))
        available-moves-right (into (move-direction game 1 -1
                                                     (+ origin-x 1) (- origin-y 1)
                                                     spaces player)
                                    (move-direction game 1 1
                                                     (+ origin-x 1) (+ origin-y 1)
                                                     spaces player))]
    (into available-moves-left available-moves-right)))

(defn move-diagonal-forward
  "Function for handling the fact that this piece can move in
   2 diagonal directions, with single-space or full-board movement
   determined by the value of the spaces variable.
   Returns a list of available moves, or an empty vector if none exist."
  [game spaces origin-x origin-y player]
  (let [available-moves-left (move-direction game -1 player
                                             (- origin-x 1) (+ player origin-y)
                                             spaces player)
        available-moves-right (move-direction game 1 player
                                              (+ origin-x 1) (+ player origin-y)
                                              spaces player)]
    (into available-moves-left available-moves-right)))

(defn move-forward
  "Function for handling the fact that this piece can move forward
  (relative to owner), with single-space or full-board movement
  determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [game spaces origin-x origin-y player]
  (move-direction game 0 player origin-x (+ player origin-y) spaces player))

(defn move-backward
  "Function for handling the fact that this piece can move backward
   (relative to owner), with single-space or full-board movement
   determined by the value of the spaces variable.
   Returns a list of available moves, or an empty vector if none exist."
  [game spaces origin-x origin-y player]
  (move-direction game 0 (* player -1) origin-x (- origin-y player) spaces player))

(defn move-jump
  "Function for handling Knights' ability to move by jumping to spaces.
   NOTE: The spaces parameter is kept, but not used, to ease generic
         movement-function calling.
   Returns a list of available jumps, or an empty vector if none are available."
  [game spaces origin-x origin-y player]
  (into (move-direction game 1 1 (+ origin-x 1) (+ origin-y (* 2 player)) 1 player)
        (move-direction game 1 1 (- origin-x 1) (+ origin-y (* 2 player)) 1 player)))


;; *****************************************************************************************
;; Board Setup:
;; *****************************************************************************************


;; ---------------------------------------------
;; Global, immutable, essentially 'const' definitions, for shorthand.
;; No need to ever change these.
;; ---------------------------------------------

(def test-img R$drawable/chessp32)
(def test-hl-img R$drawable/chess32)

(def king-type {:moves [[move-horizontal 1]
                        [move-forward 1]
                        [move-backward 1]
                        [move-diagonal 1]]
                :is-promotable false
                :promotion 'king-type
                :image test-img
                :image-highlighted test-hl-img
                :name "King"})

(def rook-type {:moves [[move-horizontal 9]
                        [move-forward 9]
                        [move-backward 9]]
                :is-promotable true
                :promotion 'promoted-rook-type
                :image test-img
                :image-highlighted test-hl-img
                :name "Rook"})

(def promoted-rook-type {:moves [[move-horizontal 1]
                                 [move-forward 1]
                                 [move-backward 1]
                                 [move-diagonal 9]]
                :is-promotable false
                :promotion 'rook-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedRook"})

(def bishop-type {:moves [[move-diagonal 9]]
                :is-promotable true
                :promotion 'promoted-bishop-type
                :image test-img
                :image-highlighted test-hl-img
                :name "Bishop"})

(def promoted-bishop-type {:moves [[move-horizontal 1]
                                   [move-forward 1]
                                   [move-backward 1]
                                   [move-diagonal 9]]
                :is-promotable false
                :promotion 'bishop-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedBishop"})

(def gold-general-type {:moves [[move-horizontal 1]
                                [move-forward 1]
                                [move-backward 1]
                                [move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'gold-general-type
                :image test-img
                :image-highlighted test-hl-img
                :name "GoldGeneral"})

(def silver-general-type {:moves [[move-forward 1]
                                  [move-diagonal 1]]
                :is-promotable true
                :promotion 'promoted-silver-general-type
                :image test-img
                :image-highlighted test-hl-img
                :name "SilverGeneral"})

(def promoted-silver-general-type {:moves [[move-horizontal 1]
                                           [move-forward 1]
                                           [move-backward 1]
                                           [move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'silver-general-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedSilverGeneral"})

(def knight-type {:moves [[move-jump 1]]
                :is-promotable true
                :promotion 'promoted-knight-type
                :image test-img
                 :image-highlighted test-hl-img
                :name "Knight"})

(def promoted-knight-type {:moves [[move-horizontal 1]
                                   [move-forward 1]
                                   [move-backward 1]
                                   [move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'knight-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedKnight"})

(def lance-type {:moves [[move-forward 9]]
                :is-promotable true
                :promotion 'promoted-lance-type
                :image test-img
                :image-highlighted test-hl-img
                :name "Lance"})

(def promoted-lance-type {:moves [[move-horizontal 1]
                                  [move-forward 1]
                                  [move-backward 1]
                                  [move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'lance-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedLance"})

(def pawn-type {:moves [[move-forward 1]]
                :is-promotable true
                :promotion 'promoted-pawn-type
                :image test-img
                :image-highlighted test-hl-img
                :name "Pawn"})

(def promoted-pawn-type {:moves [[move-horizontal 1]
                                 [move-forward 1]
                                 [move-backward 1]
                                 [move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'pawn-type
                :image test-img
                :image-highlighted test-hl-img
                :name "PromotedPawn"})



;; ---------------------------------------------
;; DEPRECATED:
;; Type Definition and Initialization:
;; ---------------------------------------------

(defn create-pieces
  "DEPRECATED:
  Function to instantiate Piece Types.  Creates the specified number of Pieces, of
   the specified Type, distributed evenly between the two players' ownership.
   NOTE: a Piece has: :owner and :type"
  [[piece-type piece-amount board]]
  (do
    (loop [index 1]
      (do
        (eval `(assoc board ~(symbol (str (piece-type :name)
                                          index)) {:owner 1 :type ~piece-type}))
        (eval `(assoc board ~(symbol (str (piece-type :name)
                                          (+ 1 index))) {:owner -1 :type ~piece-type}))
        (if (< index piece-amount)
          (recur (+ 2 index)))))
    board))

(defn create-mass-pieces
  "DEPRECATED:
  Function to call create-pieces on a list of Type-Amount pairs,
  suitable for initializing the Board at start of game."
  [list-of-pairs board]
  (reduce create-pieces list-of-pairs board))

(defn initialize-pieces
  "DEPRECATED:
  This function creates all starting pieces at once, but they still have
  to be placed on the board."
  [board]
  (create-mass-pieces
   [[king-type 2]
    [rook-type 2]
    [bishop-type 2]
    [gold-general-type 4]
    [silver-general-type 4]
    [knight-type 4]
    [lance-type 4]
    [pawn-type 18]]
   board))

(defn pretty-print
  "DRAFT 2: Development method for pretty-printing the board.
  This one is adjusted for the (x y) coordinate lookup system."
  [game]
  (map (fn [arg1]
         (println (apply str
                         (map #(if (nil? (get-in game [:board % arg1 :type :name]))
                                 (str "     | ")
                                 (str (get-in game [:board % arg1 :type :name]) " | "))
                              (range 1 10)))))
       (reverse (range 1 10))))


;; Board Definition and Initialization:
;; ---------------------------------------------


(defn setup-board
  "Function to set up the board for the initial gamestate.  Initializes all
   starting pieces,
   and sets up the board with each piece in its proper starting place.
   Also defines the board object and the game which containts that board.
   Note that these are all, essentially, global variables, but also all are
   ultimately contained, hierarchically, under the game map.

  edit 22Dec2015: Attempting to make entirely functional.
  Call it using: (setup-board)"
  []
  (let [game (hash-map)]
    (assoc game
           :turn 1
           1 {:player 1 :hand {} :in-check? false}
           -1 {:player -1 :hand {} :in-check? false}
           :board (let [board (hash-map)]
                    (initialize-pieces board)
                    (assoc board
                           1 (sorted-map 1 {:owner 1 :type lance-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type lance-type})
                           2 (sorted-map 1 {:owner 1 :type knight-type}
                                         2 {:owner 1 :type rook-type}
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type}
                                         8 {:owner -1 :type bishop-type}
                                         9 {:owner -1 :type knight-type})
                           3 (sorted-map 1 {:owner 1 :type silver-general-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type silver-general-type})
                           4 (sorted-map 1 {:owner 1 :type gold-general-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type gold-general-type})
                           5 (sorted-map 1 {:owner 1 :type king-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type king-type})
                           6 (sorted-map 1 {:owner 1 :type gold-general-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type gold-general-type})
                           7 (sorted-map 1 {:owner 1 :type silver-general-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type silver-general-type})
                           8 (sorted-map 1 {:owner 1 :type knight-type}
                                         2 {:owner 1 :type bishop-type}
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type}
                                         8 {:owner -1 :type rook-type}
                                         9 {:owner -1 :type knight-type})
                           9 (sorted-map 1 {:owner 1 :type lance-type} 2 nil
                                         3 {:owner 1 :type pawn-type} 4 nil 5 nil 6 nil
                                         7 {:owner -1 :type pawn-type} 8 nil
                                         9 {:owner -1 :type lance-type}))))))


;; ***************************************************************************
;; Game Queries:
;; ***************************************************************************


(defn get-other-player
  "Quick utility function to return a reference to the actual
   Player map opposite the parameter Player.
  Useful for quickly obtaining the other player's Hand."
  [player]
  (if (= player 1)
    -1
    1))

(defn turn
  "Given a gamestate, returns the player value for whose turn it is. (1 or -1)"
  [game]
  (if (= (rem (get game :turn) 2) 0)
    -1
    1))

(defn all-moves
  "DRAFT: function to aggregate all open moves from all movement methods of
  an unknown piece, queried by board position.
  NOTE: THESE RESULTS ARE RAW! Does NOT exclude moves for
        moving into check or other secondary rules violations."
  [game piece-x piece-y]
  ;;(distinct
  (if (nil? (get-in game [:board piece-x piece-y]))
    []
   (reduce into (map
                          (fn [[move-function num-spaces]]
                            (move-function game num-spaces
                             piece-x piece-y
                             (get-in game [:board piece-x piece-y :owner])))
                          (get-in game [:board piece-x piece-y :type :moves])))))

(defn all-moves-for-player
  "Function aggregates all possible spaces to which the parameter player's
  pieces could move semi-legally. NOTE THAT THIS IS RAW DATA!
  Secondary-rules violations, such as moving into check, are NOT checked
  at this stage.  All moves in the output vector are distinct, but are
  not matched to any particular moving piece."
 [game player]
 (remove #(= [] %)
         (distinct
          (reduce into
                  (map (fn [y-coord]
                         (reduce into (map
                                       (fn [x-coord] (if (= (get-in game [:board
                                                                          x-coord
                                                                          y-coord
                                                                          :owner])
                                                            player)
                                                       (all-moves game
                                                                        x-coord
                                                                        y-coord)
                                                       []))
                                       (range 1 10))))
                       (range 1 10))))))

(defn is-space-reachable-by-player?
  "Returns boolean result for whether any of the specified player's
   pieces could semi-legally reach the specified board space."
  [game x-coord y-coord player]
  (some #(= [x-coord y-coord] %) (all-moves-for-player game player)))

(defn is-space-reachable-by-piece?
  "Returns boolean result for whether a specified piece could (semi-)legally
   reach the specified board space."
  [game from-x from-y to-x to-y]
  (some #(= [to-x to-y] %) (all-moves game from-x from-y)))

(defn locate-king
  "DRAFT: Utility function which outputs an [x y] coordinate vector
          containing the location of the specified player's king."
  [game player]
  (first
   (for [x (range 1 10) y (range 1 10)
         :let [coords [x y]]
         :when (and (= (get-in game [:board x y :type :name]) "King")
                    (= (get-in game [:board x y :owner]) player))]
     coords)))

(defn is-empty? [game row col]
  "Utility boolean function, simply returns whether the given space is empty."
  (= nil (get-in game [:board row col])))

(defn is-promotable?
  "Utility boolean function, simply returns whether the given piece is
  of a type which can promote."
  [game row col]
  (get-in game [:board row col :type :is-promotable]))

(defn in-promotion-zone?
  "Boolean function indicating whether the given row is in the
  promotion zone for the specified player.  Does not take into account
  whether the piece is promotable"
  [row player]
  (if (= player 1)
    (>= row 6)
    (<= row 3)))

(defn can-promote?
  "Boolean function indicating whether the given move gives
  the potential to promote.  Does not take into account whether
  the piece *must* promote."
  [game from-x from-y to-y]
  (and (is-promotable? game from-x from-y)
       (or (in-promotion-zone? from-y
                               (get-in game [:board
                                             from-x from-y
                                             :owner]))
           (in-promotion-zone? to-y
                               (get-in game [:board
                                             from-x from-y
                                             :owner])))))

(defn last-row
  "Utility function to return the relative last row on
  the board for the specified player."
  [player]
  (if (= player 1)
    9
    1))

(defn next-to-last-row
  "Utility function to return the relative 2nd-to-last row on
  the board for the specified player."
  [player]
  (if (= player 1)
    8
    2))

(defn must-promote?
  "Utility boolean function indicating whether the piece
  is forced to promote at the end of a move due to a lack
  of legal moves in case it does not promote.
  NOTE: Currently only checks specific cases.  May or may not
        need to be made more general.  Further research required."
  [game from-x from-y to-y]
  (let [type (get-in game [:board from-x from-y :type])
        player (get-in game [:board from-x from-y :owner])]
    (if (or (and (= type knight-type)
                 (last-row player))
            (and (or (= type pawn-type)
                     (= type lance-type))
                 (or (last-row player)
                     (next-to-last-row player))))
      true
      false)))

;; **************************************************************
;; Movement:
;; **************************************************************

(defn capture-piece
"DRAFT: Conducts the actual capture of a piece, simply adding
        it to the attacker's hand and exchanging ownership
        of the piece, while setting its original board space
        to nil.  Only call this function when capture has
        already been verified for legality.  This function
        assumes that the proper player is performing
        the capture.
   TODO: De-promote as well.
         Test more thoroughly, particularly that both game
              and board update properly."
  [game captured-x captured-y]
  (if (= (get-in game [:board captured-x captured-y :owner]) 1)
    (assoc-in (assoc-in game [-1 :hand (count (get-in game [-1 :hand]))]
                        (get-in (assoc-in game [:board captured-x captured-y :owner] -1)
                                [:board captured-x captured-y]))
              [:board captured-x captured-y] nil)
    (assoc-in (assoc-in game [1 :hand (count (get-in game [1 :hand]))]
                        (get-in (assoc-in game [:board captured-x captured-y :owner] 1)
                                [:board captured-x captured-y]))
              [:board captured-x captured-y] nil)))

(defn move-piece
  "Changes the game state to reflect a piece's movement.  Will not deny
  illegal moves,
  TODO:  implement check/checkmate tracking,
         implement promotion"
  [game from-x from-y to-x to-y]
  (assoc-in (assoc-in game [:board to-x to-y] (get-in game [:board from-x from-y]))
            [:board from-x from-y] nil))

(defn drop-piece
  "DRAFT: Conducts a drop instead of a movement, removing the piece specified
          by player and hand-position from that hand, and putting it on the board
          at [to-x to-y]. Only call this function when the drop has already been
          verified for legality."
  [game player hand-pos to-x to-y]
  (dissoc-in (assoc-in game [:board to-x to-y]
                       (get-in game [player :hand hand-pos]))
             [player :hand  hand-pos]))

(defn promote [game row col]
  (assoc-in game [:board row col :type]
            (get-in game [:board
                          row col
                          :type
                          :promotion])))

;; TO DO:
;;        Query-all-in-hand
;;              ~for purposes of JSONification
;;        Promotions
;;             --ensure pieces aren't dropped into promotion or checkmate
;;        Turn advancement and enforcement.
;;             --checkmate compliances
;;        More thorough testing.



;; **************************************************************
;; Legality Checks:
;; **************************************************************



;;                   *******************************************
;; ------------------Refactored  to purely functional up to here------------
;;                   *******************************************



(defn is-in-check?
  "TESTING: DRAFT: simple boolean result for whether any of the
  opposing player's pieces have
   the parameter player's king as an available move.
  NOTE: DOES NOT conduct secondary checks to determine if those moves
                 can be completed legally (yet).
                 For example, it will not notice that
                 capture could only be attempted by putting
                 oneself in check."
   [board player]
  (let [[king-x king-y] (locate-king board player)
        opposing-player ((get-other-player player) :player)]
    (not (nil? (is-space-reachable-by-player? board king-x king-y
                                              opposing-player)))))

(defn is-in-checkmate?
  "TESTING: DRAFT: Simple boolean result for whether parameter player
  is currently in checkmate.
   TODO: Expand the search for moves which could displace check beyond
                just the king itself.
         Check for move repetition for stalemate."
  [board player]
  (let [[king-x king-y] (locate-king board player)]
    (if (and (is-in-check? board player) (empty? (all-moves board king-x king-y)))
      (true)
      (false))))


(defn highlighted?
  "Given x and y coordinates and a gamestate to go with them,
  determines (boolean) whether cell x2 y2 should be highlighted on the GUI."
  [game from-x from-y to-x to-y]
  (some #(= [to-x to-y] %)
        (all-moves game from-x from-y)))

;; "Is Legal Move"

(defn is-legal?
  "Returns boolean for whether a prospective movement is legal or not.
  Currently, only looks at whether the move can mechanically be
  executed by the piece and whether it's that piece's owner's turn.
  TODO: ensure that movement is not into check,
        ensure that any promotion is legal"
  [game from-x from-y to-x to-y]
  (and
   (highlighted? game from-x from-y to-x to-y)
   (= (turn game) (get-in game [:board from-x from-y :owner]))))


;; "Is Legal Drop"

;; "Is Legal Capture"


;; *****************************************************************************************
;; Conducting a move
;; *****************************************************************************************

(defn movelist-to-4tuple
  "Reading a list of moves from a string to a useable list of 4-tuples"
  [moveList]
  (for [currMove (partition 4 moveList)] (map #(Character/getNumericValue %) currMove)))

;; Fast-forwarding game state from (setup-board) to <current-state> using a list of
;; [:fromX :fromY :toX :toY] tuples.  Note that :fromX = -1 yields "from player1's hand"
;; and :fromX = -2 yields "from player2's hand"

(defn inc-turn
  "Increments the turn counter for a gamestate"
  [game]
  (assoc game :turn (inc (get game :turn))))

(defn take-turn
  "DRAFT: Function to take the parsed version of a turn and conduct the
          logic of a turn on it.  If the parse says it's a drop, it
          drops the piece from the specified hand/hand-position
          [from-x, 0==drop from p1, from-y 0==drop from p2,
          the other becomes hand-position].
          If not, then it treats it as a move, with a capture as well if the
          destination is occupied.
          Clearly, the logic is primitive.  It doesn't error handle,
          and it doesn't check move legality.
   TODO: Add promotion execution."
  [game from-x from-y to-x to-y]
  (inc-turn
    (if (= from-x 0)
      ;;TODO: "IF legal-drop", else fail
      (drop-piece game -1 from-y to-x to-y)
      (if (= from-y 0)
        ;;TODO: "IF legal-drop", else fail
        (drop-piece game 1 from-y to-x to-y)
        (if (not (= (get-in game [:board to-x to-y]) nil))
          ;;TODO: "IF legal-capture", else fail
          (move-piece (capture-piece game to-x to-y)
                      from-x from-y to-x to-y)
          ;;TODO: "IF legal-move", else fail
          (move-piece game from-x from-y to-x to-y))))))


;; TODO
;;   --------IMPORTANT------------
;;   -Set up parse from string to game state.
;;   -----------------------------
;;  -

;; *************************************************************
;; SQLite Interop (for Android):
;; *************************************************************

(defn save-move [move-list from-x from-y to-x to-y promote]
  "Saves the most recent move to a movelist, which is itself
   expected to be directly portable to SQLite.
   NOTE: If a vector is used for move-list, string saving/recovery
         seems to be seamless."
  (conj move-list [from-x from-y to-x to-y promote]))

(defn move-list-to-string
  "Quick utility method for converting a movelist format:
  [[fromX fromY toX toY promoted?]]
   to a string which can be directly stored on SQLite."
  [move-list]
  (clojure.string/join ";" (map #(clojure.string/join "," %) move-list)))

(defn stringify-move
  [from-x from-y to-x to-y promotes?]
  (str from-x "," from-y "," to-x "," to-y "," (if promotes? "y" "n")))


(defn add-move-record
  [from-x from-y to-x to-y promotes? game-string]
  (str game-string (stringify-move from-x from-y to-x to-y promotes?) ";"))


;; *************************************************************
;; Numerical translations
;; *************************************************************

(defn pos-to-x
  "Obtains a 1-9 x-coordinate from UI grid celll # (0-80)"
  [position]
  (+ 1 (mod position 9)))

(defn pos-to-y
  "Obtains a 1-9 y-coordinate from UI grid celll # (0-80)"
  [position]
  (- 9 (int (/ position 9))))

(defn xy-to-pos [x-coord y-coord]
  "Produces numerical position (0-80) of grid cell in a 9-column grid"
  (+ (* (- 9 y-coord) 9)
     (- x-coord 1)))

(defn cell-from-board
  "Returns the contents of a game board space from position in an array"
  [game position]
  (let [x-position (+ 1 (mod position 9))
        y-position (- 9 (int (/ position 9)))]
    (get-in game [:board x-position y-position])))









;; *************************************************************
;; Image-state builders (Deprecated)
;; *************************************************************

(defn list-from-game
  "DEPRECATED:
  Produces a displayable list for the GUI adapter from a gamestate.
  NOTE: This is the place to determine what data and type is being
  translated from gamestate to GUI."
  [game]
  (into (vector) (flatten (map (fn [row]
                                 (reverse (map (fn [col]
                                                 (str (first (get-in game
                                                                     [:board col
                                                                      row :type :name]))))
                                               (range 1 10))))
                               (range 1 10)))))

(defn img-list-from-game
  "DEPRECATED:
  Produces a displayable list for the GUI adapter from a gamestate.
  NOTE: This is the place to determine what data and type is being
  translated from gamestate to GUI."
  [game]
  (into (vector) (flatten (map (fn [row]
                                 (reverse (map (fn [col]
                                                 (if (nil? (get-in game [:board col row]))
                                                   R$drawable/blank
                                                   (get-in game
                                                         [:board col row :type :image])))
                                               (range 1 10))))
                               (range 1 10)))))

(defn test-highlighted-UI-vector
 "DEPRECATED:
  Produces an img-gamestate of the game contents, with the highlighted
  versions of those squares which comprise the possible move destinations,
  based only on grid-position of selected cell."
  [game origin-x origin-y]
    (into (vector)
        (flatten (map (fn [row]
                                 (reverse (map (fn [col]
                                        (if (nil? (get-in game [:board col row]))
                                          (if (highlighted? game origin-x origin-y col row)
                                            R$drawable/highlightempty
                                            R$drawable/blank)
                                          (if (highlighted? game origin-x origin-y col row)
                                            (get-in game
                                                    [:board col row :type
                                                     :image-highlighted])
                                            (get-in game
                                                    [:board col row :type :image]))))
                                      (range 1 10))))
                      (range 1 10)))))



(defn update-gamestate
  "DEPRECATED:
  DRAFT: Populates the gamesate atom with new data for the GUI adapter.
  Currently only puts in strings, but this should eventually become
  image resources."
  [state-atom game]
  (reset! state-atom
          (into (vector) (map (fn [row] (get-in game [:board 1 row :type :name]))
                              (range 1 10)))))


;; (defn make-string-adapter
;;   "DEPRECATED:
;;   DRAFT: Adapter with which to populate the onscreen board.  Currently
;;    just populating button text with strings, but this should
;;    eventually be images."
;;   [activity]
;;   (adap/ref-adapter
;;    (fn [_] [:button {:text "Butin"
;;                      :on-click (fn [_] (toast "Pressed tha buttin"))}])
;;    (fn [position view _ data]
;;      (.setText view (get @gamestate position)))
;;    gamestate))

(defn display-hl-pos [activity position]
  "DEPRECATED:
   Changes the image of the ID at position (btn-id-offset + position)
   to the highlight-empty image,"
  (on-ui (.setImageDrawable (.findViewById
                             (.findViewWithTag (activity/get-decor-view activity)
                                               "board-grid")
                             (+ 3000 position))
                            (res/get-drawable R$drawable/highlightempty))))

(defn query-test
  "DEPRECATED:
  Obtains all moves for a piece"
  [game piece-x piece-y]
  (reduce into (map
                (fn [[move-function num-spaces]]
                  (move-function (get game :board) num-spaces
                                 piece-x piece-y
                                 (get-in game [:board piece-x piece-y :owner])))
                (get-in game [:board piece-x piece-y :type :moves]))))

(defn display-moves [activity position]
  "DEPRECATED:
   Displays the highlight for available moves for a selected piece."
  (map (fn [[x-coord y-coord]]
         (display-hl-pos activity
                         (xy-to-pos x-coord y-coord)))
       (query-test (setup-board)
                        (pos-to-x position)
                        (pos-to-y position))))

;; *************************************************************
;; Global UI consts
;; *************************************************************
;;(def gamestate (atom (list-from-game (setup-board))))
(def img-gamestate (atom (img-list-from-game (setup-board))))
(def btn-height 135)
(def btn-width 125)
(def btn-id-offset 3000)
(def creation-position (ref 0))
(def hand-position (ref 0))
(def current-game (atom (setup-board)))
;; (def gamestate (atom ["1" "2" "3" "4" "5" "6" "7" "8" "9"]))
;; *************************************************************



;; *************************************************************
;; Image-resource handling:
;; *************************************************************
(defn get-image
  "Obtains a reference to the appropriate image for a space on the board,
  whether that's a piece or an empty square.  Two arities exist: either
  a basic display of the board, or with a piece selected so that move
  highlighting is taken into account."
  ([game x y]
   (if (nil? (get-in game [:board x y]))
     (res/get-drawable R$drawable/blank)
     (get-in game [:board x y :type
                   :image])))
  ([game x y highlight-x highlight-y]
   (if (highlighted? game highlight-x highlight-y x y)
                                        (if (nil? (get-in game [:board x y]))
                                          R$drawable/highlightempty
                                          (get-in game [:board x y :type
                                                        :image-highlighted]))
                                        (if (nil? (get-in game [:board x y]))
                                          R$drawable/blank
                                          (get-in game [:board x y :type
                                                        :image])))))

(defn get-hand-image
  "Obtains a reference to the image for a piece in a player's hand,
  based on position withing that player's hand."
  [game player hand-pos]
  (get-in game [player :hand hand-pos :type :image]))



;; *************************************************************
;; Android UI functionality:
;; *************************************************************

(defn display-new-gamestate
  "Placeholder to fix compilation order.  Actual function must be
   below make-imgbtn-adapter definition."
  ([activity game] ())
  ([activity game highlight-pos] ()))


(deftype OnClick [callback]
  android.content.DialogInterface$OnClickListener
  (onClick [this dialog which]
    (callback dialog which)))

(defn promote-dialog [activity game]
  (let [
        dialog
        (with-redefs [listeners/on-click-call (fn [x] (OnClick. x))]
          (-> (dlg/alert-dialog-builder
               activity
               {:message "Promote this piece?"
                :cancelable true
                :positive-text "Yes"
                :positive-callback (fn [dialog res] (toast "Pressed yes."))
                :negative-text "No"
                :negative-callback (fn [dialog res] (toast "Pressed no."))})
              .create))]
    (.show dialog)
    game))

;; (defn handle-promotion [activity game from-x from-y to-x to-y]
;; "Turn-taking logic for whether a piece is promoting or not, and the actual handling
;; of such promotion.  Forces promotion in must-promote situations, and asks the player
;; (via alert dialog) if a piece can, but does not have to, promote."
;;   (if (must-promote? game from-x from-y to-y)
;;     )

(defn take-turn-UI
  "DRAFT: Function to take the parsed version of a turn and conduct the
          logic of a turn on it.  If the parse says it's a drop, it
          drops the piece from the specified hand/hand-position
          [from-x, 0==drop from p1, from-y 0==drop from p2,
          the other becomes hand-position].
          If not, then it treats it as a move, with a capture as well if the
          destination is occupied.
   TODO: Add promotion execution."
  [activity game from-x from-y to-x to-y]
  (inc-turn
    (if (= from-x 0)
      ;;TODO: "IF legal-drop", else fail
      (drop-piece game -1 from-y to-x to-y)
      (if (= from-y 0)
        ;;TODO: "IF legal-drop", else fail
        (drop-piece game 1 from-y to-x to-y)
        (if (not (= (get-in game [:board to-x to-y]) nil))
          ;;TODO: "IF legal-capture", else fail
          (move-piece (capture-piece game to-x to-y)
                      from-x from-y to-x to-y)
          ;;TODO: "IF legal-move", else fail
          (move-piece game from-x from-y to-x to-y))))))

(defn make-imgbtn-adapter
  "DRAFT: Adapter with which to populate the onscreen board.
    Populating button images from the gamestate, and on click they 
    display highlights on the spaces available to move to."
  ([activity game]
  (adap/ref-adapter
   (fn [_] (let [position (- (dosync
                              (if (<= 81 @creation-position)
                                (ref-set creation-position 0))
                              (ref-set creation-position
                                       (inc @creation-position)))
                             1)
                 image-button (proxy [ImageButton] [activity])]
             (.setImageDrawable image-button (res/get-drawable
                                                (get-image game
                                                           (pos-to-x position)
                                                           (pos-to-y position))))
             (.setLayoutParams image-button
                               (proxy [android.widget.AbsListView$LayoutParams)
             (.setOnClickListener image-button
                                 (proxy [android.view.View$OnClickListener] []
                                   (onClick [v]
                                     (do
                     ;; (mapv (fn [pos] (display-hl-pos (*a) pos)) [0 8 72 80])
                                     (display-new-gamestate activity game position)
                                                     ))))
             (.setId image-button (+ btn-id-offset position))
     image-button))
   (fn [position view parent data] ())
   img-gamestate))
  ;; Alternate arity call to include highlighting on the board,
  ;; for when a piece has been selected to display it's potential moves.
  ([activity game highlight-pos]
  (adap/ref-adapter
   (fn [_] (let [position (- (dosync
                              (if (<= 81 @creation-position)
                                (ref-set creation-position 0))
                              (ref-set creation-position
                                       (inc @creation-position)))
                             1)
                 image-button (proxy [ImageButton] [activity])]
             (.setImageDrawable image-button (res/get-drawable
                                                (get-image game
                                                           (pos-to-x position)
                                                           (pos-to-y position)
                                                           (pos-to-x highlight-pos)
                                                           (pos-to-y highlight-pos))))
             (.setLayoutParams image-button
                               (proxy [android.widget.AbsListView$LayoutParams]
                                   [btn-width btn-height]))
             (.setOnClickListener image-button
                                 (proxy [android.view.View$OnClickListener] []
                                   (onClick [v]
                                     (do
                                       (if (is-legal?
                                            game
                                            (pos-to-x highlight-pos)
                                            (pos-to-y highlight-pos)
                                            (pos-to-x position)
                                            (pos-to-y position))
                                         (display-new-gamestate activity (take-turn
                                                                  game
                                                                  (pos-to-x highlight-pos)
                                                                  (pos-to-y highlight-pos)
                                                                  (pos-to-x position)
                                                                  (pos-to-y position)))
                                         (display-new-gamestate activity game))
                                                     ))))
             (.setId image-button (+ btn-id-offset position))
     image-button))
   (fn [position view parent data] ())
   img-gamestate)))

(defn make-hand-adapter
  "DRAFT: Adapter with which to populate the onscreen display of a player's
  captured pieces..
    Populating button images from the gamestate, and on click they
    display highlights on the spaces available to move to."
  ([activity game player]
  (adap/ref-adapter
   (fn [_] (let [handsize (count (get-in game [player :hand]))
                 position (- (dosync
                              (if (<= handsize @hand-position)
                                (ref-set hand-position 0))
                              (ref-set hand-position
                                       (inc @hand-position)))
                             1)
                 image-button (proxy [ImageButton] [activity])]
             (.setImageDrawable image-button (res/get-drawable
                                                (get-hand-image game
                                                                player
                                                                position)))
             (.setLayoutParams image-button
                               (proxy [android.widget.AbsListView$LayoutParams]
                                   [btn-width btn-height]))
             (.setOnClickListener image-button
                                 (proxy [android.view.View$OnClickListener] []
                                   (onClick [v]
                                     (do
                                       (toast "pressed")))))
     image-button))
   (fn [position view parent data] ())
   img-gamestate)))

(defn display-handstate
  "Utility funciton to display the contents of the specified player's hand (capture pieces),
  and when a piece is clicked to then allow its placement."
  [activity game player]
  (dosync
   (ref-set hand-position 0)
   (reset! current-game game)
   (on-ui (.setAdapter (.findViewWithTag (activity/get-decor-view activity)
                                         "board-grid")
                       (make-hand-adapter activity game player)))))


(defn display-new-gamestate
  "Utility function to display an arbitrary gamestate
   it by swapping the board UI adapter on the activity containing
   the game board."
  ([activity game]
  (dosync
   ;;(reset! img-atom (img-list-from-game game))
   (ref-set creation-position 0)
   (reset! current-game game)
   (on-ui (.setAdapter (.findViewWithTag (activity/get-decor-view activity)
                                           "board-grid")
                       (make-imgbtn-adapter activity game)))))
  ([activity game highlight-pos]
   (dosync
    (ref-set creation-position 0)
    (reset! current-game game)
    (on-ui (.setAdapter (.findViewWithTag (activity/get-decor-view activity)
                                          "board-grid")
                        (make-imgbtn-adapter activity game highlight-pos))))))


;; (on-ui (.setImageDrawable (.findViewById (.findViewWithTag (activity/get-decor-view (*a)) "board-grid") 3005) (res/get-drawable R$drawable/highlightempty)))

;; (.findViewById (.findViewWithTag (activity/get-decor-view (*a)) "board-grid") 3001)


;; *************************************************************
;; Google Login:
;; *************************************************************

(def login-btn-width 675)
(def login-btn-height 175)

(defn handleSignInResult [activity result]
    (if (.isSuccess result)
      (do
        (toast (str "Handling signin success" (.getDisplayName (.getSignInAccount result))))
        (on-ui (.setText (find-view activity ::mStatus-textview-id) "Logged in " )))
      (toast "Not success handling SignInResult")))



;;                      (if (.isSuccess result)
;;                        (.setText (find-view (*a) ::mStatus-textview-id)
;;                                  (.getString (.getDisplayName
;;                                                                (.geSignInAccount result))))))

(defactivity agendine.draft.shogiandroid.LoginActivity
  ;;Activity for using Google OAUTH2 login
  :key :login-activity
  :extends android.support.v4.app.FragmentActivity
  :implements [
               com.google.android.gms.common.api.GoogleApiClient$OnConnectionFailedListener
               android.view.View$OnClickListener]
  (onCreate [this bundle]
            (.superOnCreate this bundle)
            (neko.debug/keep-screen-on this)
            (on-ui
             (def rc-signin 9001) ;;magic number for return code
             (def gso (.build
                       ;; (.requestIdToken (.getString R$string/server_client_id)
                       (.requestEmail
                        (new GoogleSignInOptions$Builder GoogleSignInOptions/DEFAULT_SIGN_IN))))
             (def mGoogleApiClient (.build (.addApi (.enableAutoManage
                                                     (new GoogleApiClient$Builder this)
                                                     this this)
                                                    Auth/GOOGLE_SIGN_IN_API gso)))
             (def login-img (proxy [ImageButton] [this]))
             (set-content-view! this
                                [:linear-layout {:background-drawable
                                                 (res/get-drawable R$drawable/tatamimat)
                                                 :gravity :center
                                                 :orientation :vertical
                                                 :id ::login-screen-id
                                                 :tag "login-screen"}
                                 [:text-view {:id ::mStatus-textview-id
                                              :text "Not Signed In"}]
                                 [:button {:id ::logout-button-id
                                           :text "Sign Out"
                                           :on-click (fn [w]
                                                       (.setResultCallback
                                                        (.signOut Auth/GoogleSignInApi
                                                                  mGoogleApiClient)
                                                        (proxy [ResultCallback] []
                                                          (onResult [status]
                                                            (.setText (find-view
                                                                       (*a)
                                                                       ::mStatus-textview-id)
                                                                      "Signed Out")))
                                                            ))
                                                        }]])

               (.setImageDrawable login-img (res/get-drawable R$drawable/gsignin))
               (.setOnClickListener login-img
                                    (proxy [android.view.View$OnClickListener] []
                                      (onClick [v]
                                          (.startActivityForResult (*a)
                                                          (.getSignInIntent Auth/GoogleSignInApi
                                                                            mGoogleApiClient)
                                                          rc-signin))))
               (.setLayoutParams login-img
                                 (proxy [android.widget.AbsListView$LayoutParams]
                                     [login-btn-width login-btn-height]))
               (.addView (find-view this ::login-screen-id) login-img)
               ))
  (onConnectionFailed [connectionResult]
                      (log/d "onConnectionFailed: " connectionResult))
  (onStart [this]
           (debug/safe-for-ui
            (.superOnStart this)
            (def opr ^OptionalPendingResult (.silentSignIn Auth/GoogleSignInApi mGoogleApiClient))
            (if (.isDone opr)
              (do
                (log/d "Got cached signin.")
                (handleSignInResult this (.get opr)))
              (.setResultCallback opr
                                 (proxy [ResultCallback] []
                                   (onResult [googleSignInResult]
                                     (handleSignInResult (*a) googleSignInResult)))))))
  (onActivityResult
   [who request-code result-code data]
   (if (= request-code rc-signin)
     (let [result (.getSignInResultFromIntent Auth/GoogleSignInApi data)]
       (handleSignInResult (*a) result))))
  ;;  (revokeAccess)
           )

(defactivity agendine.draft.shogiandroid.GameActivity
  :key :game-activity
  (onCreate [this bundle]
            (.superOnCreate this bundle)
            (neko.debug/keep-screen-on this)
            (on-ui
             (set-content-view! (*a)
                                [:relative-layout {:background-drawable
                                                 (res/get-drawable R$drawable/tatamimat)
                                                 :gravity :center
                                                 :tag "main-screen"}

                                 [:button {:id ::hand1-id
                                           :layout-align-right ::board-id
                                           :text "Hand 1"
                                           :on-click (fn [_]
                                                         (display-handstate this
                                                                            @current-game
                                                                            1))}]
                                 [:button {:id ::back-to-board-id
                                           :layout-align-left ::board-id
                                           :text "Board"
                                           :on-click (fn [_]
                                                       (display-new-gamestate this
                                                                              @current-game))}]
                                 [:grid-view {:layout-below ::hand1-id
                                              :id ::board-id
                                              :num-columns 9
                                              :tag "board-grid"}]
                                 [:button {:layout-below ::board-id
                                           :layout-align-left ::board-id
                                           :id ::hand2-id
                                           :text "Hand 2"
                                           :on-click (fn [_]
                                                         (display-handstate this
                                                                                @current-game -1))}]
                                 ])
             (display-new-gamestate this @current-game))))

(defn start-login [activity]
  (let [login-intent (android.content.Intent. activity agendine.draft.shogiandroid.LoginActivity)]
    (.startActivity activity login-intent)))

(defn start-game [activity]
  (let [game-intent (android.content.Intent. activity agendine.draft.shogiandroid.GameActivity)]
    (.startActivity activity game-intent)))


(defactivity agendine.draft.shogiandroid.MainActivity
  :key :main
  (onCreate [this bundle]
            (.superOnCreate this bundle)
            (neko.debug/keep-screen-on this)
            (on-ui
             ;; (let [
             ;;       newgame-button (proxy [ImageButton] [this])
             ;;       resumegame-button (proxy [ImageButton] [this])
             ;;       login-button (proxy [ImageButton] [this])
             ;;       ]
             ;;   (.setBackgroundResource ... ...)
             ;;   (.setOnClockListenet ... ...)
             ;;   )
                ;;^put this paren after (set-content-view! etc)
             (set-content-view! (*a)
                                [:linear-layout {:background-drawable
                                                   (res/get-drawable R$drawable/tatamimat)
                                                 :orientation :vertical
                                                 :gravity :center
                                                 :id ::main-screen-id
                                                 :tag "main-screen"}
                                 [:image-view {:id ::main-logo-id
                                               :image-drawable (res/get-drawable
                                                                R$drawable/icon1small)}]
                                 [:text-view {:text "Shogi Connect!"
                                              :id ::welcome-msg-id}]
                                 [:button {
                                           :id ::newgame-button
                                           :text "New Game"
                                           :on-click (fn [_]
                                                       (do
                                                         (reset! current-game (setup-board))
                                                         (start-game this)))}]
                                 [:button {
                                           :id ::resumegame-button
                                           :text "Resume Game"
                                           :on-click (fn [_]
                                                       (start-game this))}]
                                 [:button {
                                           :id ::login-button-id
                                           :text "Login"
                                           :on-click (fn [_]
                                                       (start-login this))}]
                                 ])
             (on-ui  (.setTextSize  (find-view (*a) ::welcome-msg-id) 20))
             ;; (.setImageDrawable  (find-view (*a) ::main-logo-id)
             ;;                     (res/get-drawable R$drawable/icon1small))
                                )
            ))

