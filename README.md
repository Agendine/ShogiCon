# shogiandroid

All-clojure asynchronous, internet-enabled mobile Shogi.

*****************************************************************************************
*****************************************************************************************

This is the draft file for the logic of a Shogi game, intended as a phone app allowing
asynchronous play over the internet, using push notification.  Early draft does not
include single-player-versus-ai, so all moves are input by a player.  The goal is
for the code to be as streamlined and un-redundant as possible, and to allow simple
verification, feature implementation/extensibility, error-checking, and for the code to
be easily understandable.  This is achieved by following a thoroughly-planned and minimalistic
architecture, and thorough documentation.

Though the program is being initially created for Android due to my own familiarity,      
it is designed to be as easy to implement on other devices as possible,
largely by relying only on services to which every phone OS has a parallel, and by
using the server to handle as much as possible. Lein Droid is being used for
the actual Android app.

*****************************************************************************************
*****************************************************************************************


**************************************************

The phone side of things contains:

**************************************************

      The game logic,
      UI and options,
      SQLite database of current games (encoded as plain strings for easy parse and transmission)
      and communication with the server.
      **possibly later:**
           AI to play against


 **************************************************

 The server side handles:

 **************************************************

      User accounts (VERY simple.  NO PRIVATE INFORMATION IS STORED.  Just:
                     a username,
                     a unique id from the OAUTH service (eg Google)
                     a way to send push notifications,
                     a running game list (**possible game history?**) so that games
                              can be recovered after switching devices),
                     Ranked win/loss record and Rating,
                     a friend list

      matchmaking (simple ranked or unranked player pools.  A player can simply request to
                   be listed as available for a match, or browse the list of available
                   Players to challenge)
      notifications to the other player (via Push notificaitons through Google/Apple services),      
      user verification (via Google/Apple account authentication, since those should be
                         available for any user)
      Game history of each running game (used to verify that no illegal
                    tampering gets sent as a move, to prevent cheating).


**************************************************
* Organization:
**************************************************


A game state is defined purely by the board

Board:  A Board is:
         A 9x9 matric of "Spaces", each of which can either be "Empty" or contain a "Piece".
               The board is laid out as follows:;
                 Row
                  9
                  8                Note that the coordinates are 1-based (for easy
                  .                 transcription/math) and labelled XY-style, increasing
                  .                 bottom-to-top and left-to-right.
                  .
                  1  Col 1 2 3 ... 9


         2 "Players",
         2 "Hands", one for each player (a set of captured
         Pieces, playable as per the rules).
                Thus, "Locations" which can contain Pieces
                includes both an (x,y) grid location or "Player 1's Hand" or
                "Player 2's Hand".  A Piece, itself, thus doesn't have to store
                it's own location.
         A Current Turn variable, for who's turn it is.

 Piece: a Piece has:
        an Owner (Player 1 or Player 2),
        a Type (King, Rook, Bishop, Gold General, Silver General, Knight, Lance, Pawn,
                Promoted Rook, Promoted Bishop, Promoted Silver General, Promoted Knight,
                Promoted Lance, or Promoted Pawn),

 Type: a Type has:
         List of Movement capabilities (functions and parameters),
        "Is Promotable?" value (boolean),
        "Promotion" value (a Type), either what it promotes to (if Promotable),
                  what it promotes from (if already promoted, as these can revert if captured),
                  or nil if King or Gold General.
         Name (a simple keyword),
         Graphic (a way to call the appropriate image file)

 Player: a Player has:
         Direction (multiplier, +1 or -1), to be applied to their piece's movement directions.
                    This is also used to represent who goes first, and to link to that player's
                    "Hand".
         Send Move Function, to be called when a move is confirmed to submit to the other player.
                    This allows for moves to be "sent" to a local game (on same device), or
                    to the Server for remote player.
         **In Future**:
                Move Function, to allow for AI players to be represented by the same
                basic implementation as Human players, with a single "over-ridden" function.



*****************************************************************************************
Author: Agendine (Matthew Kroen)
*****************************************************************************************

Distributed under the Eclipse Public License, the same as Clojure.
