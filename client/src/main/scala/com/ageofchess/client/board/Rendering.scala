package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.gamestate.ClientGame
import org.scalajs.dom

import upickle.default._ // remove later
import java.awt.Event

class GameStateRenderer(val gameState: ClientGame) {
  def render(
    boardState: Vector[Vector[(SquareType, Option[Piece])]]
  ): HtmlElement = {
    val numColumns = boardState.headOption.map(_.size).getOrElse(0)

    div(
      cls := "board",
      styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
      boardState.zipWithIndex.map { case (row, rIdx) =>
        div(
          cls := "board-row",
          row.zipWithIndex.map { case ((square, piece), cIdx) =>
            val squareColor = if ((rIdx + cIdx) % 2 == 0) Grass else Dirt
            val renderableSquare = RenderableSquare(squareColor, square)
            renderSquare(Location(rIdx, cIdx), renderableSquare, piece)  
          }
          // onMountCallback { ctx =>
          //   gameState.connection.socket.onmessage = event => {
          //     val message: GameMessage = read[GameMessage](event.data.toString)
          //     val playerToMove = gameState.playerToMoveSignal.observe(ctx.owner)
          //     println(s"Received game message: $message")
          //     message match {
          //       case UpdatePieces(nextActivePlayer, pieces) => {
          //         println("Updating board state")
          //         gameState.piecesVar.set(pieces)
          //         println("playerToMove: " + playerToMove.now())
          //         if (nextActivePlayer == gameState.player && playerToMove.now() != gameState.player) {
          //           println("changing active player")
          //           gameState.moveTurnBus.emit()
          //         }
          //       }
          //       case _ =>    
          //     }
          //   }
          // }
        )  
      },
      onMountCallback { ctx =>
        gameState.connection.socket.onmessage = event => {
          val message: GameMessage = read[GameMessage](event.data.toString)
          val playerToMove = gameState.playerToMoveSignal.observe(ctx.owner)
          println(s"Received game message: $message")
          message match {
            case UpdatePieces(nextActivePlayer, pieces) => {
              println("Updating board state")
              gameState.piecesVar.set(pieces)
              println("playerToMove: " + playerToMove.now())
              if (nextActivePlayer == gameState.player && playerToMove.now() != gameState.player) {
                println("changing active player")
                gameState.moveTurnBus.emit()
              }
            }
            case _ =>    
          }
        }
      }
    )
  }

  val playerDragBus = new EventBus[(dom.DragEvent, Location)]
  val playerDragEvents = playerDragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)
  val playerDragEffects = Observer[(dom.DragEvent, Location, Boolean)](onNext = { case (e, loc, canMove) =>
    println(e, loc, canMove)
    if (canMove) {
      gameState.selectedPiece.set(Some(loc))
    } else {
      e.preventDefault()
    }
  })

  val playerClickBus = new EventBus[(dom.MouseEvent, Location)]
  val playerClickEvents = playerClickBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)
  val playerClickEffects = Observer[(dom.MouseEvent, Location, Boolean)](onNext = { case (e, loc, canMove) =>
    println(e, loc, canMove)
    if (canMove) {
      gameState.selectedPiece.set(Some(loc))
    } else {
      e.preventDefault()
    }
  })

  val clickBus = new EventBus[(dom.MouseEvent, Location)]
  val clickEvents = clickBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)
  val clickEffects = Observer[(dom.MouseEvent, Location, Boolean)](onNext = { case (e, loc, canMove) =>
    if (canMove) {
      println(gameState.selectedPiece.now())
      gameState.selectedPiece.now() match {
        case Some(position) => drop(position, loc)
        case None => gameState.selectedPiece.set(Some(loc))
      }
    } else {
      // Perhaps should also set the current piece to None
      e.preventDefault()
    }
  })

  def renderSquare(
    location: Location,
    square: RenderableSquare,
    piece: Option[Piece]
  ): HtmlElement = {

    //  TODO: DragEvent is used for both start and end of drag.  We can potentially make this more specific to our application
    // and make it EventBus[Location]
    // val dragBus = new EventBus[dom.DragEvent]
    // val dragEvents = dragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)
    // val dragEffects = Observer[(dom.DragEvent, Boolean)](onNext = { case (e, canMove) =>
    //   println(dragBus)
    //   if (canMove) {
    //     gameState.selectedPiece.set(Some(location))
    //     // gameState.moveTurnBus.emit()
    //   } else {
    //     e.preventDefault
    //   }
    // })

    div(
      cls := "board-square",
      backgroundImage := s"url(/assets/${square.asset})",
      piece.map { p =>
        img(
          src := s"/assets/pieces/${p.asset}",
          cls := "piece",
          // TODO: this doesn't work remotely as intended, need to read up on animations more
          // styleAttr := s"transform: translate(${location.y * 50}px, ${location.x}px);",
          // onDragStart --> dragBus.writer,
          // dragEvents --> dragEffects
          onDragStart.map(e => e -> location) --> playerDragBus.writer,
          playerDragEvents --> playerDragEffects
          // onDragStart --> { _ =>
          //   gameState.selectedPiece.set(Some(location))
          // }
          // also works instead of the dragBus - look into this more
          // onDragStart.compose(_.withCurrentValueOf(gameState.isPlayerTurnSignal).collect {
          //   case (e, true) => e
          // }) --> { _ =>
          //   gameState.selectedPiece.set(Some(location))
          // }
        )
      },
      // onClick --> { movePieceOnClick(location, piece, piecesVar) },
      onClick.map { e =>
        println("Clicked on: " + location)
        e -> location
      } --> clickBus.writer,
      clickEvents --> clickEffects,
      onDragOver.preventDefault --> { _ => },
      onDrop.preventDefault --> { _ =>
        gameState.selectedPiece.now().foreach { position =>
          drop(position, location)
        }
        // gameState.selectedPiece.now() match {
        //   case Some((selectedPosition)) if selectedPosition != location => {
        //     gameState.connection.socket.send(write(MovePiece(gameState.player, selectedPosition, location)))
        //     piecesVar.update { pieces =>
        //       pieces.get(selectedPosition).map { pieceToMove =>
        //         pieces - selectedPosition + (location -> pieceToMove)  
        //       }.getOrElse(pieces)
        //     }
        //     gameState.selectedPiece.set(None)
        //     gameState.moveTurnBus.emit()
        //   }
        //   case Some(_) => gameState.selectedPiece.set(None)
        //   case None => 
        // }
      }
      // onMountCallback { ctx =>
      //   gameState.connection.socket.onmessage = event => {
      //     val message: GameMessage = read[GameMessage](event.data.toString)
      //     val playerToMove = gameState.playerToMoveSignal.observe(ctx.owner)
      //     println(s"Received game message: $message")
      //     message match {
      //       case UpdatePieces(nextActivePlayer, pieces) => {
      //         println("Updating board state")
      //         gameState.piecesVar.set(pieces)
      //         println("playerToMove: " + playerToMove.now())
      //         if (nextActivePlayer == gameState.player && playerToMove.now() != gameState.player) {
      //           println("changing active player")
      //           gameState.moveTurnBus.emit()
      //         }
      //       }
      //       case _ =>    
      //     }
      //   }
      // }
    )
  }

  // def dropPiece(
  //   location: Location
  // ): Unit = {
  //   println("moving piece")
  //   gameState.selectedPiece.now() match {
  //     case Some(selectedPosition) if selectedPosition != location => {
  //       gameState.piecesVar.update { pieces =>
  //         pieces.get(selectedPosition).map { pieceToMove =>
  //           pieces - selectedPosition + (location -> pieceToMove)
  //         }.getOrElse(pieces)
  //       }
  //       gameState.connection.socket.send(write(MovePiece(gameState.player, selectedPosition, location)))
  //       gameState.selectedPiece.set(None)
  //       gameState.moveTurnBus.emit()
  //     }
  //     case Some(_) => gameState.selectedPiece.set(None)
  //     case None =>
  //   }
  // }

  def drop(
    originalPosition: Location,
    newPosition: Location
  ): Unit = {
    if (newPosition != originalPosition) {
      gameState.piecesVar.update { pieces =>
        pieces.get(originalPosition).map { pieceToMove =>
          pieces - originalPosition + (newPosition -> pieceToMove)
        }.getOrElse(pieces)
      }
      gameState.connection.socket.send(write(MovePiece(gameState.player, originalPosition, newPosition)))
      gameState.selectedPiece.set(None)
      gameState.moveTurnBus.emit()
    } 
    else {
      gameState.selectedPiece.set(None)
    }
  }

  def movePieceOnClick(
    location: Location,
    piece: Option[Piece],
    piecesVar: Var[Map[Location, Piece]]
  ): dom.Event => Unit = { _ =>
    gameState.selectedPiece.now() match {
      case Some(selectedPosition) if selectedPosition != location => {
        piecesVar.update { pieces =>
          pieces.get(selectedPosition).map { pieceToMove =>
            pieces - selectedPosition + (location -> pieceToMove)
          }.getOrElse(pieces) 
        }
        gameState.connection.socket.send(write(MovePiece(gameState.player, selectedPosition, location)))
        gameState.selectedPiece.set(None)
      }
      case Some(_) => gameState.selectedPiece.set(None)
      case None => piece match {
        case Some(_) => gameState.selectedPiece.set(Some(location))
        case None =>
      }
    }

  }
}
