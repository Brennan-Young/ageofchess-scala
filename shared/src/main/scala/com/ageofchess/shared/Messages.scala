package com.ageofchess.shared

import upickle.default.{ReadWriter, readwriter, macroRW}

object Messages {
  sealed trait MessageType
  case object InitBoard extends MessageType

  case class Message(
    messageType: MessageType
  )
}
