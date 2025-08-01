package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import scala.concurrent.duration._

object Clock {
  def clockDisplay(
    clockVar: Var[FiniteDuration],
    isActiveClockSignal: Signal[Boolean]
  ): Div = {

    div(
      cls := "clock-display",
      cls.toggle("active") <-- isActiveClockSignal,
      onMountCallback { ctx =>
        EventStream
          .periodic(1000)
          .withCurrentValueOf(isActiveClockSignal)
          .foreach { case (_, isActiveClock) =>
            if (isActiveClock) clockVar.update(d => if (d > Duration.Zero) d - 1.second else d)
          }(ctx.owner)
      },
      child.text <-- clockVar.signal.map { duration =>
        val minutes = duration.toMinutes
        val seconds = duration.toSeconds % 60
        f"$minutes%02d:$seconds%02d"
      }
    )
  }
}
