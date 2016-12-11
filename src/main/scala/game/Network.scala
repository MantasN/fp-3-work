package game

import models.Types.Moves
import com.stackmob.newman.ApacheHttpClient
import com.stackmob.newman.response.{HttpResponse, HttpResponseCode}
import com.stackmob.newman.dsl._

import scala.concurrent.Await
import scala.concurrent.duration._
import java.net.URL

import parsing.MovesParser

import scalaz.{Failure, Success}

object Network {
  private val client = new ApacheHttpClient()

  def getMoves(gameId: String, playerId: Int): Option[Moves] = {
    val response = get(gameUrl(gameId, playerId))

    println(s"getMoves status = ${response.code}")

    if (response.code != HttpResponseCode.Ok)
      return None

//    response.bodyAsCaseClass[Moves] match {
//      case Success(moves) => Some(moves)
//      case Failure(_) => None
//    }

    MovesParser.jsonToMoves(response.bodyString)
  }

  def sendMoves(gameId: String, playerId: Int, moves: Moves): Unit = {
//    val response = post(gameUrl(gameId, playerId), moves)
    val response = post(gameUrl(gameId, playerId), MovesParser.movesToJson(moves))
    println(s"sendMoves status = ${response.code}")
  }

  private def get(url: URL): HttpResponse = {
    val request = GET(url)(client).setHeaders(("Accept", "application/json")).apply
    Await.result(request, Duration.Inf)
  }

//  private def post(url: URL, body: Moves) = {
//    val request = POST(url)(client).setHeaders(("Content-Type", "application/json")).setBody(body).apply
  private def post(url: URL, body: String) = {
    val request = POST(url)(client).setHeaders(("Content-Type", "application/json")).setBodyString(body).apply
    Await.result(request, Duration.Inf)
  }

  private def gameUrl(gameId: String, playerId: Int) = {
    new URL(s"http://tictactoe.homedir.eu/game/$gameId/player/$playerId")
  }
}