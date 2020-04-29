package wappugame

import info.mukel.telegrambot4s._

import api._
import methods.{SendMessage, _}
import models.{InlineKeyboardButton, InlineKeyboardMarkup, _}
import declarative._
import collection.mutable.Buffer
import util.Random

object GameApp extends App {
   val bot = new GameBot()
   bot.run()
   println("Started")
}

class GameBot extends BasicBot {
  
     val games = Buffer[Game]()

     def messageUid(msg: Message) = msg.chat.id.toString+msg.messageId.toString

     def welcomeMessage(msg: Message) = {
       "welcomeMessage"
     }

     def helpMessage(msg: Message) = {
       "helpMessage"
     }


     def errorReply(chat: Chat, content: String)={
       request(SendMessage(ChatId.fromChat(chat.id), content + "errorReply", parseMode = Some(ParseMode.HTML)))
     }

     // Command keyword definitions
     this.command("start", welcomeMessage)
     this.command("help", helpMessage)

     
     onCommand("create") { implicit msg =>
       leaveGame(msg.chat)
       val game = new Game(msg)
       games += game
       request(SendMessage(ChatId.fromChat(msg.chat.id), "Game created with code " + game.code ))
     }
     
     
     onCommand("join") { implicit msg =>
       var response = "Type the 5-character code after the command to join a lobby"
       val code = msg.text.getOrElse("").drop(6).toUpperCase()
       
       if(code.length==5){
         val game = findGameByCode(code)
         if(game==None){
           response = "No lobby could be found with the code "+code 
         } else if (game == currentGame(msg.chat)){
           response = "You are already in this game"
         }else{
           announce(game.get, msg.from.get.firstName + " joined the game")
           game.get.join(msg)
           response = "Joined the game"
         }
       }
       request(SendMessage(ChatId.fromChat(msg.chat.id), response))
     }
     
     onCommand("leave") { implicit msg =>
       var response = ""
       val game = currentGame(msg.chat)
       if(game == None){
         response = "You aren't currently in a game"
       } else if(leaveGame(msg.chat)) {
         response = "You left the game"
         announce(game.get, msg.from.get.firstName + " left the game")
         if(game.get.playerCount == 0){
           games -= game.get
         } else if(msg.chat == game.get.hostChat) {
           game.get.hostChat = game.get.players.head.chat
           request(SendMessage(ChatId.fromChat(game.get.hostChat.id), "The host left the game. You are the new host"))
         }
       } else {
         response = "Failed to leave the game. This is an error."
       }
       request(SendMessage(ChatId.fromChat(msg.chat.id), response))
     }
     
     onCommand("info") { implicit msg =>
       request(SendMessage(ChatId.fromChat(msg.chat.id), "info"))
     }
     
     onCommand("play") { implicit msg =>
       println(games)
       println(games.map(_.players.map(_.name)))
       val game = currentGame(msg.chat)
       if(game == None){
         request(SendMessage(ChatId.fromChat(msg.chat.id), "You are not in a game"))
       } else if(game.get.hostChat != msg.chat){
         request(SendMessage(ChatId.fromChat(msg.chat.id), "Only the host can start the game"))
       } else if(game.get.hasStarted == true){
         request(SendMessage(ChatId.fromChat(msg.chat.id), "The game has already been started"))
       } else if(game.get.playerCount < 2){
         request(SendMessage(ChatId.fromChat(msg.chat.id), "At least 2 players are needed to start the game"))
       } else {
         
         // Game init
         
         val g = game.get
         
         g.started = true
         g.turnIndex = Random.nextInt(g.playerCount)
         announce(g, "Starting game. " + g.currentPlayer.get.name + " starts!")
         
         g.prepareCards()
         
         ???
         
       }
     }
     
     def notifyPlayer() = {
       ???
     }
     
     def announce(game: Game, s: String) = game.playerChats.foreach(i => request(SendMessage(ChatId.fromChat(i.id), s)))
     
     /**
      *  Finds and returns the Game by the invite code.
      *  Searches only games that haven't started yet.
      */
     def findGameByCode(code: String): Option[Game] = games.filter(!_.hasStarted).find(_.code==code)

     
     /** Finds the Game the chat is currently part of.
      *  Doesn't include games that have ended.
      */
     def currentGame(chat: Chat): Option[Game] = games.filter(!_.hasEnded).find(_.playerChats.contains(chat))
     
     /**
      * Removes the chat from an existing game.
      * Returns true if the player is removed from a game.
      */
     def leaveGame(chat: Chat) = {
       val game = currentGame(chat)
       if (game != None){
         game.get.leave(chat)
       } else {
         false
       }
     }

     onCallbackQuery { implicit cbq =>
        val data      = cbq.data.get
        val message   = cbq.message.get
        val chatId    = ChatId.fromChat(message.chat.id)
        val messageId = message.messageId

        ???
    }
    

}

class Game(val creationMessage: Message){
  val initialHandSize = 7
  val players = Buffer[Player](new Player(creationMessage.chat, creationMessage.from.get.firstName))
  var hostChat = creationMessage.chat
  def playerCount = players.size
  def playerChats = players.filter(!_.bot).map(_.chat) // List of chats of human players
  
  val code = Random.alphanumeric.take(5).mkString.toUpperCase
  
  var started = false
  
  def findPlayer(chat: Chat) = {
    players.find(_.chat==chat)
  }
  
  def hasStarted = started
  def hasEnded = hasStarted && players.count(_.alive) <= 1
  
  def join(msg: Message) = {
    if(hasStarted){
      false
    } else {
      
      players += new Player(msg.chat, msg.from.get.firstName)
      true
    }
  }
  
  /**
   * Removes the chosen chat from the game. Return true if the function works.
   */
  def leave(chat: Chat) = {
    val player = findPlayer(chat)
    if (player == None){
      false
    } else {
      if(hasEnded){ // This one should never happen
        println("Tried to leave a game that has ended")
        false
      } else if (hasStarted) {
        player.get.bot = true
        true
      } else {
        players -= player.get
        true
      }
    }
  }
  
  /**
   * Is the game currently underway
   */
  def isOn = hasStarted && !hasEnded
  
  var turnIndex = 0
  def currentPlayer = {
    if(isOn){
      Some(players(turnIndex))
    } else {
      None
    }
  }
  
  var previousCard: Option[String] = None
  var wasNoped = false
  var inAttack = false
  
  /** Advances currentPlayer */ 
  def nextPlayer() = {
    turnIndex = (turnIndex + 1) % playerCount
  }
  
  val deck = Buffer[String]()  // Cards are added when the game starts
  
  /**
   * Removes the topmost card from the deck and returns its value
   */
  def draw() = {
    if (deck.size > 0){
      val card = deck.head
      deck.remove(0)
      Some(card)
    } else {
      None
    }
  }
  
  def prepareCards(): Unit = {
    deck.clear()
    var newDeck = Buffer[String]()
    
    // Let's support huge games by duplicating these if we have a huge amount of players
    for(n <- 0 until (1 + (playerCount / 6))){
      for(i <- 0 until 4) newDeck.append("attack")
      for(i <- 0 until 4) newDeck.append("favor")
      for(i <- 0 until 5) newDeck.append("nope")
      for(i <- 0 until 5) newDeck.append("future")
      for(i <- 0 until 4) newDeck.append("shuffle")    
      for(i <- 0 until 4) newDeck.append("skip")
      for(j <- 0 until 5; i <- 0 until 4) newDeck.append("critter" + i)
    }
    
    newDeck = Random.shuffle(newDeck)
    
    players.foreach(p => {
      p.hand ++= newDeck.take(initialHandSize)
      newDeck = newDeck.drop(initialHandSize)
      p.hand += "defuse"
      println(p.name, p.hand)
    })
    
    
    for(i <- 0 until 7-playerCount) newDeck.append("defuse")
    for(i <- 0 until playerCount-1) newDeck.append("bomb")
    deck ++= Random.shuffle(newDeck)
  }
  
  
}

class Player(val chat: Chat, val name: String){
  val hand = Buffer[String]()
  var alive = true
  var bot = false
}
