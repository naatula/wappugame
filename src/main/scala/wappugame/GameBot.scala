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
       """Join a game by using /join [code]
/help to see the rules
/create to start a new game
/commands lists all commands
         """
     }

     def helpMessage(msg: Message) = {
       """The goal of the game is to avoid drawing bombs from the shared deck. You may play as many cards as you like during your turn, unless the card ends your turn (skip, attack). Drawing a card from the deck ends your turn. You don't have to play any cards. If you draw a bomb, you'll either use a defuse and return the bomb in a random position in the deck, or lose the game.

The attack card forces the next player to draw twice
The skip card skips your turn
The shuffle card shuffles the deck
The future card tells you the three topmost cards of the deck
The favor card steals a random card from a random player
The nope card cancels the action of the card immediately before it. Can be used also during other players' turns

Use /commands to list all commands"""
     }
     
     def commandsMessage(msg: Message) = {
       """<b>Game management:</b>
/join [code] - Join a game with an invite code
/leave - Leaves the current game
/create - Create a new game
/play - Starts the game (host only)
 
<b>In-game:</b>
/players - Show the player list with card amounts
/cards - View information about your hand and the deck
/draw - Draws a card and ends your turn

<b>Use cards:</b>
/attack
/skip
/shuffle
/future
/favor
/nope <i>(also when it's not your turn)</i>

/help"""
      
     }
     
     val cardNames = collection.immutable.Map("bomb"->"a bomb", "defuse"->"a defuse", "attack"->"an attack", "skip"->"a skip", "nope"->"a nope", "shuffle"->"a shuffle", "future"->"a future", "favor"->"a favor", "critter0"->"a C1", "critter1"->"a C2", "critter2"->"a C3", "critter3"->"a C4", "critter4"->"a C5")

     def errorReply(chat: Chat, content: String)={
       send(ChatId.fromChat(chat.id), content + "errorReply")
     }
     
    private def shortNaming(m: Message) = {
    val t = m.chat.title
    if(t==None) m.from.get.firstName else t.get
    }

     // Command keyword definitions
     this.command("start", welcomeMessage)
     this.command("help", helpMessage)
     this.command("commands", commandsMessage)
     
     
     onCommand("create") { implicit msg =>
       leaveGame(msg.chat)
       val game = new Game(msg)
       games += game
       send(ChatId.fromChat(msg.chat.id), "New game created! Start the game with /play once everyone has joined using the following command: <pre>/join " + game.code + "</pre>" )
     }
     
     
     onCommand("join") { implicit msg =>
       var response = "Type the 5-character code after the command to join a lobby"
       val code = msg.text.getOrElse("").drop(6).toUpperCase
       
       if(code.length==5){
         val game = findGameByCode(code)
         if(game==None){
           response = "No lobby could be found with the code "+code 
         } else if (game == currentGame(msg.chat)){
           response = "You are already in this game"
         }else{
           leaveGame(msg.chat)
           announce(game.get, shortNaming(msg) + " joined the game")
           game.get.join(msg)
           response = "Joined the game\n\nPlayer list:"
           game.get.players.foreach(i=>{ response += "\n" + i.name })
         }
       }
       send(ChatId.fromChat(msg.chat.id), response)
     }
     
     onCommand("leave") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game == None){
         send(ChatId.fromChat(msg.chat.id), "You aren't currently in a game")
       } else {
         if(leaveGame(msg.chat)) send(ChatId.fromChat(msg.chat.id), "You left the game")
       }
       
     }
     
     onCommand("players") { implicit msg =>
       var response = "Player list:"
       val game = currentGame(msg.chat)
       if(game == None) {
         response = "You are not in a game"
       } else {
         game.get.players.foreach(i=>{
           if(game.get.isCurrentPlayer(i.chat)){
             response += "<b>"
           }
           response += "\n" + i.name
           if(i.alive && game.get.hasStarted){
             response += " [" + i.hand.size + "]"
           } else if(!i.alive) {
             response += " ☠️"
           }
           if(game.get.isCurrentPlayer(i.chat)){
             response += "</b>"
           }
         }  
        
         )
         
       }
       send(ChatId.fromChat(msg.chat.id), response)
     }
     
     onCommand("cards") { implicit msg =>
       var response = "Your hand:"
       val game = currentGame(msg.chat)
       if(game == None) {
         response = "You are not in a game"
       } else {
         response += game.get.findPlayer(msg.chat).get.handString
         response += "\n\nCards in deck: " + game.get.deck.size
       }
       send(ChatId.fromChat(msg.chat.id), response )
     }
     
     onCommand("play") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game == None){
         send(ChatId.fromChat(msg.chat.id), "You are not in a game")
       } else if(game.get.hostChat != msg.chat){
         send(ChatId.fromChat(msg.chat.id), "Only the host can start the game")
       } else if(game.get.hasStarted == true){
         send(ChatId.fromChat(msg.chat.id), "The game has already been started")
       } else if(game.get.playerCount < 2){
         send(ChatId.fromChat(msg.chat.id), "At least 2 players are needed to start the game")
       } else {
         
         // Game init
         
         val g = game.get
         
         g.started = true
         g.turnIndex = Random.nextInt(g.playerCount)
         announce(g, "Starting game. " + g.currentPlayer.get.name + " starts!")
         
         g.prepareCards
         
         notifyPlayer(g)
         
       }
     }
     
     // Gameplay actions
     
     
     onCommand("draw") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         drawFromDeck(msg.chat, game.get)
       }
       
     }
     
     onCommand("attack") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         val g = game.get
         val p = g.currentPlayer.get
         if(!p.hand.contains("attack")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           p.hand -= "attack"
           g.previousCard = Some("attack")
           g.wasNoped = false
           if(g.inAttack) g.multiAttack = true else g.multiAttack = false
           g.inAttack = true
           g.nextPlayer
           announce(g, p.name + " attacks "+ g.currentPlayer.get.name + "!")
           notifyPlayer(g)
         }
       }
     }
     
     onCommand("skip") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         val g = game.get
         val p = g.currentPlayer.get
         if(!p.hand.contains("skip")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           p.hand -= "skip"
           g.previousCard = Some("skip")
           g.wasNoped = false
           
           if(g.inAttack) g.inAttack = false
           else g.nextPlayer
           announce(g, p.name + " skipped their turn!")
           notifyPlayer(g)
         }
       }
     }
     
     onCommand("future") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         val g = game.get
         val p = g.currentPlayer.get
         if(!p.hand.contains("future")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           p.hand -= "future"
           g.previousCard = Some("future") // Future card can't be noped atm
           g.wasNoped = false
           announce(g, p.name + " looked at the future!")
           if(g.deck.size >= 3) send(ChatId.fromChat(msg.chat.id), "The next three cards are " + cardNames(g.deck(0)) + ", " + cardNames(g.deck(1)) + " and " + cardNames(g.deck(2)) )
           else if(g.deck.size == 2) send(ChatId.fromChat(msg.chat.id), "The next two cards are " + cardNames(g.deck(0)) + " and " + cardNames(g.deck(1))) 
           else send(ChatId.fromChat(msg.chat.id), "You know what it is going to be..." )
         }
       }
     }
     
     onCommand("shuffle") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         val g = game.get
         val p = g.currentPlayer.get
         if(!p.hand.contains("shuffle")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           p.hand -= "shuffle"
           g.previousCard = Some("shuffle")
           g.wasNoped = false
           g.beforeShuffle = Some(g.deck)
           g.deck = Random.shuffle(g.deck)
           announce(g, p.name + " shuffled the deck!")
         }
       }
     }
     
     onCommand("favor") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.isCurrentPlayer(msg.chat)){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
         val g = game.get
         val p = g.currentPlayer.get
         if(!p.hand.contains("favor")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           p.hand -= "favor"
           g.wasNoped = false
           val t = Random.shuffle(g.players.filterNot(_==p).filter(_.alive)).head
           if(t.hand.size>0){
             val c = Random.shuffle(t.hand).head
             t.hand -= c
             p.hand += c
             g.takenCard = Some(p, t, c)
             g.previousCard = Some("favor")
             announce(g, p.name + " uses a favor on " + t.name + "!")
             send(ChatId.fromChat(p.chat.id), "You stole " + cardNames(c) + " from " + t.name)
             send(ChatId.fromChat(t.chat.id), p.name + " randomly stole " + cardNames(c) + " from you!")
           } else {
             announce(g, p.name + " tried to use a favor, but " + t.name + "didn't have any cards!")
             g.previousCard = None
           }
           notifyPlayer(g)
         }
       }
     }
     
     onCommand("nope") { implicit msg =>
       val game = currentGame(msg.chat)
       if(game==None || !game.get.findPlayer(msg.chat).get.alive){
         send(ChatId.fromChat(msg.chat.id), "You can't do that now")
       } else {
      
         val g = game.get
         val u = g.findPlayer(msg.chat).get // The user of the nope
         val cp = g.currentPlayer // Current player
         if(!u.hand.contains("nope")){
           send(ChatId.fromChat(msg.chat.id), "You don't have that card")
         } else {
           g.previousCard match {
             case None => send(ChatId.fromChat(u.chat.id), "There's nothing to undo at the moment")
             case Some("skip") => {
               announce(g, u.name + " used a nope on the skip!")
               u.hand -= "nope"
               if(g.wasNoped){
                 g.wasNoped = false
                 g.nextPlayer
                 notifyPlayer(g)
               } else {
                 g.wasNoped = true
                 g.previousPlayer
                 notifyPlayer(g)
               }
             }
             case Some("attack") => {
               u.hand -= "nope"
               announce(g, u.name + " used a nope on the attack!")
               if(g.wasNoped){
                 g.wasNoped = false
                 g.nextPlayer
                 g.inAttack = true
                 notifyPlayer(g)
               } else {
                 g.wasNoped = true
                 if(g.multiAttack) g.inAttack = true else g.inAttack = false
                 g.previousPlayer
                 notifyPlayer(g)
               }
             }
             case Some("favor") => {
               u.hand -= "nope"
               announce(g, u.name + " used a nope on the favor!")
               val s = g.takenCard.get._1 // The one who took the card
               val v = g.takenCard.get._2 // The one who it was taken from
               val c = g.takenCard.get._3 // The card
               if(g.wasNoped){
                 g.wasNoped = false
                 s.hand += c
                 v.hand -= c
               } else {
                 g.wasNoped = true
                 s.hand -= c
                 v.hand += c
               }
               notifyPlayer(g)
             }
             case Some("shuffle") => {
               u.hand -= "nope"
               announce(g, u.name + " used a nope on the shuffle!")
               if(g.wasNoped){
                 g.wasNoped = false
                 val a = g.deck
                 val b = g.beforeShuffle.get
                 g.deck = b
                 g.beforeShuffle = Some(a)
               } else {
                 g.wasNoped = true
                 val a = g.deck
                 val b = g.beforeShuffle.get
                 g.deck = b
                 g.beforeShuffle = Some(a)
               }
             }
             case _ => send(ChatId.fromChat(u.chat.id), "Nothing happens. Undoing this action has not been implemented yet")
           }
         }
       }
     }
     
     // End of gameplay actions
     
def drawFromDeck(chat: Chat, g: Game) = {
         val p = g.currentPlayer.get
         
         val c = g.draw
         if(c == "bomb"){
           if(p.hand.contains("defuse")){
             p.hand -= "defuse"
             announce(g, p.name + " drew a bomb and defused it! 💣")
             g.addBomb
         
             if(g.inAttack){
               g.inAttack = false
             } else {
               g.nextPlayer
             }
           } else {
             p.alive = false
             g.inAttack = false
             announce(g, p.name + " drew a bomb and exploded! 💥")
             send(ChatId.fromChat(chat.id), "You lost. Better luck next time")
             g.nextPlayer
           }
         } else {
           p.hand += c
           announce(g, p.name + " drew a card")
           send(ChatId.fromChat(chat.id), "You got " + cardNames(c))
           if(g.inAttack){
             g.inAttack = false
           } else {
             g.nextPlayer
           }
         }
         
         g.previousCard = None
         g.wasNoped = false
         notifyPlayer(g)
  }
     
     /**
      * Alerts the player that it's their turn. Also tells them about attacks.
      * Handles game over situations.
      */
     def notifyPlayer(g: Game): Unit = {
       if(g.hasEnded){
         announce(g, g.players.find(_.alive).get.name + " won the game!")
         g.players.clear
      }else {
         var response = "It's your turn!"
         val p = g.currentPlayer
         if(p != None){
           if(g.inAttack) response += " You have been attacked, so you must draw two cards!"
           response += "\n\nYour hand:"
           response += p.get.handString
           response += "\n\nCards in deck: " + g.deck.size
           send(ChatId.fromChat(p.get.chat.id), response)
         }
       }
     }
     
     def announce(g: Game, s: String) = g.playerChats.foreach(i => send(ChatId.fromChat(i.id), s))
     
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
         val name = game.get.findPlayer(chat).get.name
         val isCurrent = game.get.isCurrentPlayer(chat)
         val r = game.get.leave(chat)
         if(game.get.playerCount==0) {
           games -= game.get
         } else {
           announce(game.get, name + " left the game")
           if(chat == game.get.hostChat && !game.get.hasStarted && game.get.playerCount>0) {
             game.get.hostChat = game.get.players.filter(_.chat!=game.get.hostChat).head.chat
             send(ChatId.fromChat(game.get.hostChat.id), "The host left the game. You are the new host")
           }
           if(isCurrent){
             game.get.nextPlayer
             notifyPlayer(game.get)
           } else if (game.get.hasEnded){
             notifyPlayer(game.get)
           }
         }
         r
       } else {
         false
       }
     }
     
     def send(chatId: ChatId, s: String) = {
       request(SendMessage(chatId, s, parseMode = Some(ParseMode.HTML)))
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
  val initialHandSize = 4 // 7 would be with all card types
  private def naming(m: Message) = {
    val t = m.chat.title
    if(t==None){
      if(m.from.get.lastName==None){
        m.from.get.firstName
      }else{
        m.from.get.firstName + " " + m.from.get.lastName.get
      }
    } else {
      t.get
    }
  }
  val players = Buffer[Player](new Player(creationMessage.chat, naming(creationMessage)))
  var hostChat = creationMessage.chat
  def playerCount = players.size
  def playerChats = players.map(_.chat) // List of chats
  
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
      
      players += new Player(msg.chat, naming(msg))
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
      } else if (hasStarted && player.get.alive) {
        players -= player.get
        deck -= "bomb"
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
  
  def isCurrentPlayer(chat: Chat) = {
    val c = currentPlayer
    if (c == None){
      false
    } else {
      chat == c.get.chat
    }
  }
  
  var previousCard: Option[String] = None
  var beforeShuffle: Option[Buffer[String]] = None
  var takenCard: Option[(Player, Player, String)] = None // (Stealer, Victim, Card)
  var wasNoped = false
  var inAttack = false
  var multiAttack = false // Remembers if previous player was attacked too
  
  /** Advances currentPlayer */ 
  def nextPlayer = {
    turnIndex = (turnIndex + 1) % playerCount
    while(!players(turnIndex).alive) turnIndex = (turnIndex + 1) % playerCount
  }
  
  def previousPlayer = {
    turnIndex = (turnIndex - 1)
    if(turnIndex<0) turnIndex = playerCount-1
    while(!players(turnIndex).alive) {
      turnIndex = turnIndex - 1
      if(turnIndex<0) turnIndex = playerCount-1
    }
  }
  
  var deck = Buffer[String]()  // Cards are added when the game starts
  
  /** Adds a bomb into the deck in a random position */
  def addBomb = {
    val n = Random.nextInt(deck.size)
    deck = (deck.take(n) :+ ("bomb")) ++: (deck.drop(n))
  }
  
  /**
   * Removes the topmost card from the deck and returns its value
   */
  def draw = {
    val card = deck.head
    deck.remove(0)
    card
  }
  
  def prepareCards(): Unit = {
    var newDeck = Buffer[String]()
    
    // Let's support huge games by duplicating these if we have a huge amount of players
    for(n <- 0 until (1 + (playerCount / 6))){
      for(i <- 0 until 4) newDeck.append("attack")
      for(i <- 0 until 4) newDeck.append("favor")
      for(i <- 0 until 5) newDeck.append("nope")
      for(i <- 0 until 5) newDeck.append("future")
      for(i <- 0 until 4) newDeck.append("shuffle")    
      for(i <- 0 until 4) newDeck.append("skip")
      //for(j <- 0 until 5; i <- 0 until 4) newDeck.append("critter" + i)
    }
    
    newDeck = Random.shuffle(newDeck)
    
    players.foreach(p => {
      p.hand ++= newDeck.take(initialHandSize)
      newDeck = newDeck.drop(initialHandSize)
      p.hand += "defuse"
    })
    
    
    for(i <- 0 until 7-playerCount) newDeck.append("defuse")
    for(i <- 0 until playerCount-1) newDeck.append("bomb")
    deck = Random.shuffle(newDeck)
  }
  
  
}

class Player(val chat: Chat, val name: String){
  val cardNames = collection.immutable.Map("bomb"->"Bomb", "defuse"->"Defuse", "attack"->"Attack", "skip"->"Skip", "nope"->"Nope", "shuffle"->"Shuffle", "future"->"Future", "favor"->"Favor", "critter1"->"C1", "critter2"->"C2", "critter3"->"C3", "critter4"->"C4", "critter5"->"C5")
  val hand = Buffer[String]()
  var alive = true
  def handString = {
    var s = ""
    for(i <- cardNames.keys){
      if(hand.contains(i)) s += "\n" + hand.count(a => a==i) + "x " + cardNames(i)
    }
    s
  }
}
