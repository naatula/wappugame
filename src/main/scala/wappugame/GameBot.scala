package wappugame

import info.mukel.telegrambot4s._

import api._
import methods.{SendMessage, _}
import models.{InlineKeyboardButton, InlineKeyboardMarkup, _}
import declarative._

object GameApp extends App {
   val bot = new GameBot()
   bot.run()
   println("Started")
}

class GameBot extends BasicBot {

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


     onCommand("new") { implicit msg =>
        val arguments = msg.text.get.split(" ")
        try {
          request(SendMessage(ChatId.fromChat(msg.chat.id), "Test"))

        } catch  {
          case e: NumberFormatException => {
            errorReply(msg.chat, "Invalid number error")
          }
          case e: ArrayIndexOutOfBoundsException => {
            errorReply(msg.chat, "Too few arguments error")
          }
          case e: IllegalArgumentException => {
            errorReply(msg.chat, "Invalid arguments error")
          }
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
