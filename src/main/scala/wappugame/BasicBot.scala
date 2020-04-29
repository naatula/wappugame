package wappugame

import info.mukel.telegrambot4s._

import api._
import methods.{SendMessage, _}
import models.{InlineKeyboardButton, InlineKeyboardMarkup, _}
import declarative._

/**
  * A wrapper class that wraps more complicated or advanced functionality such as loading images and making
  * actual requests to Telegram API.
  * It is not necessary for the students taking this course to understand the contents of this file.
  */
class BasicBot extends TelegramBot with Polling with Commands with Callbacks {


  // Housekeeping: token, URLS, etc.
  def token = scala.io.Source.fromFile("bot_token.txt").mkString.trim

  private var chatId: ChatId = _
  private var message: Message = _
  private var data: String = _
  private var messageId: Int = _

  type Button = InlineKeyboardButton
  type Message = info.mukel.telegrambot4s.models.Message

 /**
  * Extracts the text from a Message object.
  *
  * @param msg A message containing text
  * @return The text in the message object. It it is a command, take only the parameter part as one long String.
  */

  def getString(msg: Message) = {
    val str = msg.text.getOrElse("")
    if (str.startsWith("/"))
      str.dropWhile(_ != ' ').drop(1) // Drops everything until the first space, and then the space itself
    else
      str
    }

  /**
   * Extracts the first name (if any) of a user that sent a message.
   *
   * @param msg The message sent by the user
   * @return The username of the user
   */

  def getUserFirstName(msg: Message) = msg.from.map(_.firstName).mkString

  /**
   * Extracts a chat id from a message.
   *
   * A chat id can be used to later write text to the chat using the writeToChat
   * method.
   *
   * @param msg any message written into the chat
   * @return the chat id
   */

  def getChatId(msg: Message) = msg.chat.id

  /**
    * Reacts and responds to a named command with arguments, i.e. extra words after command,
    * using the function provided as a parameter.
    *
    * @param command The name of the command, e.g. "hello" for the command "/hello"
    * @param action  A method that reacts to arguments and returns a string to send as a reply
    * @return
    */

  def commandWithArguments(command: String, action: Seq[String] => String) = {
    onCommand(command) { implicit msg =>
      withArgs {
        args => request(SendMessage(msg.chat.id, action(args), parseMode = Some(ParseMode.HTML)))
      }
    }
  }

  /**
    * Reacts and responds to commands without arguments
    *
    * @param command The name of the command, e.g. "hello" for the command "/hello"
    * @param action  A method that returns a string to send as a reply
    * @return
    */
  def command(command: String, action: Message => String) = {
    onCommand(command) { implicit msg => request(SendMessage(ChatId.fromChat(msg.chat.id), action(msg), parseMode = Some(ParseMode.HTML))) }
  }

  /**
    * Reacts to any messages on the channel (Note that most channels don't allow this)
    *
    * @param action  A method that reacts in some way to a string on the channel
    * @return
    */
  def anyString(action: String => Unit) = onMessage {
    implicit msg => action(msg.text.mkString)
  }

  /**
    * Reacts to any messages on the channel sending back a new string. (Note that most channels don't allow this)
    *
    * @param action  A method that takes in a string and returns a new one
    * @return
    */

  def replyToString(action: String => String) = onMessage {
    implicit msg => reply(action(msg.text.mkString))
  }

  /**
    * Reacts to any messages on the channel sending back a new string. (Note that most channels don't allow this, but only give commands to bots)
    *
    * @param action  A method that takes in a message and returns a string as a reaction
    * @return
    */

  def replyToMessage(action: Message => String) = onMessage {
    implicit msg => reply(action(msg))
  }


  /**
    * Reacts to any messages on the channel. (Note that most channels don't allow this, but only give commands to bots)
    *
    * @param action  A method that takes in a message. Note that the message object can be used to reply.
    * @return
    */

  def anyMessage(action: Message => Unit)   = onMessage {
    implicit msg => action(msg)
  }

  /**
    * Reacts to new users joining the channel.
    *
    * @param action  A method that takes in a new user and returns a string to be broadcasted on the channel.
    * @return
    */

  def joinMessage(action: User => String) = onMessage {
    implicit msg => for {
      members <- msg.newChatMembers
      member  <- members
    } reply(action(member))
  }

  /**
    * Reacts to users leaving the channel.
    *
    * @param action  A method that takes in a user and does something with the information.
    * @return
    */

  def leaveMessage(action: User => Unit) = onMessage {
    implicit msg => {
      msg.leftChatMember.foreach {
        user => action(user)
      }
    }
  }


  /**
    * Creates a message with a so called inline keyboard, i.e. a keyboard embedded to the message.
    *
    * @param command      The name of the command
    * @param replyMessage The textual message accompanying the keyboard
    * @param keyboard     The keyboard as a two-dimensional sequence of buttons. The first sequence represents rows,
    *                     the inner sequence represents columns within rows.
    * @return
    */
  def commandWithInlineKeyboard(command: String, replyMessage: String, keyboard: Seq[Seq[Button]]) = {
    onCommand(command) { implicit msg =>
      val kb = InlineKeyboardMarkup.apply(keyboard)
      request(SendMessage(ChatId.fromChat(msg.chat.id), replyMessage, replyMarkup = Some(kb), parseMode = Some(ParseMode.HTML)))
    }

  }

  /**
    * Creates a new button that takes the user the specified URL when pressed
    *
    * @param text The text on the button
    * @param url  The URL the button links to
    * @return A button with the given text and URL
    */
  def createURLButton(text: String, url: String): Button = InlineKeyboardButton.url(text, url)

  /**
    * Creates a button, the pressing of which, causes a change in the keyboard.
    * @param text The text on the button
    * @param data The data the button sends upon being pressed. This can be used to e.g. determine, which button
    *             was pressed if there are many
    */
  def createActiveButton(text: String, data: String): Button = InlineKeyboardButton.callbackData(text, data)


  /**
    * Listens to and handles so-called callback queries originating from clicks on active buttons.
    * @param action A method taking a string parameter (the data in the callback query) and does something with it.
    * @return
    */
  def onCallback(action: String => Any) = {
    onCallbackQuery { implicit cbq =>
      // Saving data related to the message for when we need to send the user something back.
        data      = cbq.data.get
        message   = cbq.message.get
        chatId    = ChatId.fromChat(message.chat.id)
        messageId = message.messageId

        action(data)


    }
  }

  /**
    * Updates the keyboard with the given keyboard layout
    * @param keyboard The layout of the new keyboard as a two-dimesional collection of buttons.
    * @return
    */

  def updateKeyboard(keyboard: Seq[Seq[Button]]) = {
    request(EditMessageReplyMarkup(chatId = Some(chatId), messageId = Some(messageId), replyMarkup = Some(InlineKeyboardMarkup.apply(keyboard))))

    }

  def updateMessage(newText: String) = {
    request(methods.EditMessageText(Some(chatId), Some(messageId), text = newText))
  }


  /**
    * Sends the user a message with the given text. Can be used at any time.
    *
    * Note that chat ids can be extracted from any message using getChatId(message)
    *
    * @param text The text of the message to be sent to the user.
    * @return
    * @see getChatId(msg: Message)
    */
  def writeToChat(text: String, selectedChatId: ChatId) = {
    request(SendMessage(selectedChatId, text, parseMode = Some(ParseMode.HTML)))
  }

  // Use writeToChat
  def respond(text: String) = {
    request(SendMessage(chatId, text, parseMode = Some(ParseMode.HTML)))
  }
}
