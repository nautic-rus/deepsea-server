package deepsea.mail

import akka.actor.Actor
import deepsea.mail.MailManager.Mail
import org.simplejavamail.api.mailer.config.TransportStrategy
import org.simplejavamail.email.EmailBuilder
import org.simplejavamail.mailer.MailerBuilder

object MailManager{
  case class Mail(toUser: String, toMail: String, subject: String, message: String)
}
class MailManager extends Actor{
  override def preStart(): Unit = {
//    self ! Mail("Bogdan Isaev", "redeeming.fury@gmail.com", "subject", "Some message")
  }
  override def receive: Receive = {
    case mailInfo: Mail =>
      val mailer = MailerBuilder
        .withSMTPServer("mail.nautic-rus.com", 25, "noreply@nautic-rus.com", "9W5z]ateUZ")
        .withTransportStrategy(TransportStrategy.SMTP)
        .withDebugLogging(true)
        .buildMailer()

      val mail = EmailBuilder.startingBlank()
        .from("Nautic Rus", "noreply@nautic-rus.com")
        .to(mailInfo.toUser, mailInfo.toMail)
        .withSubject("Nautic Rus Notification")
        .withHTMLText(mailInfo.message)
        .buildEmail()

      mailer.sendMail(mail);
    case _ => None
  }
}
