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
    //self ! Mail("Bogdan Isaev", "redeeming.fury@gmail.com", "subject","Some message")
  }
  override def receive: Receive = {
    case mailInfo: Mail =>
      val mailer = MailerBuilder
        .withSMTPServer("smtp.mail.ru", 465, "noreply@3deg.pro", "Ship1234")
        .withTransportStrategy(TransportStrategy.SMTPS)
        .withDebugLogging(true)
        .buildMailer()

      val mail = EmailBuilder.startingBlank()
        .from("3deg.pro", "noreply@3deg.pro")
        .to(mailInfo.toUser, mailInfo.toMail)
        .withSubject("3deg.pro status info")
        .withHTMLText(mailInfo.message)
        .buildEmail()

      mailer.sendMail(mail);
    case _ => None
  }
}
