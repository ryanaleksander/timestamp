package controllers

import play.api._
import play.api.libs.json.{JsNull, Json}
import play.api.mvc._
import org.joda.time._
import scala.util.matching.Regex

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def parse(time: String) = Action {
	  val unixPattern = new Regex("^([0-9]+)$")
	  val naturalPattern = new Regex("^([a-zA-z]+) ([0-9]{1,2}), ([0-9]{4})$")

	  time match {
		  case unixPattern(unixTime) =>
			  Ok(Json.obj(
				  "unix" -> unixTime.toLong,
				  "natural" -> Date.parse(unixTime.toLong * 1000)
			  ))
		  case naturalPattern(month, day, year) =>
			  val unixTime = Date.unixParse(Date.getMonth(month) getOrElse -1, day.toInt, year.toInt)
			  unixTime match {
				  case Some(t) =>
					  Ok(Json.obj(
						  "unix" -> t,
						  "natural" -> time
					  ))
				  case None =>
					  Ok(Json.obj(
						  "unix" -> JsNull,
						  "natural" -> JsNull
					  ))
			  }
		  case _ =>
			  Ok(Json.obj(
				  "unix" -> JsNull,
				  "natural" -> JsNull
			  ))
	  }
  }
}

object Date {
	def parse(time: Long): String = {
		import java.util.Calendar
		val cal = Calendar.getInstance
		val date = new java.util.Date(time)
		cal.setTime(date)
		val month = cal.get(Calendar.MONTH)
		val day = cal.get(Calendar.DATE)
		val year = cal.get(Calendar.YEAR)

		var naturalDate = ""
		naturalDate += (month match {
			case 0 => "January"
			case 1 => "February"
			case 2 => "March"
			case 3 => "April"
			case 4 => "May"
			case 5 => "June"
			case 6 => "July"
			case 7 => "August"
			case 8 => "September"
			case 9 => "October"
			case 10 => "November"
			case 11 => "December"
		})

		naturalDate += s" ${day + 1}, $year"
		naturalDate
	}
	def getMonth(month: String): Option[Int] = {
		month match {
			case "January" => Some(0)
			case "February" => Some(1)
			case "March" => Some(2)
			case "April" => Some(3)
			case "May" => Some(4)
			case "June" => Some(5)
			case "July" => Some(6)
			case "August" => Some(7)
			case "September" => Some(8)
			case "October" => Some(9)
			case "November" => Some(10)
			case "December" => Some(11)
			case _ => None
		}
	}

	def unixParse(month: Int, day: Int, year: Int): Option[Long] = {
		if (month >= 0 && day > 0 && year > 0) {
			import java.util.Calendar._
			val cal = getInstance
			cal.set(MONTH, month)
			cal.set(DATE, day)
			cal.set(YEAR, year)
			println(cal.get(MILLISECOND))
			Some(math.ceil(cal.getTimeInMillis / 1000).toLong)
		} else None
	}
}