package controllers

import play.api._
import play.api.mvc._
import models.Post

object Start extends Controller {

  def index = Action {
    Ok(views.html.start(Post.find(amount = 4)))
  }

}