package controllers

import play.api.mvc._
import play.api._
import models.Post
import models.Category
import models.Comment
import models.S3File

object Blog extends Controller {

  val pageSize = 5

  def index(page: Int) = Action {
    val posts = Post.find((page - 1) * pageSize, pageSize)
    Ok(views.html.blog(posts, page, (Post.count() / pageSize).toInt, 
        None, Category.getPostCounts(), Post.random()))
  }

  def category(cat: String, page: Int) = Action {
    Category.findById(cat) match {
      case Some(c) =>
        val posts = Post.find((page - 1) * pageSize, pageSize, Some(c))
        Ok(views.html.blog(posts, page, 
            (Post.count(Some(c)) / pageSize).toInt, Some(c), 
            Category.getPostCounts(), Post.random()))
      case None => NotFound
    }
  }

  def show(id: String) = Action {
    Post.findById(id) match {
      case Some(post) => Ok(views.html.show(post, 
          Category.getPostCounts(), Post.random()))
      case None => NotFound
    }
  }

  def news(id: String) = Action {
    Redirect(routes.Blog.show(id))
  }

  def archive = Action {
    Ok(views.html.archive(Post.findPerMonth(), Post.count(), 
        Post.first(), Category.getPostCounts(), Post.random()))
  }

  def maps = Action {
    Ok(views.html.maps())
  }

  def about = Action {
    Ok(views.html.about())
  }

  def file(path: String) = Action {
    Redirect(S3File.getUrl(path))
  }
}