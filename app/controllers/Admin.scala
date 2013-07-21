package controllers

import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Date
import org.apache.commons.codec.binary.Hex
import models.Post
import play.api.libs.json._
import play.api.mvc._
import play.api._
import java.util.TimeZone
import models.Category
import models.S3File
import scala.parallel.Future
import scala.parallel.Future
import util.ImageResizer
import play.api.mvc.MultipartFormData.FilePart
import play.api.libs.Files.TemporaryFile

object Admin extends Controller {

  def list(accessToken: Option[String]) = Action { implicit request =>
    authorize(request, Ok(Json.toJson(Post.findAll())))
  }

  def detail(id: String, accessToken: Option[String]) = Action { implicit request =>
    authorize(request,
      Post.findById(id) match {
        case Some(post) => Ok(Json.toJson(post))
        case None => NotFound(Json.toJson(Map("error" -> ("no post found with id " + id))))
      })
  }

  def create() = Action { implicit request =>
    authorize(request,
      request.body.asFormUrlEncoded.map { pairs =>
        val data = {
          for (property <- Seq("id", "categoryId", "title", "html"))
            yield (property, pairs.get(property).map(_(0)))
        }.toMap

        if (data.values.exists(_ == None)) {
          BadRequest(Json.toJson(Map("error" -> "missing required param")))
        } else {
          val id = data.get("id").get.get
          Post.findById(id) match {
            case Some(post) => BadRequest(Json.toJson(
                Map("error" -> ("post with id '" + id + "' already exists"))))
            case None => {
              val categoryId = data.get("categoryId").get.get
              Category.findById(categoryId) match {
                case None => NotFound(Json.toJson(
                    Map("error" -> ("no category with id '" + categoryId + "' found"))))
                case Some(category) => {
                  val title = data.get("title").get.get
                  val html = data.get("html").get.get
                  val success = Post.create(id, category, title, html)
                  Logger.debug(success.toString())
                  if (success) {
                    Ok(Json.toJson("success"))
                  } else {
                    InternalServerError(Json.toJson("SQL insert returned false"))
                  }
                }
              }
            }
          }
        }
      }.getOrElse(
        BadRequest(Json.toJson(Map("error" -> "missing required param")))))
  }

  def update(id: String) = Action { implicit request =>
    authorize(request,
      request.body.asFormUrlEncoded.map { pairs =>
        val category = {
          val categoryId = pairs.get("categoryId").map(_(0))
          if (categoryId.isDefined) {
            Category.findById(categoryId.get)
          } else {
            None
          }
        }
        if (Post.update(id, category, pairs.get("title").map(_(0)), 
            pairs.get("html").map(_(0)))) {
          Ok(Json.toJson("success"))
        } else {
          InternalServerError(Json.toJson("SQL update returned false"))
        }
      }.getOrElse(
        BadRequest(Json.toJson(Map("error" -> "missing required param")))))
  }

  def image(mode: String) = Action { implicit request =>
    authorize(request,
      request.body.asMultipartFormData.map { data =>
        if (Seq("asis", "cover", "thumbs") contains mode == false)
          BadRequest(Json.toJson(Map("error" -> ("mode " + mode + " is unknown"))))
        else
          Ok(Json.toJson(Map("success" -> s3store(mode, data.files))))
      }.getOrElse(
        BadRequest(Json.toJson(Map("error" -> "missing required param")))))
  }

  def s3store(mode: String, files: Seq[FilePart[TemporaryFile]]): Seq[String] = 
    s3store(mode, files, Seq())

  def s3store(mode: String, files: Seq[FilePart[TemporaryFile]], 
      paths: Seq[String]): Seq[String] = {
    files match {
      case Nil => paths
      case file :: rest =>
        mode match {
          case "asis" =>
            val path = S3File.create(file.ref.file, file.filename)
            s3store(mode, rest, paths ++ Seq(path))
          case "cover" =>
            val path = S3File.create(ImageResizer.resize(file.ref.file, 600), file.filename)
            s3store(mode, rest, paths ++ Seq(path))
          case "thumbs" =>
            val thumbFilename = file.filename.substring(0, file.filename.indexOf(".jpg")) + "-thumb.jpg"
            val path1 = S3File.create(ImageResizer.resize(file.ref.file, 120), thumbFilename)
            val path2 = S3File.create(file.ref.file, file.filename)
            s3store(mode, rest, paths ++ Seq(path2, path1))
        }
    }
  }

  val salt = Play.current.configuration.getString("admin.security.salt").get
  val hashAlgorithm = Play.current.configuration.getString("admin.hash.algorithm").get
  val paramName = Play.current.configuration.getString("admin.param.name").get
  val dateFormat = Play.current.configuration.getString("admin.date.format").get
  val sdf = new SimpleDateFormat(dateFormat)
  sdf.setTimeZone(TimeZone.getTimeZone("UTC"))

  def authorize(request: Request[AnyContent], res: SimpleResult[JsValue]): Result = {
    val msg = salt + sdf.format(new Date().getTime())
    val hash = MessageDigest.getInstance(hashAlgorithm).digest(msg.getBytes())
    val should = Hex.encodeHex(hash).mkString
    val is = request.headers.get(paramName).getOrElse("")
    if (is == should)
      res
    else
      Unauthorized(Json.toJson(Map("error" -> "unauthorized request")))
  }
}