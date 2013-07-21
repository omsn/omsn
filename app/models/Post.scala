package models
import play.api.db._
import play.api.libs.json._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import java.util.Date
import java.text.SimpleDateFormat
import java.util.GregorianCalendar
import play.Logger
import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.ListMap
import java.util.TimeZone
import org.joda.time.DateTime
import org.joda.time.DateTimeZone

case class Post(id: Pk[String], created: Long, category: Category, title: String, html: String) {

  /**
   * check http://docs.oracle.com/javase/1.4.2/docs/api/java/text/SimpleDateFormat.html
   * for format syntax
   */
  def createdFormat(format: String) = {
    val calendar = new GregorianCalendar()
    calendar.setTimeInMillis(created * 1000) // timestamps are stored in the DB as seconds since Jan 1st 1970
    val sdf = new SimpleDateFormat(format)
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    sdf.format(calendar.getTime())
  }
}

object Post {

  val pageSize = 5

  val simple = {
    get[Pk[String]]("id") ~
      get[Long]("created") ~
      get[String]("categoryId") ~
      get[String]("title") ~
      get[String]("html") map {
        case id ~ created ~ categoryId ~ title ~ html => 
          Post(id, created, Category.findById(categoryId).get, title, html)
      }
  }

  def findAll(): Seq[Post] = {
    DB.withConnection { implicit connection =>
      SQL("select id, created, categoryId, title from post order by created desc").as(
        get[Pk[String]]("id") ~
          get[Long]("created") ~
          get[String]("categoryId") ~
          get[String]("title") map {
            case id ~ created ~ categoryId ~ title => 
              Post(id, created, Category.findById(categoryId).get, title, "")
          } *)
    }
  }

  def findPerMonth(): Map[String, Seq[Post]] = {
    DB.withConnection { implicit connection =>
      ListMap( // preserves insertion order
        SQL("select * from post order by created desc")
          .as(Post.simple *)
          .map(p => (p.createdFormat("yyyyMM"), p))
          .groupBy(_._1) // find all posts with same yyyyMM, group them into lists with yyyyMM as key
          .toSeq // back to sequence, for the ListMap constructor
          .map(t => (t._1, t._2.map(_._2))) // remove now unnecessary yyyyMM from value tuples
          .sortBy(_._1)(Ordering[String].reverse): _* // order by reverse string order of yyyyMM 
          )
    }
  }

  def find(offset: Int = 0, amount: Int = pageSize, cat: Option[Category] = None): Seq[Post] = {
    DB.withConnection { implicit connection =>
      SQL("select * from post"
        + cat.map(cat => " where categoryId = '" + cat.id.get + "'").getOrElse("")
        + " order by created desc limit {limit} offset {offset}")
        .on(
          'offset -> offset,
          'limit -> amount)
        .as(Post.simple *)
    }
  }

  def first(): Post = {
    DB.withConnection { implicit connection =>
      SQL("select * from post where created = (select min(created) from post)").as(Post.simple.single)
    }
  }

  def count(cat: Option[Category] = None): Long = {
    DB.withConnection { implicit connection =>
      SQL("select count(*) as count from post" + cat.map(" where categoryId = '" + _.id.get + "'").getOrElse("")).apply().head[Long]("count")
    }
  }

  def findById(id: String): Option[Post] = {
    DB.withConnection { implicit connect =>
      SQL("select * from post where id = {id}").on('id -> id).as(Post.simple.singleOpt)
    }
  }

  def random(amount: Int = 5): Seq[Post] = random(Seq(), amount)

  def random(posts: Seq[Post], amount: Int): Seq[Post] = {
    if (posts.length == amount)
      posts
    else
      random(posts ++ Seq(randomPost()), amount)
  }

  def randomPost(): Post = {
    DB.withConnection { implicit connection =>
      SQL("select * from post offset random() * (select count(id) from post) limit 1").as(Post.simple.single)
    }
  }

  def create(id: String, category: Category, title: String, html: String): Boolean = {
    DB.withConnection { implicit connection =>
      SQL("""
          insert into post (id, created, categoryId, title, html) 
          values ({id}, {created}, {categoryId}, {title}, {html})
          """)
        .on('id -> id,
          'created -> new DateTime(DateTimeZone.UTC).getMillis() / 1000,
          'categoryId -> category.id.get,
          'title -> title,
          'html -> html)
        .executeUpdate() == 1
    }
  }

  def update(id: String, category: Option[Category] = None, title: Option[String] = None, html: Option[String] = None): Boolean = {
    DB.withConnection { implicit connection =>
      var should = 0
      var inserts = 0

      val categoryId = category.map(_.id.get)

      Map("categoryId" -> categoryId, "title" -> title, "html" -> html) foreach { t =>
        if (t._2.isDefined) {
          should += 1
          val sql = SQL("update post set " + t._1 + " = {val} where id = {id}").on('id -> id, 'val -> t._2.get)
          inserts += sql.executeUpdate()
        }
      }

      inserts == should
    }
  }

  implicit object PostFormat extends Format[Post] {

    def reads(json: JsValue): Post = null // TODO

    def writes(post: Post) = JsObject(Seq(
      "id" -> JsString(post.id.get),
      "created" -> JsNumber(post.created),
      "category" -> JsString(post.category.title),
      "categoryId" -> JsString(post.category.id.get),
      "title" -> JsString(post.title),
      "html" -> JsString(post.html)))
  }
}
