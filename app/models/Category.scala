package models

import play.api.db._
import play.api.Play.current;

import anorm._
import anorm.SqlParser._
import java.util.Date

case class Category(id: Pk[String], title: String)

object Category {

  val simple = {
    get[Pk[String]]("id") ~
      get[String]("title") map {
        case id ~ title => Category(id, title)
      }
  }

  def findById(id: String): Option[Category] = {
    DB.withConnection { implicit connection =>
      SQL("select * from category where id = {id}").on(
        'id -> id).as(Category.simple.singleOpt)
    }
  }

  def create(cat: Category): Unit = {
    DB.withConnection { implicit connection =>
      SQL("insert into bar(id, title) values ({id}, {title})").on(
        'id -> cat.id,
        'title -> cat.title).executeUpdate()
    }
  }

  def getPostCounts(): List[(Category, Long)] = {
    DB.withConnection { implicit connection =>
      val qry = SQL("""
          select c.id, c.title, count(p.id) as count 
          from category c, post p 
          where p.categoryId = c.id 
          group by c.id 
          order by c.title
          """)
      qry().map(row => (Category(row[Pk[String]]("id"), row[String]("title")), row[Long]("count"))).toList
    }
  }

}